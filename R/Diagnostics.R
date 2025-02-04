# Copyright 2024 Observational Health Data Sciences and Informatics
#
# This file is part of SelfControlledCaseSeries
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# source("~/git/SelfControlledCaseSeries/R/SccsDataConversion.R")

computeOutcomeRatePerMonth <- function(studyPopulation, sccsModel = NULL) {
  # Computes not only the observed outcome rate per calendar month, but also
  # observed / expected rate assuming everything is constant, and if sccsModel is
  # provided, observed / expected using the adjustments in the model.
  # To compute the expected rate for a month, we must consider only those cases
  # observed during that month, summing their respective incidence rates.

  if (nrow(studyPopulation$cases) == 0) {
    result <- tibble(
      month = 1.0,
      observedCount = 1.0,
      observationPeriodCount = 1.0,
      ratio = 1.0,
      adjustedRatio = 1.0,
      monthStartDate = as.Date("2000-01-01"),
      monthEndDate = as.Date("2000-01-01")) %>%
      filter(month == 0)
    return(result)
  }
  cases <- studyPopulation$cases %>%
    mutate(startDate = .data$observationPeriodStartDate + .data$startDay,
           endDate = .data$observationPeriodStartDate + .data$endDay) %>%
    mutate(startMonth = convertDateToMonth(.data$startDate),
           endMonth = convertDateToMonth(.data$endDate),
           startMonthFraction = computeMonthFraction(.data$startDate, TRUE),
           endMonthFraction = computeMonthFraction(.data$endDate))  %>%
    select("caseId", "startMonth", "endMonth", "startMonthFraction", "endMonthFraction") %>%
    left_join(
      studyPopulation$outcomes %>%
        group_by(.data$caseId) %>%
        summarize(outcomeCount = n()),
      by = join_by("caseId")
    ) %>%
    mutate(outcomeCount = if_else(is.na(.data$outcomeCount), 0, .data$outcomeCount)) %>%
    mutate(rate = .data$outcomeCount / (.data$endMonth - .data$startMonth + .data$startMonthFraction + .data$endMonthFraction))
  hasAdjustment <- FALSE
  monthAdjustments <- tibble(month = seq(min(cases$startMonth), max(cases$endMonth)),
                             totalRr = 1)
  if (!is.null(sccsModel) && hasCalendarTimeEffect(sccsModel)) {
    hasAdjustment <- TRUE
    estimates <- sccsModel$estimates
    splineCoefs <- estimates[estimates$covariateId >= 300 & estimates$covariateId < 400, "logRr"]
    calendarTimeKnotsInPeriods <- sccsModel$metaData$calendarTime$calendarTimeKnotsInPeriods
    designMatrix <- createMultiSegmentDesignMatrix(x = monthAdjustments$month,
                                                   knotsPerSegment = calendarTimeKnotsInPeriods)
    monthAdjustments <- monthAdjustments %>%
      mutate(calendarTimeRr = exp(apply(designMatrix %*% splineCoefs, 1, sum))) %>%
      mutate(totalRr = .data$totalRr * .data$calendarTimeRr)
  }
  if (!is.null(sccsModel) && hasSeasonality(sccsModel)) {
    hasAdjustment <- TRUE
    estimates <- sccsModel$estimates
    splineCoefs <- estimates[estimates$covariateId >= 200 & estimates$covariateId < 300, "logRr"]
    seasonKnots <- sccsModel$metaData$seasonality$seasonKnots
    season <- 1:12
    seasonDesignMatrix <- cyclicSplineDesign(season, seasonKnots)
    logRr <- apply(seasonDesignMatrix %*% splineCoefs, 1, sum)
    monthAdjustments <- monthAdjustments %>%
      mutate(monthOfYear = .data$month %% 12 + 1) %>%
      inner_join(
        tibble(
          monthOfYear = season,
          seasonRr = exp(logRr)
        ),
        by = join_by("monthOfYear")
      ) %>%
      select(-"monthOfYear") %>%
      mutate(totalRr = .data$totalRr * .data$seasonRr)
  }
  if (hasAdjustment) {
    # Need to correct for the fact that a person may have seen only part of the spline, so
    # techincally has a different intercept:
    cases <- cases %>%
      mutate(correction = computeCorrections(cases, monthAdjustments))
  } else {
    cases <- cases %>%
      mutate(correction = 1)
  }
  observedCounts <- studyPopulation$outcomes %>%
    inner_join(studyPopulation$cases, by = join_by("caseId")) %>%
    transmute(month = convertDateToMonth(.data$observationPeriodStartDate + .data$outcomeDay)) %>%
    group_by(.data$month) %>%
    summarise(observedCount = n())
  computeExpected <- function(monthAdjustment) {
    month <- monthAdjustment$month
    cases %>%
      filter(month >= .data$startMonth & month <= .data$endMonth) %>%
      mutate(weight = if_else(month == .data$startMonth,
                              .data$startMonthFraction,
                              if_else(month == .data$endMonth,
                                      .data$endMonthFraction,
                                      1))) %>%
      summarize(month = !!month,
                expectedCount = sum(.data$weight * .data$rate),
                adjustedExpectedCount = if_else(monthAdjustment$totalRr == 0,
                                                0,
                                                sum(.data$weight * .data$rate * monthAdjustment$totalRr / .data$correction)),
                observationPeriodCount = sum(.data$weight)) %>%
      return()
  }
  expectedCounts <- bind_rows(lapply(split(monthAdjustments, seq_len(nrow(monthAdjustments))), computeExpected))

  data <- expectedCounts %>%
    left_join(observedCounts, by = join_by("month")) %>%
    mutate(observedCount = if_else(is.na(.data$observedCount), 0, .data$observedCount)) %>%
    mutate(ratio = if_else(.data$observedCount == 0, .data$expectedCount == 0, 1, .data$observedCount / .data$expectedCount))  %>%
    mutate(adjustedRatio = if_else(.data$observedCount == 0, .data$adjustedExpectedCount == 0, 1, .data$observedCount / .data$adjustedExpectedCount))  %>%
    mutate(monthStartDate = convertMonthToStartDate(.data$month),
           monthEndDate = convertMonthToEndDate(.data$month)) %>%
    select(-"expectedCount")
  return(data)
}

#' Compute stability of outcome rate over time
#'
#' @details
#' Computes for each month the observed and expected count, and computes the (weighted) mean ratio between the two. If
#' splines are used to adjust for seasonality and/or calendar time, these adjustments are taken into consideration when
#' considering the expected count. A one-sided p-value is computed against the null hypothesis that the ratio is smaller
#' than `maxRatio`. If this p-value exceeds the specified alpha value, the series is considered stable.
#'
#' @template StudyPopulation
#' @param sccsModel         Optional: A fitted SCCS model as created using [fitSccsModel()]. If the
#'                          model contains splines for seasonality and or calendar time these will be adjusted
#'                          for before computing stability.
#' @param maxRatio          The maximum global ratio between the observed and expected count.
#' @param alpha             The alpha (type 1 error) used to test for stability.
#'
#' @return
#' A tibble with one row and three columns: `ratio` indicates the estimated mean ratio between observed and expected.
#' `p` is the p-value against the null-hypothesis that the ratio is smaller than `maxRatio`, and `stable` is `TRUE`
#' if `p` is greater than `alpha`.
#'
#' @export
computeTimeStability <- function(studyPopulation, sccsModel = NULL, maxRatio = 1.25, alpha = 0.05) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(studyPopulation, min.len = 1, add = errorMessages)
  checkmate::assertClass(sccsModel, "SccsModel", null.ok = TRUE, add = errorMessages)
  checkmate::assertNumeric(maxRatio, lower = 1, len = 1, add = errorMessages)
  checkmate::assertNumber(alpha, lower = 0, upper = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  data <- computeOutcomeRatePerMonth(studyPopulation, sccsModel)
  if (nrow(data) < 2) {
    result <- tibble(ratio = NA,
                     p = 1,
                     stable = TRUE)
    return(result)
  }
  o <- data$observedCount
  e <- data$adjustedExpectedCount
  e[e == 0] <- .Machine$double.eps

  # logLikelihood <- function(x) {
  #   return(-sum(log(dpois(o, e*x) + dpois(o, e/x))))
  # }
  # From https://cdsmithus.medium.com/the-logarithm-of-a-sum-69dd76199790
  smoothMax <- function(x, y) {
    return(ifelse(abs(x-y) > 100, pmax(x,y), x + log(1 + exp(y-x))))
  }
  logLikelihood <- function(x) {
    return(-sum(smoothMax(dpois(o, e*x, log = TRUE), dpois(o, e/x, log = TRUE))))
  }
  likelihood <- function(x) {
    return(exp(-logLikelihood(x)))
  }
  vectorLikelihood <- function(x) {
    return(sapply(x, likelihood))
  }
  x <- seq(1, 10, by = 0.1)
  ll <- sapply(x, logLikelihood)
  maxX <- x[max(which(!is.na(ll) & !is.infinite(ll)))]
  minX <- x[min(which(!is.na(ll) & !is.infinite(ll)))]
  xHat <- optim(1.5, logLikelihood, lower = minX, upper = maxX, method = "L-BFGS-B")$par
  l0 <- integrate(vectorLikelihood, lower = 1, upper = maxRatio)$value
  l1 <- integrate(vectorLikelihood, lower = maxRatio, upper = Inf)$value
  llr <- 2*(log(l1) - log(l0))
  if (is.nan(llr)) {
    if (xHat > maxRatio) {
      p <- 0
    } else {
      p <- 1
    }
  } else {
    p <- pchisq(llr, 1, lower.tail = FALSE)
  }
  result <- tibble(ratio = xHat,
                   p = p,
                   stable = p > alpha)
  return(result)
}

#' Compute P for pre-exposure risk gain
#'
#' @param exposureEraId       The exposure to create the era data for. If not specified it is
#'                            assumed to be the one exposure for which the data was loaded from
#'                            the database.
#' @template StudyPopulation
#' @template SccsData
#'
#' @description
#' This function is deprecated. Use `computePreExposureGain()` instead.
#'
#'
#' @return
#' A one-sided p-value for whether the rate before exposure is higher than after, against
#' the null of no change.#'
#'
#' @export
computePreExposureGainP <- function(sccsData, studyPopulation, exposureEraId = NULL) {
  .Deprecated("computePreExposureGain")
  return(computePreExposureGain(sccsData, studyPopulation, exposureEraId)$p)
}


#' Compute pre-exposure risk gain diagnostic
#'
#' @details
#' Compares the rate of the outcome in the 30 days prior to exposure to the rate
#' of the outcome in the 30 days following exposure. If the rate before exposure
#' is higher, this indicates there might reverse causality, that the outcome, or
#' some precursor of the outcome, increases the probability of having the exposure.
#'
#' The resulting p-value is computed using a Poisson model conditioned on the person.
#'
#' @param exposureEraId       The exposure to create the era data for. If not specified it is
#'                            assumed to be the one exposure for which the data was loaded from
#'                            the database.
#' @template StudyPopulation
#' @template SccsData
#' @param alpha             The alpha (type 1 error) used to test for pre-exposure gain
#'
#' @return
#' A tibble with one row and three columns: `ratio` indicates the ratio (rate of outcomes before the first exposure start) /
#' (rate of outcomes after the first exposure start). `p` is the p-value against the null-hypothesis that the ratio is
#' smaller than or equal to 1, and `stable` is `TRUE` if `p` is greater than `alpha`.
#'
#' @export
computePreExposureGain <- function(sccsData, studyPopulation, exposureEraId = NULL, alpha = 0.05) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsData, "SccsData", add = errorMessages)
  checkmate::assertList(studyPopulation, min.len = 1, add = errorMessages)
  checkmate::assertInt(exposureEraId, null.ok = TRUE, add = errorMessages)
  checkmate::assertNumber(alpha, lower = 0, upper = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (is.null(exposureEraId)) {
    exposureEraId <- attr(sccsData, "metaData")$exposureIds
    if (length(exposureEraId) != 1) {
      stop("No exposure ID specified, but multiple exposures found")
    }
  }

  cases <- studyPopulation$cases %>%
    select("caseId", "startDay", "endDay")

  exposures <- sccsData$eras %>%
    filter(.data$eraId == exposureEraId & .data$eraType == "rx") %>%
    inner_join(cases,
               by = join_by("caseId", "eraStartDay" >= "startDay", "eraStartDay" < "endDay"),
               copy = TRUE) %>%
    collect()

  if (nrow(exposures) == 0) {
    warning("No exposures found with era ID ", exposureEraId)
    return(tibble(ratio = NA,
                  p = NA,
                  stable = NA))
  }
  firstExposures <- exposures %>%
    group_by(.data$caseId, .data$startDay, .data$endDay) %>%
    summarise(
      eraStartDay = min(.data$eraStartDay, na.rm = TRUE),
      eraEndDay = min(.data$eraEndDay, na.rm = TRUE),
      .groups = "drop"
    )

  outcomes <- studyPopulation$outcomes %>%
    inner_join(firstExposures, by = join_by("caseId")) %>%
    mutate(delta = .data$outcomeDay - .data$eraStartDay) %>%
    select("caseId", "outcomeDay", "delta")

  # Restrict to 30 days before and after exposure start:
  outcomes <- outcomes %>%
    filter(.data$delta >= -30 & .data$delta <= 30) %>%
    mutate(
      beforeExposure = .data$delta < 0,
      y = 1
    ) %>%
    group_by(.data$caseId, .data$beforeExposure) %>%
    summarize(
      y = sum(.data$y),
      .groups = "drop"
    )

  observed <- bind_rows(
    firstExposures %>%
      mutate(daysBeforeExposure = .data$eraStartDay - .data$startDay) %>%
      mutate(
        daysObserved = if_else(.data$daysBeforeExposure > 30, 30, .data$daysBeforeExposure),
        beforeExposure = TRUE
      ) %>%
      select("caseId", "daysObserved", "beforeExposure"),
    firstExposures %>%
      mutate(daysAfterExposure = .data$endDay - .data$eraStartDay) %>%
      mutate(
        daysObserved = if_else(.data$daysAfterExposure > 30, 30, .data$daysAfterExposure),
        beforeExposure = FALSE
      ) %>%
      select("caseId", "daysObserved", "beforeExposure")
  ) %>%
    filter(.data$daysObserved > 0)

  poissonData <- observed %>%
    left_join(outcomes, by = join_by("caseId", "beforeExposure")) %>%
    mutate(
      rowId = row_number(),
      y = if_else(is.na(.data$y), 0, .data$y),
      covariateId = 1
    ) %>%
    select(
      "rowId",
      stratumId = "caseId",
      "covariateId",
      covariateValue = "beforeExposure",
      time = "daysObserved",
      "y"
    )
  cyclopsData <- Cyclops::convertToCyclopsData(
    outcomes = poissonData,
    covariates = poissonData,
    addIntercept = FALSE,
    modelType = "cpr",
    quiet = TRUE
  )
  fit <- Cyclops::fitCyclopsModel(cyclopsData)
  if (fit$return_flag != "SUCCESS") {
    return(tibble(ratio = NA,
                  p = NA,
                  stable = NA))
  }
  # compute one-sided p-value:
  llNull <- Cyclops::getCyclopsProfileLogLikelihood(
    object = fit,
    parm = 1,
    x = 0
  )$value
  llr <- fit$log_likelihood - llNull
  p <- EmpiricalCalibration:::computePFromLlr(llr, coef(fit))
  names(p) <- NULL
  return(tibble(ratio = exp(coef(fit)),
                p = p,
                stable = p > alpha))
}

#' Compute p-value for event-dependent observation end
#'
#' @param sccsModel         A fitted SCCS model as created using [fitSccsModel()].
#' @param alpha             The alpha (type 1 error) used to test for exposure rate change.
#'
#' @return
#' A tibble with one row and three columns: `ratio` indicates the estimates incidence rate ratio for the
#' probe at the end of observation. `p` is the p-value against the null-hypothesis that the log ratio is
#' between the `endOfObservationEffectBounds` specified when calling `fitSccsModel()`, and `stable` is
#' `TRUE` if `p` is greater than `alpha`.
#'
#' @export
computeEventDependentObservation <- function(sccsModel, alpha = 0.05) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsModel, "SccsModel", null.ok = TRUE, add = errorMessages)
  checkmate::assertNumber(alpha, lower = 0, upper = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (is.null(sccsModel$estimates)) {
    return(tibble(ratio = NA,
                  p = NA,
                  stable = NA))
  }
  estimate <- sccsModel$estimates |>
    filter(.data$covariateId == 99) |>
    select("logRr", "llr")
  if (length(estimate) == 0) {
    warning("No estimate found for the end of observation probe")
    return(tibble(ratio = NA,
                  p = NA,
                  stable = NA))
  }
  p <- 0.5 * pchisq(2 * estimate$llr, df = 0, lower.tail = FALSE) + 0.5 * pchisq(2 * estimate$llr, df = 1, lower.tail = FALSE)
  return(tibble(ratio = exp(estimate$logRr),
                p = p,
                stable = p > alpha))
}

mergeOverlappingExposures <- function(exposures) {
  # Assumes there is only 1 type of exposure:
  exposures <- exposures |>
    arrange(.data$caseId, .data$eraStartDay) |>
    group_by(.data$caseId) |>
    mutate(newGroup = cumsum(lag(.data$eraEndDay, default = first(.data$eraEndDay)) < .data$eraStartDay)) |>
    group_by(.data$caseId, .data$newGroup) |>
    summarise(
      eraStartDay = min(.data$eraStartDay),
      eraEndDay = max(.data$eraEndDay),
      .groups = 'drop'
    ) |>
    select("caseId", "eraStartDay", "eraEndDay")
  return(exposures)
}

computeExposureDaysToEvent <- function(studyPopulation, sccsData, exposureEraId, timeWindows, ignoreExposureStarts = FALSE) {
  cases <- studyPopulation$cases |>
    select("caseId", "startDay", "endDay")

  # Keep only exposures that overlap with the observation periods of the study population (also
  # truncate exposures to the observation period):
  exposures <- sccsData$eras |>
    filter(.data$eraId == exposureEraId & .data$eraType == "rx") |>
    inner_join(cases,
               by = join_by("caseId", "eraEndDay" >= "startDay", "eraStartDay" < "endDay"),
               copy = TRUE) |>
    collect() |>
    mutate(eraStartDay = pmax(.data$eraStartDay, .data$startDay),
           eraEndDay = pmin(.data$eraEndDay, .data$endDay))
  if (nrow(exposures) == 0) {
    warning("No exposures found with era ID ", exposureEraId)
  }

  exposures <- mergeOverlappingExposures(exposures)

  firstOutcomes <- studyPopulation$outcomes |>
    group_by(.data$caseId) |>
    filter(row_number(.data$outcomeDay) == 1)

  # Compute days exposed per window
  exposuresRelativeToOutcome <- exposures |>
    inner_join(firstOutcomes, by = join_by("caseId")) |>
    mutate(deltaExposureStart = .data$eraStartDay - .data$outcomeDay,
           deltaExposureEnd = .data$eraEndDay - .data$outcomeDay)

  exposureDaysPerWindow <- exposuresRelativeToOutcome |>
    cross_join(timeWindows) |>
    filter(.data$deltaExposureEnd >= .data$startDay & .data$deltaExposureStart <= .data$endDay) |>
    filter(!ignoreExposureStarts | .data$deltaExposureStart <= .data$startDay) |>
    mutate(deltaExposureStart = pmax(.data$deltaExposureStart, .data$startDay),
           deltaExposureEnd = pmin(.data$deltaExposureEnd, .data$endDay)) |>
    group_by(.data$caseId, .data$windowId, .data$startDay, .data$endDay) |>
    summarise(daysExposed = sum(.data$deltaExposureEnd - .data$deltaExposureStart + 1),
              .groups = "drop")

  # Compute days observed per window
  observationRelativeToOutcome <- firstOutcomes |>
    inner_join(studyPopulation$cases, by = join_by("caseId")) |>
    mutate(deltaObservationStart = .data$startDay - .data$outcomeDay,
           deltaObservationEnd = .data$endDay - .data$outcomeDay) |>
    select("caseId", "deltaObservationStart", "deltaObservationEnd")

  observationDaysPerWindow <- observationRelativeToOutcome |>
    cross_join(timeWindows) |>
    filter(.data$deltaObservationEnd >= .data$startDay & .data$deltaObservationStart <= .data$endDay) |>
    filter(.data$deltaObservationStart < .data$startDay | .data$deltaObservationStart > .data$endDay) |>
    mutate(deltaObservationStart = pmax(.data$deltaObservationStart, .data$startDay),
           deltaObservationEnd = pmin(.data$deltaObservationEnd, .data$endDay)) |>
    group_by(.data$caseId, .data$windowId, .data$startDay, .data$endDay) |>
    summarise(daysObserved = sum(.data$deltaObservationEnd - .data$deltaObservationStart + 1),
              .groups = "drop")

  data <- observationDaysPerWindow |>
    left_join(exposureDaysPerWindow, by = join_by("caseId", "windowId", "startDay", "endDay")) |>
    mutate(daysExposed = if_else(is.na(.data$daysExposed), 0, .data$daysExposed))

  return(data)
}


#' Compute diagnostic whether exposure probability changed following the outcome
#'
#' @param exposureEraId       The exposure to create the era data for. If not specified it is
#'                            assumed to be the one exposure for which the data was loaded from
#'                            the database.
#' @template StudyPopulation
#' @template SccsData
#' @param bounds              Bounds for the null of no change in the exposure rate.
#' @param alpha               The alpha (type 1 error) used to test for exposure rate change.
#' @param ignoreExposureStarts Ignore exposure starts when computing the diagnostic. This makes the
#'                             diagnostic robust against the outcome temporarily preventing exposure
#'                             starting, which should be dealt with by the pre-exposure window.
#'
#' @return
#' A tibble with one row and three columns: `ratio` indicates the ratio (rate of exposure days after the outcome) /
#' (rate of exposure days before the outcome). `p` is the p-value against the null-hypothesis that the log ratio is
#' between the provided `bounds`, and `stable` is `TRUE` if `p` is greater than `alpha`.
#'
#' @export
computeExposureChange <- function(sccsData,
                                  studyPopulation,
                                  exposureEraId = NULL,
                                  bounds = log(c(1/1.25, 1.25)),
                                  alpha = 0.05,
                                  ignoreExposureStarts = FALSE) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsData, "SccsData", add = errorMessages)
  checkmate::assertList(studyPopulation, min.len = 1, add = errorMessages)
  checkmate::assertInt(exposureEraId, null.ok = TRUE, add = errorMessages)
  checkmate::assertNumeric(bounds, len = 2, any.missing = FALSE, sorted = TRUE, add = errorMessages)
  checkmate::assertNumber(alpha, lower = 0, upper = 1, add = errorMessages)
  checkmate::assertLogical(ignoreExposureStarts, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (nrow(studyPopulation$cases) == 0) {
    return(tibble(ratio = NA,
                  p = NA,
                  stable = NA))
  }

  if (is.null(exposureEraId)) {
    exposureEraId <- attr(sccsData, "metaData")$exposureIds
    if (length(exposureEraId) != 1) {
      stop("No exposure ID specified, but multiple exposures found")
    }
  }

  timeWindows <- tibble(
    windowId = c(0, 1),
    startDay = c(-60, 0),
    endDay = c(-1, 59)
  )
  data <- computeExposureDaysToEvent(studyPopulation = studyPopulation,
                                     sccsData = sccsData,
                                     exposureEraId = exposureEraId,
                                     timeWindows = timeWindows,
                                     ignoreExposureStarts = ignoreExposureStarts)

  poissonData <- data |>
    filter(.data$daysObserved > 0) |>
    mutate(
      rowId = row_number(),
      covariateId = 1
    ) |>
    select(
      "rowId",
      stratumId = "caseId",
      "covariateId",
      covariateValue = "windowId",
      time = "daysObserved",
      y = "daysExposed"
    )

  casesWithExposure <- poissonData |>
    filter(.data$y > 0) |>
    pull(.data$stratumId)

  poissonData <- poissonData |>
    filter(.data$stratumId %in% casesWithExposure)

  if (nrow(poissonData) < 5) {
    return(tibble(ratio = NA,
                  p = NA,
                  stable = NA))
  }

  cyclopsData <- Cyclops::convertToCyclopsData(outcomes = poissonData,
                                               covariates = poissonData,
                                               addIntercept = FALSE,
                                               modelType = "cpr",
                                               quiet = TRUE)
  fit <- Cyclops::fitCyclopsModel(cyclopsData)
  if (fit$return_flag != "SUCCESS") {
    return(tibble(ratio = NA,
                  p = NA,
                  stable = NA))
  }
  logRr <- coef(fit)
  if (logRr >= bounds[1] && logRr <= bounds[2]) {
    llNull <- fit$log_likelihood
  } else {
    if (logRr < bounds[1]) {
      nullLogRr <- bounds[1]
    } else {
      nullLogRr <- bounds[2]
    }
    llNull <- Cyclops::getCyclopsProfileLogLikelihood(
      object = fit,
      parm = 1,
      x = nullLogRr,
      includePenalty = TRUE
    )$value
  }
  llr <- fit$log_likelihood - llNull
  p <- pchisq(2 * llr, df = 1, lower.tail = FALSE)
  return(tibble(ratio = exp(logRr),
                p = p,
                stable = p > alpha))
}




#' @export
computeExposureStability <- function(studyPopulation,
                                     sccsData,
                                     exposureEraId,
                                     bounds = log(c(1/1.25, 1.25)),
                                     alpha = 0.05) {
  cases <- studyPopulation$cases |>
    filter(.data$endDay - .data$startDay > 2) |>
    mutate(midDay = .data$startDay + round((.data$endDay - .data$startDay)/2)) |>
    select("caseId", "startDay", "midDay", "endDay")

  # Keep only exposures that overlap with the observation periods of the study population (also
  # truncate exposures to the observation period):
  exposures <- sccsData$eras |>
    filter(.data$eraId == exposureEraId & .data$eraType == "rx") |>
    inner_join(cases,
               by = join_by("caseId", "eraEndDay" >= "startDay", "eraStartDay" < "endDay"),
               copy = TRUE) |>
    collect() |>
    mutate(eraStartDay = pmax(.data$eraStartDay, .data$startDay),
           eraEndDay = pmin(.data$eraEndDay, .data$endDay))

  exposures <- mergeOverlappingExposures(exposures)

  # Pivot cases to windows:
  timeWindows <- bind_rows(
    cases |>
      select("caseId", startDay = "startDay", endDay = "midDay") |>
      mutate(windowId = 0),
    cases |>
      select("caseId", startDay = "midDay", endDay = "endDay") |>
      mutate(windowId = 1),
  )

  # Compute days exposed per window
  exposureDaysPerWindow <- exposures |>
    inner_join(timeWindows, by = join_by("caseId"), relationship = "many-to-many") |>
    filter(.data$eraEndDay >= .data$startDay & .data$eraStartDay <= .data$endDay) |>
    mutate(eraStartDay = pmax(.data$eraStartDay, .data$startDay),
           eraEndDay = pmin(.data$eraEndDay, .data$endDay)) |>
    group_by(.data$caseId, .data$windowId) |>
    summarise(daysExposed = sum(.data$eraEndDay - .data$eraStartDay + 1),
              .groups = "drop")

  # Compute days observed per window
  observationDaysPerWindow <- timeWindows |>
    mutate(daysObserved = .data$endDay - .data$startDay + 1) |>
    select("caseId", "windowId", "daysObserved")

  data <- observationDaysPerWindow |>
    left_join(exposureDaysPerWindow, by = join_by("caseId", "windowId")) |>
    mutate(daysExposed = if_else(is.na(.data$daysExposed), 0, .data$daysExposed))

  poissonData <- data |>
    filter(.data$daysObserved > 0) |>
    mutate(
      rowId = row_number(),
      covariateId = 1
    ) |>
    select(
      "rowId",
      stratumId = "caseId",
      "covariateId",
      covariateValue = "windowId",
      time = "daysObserved",
      y = "daysExposed"
    )

  casesWithExposure <- poissonData |>
    filter(.data$y > 0) |>
    pull(.data$stratumId)

  poissonData <- poissonData |>
    filter(.data$stratumId %in% casesWithExposure)

  if (nrow(poissonData) < 5) {
    return(tibble(ratio = NA,
                  p = NA,
                  stable = TRUE))
  }

  cyclopsData <- Cyclops::convertToCyclopsData(outcomes = poissonData,
                                               covariates = poissonData,
                                               addIntercept = FALSE,
                                               modelType = "cpr",
                                               quiet = TRUE)
  fit <- Cyclops::fitCyclopsModel(cyclopsData)
  if (fit$return_flag != "SUCCESS") {
    return(tibble(ratio = NA,
                  p = NA,
                  stable = TRUE))
  }
  logRr <- coef(fit)
  if (logRr >= bounds[1] && logRr <= bounds[2]) {
    llNull <- fit$log_likelihood
  } else {
    if (logRr < bounds[1]) {
      nullLogRr <- bounds[1]
    } else {
      nullLogRr <- bounds[2]
    }
    llNull <- Cyclops::getCyclopsProfileLogLikelihood(
      object = fit,
      parm = 1,
      x = nullLogRr,
      includePenalty = TRUE
    )$value
  }
  llr <- fit$log_likelihood - llNull
  p <- pchisq(2 * llr, df = 1, lower.tail = FALSE)
  return(tibble(ratio = exp(logRr),
                p = p,
                stable = p > alpha))
}

#' Check if rare outcome assumption is violated
#'
#' @template StudyPopulation
#' @param firstOutcomeOnly   Was the analysis restricted to the first outcome only? If left at NULL,
#'                           will be determined by whether `firstOutcomeOnly` was set to `TRUE` when
#'                           calling `createStudyPopulation()` or whether each person only had one
#'                           outcome when pulling the data from the server.
#' @param maxPrevalence      The maximum allowed prevalence (proportion of people with the outcome)
#'                           allowed when restricting to first outcome only.
#'
#' @details
#' Most SCCS analyses restrict to the first outcome occurrence per person to avoid violating the
#' assumption that subsequent occurrences are independent. This is fine, as long as the outcome is
#' rare. According to Farrington et al., the magnitude of the bias from violating this assumption is
#' 0.5p, where p is the prevalence. By default we set the threshold for p at 0.1, corresponding to
#' at most 5 percent bias.
#'
#' The prevalence was computed in the `getDbSccsData()` function, within the population defined by
#' the `observation_period` table, and restricted to the study period(s) and nesting cohort if
#' used.
#'
#' @references
#' Farrington P, Whitaker H, Ghebremichael-Weldeselassie Y, Self-Controlled Case Series Studies: A
#' Modelling Guide with R, CRC Press, 2018
#'
#' @return
#' A tibble with one row and three columns: `outcomeProportion` indicates the proportion of people
#' having the outcome at least once. `firstOutcomeOnly` indicated whether the analysis was restricted
#' to the first outcome only. `rare` is TRUE if the rare outcome assumption is met, or the analysis
#' was not restricted to the first outcome.
#'
#' @export
checkRareOutcomeAssumption <- function(studyPopulation,
                                       firstOutcomeOnly = NULL,
                                       maxPrevalence = 0.1) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(studyPopulation, min.len = 1, add = errorMessages)
  checkmate::assertLogical(firstOutcomeOnly, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertNumber(maxPrevalence, lower = 0, upper = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  prevalence <- studyPopulation$metaData$prevalence
  if (is.null(firstOutcomeOnly)) {
    firstOutcomeOnly <- prevalence$definitelyFirstOutcomeOnly | prevalence$probablyFirstOutcomeOnly
  }
  if (firstOutcomeOnly) {
    rare <- prevalence$outcomeProportion <= maxPrevalence
  } else {
    rare <- TRUE
  }
  return(tibble(outcomeProportion = prevalence$outcomeProportion,
                firstOutcomeOnly = firstOutcomeOnly,
                rare = rare))
}
