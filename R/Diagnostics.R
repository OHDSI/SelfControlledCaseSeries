# Copyright 2025 Observational Health Data Sciences and Informatics
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
      monthEndDate = as.Date("2000-01-01")) |>
      filter(month == 0)
    return(result)
  }
  cases <- studyPopulation$cases |>
    mutate(startDate = .data$observationPeriodStartDate + .data$startDay,
           endDate = .data$observationPeriodStartDate + .data$endDay) |>
    mutate(startMonth = convertDateToMonth(.data$startDate),
           endMonth = convertDateToMonth(.data$endDate),
           startMonthFraction = computeMonthFraction(.data$startDate, TRUE),
           endMonthFraction = computeMonthFraction(.data$endDate))  |>
    select("caseId", "startMonth", "endMonth", "startMonthFraction", "endMonthFraction") |>
    left_join(
      studyPopulation$outcomes |>
        group_by(.data$caseId) |>
        summarize(outcomeCount = n()),
      by = join_by("caseId")
    ) |>
    mutate(outcomeCount = if_else(is.na(.data$outcomeCount), 0, .data$outcomeCount)) |>
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
    monthAdjustments <- monthAdjustments |>
      mutate(calendarTimeRr = exp(apply(designMatrix %*% splineCoefs, 1, sum))) |>
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
    monthAdjustments <- monthAdjustments |>
      mutate(monthOfYear = .data$month %% 12 + 1) |>
      inner_join(
        tibble(
          monthOfYear = season,
          seasonRr = exp(logRr)
        ),
        by = join_by("monthOfYear")
      ) |>
      select(-"monthOfYear") |>
      mutate(totalRr = .data$totalRr * .data$seasonRr)
  }
  if (hasAdjustment) {
    # Need to correct for the fact that a person may have seen only part of the spline, so
    # techincally has a different intercept:
    cases <- cases |>
      mutate(correction = computeCorrections(cases, monthAdjustments))
  } else {
    cases <- cases |>
      mutate(correction = 1)
  }
  observedCounts <- studyPopulation$outcomes |>
    inner_join(studyPopulation$cases, by = join_by("caseId")) |>
    transmute(month = convertDateToMonth(.data$observationPeriodStartDate + .data$outcomeDay)) |>
    group_by(.data$month) |>
    summarise(observedCount = n())
  computeExpected <- function(monthAdjustment) {
    month <- monthAdjustment$month
    expected <- cases |>
      filter(month >= .data$startMonth & month <= .data$endMonth) |>
      mutate(weight = if_else(month == .data$startMonth,
                              .data$startMonthFraction,
                              if_else(month == .data$endMonth,
                                      .data$endMonthFraction,
                                      1))) |>
      summarize(month = !!month,
                expectedCount = sum(.data$weight * .data$rate),
                adjustedExpectedCount = if_else(monthAdjustment$totalRr == 0,
                                                0,
                                                sum(.data$weight * .data$rate * monthAdjustment$totalRr / .data$correction)),
                observationPeriodCount = sum(.data$weight))
    return(expected)
  }
  expectedCounts <- bind_rows(lapply(split(monthAdjustments, seq_len(nrow(monthAdjustments))), computeExpected))

  data <- expectedCounts |>
    left_join(observedCounts, by = join_by("month")) |>
    mutate(observedCount = if_else(is.na(.data$observedCount), 0, .data$observedCount)) |>
    mutate(ratio = if_else(.data$expectedCount == 0, 0, .data$observedCount / .data$expectedCount))  |>
    mutate(adjustedRatio = if_else(.data$adjustedExpectedCount == 0, 0, .data$observedCount / .data$adjustedExpectedCount))  |>
    mutate(monthStartDate = convertMonthToStartDate(.data$month),
           monthEndDate = convertMonthToEndDate(.data$month)) |>
    select(-"expectedCount")
  return(data)
}

#' Check stability of outcome rate over time
#'
#' @details
#' DEPRECATED. Use `checkTimeStabilityAssumption()` instead.
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
#' `p` is the p-value against the null-hypothesis that the ratio is smaller than `maxRatio`, and `pass` is `TRUE`
#' if `p` is greater than `alpha`.
#'
#' @export
computeTimeStability <- function(studyPopulation, sccsModel = NULL, maxRatio = 1.10, alpha = 0.05) {
  .Deprecated("checkTimeStabilityAssumption")
  result <- checkTimeStabilityAssumption(studyPopulation, sccsModel, maxRatio, alpha)
  result <- result |>
    rename(stable = "pass")
}

#' Check stability of outcome rate over time
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
#' `p` is the p-value against the null-hypothesis that the ratio is smaller than `maxRatio`, and `pass` is `TRUE`
#' if `p` is greater than `alpha`.
#'
#' @export
checkTimeStabilityAssumption <- function(studyPopulation, sccsModel = NULL, maxRatio = 1.10, alpha = 0.05) {
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
                     pass = TRUE)
    return(result)
  }
  o <- data$observedCount
  e <- data$adjustedExpectedCount
  e[e == 0] <- .Machine$double.eps

  logLikelihood <- function(x) {
    return(-sum(log(dpois(o, e*x) + dpois(o, e/x))))
  }
  x <- seq(1, 10, by = 0.1)
  ll <- sapply(x, logLikelihood)
  maxX <- x[max(which(!is.na(ll) & !is.infinite(ll)))]
  minX <- x[min(which(!is.na(ll) & !is.infinite(ll)))]
  xHat <- optim(1.5, logLikelihood, lower = minX, upper = maxX, method = "L-BFGS-B")$par
  x0 <- if (xHat > maxRatio) maxRatio else xHat
  x1 <- if (xHat < maxRatio) maxRatio else xHat
  ll0 <- -logLikelihood(x0)
  ll1 <- -logLikelihood(x1)
  llr <- 2 * (ll1 - ll0)
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
                   pass = p > alpha)
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
  .Deprecated("checkEventExposureIndependenceAssumption")
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsData, "SccsData", add = errorMessages)
  checkmate::assertList(studyPopulation, min.len = 1, add = errorMessages)
  checkmate::assertInt(exposureEraId, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (is.null(exposureEraId)) {
    exposureEraId <- attr(sccsData, "metaData")$exposureIds
    if (length(exposureEraId) != 1) {
      stop("No exposure ID specified, but multiple exposures found")
    }
  }

  cases <- studyPopulation$cases |>
    select("caseId", "startDay", "endDay")

  exposures <- sccsData$eras |>
    filter(.data$eraId == exposureEraId & .data$eraType == "rx") |>
    inner_join(cases,
               by = join_by("caseId", "eraStartDay" >= "startDay", "eraStartDay" < "endDay"),
               copy = TRUE) |>
    collect()

  if (nrow(exposures) == 0) {
    warning("No exposures found with era ID ", exposureEraId)
    return(as.numeric(NA))
  }
  firstExposures <- exposures |>
    group_by(.data$caseId, .data$startDay, .data$endDay) |>
    summarise(
      eraStartDay = min(.data$eraStartDay, na.rm = TRUE),
      eraEndDay = min(.data$eraEndDay, na.rm = TRUE),
      .groups = "drop"
    )

  outcomes <- studyPopulation$outcomes |>
    inner_join(firstExposures, by = join_by("caseId")) |>
    mutate(delta = .data$outcomeDay - .data$eraStartDay) |>
    select("caseId", "outcomeDay", "delta")

  # Restrict to 30 days before and after exposure start:
  outcomes <- outcomes |>
    filter(.data$delta >= -30 & .data$delta <= 30) |>
    mutate(
      beforeExposure = .data$delta < 0,
      y = 1
    ) |>
    group_by(.data$caseId, .data$beforeExposure) |>
    summarize(
      y = sum(.data$y),
      .groups = "drop"
    )

  observed <- bind_rows(
    firstExposures |>
      mutate(daysBeforeExposure = .data$eraStartDay - .data$startDay) |>
      mutate(
        daysObserved = if_else(.data$daysBeforeExposure > 30, 30, .data$daysBeforeExposure),
        beforeExposure = TRUE
      ) |>
      select("caseId", "daysObserved", "beforeExposure"),
    firstExposures |>
      mutate(daysAfterExposure = .data$endDay - .data$eraStartDay) |>
      mutate(
        daysObserved = if_else(.data$daysAfterExposure > 30, 30, .data$daysAfterExposure),
        beforeExposure = FALSE
      ) |>
      select("caseId", "daysObserved", "beforeExposure")
  ) |>
    filter(.data$daysObserved > 0)

  poissonData <- observed |>
    left_join(outcomes, by = join_by("caseId", "beforeExposure")) |>
    mutate(
      rowId = row_number(),
      y = if_else(is.na(.data$y), 0, .data$y),
      covariateId = 1
    ) |>
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
    return(as.numeric(NA))
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
  return(p)
}

getNaDiagnostic <- function() {
  return(tibble(ratio = NA,
                lb = NA,
                ub = NA,
                pass = NA))
}

#' Check diagnostic for event-dependent observation end
#'
#' @description
#' This diagnostic tests whether there is a dependency between the event and the end of observation.
#' It does so by adding a probe window at the end of observation, and checking whether the rate of
#' the outcome is elevated (or decreased) during this window.
#'
#' The end of observation probe window will automatically be added to the model by the
#' `createSccsIntervalData()` function, unless the `endOfObservationEraLength` argument is set to 0.
#' This function extracts the estimate for that window from the model, and compares it to the
#' `nullBounds`.#'
#'
#' @param sccsModel         A fitted SCCS model as created using [fitSccsModel()].
#' @param nullBounds        The bounds for the null hypothesis on the incidence rate ratio scale.
#'
#' @return
#' A tibble with one row and four columns: `ratio` indicates the estimates incidence rate ratio for the
#' probe at the end of observation. `lb` and `ub` represent the upper and lower bounds of the 95 percent
#' confidence interval, and `pass` is `TRUE` if the confidence interval intersects the null bounds.
#'
#' @export
checkEventObservationIndependenceAssumption <- function(sccsModel, nullBounds = c(0.5, 2.0)) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsModel, "SccsModel", null.ok = TRUE, add = errorMessages)
  checkmate::assertNumeric(nullBounds, lower = 0, len = 2, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (is.null(sccsModel$estimates)) {
    return(getNaDiagnostic())
  }
  estimate <- sccsModel$estimates |>
    filter(.data$covariateId == 99) |>
    select("logRr", "logLb95", "logUb95")
  if (length(estimate) == 0) {
    warning("No estimate found for the end of observation probe. ",
            "Did you set endOfObservationEraLength = 0 when calling createSccsIntervalData()?")
    return(getNaDiagnostic())
  }
  result <- tibble(ratio = exp(estimate$logRr),
                   lb = exp(estimate$logLb95),
                   ub = exp(estimate$logUb95)) |>
    mutate(pass = .data$lb <= nullBounds[2] & .data$ub >= nullBounds[1])
  return(result)
}

#' Check diagnostic for event-dependent exposure
#'
#' @description
#' This diagnostic tests whether there is a dependency between the event and subsequent exposures.
#' This requires you have indicated one of the era covariates to be a pre-exposure window. This
#' function simply checks whether the confidence interval for the effect estimate of that pre-
#' exposure window overlaps with the `nullBounds`.
#'
#' To designate an era covariate to be the pre-exposure window, set `preExposure = TRUE` when
#' calling `createEraCovariateSettings()`. Note that, by default, `preExposure` will be `TRUE` if
#' `start` is smaller than 0.
#'
#' @param sccsModel         A fitted SCCS model as created using [fitSccsModel()].
#' @param nullBounds        The bounds for the null hypothesis on the incidence rate ratio scale.
#'
#' @return
#' A tibble with one row per pre-exposure window and four columns: `ratio` indicates the estimates
#' incidence rate ratio for the pre-exposure window. `lb` and `ub` represent the upper and lower
#' bounds of the 95 percent confidence interval, and `pass` is `TRUE` if the confidence interval
#' intersects the null bounds.
#'
#' @export
checkEventExposureIndependenceAssumption <- function(sccsModel, nullBounds = c(0.8, 1.25)) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsModel, "SccsModel", null.ok = TRUE, add = errorMessages)
  checkmate::assertNumeric(nullBounds, lower = 0, len = 2, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (is.null(sccsModel$estimates)) {
    return(getNaDiagnostic())
  }
  estimate <- sccsModel$estimates |>
    inner_join(sccsModel$metaData$covariateRef |>
                 select("covariateId", "preExposure"),
               by = join_by("covariateId", "preExposure")) |>
    filter(.data$preExposure == 1) |>
    select("logRr", "logLb95", "logUb95")
  if (nrow(estimate) == 0) {
    warning("No estimate found for the pre-exposure period. ",
            "Make sure to use at least one createEraCovariateSettings() where preExposure = TRUE")
    return(getNaDiagnostic())
  }
  result <- tibble(ratio = exp(estimate$logRr),
                   lb = exp(estimate$logLb95),
                   ub = exp(estimate$logUb95)) |>
    mutate(pass = .data$lb <= nullBounds[2] & .data$ub >= nullBounds[1])
  return(result)
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
    pass <- prevalence$outcomeProportion <= maxPrevalence
  } else {
    pass <- TRUE
  }
  return(tibble(outcomeProportion = prevalence$outcomeProportion,
                firstOutcomeOnly = firstOutcomeOnly,
                pass = pass))
}
