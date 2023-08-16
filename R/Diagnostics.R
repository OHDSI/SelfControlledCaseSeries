# Copyright 2023 Observational Health Data Sciences and Informatics
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

computeOutcomeRatePerMonth <- function(studyPopulation) {
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
  observedCounts <- studyPopulation$outcomes %>%
    inner_join(studyPopulation$cases, by = join_by("caseId")) %>%
    transmute(month = convertDateToMonth(.data$observationPeriodStartDate + .data$outcomeDay)) %>%
    group_by(.data$month) %>%
    summarise(observedCount = n())
  if (nrow(cases) == 0) {
    expectedCounts <- tibble(month = 1, expectedCount = 1.0, observationPeriodCount = 1.0) %>%
      filter(month == 0)
  } else {
    months <- seq(min(cases$startMonth), max(cases$endMonth))
    computeExpected <- function(month) {
      cases %>%
        filter(month >= .data$startMonth & month <= .data$endMonth) %>%
        mutate(weight = if_else(month == .data$startMonth,
                                .data$startMonthFraction,
                                if_else(month == .data$endMonth,
                                        .data$endMonthFraction,
                                        1))) %>%
        summarize(month = !!month,
                  expectedCount = sum(.data$weight * .data$rate),
                  observationPeriodCount = sum(.data$weight)) %>%
        return()
    }
    expectedCounts <- bind_rows(lapply(months, computeExpected))
  }
  data <- observedCounts %>%
    inner_join(expectedCounts, by = join_by("month")) %>%
    mutate(rate = .data$observedCount / .data$expectedCount)  %>%
    mutate(monthStartDate = convertMonthToStartDate(.data$month),
           monthEndDate = convertMonthToEndDate(.data$month))
  return(data)
}

adjustOutcomeRatePerMonth <- function(data, sccsModel) {
  data$adjustedRate <- data$rate

  if (hasCalendarTimeEffect(sccsModel)) {
    estimates <- sccsModel$estimates
    splineCoefs <- estimates[estimates$covariateId >= 300 & estimates$covariateId < 400, "logRr"]
    calendarTimeKnotsInPeriods <- sccsModel$metaData$calendarTime$calendarTimeKnotsInPeriods
    designMatrix <- createMultiSegmentDesignMatrix(x = data$month,
                                                   knotsPerSegment = calendarTimeKnotsInPeriods)
    data$logRr <- apply(designMatrix %*% splineCoefs, 1, sum)
    # Each segment can be thought of having its own intercept, so adjust logRr by mean in segment:
    data <- data %>%
      mutate(gap = .data$month - lag(.data$month) > 1) %>%
      mutate(gap = if_else(is.na(.data$gap), 0, .data$gap)) %>%
      mutate(segment = cumsum(.data$gap))
    data <- data %>%
      inner_join(
        data %>%
          group_by(.data$segment) %>%
          summarize(meanLogRr = mean(logRr)),
        by = join_by("segment")
      ) %>%
      mutate(logRr = .data$logRr - .data$meanLogRr) %>%
      mutate(calendarTimeRr = exp(.data$logRr))
    data <- data %>%
      mutate(adjustedRate = .data$adjustedRate / .data$calendarTimeRr)
  }

  if (hasSeasonality(sccsModel)) {
    estimates <- sccsModel$estimates
    splineCoefs <- estimates[estimates$covariateId >= 200 & estimates$covariateId < 300, "logRr"]
    seasonKnots <- sccsModel$metaData$seasonality$seasonKnots
    season <- 1:12
    seasonDesignMatrix <- cyclicSplineDesign(season, seasonKnots)
    logRr <- apply(seasonDesignMatrix %*% splineCoefs, 1, sum)
    logRr <- logRr - mean(logRr)

    data <- data %>%
      mutate(monthOfYear = .data$month %% 12 + 1) %>%
      inner_join(
        tibble(
          monthOfYear = season,
          seasonRr = exp(logRr)
        ),
        by = join_by("monthOfYear")
      )

    data <- data %>%
      mutate(adjustedRate = .data$adjustedRate / .data$seasonRr)
  }
  return(data)
}

#' Compute stability of outcome rate over time
#'
#' @details
#' Computes for each calendar month the rate of the outcome, and evaluates whether that rate is constant over time. If
#' splines are used to adjust for seasonality and/or calendar time, these adjustments are taken into consideration. For each
#' month a two-sided p-value is computed against the null hypothesis that the rate in that month deviates from the mean rate
#' no more than `maxRatio`. This p-value is compared to an alpha value, using a Bonferroni correction to adjust for the
#' multiple testing across months.
#'
#' @template StudyPopulation
#' @param sccsModel         Optional: A fitted SCCS model as created using [fitSccsModel()]. If the
#'                          model contains splines for seasonality and or calendar time these will be adjusted
#'                          for before computing stability.
#' @param maxRatio          The maximum ratio between the (adjusted) rate in a month, and the mean (adjusted) rate that
#'                          we would consider to be irrelevant.
#' @param alpha             The alpha (type 1 error) used to test for stability. A Bonferroni correction will
#'                          be applied for the number of months tested.
#'
#' @return
#' A tibble with information on the temporal stability per month. The column `stable` indicates whether the rate
#' of the outcome is within the expected range for that month, assuming the rate is constant over time.
#'
#' @export
computeTimeStability <- function(studyPopulation, sccsModel = NULL, maxRatio = 1.25, alpha = 0.05) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(studyPopulation, min.len = 1, add = errorMessages)
  checkmate::assertClass(sccsModel, "SccsModel", null.ok = TRUE, add = errorMessages)
  checkmate::assertNumeric(maxRatio, lower = 1, len = 1, add = errorMessages)
  checkmate::assertNumeric(alpha, lower = 0, upper = 1, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  data <- computeOutcomeRatePerMonth(studyPopulation)
  if (is.null(sccsModel)) {
    data <- data %>%
      mutate(adjustedRate = .data$rate)
  } else {
    data <- adjustOutcomeRatePerMonth(data, sccsModel)
  }
  computeTwoSidedP <- function(observed, expected) {
    pUpperBound <- 1 - ppois(observed, expected * maxRatio, lower.tail = TRUE)
    pLowerBound <- 1 - ppois(observed, expected / maxRatio, lower.tail = FALSE)
    return(pmin(1, 2 * pmin(pUpperBound, pLowerBound)))
  }
  data <- data %>%
    mutate(expected = .data$observedCount / .data$adjustedRate) %>%
    mutate(
      p = computeTwoSidedP(.data$observedCount , .data$expected),
      alpha = !!alpha / n()
    ) %>%
    mutate(stable = .data$p >= .data$alpha)
  return(data)
}



#' Compute P for pre-exposure risk gain
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
#'
#' @return
#' A one-sided p-value for whether the rate before exposure is higher than after, against
#' the null of no change.
#'
#' @export
computePreExposureGainP <- function(sccsData, studyPopulation, exposureEraId = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsData, "SccsData", add = errorMessages)
  checkmate::assertList(studyPopulation, min.len = 1, add = errorMessages)
  checkmate::assertInt(exposureEraId, add = errorMessages)
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
    return(NA)
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
    return(NA)
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
