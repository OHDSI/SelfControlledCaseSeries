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
  observationPeriodCounts <- computeObservedPerMonth(studyPopulation)
  outcomeCounts <- studyPopulation$outcomes %>%
    inner_join(studyPopulation$cases, by = "caseId") %>%
    transmute(month = convertDateToMonth(.data$observationPeriodStartDate + .data$outcomeDay)) %>%
    group_by(.data$month) %>%
    summarise(outcomeCount = n())
  data <- observationPeriodCounts %>%
    inner_join(outcomeCounts, by = "month") %>%
    mutate(rate = .data$outcomeCount / .data$observationPeriodCount) %>%
    mutate(
      monthStartDate = convertMonthToStartDate(.data$month),
      monthEndDate = convertMonthToEndDate(.data$month)
    )
  return(data)
}

adjustOutcomeRatePerMonth <- function(data, sccsModel) {
  data$adjustedRate <- data$rate

  if (hasCalendarTimeEffect(sccsModel)) {
    estimates <- sccsModel$estimates
    estimates <- estimates[estimates$covariateId >= 300 & estimates$covariateId < 400, ]
    splineCoefs <- c(0, estimates$logRr)
    calendarTimeKnots <- sccsModel$metaData$calendarTime$calendarTimeKnots
    calendarTime <- data$month
    calendarTime[calendarTime < calendarTimeKnots[1]] <- calendarTimeKnots[1]
    calendarTime[calendarTime > calendarTimeKnots[length(calendarTimeKnots)]] <- calendarTimeKnots[length(calendarTimeKnots)]
    calendarTimeDesignMatrix <- splines::bs(calendarTime,
      knots = calendarTimeKnots[2:(length(calendarTimeKnots) - 1)],
      Boundary.knots = calendarTimeKnots[c(1, length(calendarTimeKnots))]
    )
    logRr <- apply(calendarTimeDesignMatrix %*% splineCoefs, 1, sum)
    logRr <- logRr - mean(logRr)
    data$calendarTimeRr <- exp(logRr)
    data <- data %>%
      mutate(adjustedRate = .data$adjustedRate / .data$calendarTimeRr)
  }

  if (hasSeasonality(sccsModel)) {
    estimates <- sccsModel$estimates
    estimates <- estimates[estimates$covariateId >= 200 & estimates$covariateId < 300, ]
    splineCoefs <- c(0, estimates$logRr)
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
        by = "monthOfYear"
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

  # Season and calendar time splines lack intercept, so need to compute expected count in indirect way:
  meanAdjustedRate <- sum(data$adjustedRate * data$observationPeriodCount) / sum(data$observationPeriodCount)
  data <- data %>%
    mutate(expected = .data$outcomeCount * meanAdjustedRate / .data$adjustedRate) %>%
    mutate(
      p = computeTwoSidedP(.data$outcomeCount, .data$expected),
      alpha = !!alpha / n()
    ) %>%
    mutate(stable = .data$p >= .data$alpha)
  # print(data[50:100, ], n = 35)
  # sum(!data$stable)

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
    inner_join(cases, by = "caseId", copy = TRUE) %>%
    filter(.data$eraStartDay >= .data$startDay, .data$eraStartDay < .data$endDay) %>%
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
    inner_join(firstExposures, by = "caseId") %>%
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
    left_join(outcomes, by = c("caseId", "beforeExposure")) %>%
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
