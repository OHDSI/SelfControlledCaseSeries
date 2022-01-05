# Copyright 2021 Observational Health Data Sciences and Informatics
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
    inner_join(studyPopulation$cases , by = "caseId") %>%
    transmute(month = convertDateToMonth(.data$startDate + .data$outcomeDay)) %>%
    group_by(.data$month) %>%
    summarise(outcomeCount = n())
  data <- observationPeriodCounts %>%
    inner_join(outcomeCounts, by = "month") %>%
    mutate(rate = .data$outcomeCount / .data$observationPeriodCount) %>%
    mutate(monthStartDate = convertMonthToStartDate(.data$month),
           monthEndDate = convertMonthToEndDate(.data$month))
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
                                            Boundary.knots = calendarTimeKnots[c(1, length(calendarTimeKnots))])
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
      inner_join(tibble(monthOfYear = season,
                        seasonRr = exp(logRr)),
                 by = "monthOfYear")

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
#' month a two-sidede p-value is computed against the null hypothesis that the rate in that month equals the mean rate. This
#' p-value is compared to an alpha value, using a Bonferroni correction to adjust for the multiple testing across months.
#'
#' @template StudyPopulation
#' @param sccsModel         Optional: A fitted SCCS model as created using [fitSccsModel()]. If the
#'                          model contains splines for seasonality and or calendar time these will be adjusted
#'                          for before computing stability.
#' @param alpha             The alpha (type 1 error) used to test for stability. A Bonferroni correction will
#'                          be applied for the number of months tested.
#'
#' @return
#' A tibble with information on the temporal stability per month. The column `stable` indicates whether the rate
#' of the outcome is within the expected range for that month, assuming the rate is constant over time.
#'
#' @export
computeTimeStability <- function(studyPopulation, sccsModel = NULL, alpha = 0.05) {
  data <- computeOutcomeRatePerMonth(studyPopulation)
  if (is.null(sccsModel)) {
    data <- data %>%
      mutate(adjustedRate = .data$rate)
  } else {
    data <- adjustOutcomeRatePerMonth(data, sccsModel)
  }

  computeTwoSidedP <- function(observed, expected) {
    pUpperBound = 1 - ppois(observed, expected, lower.tail = TRUE)
    pLowerBound = 1 - ppois(observed, expected, lower.tail = FALSE)
    return(2 * pmin(pUpperBound, pLowerBound))
  }

  # Season and calendar time splines lack intercept, so need to compute expected count in indirect way:
  meanAdjustedRate <- sum(data$adjustedRate * data$observationPeriodCount ) / sum(data$observationPeriodCount)
  data <- data %>%
    mutate(expected = .data$outcomeCount * meanAdjustedRate / .data$adjustedRate) %>%
    mutate(p = computeTwoSidedP(.data$outcomeCount, .data$expected),
           alpha = !!alpha / n()) %>%
    mutate(stable = .data$p >= .data$alpha)
  # print(data[50:100, ], n = 35)
  # sum(!data$stable)

  return(data)
}
