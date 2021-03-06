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

#' Create a study population
#'
#' @template SccsData
#' @param outcomeId             The outcome to create the era data for. If not specified it is
#'                              assumed to be the one outcome for which the data was loaded from
#'                              the database.
#' @param naivePeriod           The number of days at the start of a patient's observation period
#'                              that should not be included in the risk calculations. Note that
#'                              the naive period can be used to determine current covariate
#'                              status right after the naive period, and whether an outcome is
#'                              the first one.
#' @param firstOutcomeOnly      Whether only the first occurrence of an outcome should be
#'                              considered.
#' @param minAge                Minimum age at which patient time will be included in the analysis. Note
#'                              that information prior to the min age is still used to determine exposure
#'                              status after the minimum age (e.g. when a prescription was started just prior
#'                              to reaching the minimum age). Also, outcomes occurring before the minimum age
#'                              is reached will be considered as prior outcomes when using first outcomes only.
#'                              Age should be specified in years, but non-integer values are allowed. If not
#'                              specified, no age restriction will be applied.
#' @param maxAge                Maximum age at which patient time will be included in the analysis. Age should
#'                              be specified in years, but non-integer values are allowed. If not
#'                              specified, no age restriction will be applied.
#'
#' @export
createStudyPopulation <- function(sccsData,
                                  outcomeId = NULL,
                                  firstOutcomeOnly = FALSE,
                                  naivePeriod = 0,
                                  minAge = NULL,
                                  maxAge = NULL) {
  outcomes <- sccsData$eras %>%
    filter(.data$eraType == "hoi") %>%
    collect()

  cases <- sccsData$cases %>%
    collect()

  attrition <- attr(sccsData, "metaData")$attrition
  if (is.null(outcomeId)) {
    if (outcomes %>%
      distinct(.data$eraId) %>%
      count() %>%
      pull() > 1) {
      stop("No outcome ID specified, but more than one outcome ID found.")
    }
  } else {
    outcomes <- outcomes %>%
      filter(.data$eraId == !!outcomeId)

    attrition <- attrition %>%
      filter(.data$outcomeId == !!outcomeId)
  }

  if (firstOutcomeOnly) {
    outcomes <- outcomes %>%
      group_by(.data$caseId) %>%
      filter(row_number(.data$startDay) == 1) %>%
      ungroup()

    attrition <- bind_rows(attrition,
                           countOutcomes(outcomes, cases, "First outcome only"))
  }

  cases <- cases %>%
    mutate(startAgeInDays = .data$ageInDays,
           endAgeInDays = .data$ageInDays + .data$observationDays - 1)

  if (naivePeriod != 0) {
    cases <- cases %>%
      mutate(startAgeInDays = case_when(naivePeriod > .data$censoredDays ~ .data$startAgeInDays + naivePeriod - .data$censoredDays,
                                        TRUE ~ .data$startAgeInDays)) %>%
      filter(.data$endAgeInDays > .data$startAgeInDays)

    outcomes <- outcomes %>%
      inner_join(select(cases, .data$caseId, .data$startAgeInDays, .data$ageInDays), by = "caseId") %>%
      filter(.data$startDay >= .data$startAgeInDays - .data$ageInDays) %>%
      select(-.data$startAgeInDays, -.data$ageInDays)

    cases <- cases %>%
      filter(.data$caseId %in% unique(outcomes$caseId))

    attrition <- bind_rows(attrition,
                           countOutcomes(outcomes, cases, sprintf("%s days naive period", naivePeriod)))
  }

  if (!is.null(minAge) || !is.null(maxAge)) {
    labels <- c()
    if (!is.null(minAge)) {
      minAgeInDays <- minAge * 365.25
      cases <- cases %>%
        mutate(startAgeInDays = case_when(.data$startAgeInDays < minAgeInDays ~ minAgeInDays,
                                          TRUE ~ .data$startAgeInDays)) %>%
        filter(.data$endAgeInDays > .data$startAgeInDays)
      labels <- c(labels, sprintf("Age >= %s", minAge))
    }
    if (!is.null(maxAge)) {
      maxAgeInDays <- round((maxAge + 1) * 365.25)
      cases <- cases %>%
        mutate(noninformativeEndCensor = case_when(.data$endAgeInDays > maxAgeInDays ~ 1,
                                                   TRUE ~ noninformativeEndCensor),
               endAgeInDays = case_when(.data$endAgeInDays > maxAgeInDays ~ maxAgeInDays,
                                        TRUE ~ .data$endAgeInDays)) %>%
        filter(.data$endAgeInDays > .data$startAgeInDays)
      labels <- c(labels, sprintf("Age <= %s", maxAge))
    }

    outcomes <- outcomes %>%
      inner_join(select(cases, .data$observationPeriodId, .data$caseId, .data$startAgeInDays, .data$endAgeInDays, .data$ageInDays), by = "caseId") %>%
      filter(.data$startDay >= .data$startAgeInDays - .data$ageInDays &
             .data$startDay <= .data$endAgeInDays - .data$ageInDays) %>%
      select(-.data$startAgeInDays, -.data$endAgeInDays, -.data$ageInDays)

    attrition <- bind_rows(attrition,
                           countOutcomes(outcomes, cases, paste(labels, collapse = " & ")))
  }

  metaData <- list(exposureIds = attr(sccsData, "metaData")$exposureIds,
                   outcomeId = unique(outcomes$eraId),
                   attrition = attrition)

  cases$startDate <- as.Date(paste(cases$startYear, cases$startMonth, cases$startDay, sep = "-"), format="%Y-%m-%d")
  cases <- cases %>%
    mutate(offset = .data$startAgeInDays - .data$ageInDays,
           startDate = .data$startDate + .data$startAgeInDays - .data$ageInDays,
           endDay = .data$endAgeInDays - .data$startAgeInDays) %>%
    mutate(ageInDays = .data$startAgeInDays) %>%
    select(.data$observationPeriodId, .data$caseId, .data$personId, .data$startDate, .data$endDay, .data$ageInDays, .data$offset, .data$noninformativeEndCensor)

  outcomes <- outcomes %>%
    inner_join(select(cases, .data$caseId, .data$offset), by = "caseId") %>%
    mutate(startDay = .data$startDay - .data$offset) %>%
    select(.data$caseId, outcomeDay = .data$startDay)

  result <- list(outcomes = outcomes, cases = cases, metaData = metaData)
  if (nrow(outcomes) == 0) {
    warning("No cases left in study population.")
  }
  return(result)
}

countOutcomes <- function(outcomes, cases, description) {
  counts <- outcomes %>%
    inner_join(cases, by = "caseId") %>%
    group_by(.data$eraId) %>%
    summarise(outcomeSubjects = n_distinct(.data$personId),
              outcomeEvents = n(),
              outcomeObsPeriods = n_distinct(.data$caseId),
              .groups = "drop_last") %>%
    rename(outcomeId = .data$eraId) %>%
    mutate(description = description)
  return(counts)
}

#' Get the attrition table for a population
#'
#' @param object   Either an object of type \link{SccsData}, a population object generated by
#'                 functions like \code{\link{createStudyPopulation}}, or an object of type
#'                 \code{outcomeModel}.
#'
#' @return
#' A `tibble` specifying the number of people and exposures in the population after specific steps
#' of filtering.
#'
#'
#' @export
getAttritionTable <- function(object) {
  if ("metaData" %in% names(object)) {
    return(object$metaData$attrition)
  } else if (is(object, "OutcomeModel")) {
    return(object$attrition)
  } else {
    return(attr(object, "metaData")$attrition)
  }
}
