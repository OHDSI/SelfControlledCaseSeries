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

#' Create a study population
#'
#' @details
#' Create a study population for a specific outcome, applying several restrictions.
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
#' @param genderConceptIds      Set of gender concept IDs to restrict the population to. If not specified,
#'                              no restriction on gender will be applied.
#' @param restrictTimeToEraId   If provided, study time (for all patients) will be restricted to the calender
#'                              time when that era was observed in the data. For example, if the era ID refers
#'                              to a drug, study time will be restricted to when the drug was on the market.
#'
#' @return
#' A `list` specifying the study population, with the following items:
#'
#' - `cases`: A `tibble` with one row per observation period of a person with the outcome.
#' - `outcomes`: A `tibble` listing the days when a case has the outcome.
#' - `metaData`: A `list` with meta data about the study population, including the attrition.
#'
#' @export
createStudyPopulation <- function(sccsData,
                                  outcomeId = NULL,
                                  firstOutcomeOnly = FALSE,
                                  naivePeriod = 0,
                                  minAge = NULL,
                                  maxAge = NULL,
                                  genderConceptIds = NULL,
                                  restrictTimeToEraId = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsData, "SccsData", add = errorMessages)
  checkmate::assertInt(outcomeId, null.ok = TRUE, add = errorMessages)
  checkmate::assertLogical(firstOutcomeOnly, len = 1, add = errorMessages)
  checkmate::assertInt(naivePeriod, lower = 0, add = errorMessages)
  checkmate::assertNumeric(minAge, lower = 0, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertNumeric(maxAge, lower = 0, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertIntegerish(genderConceptIds, lower = 0, null.ok = TRUE, add = errorMessages)
  checkmate::assertIntegerish(restrictTimeToEraId, lower = 0, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (!is.null(minAge) && !is.null(maxAge) && maxAge < minAge) {
    stop("Maxinum age should be greater than or equal to minimum age")
  }

  metaData <- list(
    exposureIds = attr(sccsData, "metaData")$exposureIds
  )

  outcomes <- sccsData$eras %>%
    filter(.data$eraType == "hoi") %>%
    collect()

  cases <- sccsData$cases %>%
    collect()
  if (!is(cases$observationPeriodStartDate, "Date")) {
    cases <- cases %>%
      mutate(observationPeriodStartDate = Andromeda::restoreDate(.data$observationPeriodStartDate))
  }
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
      filter(row_number(.data$eraStartDay ) == 1) %>%
      ungroup()

    attrition <- bind_rows(
      attrition,
      countOutcomes(outcomes, cases, "First outcome only")
    )
  }

  if (naivePeriod != 0) {
    cases <- cases %>%
      mutate(startDay = ifelse(
        naivePeriod > .data$startDay,
        naivePeriod,
        .data$startDay
      )) %>%
      filter(.data$startDay < .data$endDay)

    outcomes <- outcomes %>%
      inner_join(select(cases, "caseId", "startDay"),
                 by = join_by("caseId", "eraStartDay" >= "startDay")) %>%
      select(-"startDay")

    cases <- cases %>%
      filter(.data$caseId %in% unique(outcomes$caseId))

    attrition <- bind_rows(
      attrition,
      countOutcomes(outcomes, cases, sprintf("Requiring %s days naive period", naivePeriod))
    )
  }

  if (!is.null(minAge) || !is.null(maxAge)) {
    labels <- c()
    if (!is.null(minAge) && nrow(cases) > 0) {
      minAgeInDays <- minAge * 365.25
      cases <- cases %>%
        mutate(startAge = .data$ageAtObsStart + .data$startDay ,
               endAge = .data$ageAtObsStart + .data$endDay) %>%
        mutate(startAge = case_when(
          .data$startAge < minAgeInDays ~ minAgeInDays,
          TRUE ~ .data$startAge
        )) %>%
        filter(.data$startAge < .data$endAge) %>%
        mutate(startDay = .data$startAge - .data$ageAtObsStart,
               endDay = .data$endAge - .data$ageAtObsStart) %>%
        select(-"startAge", -"endAge")
      labels <- c(labels, sprintf("age >= %s", minAge))
    }
    if (!is.null(maxAge) && nrow(cases) > 0) {
      maxAgeInDays <- round((maxAge + 1) * 365.25) - 1
      cases <- cases %>%
        mutate(startAge = .data$ageAtObsStart + .data$startDay ,
               endAge = .data$ageAtObsStart + .data$endDay) %>%
        mutate(
          noninformativeEndCensor = case_when(
            .data$endAge > maxAgeInDays ~ 1,
            TRUE ~ noninformativeEndCensor
          ),
          endAge = case_when(
            .data$endAge > maxAgeInDays ~ maxAgeInDays,
            TRUE ~ .data$endAge
          )
        ) %>%
        filter(.data$startAge < .data$endAge) %>%
        mutate(startDay = .data$startAge - .data$ageAtObsStart,
               endDay = .data$endAge - .data$ageAtObsStart) %>%
        select(-"startAge", -"endAge")
      labels <- c(labels, sprintf("age <= %s", maxAge))
    }

    outcomes <- outcomes %>%
      inner_join(select(cases, "caseId", "startDay", "endDay"),
                 by = join_by("caseId", "eraStartDay" >= "startDay", "eraStartDay" <= "endDay")) %>%
      select(-"startDay", -"endDay")

    attrition <- bind_rows(
      attrition,
      countOutcomes(outcomes, cases, paste("Restrict to", paste(labels, collapse = " & ")))
    )
  }

  if (!is.null(genderConceptIds)) {
    cases <- cases %>%
      filter(.data$genderConceptId %in% genderConceptIds)

    outcomes <- outcomes  %>%
      filter(.data$caseId %in% cases$caseId)

    attrition <- bind_rows(
      attrition,
      countOutcomes(outcomes, cases, sprintf("Restricting gender to concept(s) %s", paste(genderConceptIds, collapse = ", ")))
    )
  }

  if (!is.null(restrictTimeToEraId)) {
    minMaxDates <- sccsData$eraRef %>%
      filter(.data$eraId == restrictTimeToEraId) %>%
      collect()
    if (nrow(minMaxDates) == 0) {
      warning(sprintf("Era with ID %d was not observed in the data at all, so cannot restrict time to when it was observed", restrictTimeToEraId))
    } else {
      minDate <- minMaxDates$minObservedDate
      maxDate <- minMaxDates$maxObservedDate
      cases <- cases %>%
        mutate(startDate = .data$observationPeriodStartDate + .data$startDay,
               endDate = .data$observationPeriodStartDate + .data$endDay) %>%
        mutate(
          startDate = case_when(
            minDate > .data$startDate ~ minDate,
            TRUE ~ .data$startDate
          ),
          endDate = case_when(
            maxDate < .data$endDate ~ maxDate,
            TRUE ~ .data$endDate
          )
        ) %>%
        filter(.data$startDate < .data$endDate) %>%
        mutate(startDay = as.numeric(.data$startDate - .data$observationPeriodStartDate),
               endDay = as.numeric(.data$endDate - .data$observationPeriodStartDate)) %>%
        select(-"startDate", -"endDate")

      outcomes <- outcomes   %>%
        inner_join(select(cases, "caseId", "startDay", "endDay"),
                   by = join_by("caseId", "eraStartDay" >= "startDay", "eraStartDay" <= "endDay")) %>%
        select(-"startDay", -"endDay")

      attrition <- bind_rows(
        attrition,
        countOutcomes(outcomes, cases, sprintf("Restricting to time when %s was observed", minMaxDates$eraName))
      )
      metaData$restrictedTimeToEra <- minMaxDates
    }
  }

  metaData$outcomeId <- unique(outcomes$eraId)
  metaData$attrition <- attrition

  # Restrict cases to those that have at least one outcome between start and end:
  cases <- outcomes %>%
    select("caseId", "eraStartDay") %>%
    inner_join(cases,
               by = join_by("caseId", between("eraStartDay", "startDay", "endDay"))) %>%
    select(-"eraStartDay") %>%
    distinct()

  cases <- cases %>%
    select("observationPeriodId", "caseId", "personId", "observationPeriodStartDate", "ageAtObsStart", "startDay", "endDay", "noninformativeEndCensor")

  outcomes <- outcomes %>%
    select("caseId", outcomeDay = "eraStartDay")

  result <- list(outcomes = outcomes, cases = cases, metaData = metaData)
  if (nrow(outcomes) == 0) {
    warning("No cases left in study population.")
  }
  return(result)
}

countOutcomes <- function(outcomes, cases, description) {
  counts <- outcomes %>%
    inner_join(cases, by = join_by("caseId", between("eraStartDay", "startDay", "endDay"))) %>%
    group_by(.data$eraId) %>%
    summarise(
      outcomeSubjects = n_distinct(.data$personId),
      outcomeEvents = n(),
      outcomeObsPeriods = n_distinct(.data$caseId),
      observedDays = sum(.data$endDay - .data$startDay + 1),
      .groups = "drop_last"
    ) %>%
    rename(outcomeId = "eraId") %>%
    mutate(description = description)
  return(counts)
}

#' Get the attrition table for a population
#'
#' @param object   Either an object of type `SccsData`, a population object generated by
#'                 functions like [createStudyPopulation()], or an object of type
#'                 `outcomeModel`.
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
