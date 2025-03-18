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

#' Create a study population
#'
#' @details
#' Create a study population for a specific outcome, applying several restrictions.
#'
#' @template SccsData
#' @param outcomeId             The outcome to create the era data for. If not specified it is
#'                              assumed to be the one outcome for which the data was loaded from
#'                              the database.
#' @param createStudyPopulationArgs An object of type `CreateStudyPopulationArgs` as created using the
#'                                  `createCreateStudyPopulationArgs()` function.
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
                                  createStudyPopulationArgs) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsData, "SccsData", add = errorMessages)
  checkmate::assertInt(outcomeId, null.ok = TRUE, add = errorMessages)
  checkmate::assertR6(createStudyPopulationArgs, "CreateStudyPopulationArgs", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  metaData <- list(
    exposureIds = attr(sccsData, "metaData")$exposureIds
  )

  outcomes <- sccsData$eras |>
    filter(.data$eraType == "hoi") |>
    collect()

  cases <- sccsData$cases |>
    collect()
  if (!is(cases$observationPeriodStartDate, "Date")) {
    cases <- cases |>
      mutate(observationPeriodStartDate = Andromeda::restoreDate(.data$observationPeriodStartDate))
  }
  attrition <- attr(sccsData, "metaData")$attrition
  prevalence <- attr(sccsData, "metaData")$prevalences |>
    mutate(definitelyFirstOutcomeOnly = createStudyPopulationArgs$firstOutcomeOnly)
  if (is.null(outcomeId)) {
    if (outcomes |>
      distinct(.data$eraId) |>
      count() |>
      pull() > 1) {
      stop("No outcome ID specified, but more than one outcome ID found.")
    }
  } else {
    outcomes <- outcomes |>
      filter(.data$eraId == !!outcomeId)

    attrition <- attrition |>
      filter(.data$outcomeId == !!outcomeId)

    prevalence <- prevalence |>
      filter(.data$outcomeId == !!outcomeId)
  }

  if (createStudyPopulationArgs$firstOutcomeOnly) {
    outcomes <- outcomes |>
      group_by(.data$caseId) |>
      filter(row_number(.data$eraStartDay ) == 1) |>
      ungroup()

    attrition <- bind_rows(
      attrition,
      countOutcomes(outcomes, cases, "First outcome only")
    )
  }

  if (createStudyPopulationArgs$naivePeriod != 0) {
    cases <- cases |>
      mutate(startDay = ifelse(
        createStudyPopulationArgs$naivePeriod > .data$startDay,
        createStudyPopulationArgs$naivePeriod,
        .data$startDay
      )) |>
      filter(.data$startDay < .data$endDay)

    outcomes <- outcomes |>
      inner_join(select(cases, "caseId", "startDay"),
                 by = join_by("caseId", "eraStartDay" >= "startDay")) |>
      select(-"startDay")

    cases <- cases |>
      filter(.data$caseId %in% unique(outcomes$caseId))

    attrition <- bind_rows(
      attrition,
      countOutcomes(outcomes, cases, sprintf("Requiring %s days naive period", createStudyPopulationArgs$naivePeriod))
    )
  }

  if (!is.null(createStudyPopulationArgs$minAge) || !is.null(createStudyPopulationArgs$maxAge)) {
    labels <- c()
    if (!is.null(createStudyPopulationArgs$minAge) && nrow(cases) > 0) {
      minAgeInDays <- createStudyPopulationArgs$minAge * 365.25
      cases <- cases |>
        mutate(startAge = .data$ageAtObsStart + .data$startDay ,
               endAge = .data$ageAtObsStart + .data$endDay) |>
        mutate(startAge = case_when(
          .data$startAge < minAgeInDays ~ minAgeInDays,
          TRUE ~ .data$startAge
        )) |>
        filter(.data$startAge < .data$endAge) |>
        mutate(startDay = .data$startAge - .data$ageAtObsStart,
               endDay = .data$endAge - .data$ageAtObsStart) |>
        select(-"startAge", -"endAge")
      labels <- c(labels, sprintf("age >= %s", createStudyPopulationArgs$minAge))
    }
    if (!is.null(createStudyPopulationArgs$maxAge) && nrow(cases) > 0) {
      maxAgeInDays <- round((createStudyPopulationArgs$maxAge + 1) * 365.25) - 1
      cases <- cases |>
        mutate(startAge = .data$ageAtObsStart + .data$startDay ,
               endAge = .data$ageAtObsStart + .data$endDay) |>
        mutate(
          noninformativeEndCensor = case_when(
            .data$endAge > maxAgeInDays ~ 1,
            TRUE ~ noninformativeEndCensor
          ),
          endAge = case_when(
            .data$endAge > maxAgeInDays ~ maxAgeInDays,
            TRUE ~ .data$endAge
          )
        ) |>
        filter(.data$startAge < .data$endAge) |>
        mutate(startDay = .data$startAge - .data$ageAtObsStart,
               endDay = .data$endAge - .data$ageAtObsStart) |>
        select(-"startAge", -"endAge")
      labels <- c(labels, sprintf("age <= %s", createStudyPopulationArgs$maxAge))
    }

    outcomes <- outcomes |>
      inner_join(select(cases, "caseId", "startDay", "endDay"),
                 by = join_by("caseId", "eraStartDay" >= "startDay", "eraStartDay" <= "endDay")) |>
      select(-"startDay", -"endDay")

    attrition <- bind_rows(
      attrition,
      countOutcomes(outcomes, cases, paste("Restrict to", paste(labels, collapse = " & ")))
    )
  }

  if (!is.null(createStudyPopulationArgs$genderConceptIds)) {
    cases <- cases |>
      filter(.data$genderConceptId %in% createStudyPopulationArgs$genderConceptIds)

    outcomes <- outcomes  |>
      filter(.data$caseId %in% cases$caseId)

    attrition <- bind_rows(
      attrition,
      countOutcomes(outcomes, cases, sprintf("Restricting gender to concept(s) %s", paste(createStudyPopulationArgs$genderConceptIds, collapse = ", ")))
    )
  }

  if (!is.null(createStudyPopulationArgs$restrictTimeToEraId)) {
    minMaxDates <- sccsData$eraRef |>
      filter(.data$eraId == createStudyPopulationArgs$restrictTimeToEraId) |>
      collect()
    if (nrow(minMaxDates) == 0) {
      warning(sprintf("Era with ID %d was not observed in the data at all, so cannot restrict time to when it was observed", createStudyPopulationArgs$restrictTimeToEraId))
    } else {
      minDate <- minMaxDates$minObservedDate
      maxDate <- minMaxDates$maxObservedDate
      cases <- cases |>
        mutate(startDate = .data$observationPeriodStartDate + .data$startDay,
               endDate = .data$observationPeriodStartDate + .data$endDay) |>
        mutate(
          startDate = case_when(
            minDate > .data$startDate ~ minDate,
            TRUE ~ .data$startDate
          ),
          endDate = case_when(
            maxDate < .data$endDate ~ maxDate,
            TRUE ~ .data$endDate
          )
        ) |>
        filter(.data$startDate < .data$endDate) |>
        mutate(startDay = as.numeric(.data$startDate - .data$observationPeriodStartDate),
               endDay = as.numeric(.data$endDate - .data$observationPeriodStartDate)) |>
        select(-"startDate", -"endDate")

      outcomes <- outcomes   |>
        inner_join(select(cases, "caseId", "startDay", "endDay"),
                   by = join_by("caseId", "eraStartDay" >= "startDay", "eraStartDay" <= "endDay")) |>
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
  metaData$prevalence <- prevalence

  # Restrict cases to those that have at least one outcome between start and end:
  cases <- outcomes |>
    select("caseId", "eraStartDay") |>
    inner_join(cases,
               by = join_by("caseId", between("eraStartDay", "startDay", "endDay"))) |>
    select(-"eraStartDay") |>
    distinct()

  cases <- cases |>
    select("observationPeriodId", "caseId", "personId", "observationPeriodStartDate", "ageAtObsStart", "startDay", "endDay", "noninformativeEndCensor")

  outcomes <- outcomes |>
    select("caseId", outcomeDay = "eraStartDay")

  result <- list(outcomes = outcomes, cases = cases, metaData = metaData)
  if (nrow(outcomes) == 0) {
    warning("No cases left in study population.")
  }
  return(result)
}

countOutcomes <- function(outcomes, cases, description) {
  counts <- outcomes |>
    inner_join(cases, by = join_by("caseId", between("eraStartDay", "startDay", "endDay"))) |>
    group_by(.data$eraId) |>
    summarise(
      outcomeSubjects = n_distinct(.data$personId),
      outcomeEvents = n(),
      outcomeObsPeriods = n_distinct(.data$caseId),
      observedDays = sum(.data$endDay - .data$startDay + 1),
      .groups = "drop_last"
    ) |>
    rename(outcomeId = "eraId") |>
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
