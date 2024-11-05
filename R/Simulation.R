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

#' Create a risk window definition for simulation
#'
#' @param start                 Start of the risk window relative to exposure start.
#' @param end                   The end of the risk window (in days) relative to the `endAnchor`.
#' @param endAnchor             The anchor point for the end of the risk window. Can be `"era start"`
#'                              or `"era end"`.
#' @param splitPoints           Subdivision of the risk window in to smaller sub-windows.
#' @param relativeRisks         Either a single number representing the relative risk in the risk
#'                              window, or when splitPoints have been defined a vector of relative
#'                              risks, one for each sub-window.
#'
#' @return
#' An object of type `SimulationRiskWindow`.
#'
#' @export
createSimulationRiskWindow <- function(start = 0,
                                       end = 0,
                                       endAnchor = "era end",
                                       splitPoints = c(),
                                       relativeRisks = c(0)) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertInt(start, add = errorMessages)
  checkmate::assertInt(end, add = errorMessages)
  checkmate::assert_choice(endAnchor, c("era start", "era end"), add = errorMessages)
  checkmate::assertIntegerish(splitPoints, null.ok = TRUE, add = errorMessages)
  checkmate::assertNumeric(relativeRisks, lower = 0, min.len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  isEnd <- function(anchor) {
    return(grepl("end$", anchor, ignore.case = TRUE))
  }

  # First: get default values:
  analysis <- list()
  for (name in names(formals(createSimulationRiskWindow))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis)) {
      analysis[[name]] <- values[[name]]
    }
  }
  class(analysis) <- "SimulationRiskWindow"
  return(analysis)
}

#' Create SCCS simulation settings
#'
#' @details
#' Create an object of settings for an SCCS simulation.
#'
#' @param meanPatientTime             Mean number of observation days per patient.
#' @param sdPatientTime               Standard deviation of the observation days per patient.
#' @param minAge                      The minimum age in days.
#' @param maxAge                      The maximum age in days.
#' @param minBaselineRate             The minimum baseline rate (per day).
#' @param maxBaselineRate             The maximum baseline rate (per day).
#' @param minCalendarTime             The minimum date patients are to be observed.
#' @param maxCalendarTime             The maximum date patients are to be observed.
#' @param eraIds                      The IDs for the covariates to be generated.
#' @param patientUsages               The fraction of patients that use the drugs.
#' @param usageRate                   The rate of prescriptions per person that uses the drug.
#' @param usageRateSlope              The change in the usage rate from one day to the next.
#'                                    `usageRate` is the intercept at day 0
#' @param meanPrescriptionDurations   The mean duration of a prescription, per drug.
#' @param sdPrescriptionDurations     The standard deviation of the duration of a prescription, per
#'                                    drug.
#' @param simulationRiskWindows       One or a list of objects of type `SimulationRiskWindow` as
#'                                    created using the [createSimulationRiskWindow()] function.
#'                                    function.
#' @param includeAgeEffect            Include an age effect for the outcome?
#' @param ageKnots                    Number of knots in the age spline.
#' @param includeSeasonality          Include seasonality for the outcome?
#' @param seasonKnots                 Number of knots in the seasonality spline.
#' @param includeCalendarTimeEffect   Include a calendar time effect for the outcome?
#' @param calendarTimeKnots           Number of knots in the calendar time spline.
#' @param outcomeId                   The ID to be used for the outcome.
#'
#' @return
#' An object of type `SccsSimulationSettings`.
#'
#' @export
createSccsSimulationSettings <- function(meanPatientTime = 4 * 365,
                                         sdPatientTime = 2 * 365,
                                         minAge = 18 * 365,
                                         maxAge = 65 * 365,
                                         minBaselineRate = 0.001,
                                         maxBaselineRate = 0.01,
                                         minCalendarTime = as.Date("2000-01-01"),
                                         maxCalendarTime = as.Date("2010-01-01"),
                                         eraIds = c(1, 2),
                                         patientUsages = c(0.2, 0.1),
                                         usageRate = c(0.01, 0.01),
                                         usageRateSlope = c(0, 0),
                                         meanPrescriptionDurations = c(14, 30),
                                         sdPrescriptionDurations = c(7, 14),
                                         simulationRiskWindows = list(
                                           createSimulationRiskWindow(relativeRisks = 1),
                                           createSimulationRiskWindow(relativeRisks = 1.5)
                                         ),
                                         includeAgeEffect = TRUE,
                                         ageKnots = 5,
                                         includeSeasonality = TRUE,
                                         seasonKnots = 5,
                                         includeCalendarTimeEffect = TRUE,
                                         calendarTimeKnots = 5,
                                         outcomeId = 10) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertNumeric(meanPatientTime, lower = 0, len = 1, add = errorMessages)
  checkmate::assertNumeric(sdPatientTime, lower = 0, len = 1, add = errorMessages)
  checkmate::assertInt(minAge, add = errorMessages)
  checkmate::assertInt(maxAge, add = errorMessages)
  checkmate::assertNumeric(minBaselineRate, lower = 0, len = 1, add = errorMessages)
  checkmate::assertNumeric(maxBaselineRate, lower = 0, len = 1, add = errorMessages)
  checkmate::assertDate(minCalendarTime, len = 1, add = errorMessages)
  checkmate::assertDate(maxCalendarTime, len = 1, add = errorMessages)
  checkmate::assertIntegerish(eraIds, min.len = 1, add = errorMessages)
  checkmate::assertNumeric(patientUsages, lower = 0, min.len = 1, add = errorMessages)
  checkmate::assertNumeric(usageRate, lower = 0, min.len = 1, add = errorMessages)
  checkmate::assertNumeric(usageRateSlope, min.len = 1, add = errorMessages)
  checkmate::assertNumeric(meanPrescriptionDurations, lower = 0, min.len = 1, add = errorMessages)
  checkmate::assertNumeric(sdPrescriptionDurations, lower = 0, min.len = 1, add = errorMessages)
  checkmate::assertList(simulationRiskWindows, min.len = 1, add = errorMessages)
  for (i in 1:length(simulationRiskWindows)) {
    checkmate::assertClass(simulationRiskWindows[[i]], "SimulationRiskWindow", add = errorMessages)
  }
  checkmate::assertLogical(includeAgeEffect, len = 1, add = errorMessages)
  checkmate::assertInt(ageKnots, lower = 2, add = errorMessages)
  checkmate::assertLogical(includeSeasonality, len = 1, add = errorMessages)
  checkmate::assertInt(seasonKnots, lower = 2, add = errorMessages)
  checkmate::assertLogical(includeCalendarTimeEffect, len = 1, add = errorMessages)
  checkmate::assertInt(calendarTimeKnots, lower = 2, add = errorMessages)
  checkmate::assertInt(outcomeId, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  # First: get default values:
  analysis <- list()
  for (name in names(formals(createSccsSimulationSettings))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis)) {
      analysis[[name]] <- values[[name]]
    }
  }
  class(analysis) <- "SccsSimulationSettings"
  return(analysis)
}

simulateBatch <- function(settings, ageFun, seasonFun, calendarTimeFun, caseIdOffset) {
  # Simulate a batch of persons, and eliminate non-cases
  n <- 1000

  # Generate patients  -----------------------------------------------------------------------------
  observationDays <- round(rnorm(n, settings$meanPatientTime, settings$sdPatientTime))
  observationDays[observationDays < 1] <- 1
  observationDays[observationDays > settings$maxAge - settings$minAge] <- settings$maxAge - settings$minAge
  maxCalendarDays <- as.numeric(settings$maxCalendarTime) - as.numeric(settings$minCalendarTime)
  observationDays[observationDays > maxCalendarDays] <- maxCalendarDays
  ageInDays <- round(runif(n, settings$minAge, settings$maxAge - observationDays))
  observationPeriodStartDate <- round(runif(
    n,
    rep(as.numeric(settings$minCalendarTime), n),
    as.numeric(settings$maxCalendarTime) - observationDays
  ))
  observationPeriodStartDate <- as.Date(observationPeriodStartDate, origin = "1970-01-01")
  cases <- tibble(
    observationPeriodId = 1:n,
    caseId = 1:n,
    personId = 1:n,
    observationPeriodStartDate = observationPeriodStartDate,
    startDay = 0,
    endDay = observationDays,
    ageAtObsStart = ageInDays,
    noninformativeEndCensor = 0
  )

  # Generate eras ----------------------------------------------------------------------------------
  cumulativeIntensity <- function(t, usageRate, usageRateSlope) {
    usageRate * t + 0.5 * usageRateSlope * t^2
  }

  inverseCumulativeIntensity <- function(u, usageRate, usageRateSlope, days) {
    if (usageRateSlope == 0) {
      t <- u * days
    } else {
      lambdaT <- usageRate * days + 0.5 * usageRateSlope * days^2
      t <- (-usageRate + sqrt(usageRate^2 + 2 * usageRateSlope * u * lambdaT)) / usageRateSlope
    }
    return(t)
  }

  sampleErasForPerson <- function(idx, eraId, usageRate, usageRateSlope, meanPrescriptionDurations, sdPrescriptionDurations) {
    nEvents <- rpois(1, cumulativeIntensity(observationDays[idx], usageRate, usageRateSlope))
    if (nEvents == 0) {
      return(tibble())
    } else {
      u <- runif(nEvents)
      startDay <- sapply(u,
                         inverseCumulativeIntensity,
                         usageRate = settings$usageRate[i],
                         usageRateSlope = settings$usageRateSlope[i],
                         days = observationDays[idx]) |>
        round() |>
        unique() |>
        sort()
      duration <- round(rnorm(length(startDay), meanPrescriptionDurations, sdPrescriptionDurations))
      duration[duration < 1] <- 1
      endDay <- startDay + duration
      endDay[endDay > observationDays[idx]] <- observationDays[idx]
      newEras <- tibble(
        eraType = "rx",
        caseId = idx,
        eraId = eraId,
        eraValue = 1,
        eraStartDay = startDay,
        eraEndDay = endDay
      )
      return(newEras)
    }
  }

  eras <- tibble()
  for (i in 1:length(settings$eraIds)) {
    # i <- 1
    patientsOnDrug <- sample.int(nrow(cases),
                                 settings$patientUsages[i] * nrow(cases),
                                 replace = FALSE
    )
    patientsOnDrug <- sort(patientsOnDrug)

    newEras <- lapply(patientsOnDrug,
                      sampleErasForPerson,
                      eraId = settings$eraIds[i],
                      usageRate = settings$usageRate[i],
                      usageRateSlope = settings$usageRateSlope[i],
                      meanPrescriptionDurations = settings$meanPrescriptionDurations[i],
                      sdPrescriptionDurations = settings$sdPrescriptionDurations[i])
    newEras <- bind_rows(newEras)
    eras <- bind_rows(eras, newEras)
  }
  eras <- eras[order(eras$caseId, eras$eraId), ]

  # Generate outcomes  -----------------------------------------------------------------------------
  baselineRates <- runif(n, min = settings$minBaselineRate, max = settings$maxBaselineRate)
  if (settings$includeAgeEffect) {
    ageRrs <- exp(ageFun(settings$minAge:settings$maxAge))
  } else {
    ageRrs <- c(0)
  }
  if (settings$includeSeasonality) {
    seasonRrs <- exp(seasonFun(1:366))
  } else {
    seasonRrs <- c(0)
  }
  if (settings$includeCalendarTimeEffect) {
    calendarTimeRrs <- exp(calendarTimeFun(as.numeric(settings$minCalendarTime):as.numeric(settings$maxCalendarTime)))
  } else {
    calendarTimeRrs <- c(0)
  }
  # Apply risk windows:
  newEras <- tibble()
  eraIds <- c()
  rrs <- c()
  eraId <- 1000
  for (i in 1:length(settings$simulationRiskWindows)) {
    simulationRiskWindow <- settings$simulationRiskWindows[[i]]
    sourceEras <- eras[eras$eraId == settings$eraIds[i], ]
    riskEnds <- rep(simulationRiskWindow$end, nrow(sourceEras))
    if (simulationRiskWindow$endAnchor == "era end") {
      riskEnds <- riskEnds + sourceEras$eraEndDay - sourceEras$eraStartDay
    }
    start <- simulationRiskWindow$start
    for (j in 1:(length(simulationRiskWindow$splitPoints) + 1)) {
      if (j > length(simulationRiskWindow$splitPoints)) {
        end <- riskEnds
      } else {
        end <- simulationRiskWindow$splitPoints[j]
      }
      truncatedEnds <- riskEnds
      truncatedEnds[truncatedEnds > end] <- end
      filteredIndex <- truncatedEnds >= start
      riskEras <- tibble(
        eraType = "rx",
        caseId = sourceEras$caseId[filteredIndex],
        eraId = eraId,
        eraValue = 1,
        eraStartDay = sourceEras$eraStartDay[filteredIndex] + start,
        eraEndDay = sourceEras$eraStartDay[filteredIndex] + truncatedEnds[filteredIndex]
      )
      newEras <- rbind(newEras, riskEras)
      eraIds <- c(eraIds, eraId)
      rrs <- c(rrs, simulationRiskWindow$relativeRisks[j])
      eraId <- eraId + 1
      start <- end + 1
    }
  }
  newEras <- newEras[order(newEras$caseId, newEras$eraId), ]
  eraRrs <- tibble(eraId = eraIds, rr = rrs)
  outcomes <- simulateSccsOutcomes(
    cases,
    newEras,
    baselineRates,
    eraRrs,
    settings$includeAgeEffect,
    settings$minAge,
    ageRrs,
    settings$includeSeasonality,
    seasonRrs,
    settings$includeCalendarTimeEffect,
    settings$minCalendarTime,
    calendarTimeRrs
  )
  outcomes <- tibble(
    eraType = "hoi",
    caseId = outcomes$caseId,
    eraId = settings$outcomeId,
    eraValue = 1,
    eraStartDay = outcomes$startDay,
    eraEndDay = outcomes$startDay
  )

  # ** Remove non-cases ***
  caseIds <- unique(outcomes$caseId)
  cases <- cases[cases$caseId %in% caseIds, ]
  eras <- eras[eras$caseId %in% caseIds, ]

  eras <- rbind(eras, outcomes)
  eras <- eras[order(eras$caseId), ]
  cases$caseId <- cases$caseId + caseIdOffset
  cases$observationPeriodId <- cases$observationPeriodId + caseIdOffset
  cases$personId <- cases$personId + caseIdOffset
  eras$caseId <- eras$caseId + caseIdOffset
  result <- list(cases = cases, eras = eras)
  return(result)
}

#' Simulate SCCS data
#'
#' @param nCases     The number of cases to simulate.
#' @param settings   An object of type `SccsSimulationSettings` as created using the
#'                   [createSccsSimulationSettings()] function.
#'
#' @return
#' An object of type `SccsData`.
#'
#' @export
simulateSccsData <- function(nCases, settings) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertInt(nCases, lower = 1, add = errorMessages)
  checkmate::assertClass(settings, "SccsSimulationSettings", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (settings$includeAgeEffect) {
    age <- seq(settings$minAge, settings$maxAge, length.out = settings$ageKnots)
    ageRisk <- runif(settings$ageKnots, -2, 2)
    ageFun <- splinefun(age, ageRisk)
  } else {
    ageFun <- NULL
  }
  if (settings$includeSeasonality) {
    seasonKnots <- seq(1, 366, length.out = settings$seasonKnots)
    seasonRisk <- runif(settings$seasonKnots - 1, -2, 2)
    seasonFun <- function(x) {
      designMatrix <- cyclicSplineDesign(x, seasonKnots)
      return(apply(designMatrix %*% seasonRisk, 1, sum))
    }
  } else {
    seasonFun <- NULL
  }
  if (settings$includeCalendarTimeEffect) {
    calendarTime <- seq(settings$minCalendarTime, settings$maxCalendarTime, length.out = settings$calendarTimeKnots)
    calendarTimeRisk <- runif(settings$calendarTimeKnots, -2, 2)
    calendarTimeFun <- splinefun(as.numeric(calendarTime), calendarTimeRisk)
  } else {
    calendarTimeFun <- NULL
  }

  cases <- tibble()
  eras <- tibble()
  lastCaseId <- 0
  while (nrow(cases) < nCases) {
    batch <- simulateBatch(settings, ageFun, seasonFun, calendarTimeFun, lastCaseId)
    need <- nCases - nrow(cases)
    if (nrow(batch$cases) < need) {
      cases <- rbind(cases, batch$cases)
      eras <- rbind(eras, batch$eras)
      lastCaseId <- max(batch$cases$caseId)
    } else {
      cases <- rbind(cases, batch$cases[1:need, ])
      eras <- rbind(
        eras,
        batch$eras[batch$eras$caseId %in% batch$cases$caseId[1:need], ]
      )
    }
  }
  cases$observationPeriodId <- as.character(cases$observationPeriodId)
  cases$personId <- as.character(cases$personId)
  data <- Andromeda::andromeda(
    cases = cases,
    eras = eras,
    eraRef = tibble(
      eraId = settings$eraIds,
      eraType = "",
      eraName = ""
    )
  )

  attr(data, "metaData") <- list(
    sccsSimulationSettings = settings,
    ageFun = ageFun,
    seasonFun = seasonFun,
    calendarTimeFun = calendarTimeFun,
    exposureIds = settings$eraIds,
    outcomeIds = settings$outcomeId,
    prevalences = tibble(
      outcomeId = settings$outcomeId,
      outcomeProportion = 0.01,
      probablyFirstOutcomeOnly = FALSE
    ),
    attrition = tibble(
      outcomeId = settings$outcomeId,
      description = "All outcome occurrences",
      outcomeSubjects = 0,
      outcomeEvents = 0,
      outcomeObsPeriods = 0
    )
  )

  class(data) <- "SccsData"
  attr(class(data), "package") <- "SelfControlledCaseSeries"
  return(data)
}
