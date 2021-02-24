# @file Simulation.R
#
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

#' Create a risk window definition for simulation
#'
#' @param start                 Start of the risk window relative to exposure start.
#' @param end                     The end of the risk window (in days) relative to the `endAnchor`.
#' @param endAnchor               The anchor point for the end of the risk window. Can be `"era start"`
#'                                or `"era end"`.
#' @param splitPoints           Subdivision of the risk window in to smaller sub-windows.
#' @param relativeRisks         Either a single number representing the relative risk in the risk
#'                              window, or when splitPoints have been defined a vector of relative
#'                              risks, one for each sub-window.
#'
#' @return
#' An object of type \code{simulationRiskWindow}.
#'
#' @export
createSimulationRiskWindow <- function(start = 0,
                                       end = 0,
                                       endAnchor = "era end",
                                       splitPoints = c(),
                                       relativeRisks = c(0)) {
  if (!grepl("start$|end$", endAnchor, ignore.case = TRUE)) {
    stop("endAnchor should have value 'era start' or 'era end'")
  }
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
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  class(analysis) <- "simulationRiskWindow"
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
#' @param eraIds                The IDs for the covariates to be generated.
#' @param patientUsages               The fraction of patients that use the drugs.
#' @param usageRate                   The rate of prescriptions per person that uses the drug.
#' @param meanPrescriptionDurations   The mean duration of a prescription, per drug.
#' @param sdPrescriptionDurations     The standard deviation of the duration of a prescription, per
#'                                    drug.
#' @param simulationRiskWindows       One or a list of objects of type \code{simulationRiskWindow} as
#'                                    created using the \code{\link{createSimulationRiskWindow}}
#'                                    function.
#' @param includeAgeEffect            Include an age effect for the outcome?
#' @param ageKnots                    Number of knots in the age spline.
#' @param includeSeasonality          Include seasonality for the outcome?
#' @param seasonKnots                 Number of knots in the seasonality spline.
#' @param outcomeId                   The ID to be used for the outcome.
#'
#' @return
#' An object of type \code{sccsSimulationSettings}.
#'
#' @export
createSccsSimulationSettings <- function(meanPatientTime = 4 * 365,
                                         sdPatientTime = 2 * 365,
                                         minAge = 18 * 365,
                                         maxAge = 65 * 365,
                                         minBaselineRate = 0.001,
                                         maxBaselineRate = 0.01,
                                         eraIds = c(1, 2),
                                         patientUsages = c(0.2, 0.1),
                                         usageRate = c(0.01, 0.01),
                                         meanPrescriptionDurations = c(14, 30),
                                         sdPrescriptionDurations = c(7, 14),
                                         simulationRiskWindows = list(createSimulationRiskWindow(relativeRisks = 1),
                                                                      createSimulationRiskWindow(relativeRisks = 1.5)),
                                         includeAgeEffect = TRUE,
                                         ageKnots = 5,
                                         includeSeasonality = TRUE,
                                         seasonKnots = 5,
                                         outcomeId = 10) {
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createSccsSimulationSettings))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  class(analysis) <- "sccsSimulationSettings"
  return(analysis)
}

simulateBatch <- function(settings, ageFun, seasonFun, caseIdOffset) {
  # Simulate a batch of persons, and eliminate non-cases
  n <- 1000

  ### Generate patients ###
  observationDays <- round(rnorm(n, settings$meanPatientTime, settings$sdPatientTime))
  observationDays[observationDays < 1] <- 1
  observationDays[observationDays > settings$maxAge - settings$minAge] <- settings$maxAge - settings$minAge
  ageInDays <- round(runif(n, settings$minAge, settings$maxAge - observationDays))
  startYear <- round(runif(n, 2000, 2010))
  startMonth <- round(runif(n, 1, 12))
  startDay <- round(runif(n, 1, 28))
  cases <- tibble(observationPeriodId = 1:n,
                  caseId = 1:n,
                  personId = 1:n,
                  observationDays = observationDays,
                  ageInDays = ageInDays,
                  startYear = startYear,
                  startMonth = startMonth,
                  startDay = startDay,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)

  ### Generate eras ###
  eras <- tibble()
  for (i in 1:length(settings$eraIds)) {
    # i <- 1
    patientsOnDrug <- sample.int(nrow(cases),
                                 settings$patientUsages[i] * nrow(cases),
                                 replace = FALSE)
    patientsOnDrug <- patientsOnDrug[order(patientsOnDrug)]
    count <- rpois(length(patientsOnDrug), observationDays[patientsOnDrug] * settings$usageRate[i])
    observationPeriodId <- rep(patientsOnDrug, count)
    patientsOnDrug <- patientsOnDrug[count != 0]
    startDay <- round(runif(sum(count), 0, cases$observationDays[observationPeriodId]))
    duration <- round(rnorm(sum(count),
                            settings$meanPrescriptionDurations[i],
                            settings$sdPrescriptionDurations[i]))
    duration[duration < 1] <- 1
    endDay <- startDay + duration
    endDay[endDay > cases$observationDays[observationPeriodId]] <- cases$observationDays[observationPeriodId][endDay >
                                                                                                                cases$observationDays[observationPeriodId]]
    newEras <- tibble(eraType = "hei",
                      caseId = observationPeriodId,
                      eraId = settings$eraIds[i],
                      value = 1,
                      startDay = startDay,
                      endDay = endDay)
    eras <- rbind(eras, newEras)
  }
  eras <- eras[order(eras$caseId, eras$eraId), ]

  ### Generate outcomes ###
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
      riskEnds <- riskEnds + sourceEras$endDay - sourceEras$startDay
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
      riskEras <- tibble(eraType = "rx",
                         caseId = sourceEras$caseId[filteredIndex],
                         eraId = eraId,
                         value = 1,
                         startDay = sourceEras$startDay[filteredIndex] + start,
                         endDay = sourceEras$startDay[filteredIndex] + truncatedEnds[filteredIndex])
      newEras <- rbind(newEras, riskEras)
      eraIds <- c(eraIds, eraId)
      rrs <- c(rrs, simulationRiskWindow$relativeRisks[j])
      eraId <- eraId + 1
      start <- end + 1
    }
  }
  newEras <- newEras[order(newEras$caseId, newEras$eraId), ]
  eraRrs <- tibble(eraId = eraIds, rr = rrs)
  outcomes <- simulateSccsOutcomes(cases,
                                   newEras,
                                   baselineRates,
                                   eraRrs,
                                   settings$includeAge,
                                   settings$minAge,
                                   ageRrs,
                                   settings$includeSeasonality,
                                   seasonRrs)
  outcomes <- tibble(eraType = "hoi",
                     caseId = outcomes$caseId,
                     eraId = settings$outcomeId,
                     value = 1,
                     startDay = outcomes$startDay,
                     endDay = outcomes$startDay)

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
#' @param settings   An object of type \code{sccsSimulationSettings} as created using the \code{
#'                   \link{createSccsSimulationSettings}}.
#'
#' @return
#' An object of type \code{sccsData}.
#'
#' @export
simulateSccsData <- function(nCases, settings) {
  if (settings$includeAgeEffect) {
    age <- seq(settings$minAge, settings$maxAge, length.out = settings$ageKnots)
    risk <- runif(settings$ageKnots, -2, 2)
    ageFun <- splinefun(age, risk)
  } else {
    ageFun <- NULL
  }
  if (settings$includeSeasonality) {
    seasonKnots <- seq(1, 366, length.out = settings$seasonKnots)
    risk <- runif(settings$seasonKnots - 1, -2, 2)
    seasonFun <- function(x) {
      designMatrix <- cyclicSplineDesign(x, seasonKnots)
      return(apply(designMatrix %*% risk, 1, sum))
    }
  } else {
    seasonFun <- NULL
  }
  cases <- tibble()
  eras <- tibble()
  lastCaseId <- 0
  while (nrow(cases) < nCases) {
    batch <- simulateBatch(settings, ageFun, seasonFun, lastCaseId)
    need <- nCases - nrow(cases)
    if (nrow(batch$cases) < need) {
      cases <- rbind(cases, batch$cases)
      eras <- rbind(eras, batch$eras)
      lastCaseId <- max(batch$cases$caseId)
    } else {
      cases <- rbind(cases, batch$cases[1:need, ])
      eras <- rbind(eras,
                    batch$eras[batch$eras$caseId %in% batch$cases$caseId[1:need],])
    }
  }
  cases$observationPeriodId <- as.character(cases$observationPeriodId)
  cases$personId <- as.character(cases$personId)
  data <- Andromeda::andromeda(cases = cases,
                               eras = eras,
                               eraRef = tibble(eraId = settings$eraIds,
                                               eraType = "",
                                               eraName = ""))

  attr(data, "metaData") <- list(sccsSimulationSettings = settings,
                                 ageFun = ageFun,
                                 seasonFun = seasonFun,
                                 exposureIds = settings$eraIds,
                                 outcomeIds = settings$outcomeId,
                                 attrition = tibble(outcomeId = settings$outcomeId,
                                                    description = "Outcomes",
                                                    outcomeSubjects  = 0,
                                                    outcomeEvents  = 0,
                                                    outcomeObsPeriods = 0))

  class(data) <- "SccsData"
  attr(class(data), "package") <- "SelfControlledCaseSeries"
  return(data)
}
