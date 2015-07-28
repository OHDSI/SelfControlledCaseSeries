# @file Simulation.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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

#' @export
createSimulationRiskWindow <- function(start = 0, end = 0, addExposedDaysToEnd = TRUE, splitPoints = c(), relativeRisks = c(2)) {
  OhdsiRTools::convertArgsToList(match.call(), "simulationRiskWindow")
}

#' @export
createSccsSimulationSettings <- function(meanPatientTime = 4*365,
                                         sdPatientTime = 2*365,
                                         minAge = 18*365,
                                         maxAge = 65*365,
                                         minBaselineRate = 0.001,
                                         maxBaselineRate = 0.01,
                                         covariateIds = c(1,2),
                                         patientUsages = c(0.2,0.1),
                                         usageRate = c(0.01,0.01),
                                         meanPrescriptionDurations = c(14,30),
                                         sdPrescriptionDurations = c(7,14),
                                         simulationRiskWindows = list(createSimulationRiskWindow(),
                                                                      createSimulationRiskWindow(relativeRisks = 1.5)),
                                         includeAgeEffect = TRUE,
                                         ageKnots = 5,
                                         includeSeasonality = TRUE,
                                         seasonKnots = 5,
                                         outcomeId = 10) {
  return(OhdsiRTools::convertArgsToList(match.call(), "sccsSimulationSettings"))
}

simulateBatch <- function(settings, ageFun, seasonFun, caseIdOffset){
  # settings <- createSccsSimulationSettings(includeAgeEffect = FALSE, includeSeasonality = FALSE)
  # Simulate a batch of persons, and eliminate non-cases
  n <- 1000

  ### Generate patients ###
  observationDays <- round(rnorm(n, settings$meanPatientTime, settings$sdPatientTime))
  observationDays[observationDays < 1] <- 1
  observationDays[observationDays > settings$maxAge - settings$minAge] <- settings$maxAge - settings$minAge
  ageInDays = round(runif(n, settings$minAge, settings$maxAge-observationDays))
  observationStartYear <- round(runif(n, 2000,2010))
  observationStartMonth <- round(runif(n, 1, 12))
  observationStartDay <- round(runif(n, 1, 28))
  cases <- data.frame(observationPeriodId = 1:n,
                      personId = 1:n,
                      observationDays = observationDays,
                      ageInDays = ageInDays,
                      observationStartYear = observationStartYear,
                      observationStartMonth = observationStartMonth,
                      observationStartDay = observationStartDay)

  ### Generate eras ###
  eras <- data.frame()
  for (i in 1:length(settings$covariateIds)){
    # i <- 1
    patientsOnDrug <- sample.int(nrow(cases), settings$patientUsages[i] * nrow(cases), replace = FALSE)
    patientsOnDrug <- patientsOnDrug[order(patientsOnDrug)]
    count <- rpois(length(patientsOnDrug), observationDays[patientsOnDrug] * settings$usageRate[i])
    observationPeriodId <- rep(patientsOnDrug,count)
    patientsOnDrug <- patientsOnDrug[count != 0]
    startDay <- round(runif(sum(count), 0, cases$observationDays[observationPeriodId]))
    duration <- round(rnorm(sum(count), settings$meanPrescriptionDurations[i], settings$sdPrescriptionDurations[i]))
    duration[duration < 1] <- 1
    endDay <- startDay + duration
    endDay[endDay > cases$observationDays[observationPeriodId]] <- cases$observationDays[observationPeriodId][endDay > cases$observationDays[observationPeriodId]]
    newEras <- data.frame(eraType = "hei",
                          observationPeriodId = observationPeriodId,
                          conceptId = settings$covariateIds[i],
                          value = 1,
                          startDay = startDay,
                          endDay = endDay)
    eras <- rbind(eras, newEras)
  }
  eras <- eras[order(eras$observationPeriodId, eras$conceptId),]

  ### Generate outcomes ###
  baselineRates <- runif(n,min=settings$minBaselineRate,max=settings$maxBaselineRate)
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
  newEras <- data.frame()
  conceptIds <- c()
  rrs <- c()
  conceptId <- 1000
  for (i in 1:length(settings$simulationRiskWindows)){
    simulationRiskWindow <- settings$simulationRiskWindows[[i]]
    sourceEras <- eras[eras$conceptId == settings$covariateIds[i],]
    riskEnds <- rep(simulationRiskWindow$end, nrow(sourceEras))
    if (simulationRiskWindow$addExposedDaysToEnd) {
      riskEnds <- riskEnds + sourceEras$endDay - sourceEras$startDay
    }
    start <- simulationRiskWindow$start
    for (j in 1:(length(simulationRiskWindow$splitPoints) + 1)) {
      if (j > length(simulationRiskWindow$splitPoints)){
        end <- riskEnds
      } else {
        end <- simulationRiskWindow$splitPoints[j]
      }
      truncatedEnds <- riskEnds
      truncatedEnds[truncatedEnds > end] <- end
      filteredIndex <- truncatedEnds >= start
      riskEras <- data.frame(eraType = "hei",
                             observationPeriodId = sourceEras$observationPeriodId[filteredIndex],
                             conceptId = conceptId,
                             value = 1,
                             startDay = sourceEras$startDay[filteredIndex] + start,
                             endDay = sourceEras$startDay[filteredIndex] + truncatedEnds[filteredIndex])
      newEras <- rbind(newEras, riskEras)
      conceptIds <- c(conceptIds, conceptId)
      rrs <- c(rrs, simulationRiskWindow$relativeRisks[j])
      conceptId <- conceptId + 1
      start <- end + 1
    }
  }
  newEras <- newEras[order(newEras$observationPeriodId, newEras$conceptId),]
  eraRrs <- data.frame(conceptId = conceptIds, rr = rrs)
  outcomes <- .Call('SelfControlledCaseSeries_simulateSccsOutcomes', PACKAGE = 'SelfControlledCaseSeries', cases, newEras, baselineRates, eraRrs, settings$includeAge, settings$minAge, ageRrs, settings$includeSeasonality, seasonRrs)
  outcomes <- data.frame(eraType = "hoi",
                         observationPeriodId = outcomes$observationPeriodId,
                         conceptId = settings$outcomeId,
                         value = 1,
                         startDay = outcomes$startDay,
                         endDay = outcomes$startDay)

  #** Remove non-cases ***
  caseIds <- unique(outcomes$observationPeriodId)
  cases <- cases[cases$observationPeriodId %in% caseIds,]
  eras <- eras[eras$observationPeriodId %in% caseIds,]

  eras <- rbind(eras, outcomes)
  eras <- eras[order(eras$observationPeriodId),]
  cases$observationPeriodId = cases$observationPeriodId + caseIdOffset
  cases$personId = cases$personId + caseIdOffset
  eras$observationPeriodId = eras$observationPeriodId + caseIdOffset
  result <- list(cases = cases, eras = eras)
  return(result)
}

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
    risk <- runif(settings$seasonKnots-1, -2, 2)
    seasonFun <- function(x) {
      designMatrix <- cyclicSplineDesign(x, seasonKnots)
      return(apply(designMatrix %*% risk, 1, sum))
    }
  } else {
    seasonFun <- NULL
  }
  cases <- data.frame()
  eras <- data.frame()
  lastCaseId <- 0
  while (nrow(cases) < nCases) {
    batch <- simulateBatch(settings, ageFun, seasonFun, lastCaseId)
    need <- nCases - nrow(cases)
    if (nrow(batch$cases) < need) {
      cases <- rbind(cases, batch$cases)
      eras <- rbind(eras, batch$eras)
      lastCaseId <- max(batch$cases$observationPeriodId)
    } else {
      cases <- rbind(cases, batch$cases[1:need,])
      eras <- rbind(eras, batch$eras[batch$eras$observationPeriodId %in% batch$cases$observationPeriodId[1:need],])
    }
  }
  data <- list(cases = ff::as.ffdf(cases),
               eras = ff::as.ffdf(eras),
               metaData = list(sccsSimulationSettings = settings,
                               ageFun = ageFun,
                               seasonFun = seasonFun,
                               exposureIds = settings$covariateIds,
                               outcomeIds = settings$outcomeId),
               covariateRef = ff::as.ffdf(data.frame(covariateId = c(1), covariateName = c(""))))
  class(data) <- "sccsData"
  return(data)
}
