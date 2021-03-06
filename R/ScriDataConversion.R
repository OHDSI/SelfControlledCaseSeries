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


#' Create Self-Controlled Risk Interval (SCRI) era data
#'
#' @details
#' This function creates interval data according to the elf-Controlled Risk Interval (SCRI) design.
#' Unlike the generic SCCS design, where all patient time is used to establish a background rate,
#' in the SCRI design a specific control interval (relative to the exposure) needs to be defined.
#' The final model will only include time that is either part of the risk interval (defined using
#' the `eraCovariateSettings` argument, or the control interval (defined using `controlIntervalSettings`).
#'
#' @template StudyPopulation
#' @template SccsData
#' @param eraCovariateSettings        Either an object of type `EraCovariateSettings` as created
#'                                    using the [createEraCovariateSettings()] function, or a
#'                                    list of such objects.
#' @param controlIntervalSettings     An object of type `ControlIntervalSettings` as created
#'                                    using the [createControlIntervalSettings()] function.
#'
#' @references
#' Greene SK, Kulldorff M, Lewis EM, Li R, Yin R, Weintraub ES, Fireman BH, Lieu TA, Nordin JD,
#' Glanz JM, Baxter R, Jacobsen SJ, Broder KR, Lee GM. Near real-time surveillance for influenza
#' vaccine safety: proof-of-concept in the Vaccine Safety Datalink Project. Am J Epidemiol. 2010
#' Jan 15;171(2):177-88. doi: 10.1093/aje/kwp345.
#'
#' @return
#' An object of type [SccsIntervalData].
#'
#' @export
createScriIntervalData <- function(studyPopulation,
                                   sccsData,
                                   eraCovariateSettings,
                                   controlIntervalSettings) {
  if (class(controlIntervalSettings) != "ControlIntervalSettings")
    stop("The controlIntervalSettings argument should be of type 'ControlIntervalSettings'")

  start <- Sys.time()
  if (nrow(studyPopulation$outcomes) == 0) {
    sccsIntervalData <- createEmptySccsIntervalData()
    metaData <- studyPopulation$metaData
    metaData$error <- "Error: No cases left"
    attr(sccsIntervalData, "metaData") <- metaData

    class(sccsIntervalData) <- "SccsIntervalData"
    attr(class(sccsIntervalData), "package") <- "SelfControlledCaseSeries"
    return(sccsIntervalData)
  }

  settings <- list()
  settings$metaData <- list()
  settings$covariateRef <- tibble()

  if (is.list(eraCovariateSettings) && class(eraCovariateSettings) != "EraCovariateSettings") {
    covariateSettings <- eraCovariateSettings
  } else {
    covariateSettings <- list(eraCovariateSettings)
  }
  covariateSettings[[length(covariateSettings) + 1]]  <- controlIntervalSettings
  settings <- addEraCovariateSettings(settings, covariateSettings, sccsData)
  settings$metaData$covariateSettingsList <- cleanCovariateSettingsList(settings$covariateSettingsList)
  metaData <- append(studyPopulation$metaData, settings$metaData)

  ParallelLogger::logInfo("Converting person data to SCRI intervals. This might take a while.")
  # Ensure all sorted bv caseId:
  cases <- studyPopulation$cases[order(studyPopulation$cases$caseId), ]
  outcomes <- studyPopulation$outcomes[order(studyPopulation$outcomes$caseId), ]
  eras <- sccsData$eras %>%
    arrange(.data$caseId)

  controlIntervalId <- settings$covariateSettingsList[sapply(settings$covariateSettingsList, function(x) x$isControlInterval)][[1]]$outputIds[1, 1]
  data <- convertToSccs(cases = cases,
                        outcomes = outcomes,
                        eras = eras,
                        includeAge = FALSE,
                        ageOffset = 0,
                        ageDesignMatrix = matrix(),
                        includeSeason = FALSE,
                        seasonDesignMatrix = matrix(),
                        ageSeasonsCases = numeric(0),
                        covariateSettingsList = settings$covariateSettingsList,
                        eventDependentObservation = FALSE,
                        censorModel = list(model = 0, p = c(0)),
                        scri = TRUE,
                        controlIntervalId = controlIntervalId)

  if (is.null(data$outcomes) || is.null(data$covariates)) {
    warning("Conversion resulted in empty data set. Perhaps no one with the outcome had any exposure of interest?")
    data <- createEmptySccsIntervalData()
    metaData$daysObserved <- 0
    if (nrow(settings$covariateRef) > 0) {
      data$covariateRef <- settings$covariateRef
    }

  } else {
    metaData$covariateStatistics <- collect(data$covariateStatistics)
    metaData$daysObserved <- pull(data$observedDays, .data$observedDays)
    data$covariateStatistics <- NULL
    data$observedDays <- NULL
    data$covariateRef <- settings$covariateRef

  }
  attr(data, "metaData") <- metaData
  class(data) <- "SccsIntervalData"
  attr(class(data), "package") <- "SelfControlledCaseSeries"

  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Generating SCRI interval data took", signif(delta, 3), attr(delta, "units")))
  return(data)
}

cleanCovariateSettingsList <- function(covariateSettingsList) {
  # Remove control interval settings and field:
  noCi <- covariateSettingsList[!sapply(covariateSettingsList, function(x) x$isControlInterval)]
  return(lapply(noCi, function(x) {x$isControlInterval <- NULL; return(x)}))
}
