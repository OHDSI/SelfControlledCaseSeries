# Copyright 2022 Observational Health Data Sciences and Informatics
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

#' Create a SelfControlledCaseSeries analysis specification
#'
#' @param analysisId                   An integer that will be used later to refer to this specific set
#'                                     of analysis choices.
#' @param description                  A short description of the analysis.
#' @param getDbSccsDataArgs            An object representing the arguments to be used when calling the
#'                                     [getDbSccsData] function.
#' @param createStudyPopulationArgs    An object representing the arguments to be used when calling the
#'                                     [getDbSccsData] function.
#' @param createIntervalDataArgs       An object representing the arguments to be used when calling the
#'                                     [createSccsIntervalData] or [createScriIntervalData] function.
#' @param fitSccsModelArgs             An object representing the arguments to be used when calling the
#'                                     [fitSccsModel] function.
#'
#' @return
#' An object of type `sccsAnalysis`, to be used with the [runSccsAnalyses] function.
#'
#' @export
createSccsAnalysis <- function(analysisId = 1,
                               description = "",
                               getDbSccsDataArgs,
                               createStudyPopulationArgs,
                               createIntervalDataArgs = NULL,
                               fitSccsModelArgs) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertInt(analysisId, add = errorMessages)
  checkmate::assertCharacter(description, len = 1, add = errorMessages)
  checkmate::assertClass(getDbSccsDataArgs, "args", add = errorMessages)
  checkmate::assertClass(createStudyPopulationArgs, "args", add = errorMessages)
  checkmate::assertClass(createIntervalDataArgs, "args", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(fitSccsModelArgs, "args", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  analysis <- list()
  for (name in names(formals(createSccsAnalysis))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "sccsAnalysis"
  return(analysis)
}

#' Save a list of sccsAnalysis to file
#'
#' @description
#' Write a list of objects of type `sccsAnalysis` to file. The file is in JSON format.
#'
#' @param sccsAnalysisList   The `sccsAnalysis` list to be written to file
#' @param file               The name of the file where the results will be written
#'
#' @export
saveSccsAnalysisList <- function(sccsAnalysisList, file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(sccsAnalysisList, min.len = 1, add = errorMessages)
  for (i in 1:length(sccsAnalysisList)) {
    checkmate::assertClass(sccsAnalysisList[[i]], "sccsAnalysis", add = errorMessages)
  }
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  ParallelLogger::saveSettingsToJson(sccsAnalysisList, file)
}


#' Load a list of sccsAnalysis from file
#'
#' @description
#' Load a list of objects of type `sccsAnalysis` from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type `sccsAnalysis`.
#'
#' @export
loadSccsAnalysisList <- function(file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  return(ParallelLogger::loadSettingsFromJson(file))
}

#' Create exposure definition
#'
#' @details
#' Create an exposur definition, to be used with the [createExposuresOutcome] function.
#'
#' @param exposureId     An integer used to identify the exposure in the exposure cohort table.
#' @param exposureIdRef  A string used to refer to the exposure when defining covariates using the
#'                       `createEraCovariateSettings()` function.
#' @param trueEffectSize For negative and positive controls: the known true effect size. To be
#'                       used for empirical calibration. Negative controls have
#'                       `trueEffectSize = 1`. If the true effect size is unknown, use
#'                       `trueEffectSize = NA`.
#'
#' @return
#' An object of type `exposure`.
#'
#' @export
createExposure <- function(exposureId, exposureIdRef = "exposureId", trueEffectSize = NA) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertInt(exposureId, add = errorMessages)
  checkmate::assertCharacter(exposureIdRef, len = 1, add = errorMessages)
  checkmate::assertNumeric(trueEffectSize, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  exposure <- list()
  for (name in names(formals(createExposure))) {
    exposure[[name]] <- get(name)
  }
  class(exposure) <- "exposure"
  return(exposure)
}

#' Create a exposures-outcome combination.
#'
#' @details
#' Create a set of hypotheses of interest, to be used with the [runSccsAnalyses] function.
#'
#' @param outcomeId    An integer used to identify the outcome in the outcome cohort table.
#' @param exposures    A list of object of type `exposure` as created by [createExposure()].
#'
#' @return
#' An object of type `exposuresOutcome`.
#'
#' @export
createExposuresOutcome <- function(outcomeId, exposures) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertInt(outcomeId, add = errorMessages)
  checkmate::assertList(exposures, min.len = 1, add = errorMessages)
  for (i in seq_along(exposures)) {
    checkmate::assertClass(exposures[[i]], "exposure", add = errorMessages)
  }
  checkmate::reportAssertions(collection = errorMessages)
  uniqueExposureIdRefs <- unlist(unique(ParallelLogger::selectFromList(exposures, "exposureIdRef")))
  if (length(uniqueExposureIdRefs) != length(exposures)) {
    stop("Duplicate exposureIdRefs are not allowed. Please give each exposure a unique exposureIdRef.")
  }

  exposuresOutcome <- list()
  for (name in names(formals(createExposuresOutcome))) {
    exposuresOutcome[[name]] <- get(name)
  }
  class(exposuresOutcome) <- "exposuresOutcome"
  return(exposuresOutcome)
}

#' Save a list of `exposuresOutcome` to file
#'
#' @description
#' Write a list of objects of type `exposuresOutcome` to file. The file is in JSON format.
#'
#' @param exposuresOutcomeList  The `exposuresOutcome` list to be written to file
#' @param file                  The name of the file where the results will be written
#'
#' @export
saveExposuresOutcomeList <- function(exposuresOutcomeList, file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(exposuresOutcomeList, min.len = 1, add = errorMessages)
  for (i in 1:length(exposuresOutcomeList)) {
    checkmate::assertClass(exposuresOutcomeList[[i]], "exposuresOutcome", add = errorMessages)
  }
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  ParallelLogger::saveSettingsToJson(exposuresOutcomeList, file)
}

#' Load a list of `exposuresOutcome` from file
#'
#' @description
#' Load a list of objects of type `exposuresOutcome` from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type `exposuresOutcome`.
#'
#' @export
loadExposuresOutcomeList <- function(file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  return(ParallelLogger::loadSettingsFromJson(file))
}
