# @file Analyses.R
#
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
#' @details
#' Create a set of analysis choices, to be used with the [runSccsAnalyses] function.
#'
#' @param analysisId                   An integer that will be used later to refer to this specific set
#'                                     of analysis choices.
#' @param description                  A short description of the analysis.
#' @param exposureType                 If more than one exposure is provided for each
#'                                     `exposureOutcome`, this field should be used to select the
#'                                     specific exposure to use in this analysis.
#' @param outcomeType                  If more than one outcome is provided for each `exposureOutcom`e,
#'                                     this field should be used to select the specific outcome to use
#'                                     in this analysis.
#' @param getDbSccsDataArgs            An object representing the arguments to be used when calling the
#'                                     [getDbSccsData] function.
#' @param createStudyPopulationArgs    An object representing the arguments to be used when calling the
#'                                     [getDbSccsData] function.
#' @param design                       Either "SCCS" for the general self-controlled case series design,
#'                                     or "SCRI" for the self-controlled risk interval design.
#' @param createSccsIntervalDataArgs   An object representing the arguments to be used when calling the
#'                                     [createSccsIntervalData] function. Ignored when `design = "SCRI"`.
#' @param createScriIntervalDataArgs   An object representing the arguments to be used when calling the
#'                                     [createScriIntervalData] function. Ignored when `design = "SCCS"`.
#' @param fitSccsModelArgs             An object representing the arguments to be used when calling the
#'                                     [fitSccsModel] function.
#' @export
createSccsAnalysis <- function(analysisId = 1,
                               description = "",
                               exposureType = NULL,
                               outcomeType = NULL,
                               getDbSccsDataArgs,
                               createStudyPopulationArgs,
                               design = "SCCS",
                               createSccsIntervalDataArgs = NULL,
                               createScriIntervalDataArgs = NULL,
                               fitSccsModelArgs) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertInt(analysisId, add = errorMessages)
  checkmate::assertCharacter(description, len = 1, add = errorMessages)
  checkmate::assertCharacter(exposureType, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(outcomeType, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(getDbSccsDataArgs, "args", add = errorMessages)
  checkmate::assertClass(createStudyPopulationArgs, "args", add = errorMessages)
  checkmate::assertChoice(toupper(design), .var.name = "design", c("SCCS", "SCRI"), add = errorMessages)
  checkmate::assertClass(createSccsIntervalDataArgs, "args", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(createScriIntervalDataArgs, "args", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(fitSccsModelArgs, "args", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (toupper(design) == "SCCS" && is.null(createSccsIntervalDataArgs)) {
    stop("Must provide createSccsIntervalDataArgs argument when design = 'SCCS'")
  }
  if (toupper(design) == "SCRI" && is.null(createScriIntervalDataArgs)) {
    stop("Must provide createScriIntervalDataArgs argument when design = 'SCRI'")
  }
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

#' Create a exposure-outcome combination.
#'
#' @details
#' Create a set of hypotheses of interest, to be used with the [runSccsAnalyses] function.
#'
#' @param exposureId   A concept ID identifying the target drug in the exposure table. If multiple
#'                     strategies for picking the exposure will be tested in the analysis, a named list
#'                     of numbers can be provided instead. In the analysis, the name of the number to
#'                     be used can be specified using the `exposureType` parameter in the
#'                     [createSccsAnalysis] function.
#' @param outcomeId    A concept ID identifying the outcome in the outcome table.
#' @param ...          Custom variables, to be used in the analyses.
#'
#' @export
createExposureOutcome <- function(exposureId, outcomeId, ...) {
  errorMessages <- checkmate::makeAssertCollection()
  if (!is.list(exposureId)) {
    checkmate::assertInt(exposureId, add = errorMessages)
  }
  if (!is.list(outcomeId)) {
    checkmate::assertInt(outcomeId, add = errorMessages)
  }
  checkmate::reportAssertions(collection = errorMessages)

  exposureOutcome <- list(...)
  exposureOutcome$exposureId <- exposureId
  exposureOutcome$outcomeId <- outcomeId
  class(exposureOutcome) <- "exposureOutcome"
  return(exposureOutcome)
}

#' Save a list of `exposureOutcome` to file
#'
#' @description
#' Write a list of objects of type `exposureOutcome` to file. The file is in JSON format.
#'
#' @param exposureOutcomeList   The `exposureOutcome` list to be written to file
#' @param file                  The name of the file where the results will be written
#'
#' @export
saveExposureOutcomeList <- function(exposureOutcomeList, file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(exposureOutcomeList, min.len = 1, add = errorMessages)
  for (i in 1:length(exposureOutcomeList)) {
    checkmate::assertClass(exposureOutcomeList[[i]], "exposureOutcome", add = errorMessages)
  }
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  ParallelLogger::saveSettingsToJson(exposureOutcomeList, file)
}

#' Load a list of exposureOutcome from file
#'
#' @description
#' Load a list of objects of type `exposureOutcome` from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type `exposureOutcome`.
#'
#' @export
loadExposureOutcomeList <- function(file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  return(ParallelLogger::loadSettingsFromJson(file))
}
