# @file Analyses.R
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
#' @param createSccsIntervalDataArgs   An object representing the arguments to be used when calling the
#'                                     [createSccsIntervalData] function.
#' @param fitSccsModelArgs             An object representing the arguments to be used when calling the
#'                                     [fitSccsModel] function.
#' @export
createSccsAnalysis <- function(analysisId = 1,
                               description = "",
                               exposureType = NULL,
                               outcomeType = NULL,
                               getDbSccsDataArgs,
                               createStudyPopulationArgs,
                               createSccsIntervalDataArgs,
                               fitSccsModelArgs) {
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
  stopifnot(is.list(sccsAnalysisList))
  stopifnot(length(sccsAnalysisList) > 0)
  for (i in 1:length(sccsAnalysisList)) {
    stopifnot(class(sccsAnalysisList[[i]]) == "sccsAnalysis")
  }
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
  stopifnot(is.list(exposureOutcomeList))
  stopifnot(length(exposureOutcomeList) > 0)
  for (i in 1:length(exposureOutcomeList)) {
    stopifnot(class(exposureOutcomeList[[i]]) == "exposureOutcome")
  }
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
  return(ParallelLogger::loadSettingsFromJson(file))
}
