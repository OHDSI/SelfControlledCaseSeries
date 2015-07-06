# @file DataConversion.R
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

in.ff <- function(a, b) {
  if (length(b) == 0)
    return(ff::as.ff(rep(FALSE, length(a)))) else return(ffbase::ffmatch(x = a,
                                                                         table = b,
                                                                         nomatch = 0L) > 0L)
}

#' @export
createSccsEraData <- function(sccsData,
                              covariateStart = 0,
                              covariatePersistencePeriod = 0,
                              naivePeriod = 0,
                              firstOutcomeOnly = FALSE,
                              excludeConceptIds = NULL) {
  if (is.null(excludeConceptIds)) {
    erasSubset <- sccsData$eras
  } else {
    t <- in.ff(sccsData$eras$conceptId, ff::as.ff(excludeConceptIds))
    erasSubset <- sccsData$eras[ffbase::ffwhich(t, t == FALSE), ]
  }
  metaData <- sccsData$metaData
  metaData$call2 <- match.call()
  writeLines("Converting person data to SCCS eras. This might take a while.")
  data <- .convertToSccs(sccsData$cases,
                         erasSubset,
                         covariateStart,
                         covariatePersistencePeriod,
                         naivePeriod,
                         firstOutcomeOnly)
  result <- list(outcomes = data$outcomes,
                 covariates = data$covariates,
                 covariateRef = ff::clone(sccsData$covariateRef),
                 metaData = metaData)
  open(result$outcomes)
  open(result$covariates)
  open(result$covariateRef)
  class(result) <- "sccsEraData"
  return(result)
}

#' Save the SCCS era data to folder
#'
#' @description
#' \code{saveSccsEraData} saves an object of type sccsEraData to folder.
#'
#' @param sccsEraData   An object of type \code{sccsEraData} as generated using
#'                      \code{\link{createsccsEraData}}.
#' @param folder        The name of the folder where the data will be written. The folder should not
#'                      yet exist.
#'
#' @details
#' The data will be written to a set of files in the specified folder.
#'
#' @examples
#' # todo
#'
#' @export
saveSccsEraData <- function(sccsEraData, folder) {
  if (missing(sccsEraData))
    stop("Must specify sccsEraData")
  if (missing(folder))
    stop("Must specify folder")
  if (class(sccsEraData) != "sccsEraData")
    stop("Data not of class sccsEraData")

  outcomes <- sccsEraData$outcomes
  covariates <- sccsEraData$covariates
  covariateRef <- sccsEraData$covariateRef
  ffbase::save.ffdf(outcomes, covariates, covariateRef, dir = folder)
  metaData <- sccsEraData$metaData
  save(metaData, file = file.path(folder, "metaData.Rdata"))
  # Open all ffdfs to prevent annoying messages later:
  dummay <- open(sccsEraData$outcomes)
  dummay <- open(sccsEraData$covariates)
  dummay <- open(sccsEraData$covariateRef)
}

#' Load the SCCS era data from a folder
#'
#' @description
#' \code{loadSccsEraData} loads an object of type sccsEraData from a folder in the file system.
#'
#' @param file       The name of the folder containing the data.
#' @param readOnly   If true, the data is opened read only.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @return
#' An object of class cohortEraData.
#'
#' @examples
#' # todo
#'
#' @export
loadSccsEraData <- function(folder, readOnly = FALSE) {
  if (!file.exists(folder))
    stop(paste("Cannot find folder", folder))
  if (!file.info(folder)$isdir)
    stop(paste("Not a folder:", folder))

  temp <- setwd(folder)
  absolutePath <- setwd(temp)

  e <- new.env()
  ffbase::load.ffdf(absolutePath, e)
  load(file.path(absolutePath, "metaData.Rdata"), e)
  result <- list(outcomes = get("outcomes", envir = e),
                 covariates = get("covariates", envir = e),
                 covariateRef = get("covariateRef", envir = e),
                 metaData = get("metaData", envir = e))
  # Open all ffdfs to prevent annoying messages later:
  open(result$outcomes, readonly = readOnly)
  open(result$covariates, readonly = readOnly)
  open(result$covariateRef, readonly = readOnly)

  class(result) <- "sccsEraData"
  rm(e)
  return(result)
}

