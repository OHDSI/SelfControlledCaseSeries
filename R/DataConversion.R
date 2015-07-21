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


#' Create SCCS era data
#'
#' @details
#' This function chops patient time into periods during which all covariates remain constant. The output details
#' these periods, their durations, and a sparse representation of the covariate values.
#'
#' @param sccsData       An object of type sccsData as created using the \code{\link{getDbSccsData}} function.
#' @param covariateStart Start day relative to the start of a covariate when the covariate should be considered in the risk profile.
#' @param covariatePersistencePeriod  Number of days after the end of the covariate when the risk is assumed to stop.
#' @param naivePeriod    The number of days at the start of a patient's observation period that should not be included in the risk
#' calculations. Note that the naive period can be used to determine current covariate status right after the naive period, and whether
#' an outcome is the first one.
#' @param firstOutcomeOnly  Whether only the first occurrence of an outcome should be considered.
#' @param excludeConceptIds Concept IDs that should be excluded from the list of covariates.
#'
#' @return
#' An object of type sccsEraData.
#'
#' @export
createSccsEraData <- function(sccsData,
                              covariateStart = 0,
                              covariatePersistencePeriod = 0,
                              naivePeriod = 0,
                              firstOutcomeOnly = FALSE,
                              excludeConceptIds = NULL,
                              includeAge = FALSE,
                              ageKnots = 5) {
  start <- Sys.time()
  if (is.null(excludeConceptIds)) {
    erasSubset <- sccsData$eras
  } else {
    t <- in.ff(sccsData$eras$conceptId, ff::as.ff(excludeConceptIds))
    erasSubset <- sccsData$eras[ffbase::ffwhich(t, t == FALSE), ]
  }
  if (!includeAge){
    ageOffset <- 0
    ageDesignMatrix <- matrix()
  } else {
    if (length(ageKnots) == 1){
      # Single number, should interpret as number of knots. Spread out knots evenly:
      minAge <- min(sccsData$cases$ageInDays + naivePeriod)
      maxAge <- max(sccsData$cases$ageInDays + sccsData$cases$observationDays)
      ageKnots <- seq(minAge, maxAge, length.out = ageKnots)
    } else {
      ageKnotCount <- length(ageKnots)
    }
    ageOffset <- minAge
    ageDesignMatrix <- splines::bs(minAge:maxAge, knots = ageKnots)[,1:(length(ageKnots)-1)]
  }

  metaData <- sccsData$metaData
  metaData$call2 <- match.call()
  writeLines("Converting person data to SCCS eras. This might take a while.")
  data <- .convertToSccs(sccsData$cases,
                         erasSubset,
                         covariateStart,
                         covariatePersistencePeriod,
                         naivePeriod,
                         firstOutcomeOnly,
                         includeAge,
                         ageOffset,
                         ageDesignMatrix)
  result <- list(outcomes = data$outcomes,
                 covariates = data$covariates,
                 covariateRef = ff::clone(sccsData$covariateRef),
                 metaData = metaData)
  open(result$outcomes)
  open(result$covariates)
  open(result$covariateRef)
  class(result) <- "sccsEraData"
  delta <- Sys.time() - start
  writeLines(paste("Analysis took", signif(delta, 3), attr(delta, "units")))
  return(result)
}

#' Save the SCCS era data to folder
#'
#' @description
#' \code{saveSccsEraData} saves an object of type sccsEraData to folder.
#'
#' @param sccsEraData   An object of type \code{sccsEraData} as generated using
#'                      \code{\link{createSccsEraData}}.
#' @param folder        The name of the folder where the data will be written. The folder should not
#'                      yet exist.
#'
#' @details
#' The data will be written to a set of files in the specified folder.
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
  open(sccsEraData$outcomes)
  open(sccsEraData$covariates)
  open(sccsEraData$covariateRef)
  invisible(TRUE)
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
#' An object of class sccsEraData
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

#' @export
print.sccsEraData <- function(x, ...) {
  writeLines("sccsEraData object")
  writeLines("")
  writeLines(paste("Exposure concept ID(s):", paste(x$metaData$exposureConceptIds, collapse = ",")))
  writeLines(paste("Outcome concept ID(s):", paste(x$metaData$outcomeConceptIds, collapse = ",")))
}

#' @export
summary.sccsEraData  <- function(object, ...) {
  caseCount <- length(ffbase::unique.ff(object$outcomes$stratumId))
  eraCount <- nrow(object$outcomes)

  outcomeCounts <- data.frame(outcomeConceptId = object$metaData$outcomeConceptIds,
                              eventCount = 0,
                              caseCount = 0)
  t <- object$outcomes$y == 1
  hois <- object$outcomes[ffbase::ffwhich(t, t == TRUE),]
  for (i in 1:nrow(outcomeCounts)) {
    outcomeCounts$eventCount[i] <- ffbase::sum.ff(hois$outcomeId == object$metaData$outcomeConceptIds[i])
    if (outcomeCounts$eventCount[i] == 0) {
      outcomeCounts$caseCount[i] <- 0
    } else {
      t <- (hois$outcomeId == object$metaData$outcomeConceptIds[i])
      outcomeCounts$caseCount[i] <- length(ffbase::unique.ff(hois$stratumId[ffbase::ffwhich(t, t == TRUE)]))
    }
  }
  covariateValueCount <- nrow(object$covariates)

  result <- list(metaData = object$metaData,
                 caseCount = caseCount,
                 eraCount = eraCount,
                 outcomeCounts = outcomeCounts,
                 covariateCount = nrow(object$covariateRef),
                 covariateValueCount = covariateValueCount)
  class(result) <- "summary.sccsEraData"
  return(result)
}

#' @export
print.summary.sccsEraData <- function(x, ...) {
  writeLines("sccsEraData object summary")
  writeLines("")
  writeLines(paste("Exposure concept ID(s):", paste(x$metaData$exposureConceptIds, collapse = ",")))
  writeLines(paste("Outcome concept ID(s):", paste(x$metaData$outcomeConceptIds, collapse = ",")))
  writeLines("")
  writeLines(paste("Cases:", paste(x$caseCount)))
  writeLines(paste("Eras:", paste(x$eraCount)))
  writeLines("")
  writeLines("Outcome counts:")
  outcomeCounts <- x$outcomeCounts
  rownames(outcomeCounts) <- outcomeCounts$outcomeConceptId
  outcomeCounts$outcomeConceptId <- NULL
  colnames(outcomeCounts) <- c("Event count", "Case count")
  printCoefmat(outcomeCounts)
  writeLines("")
  writeLines("Covariates:")
  writeLines(paste("Number of covariates:", x$covariateCount))
  writeLines(paste("Number of non-zero covariate values:", x$covariateValueCount))
}


