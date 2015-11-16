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
                              outcomeId = NULL,
                              naivePeriod = 0,
                              firstOutcomeOnly = FALSE,
                              covariateSettings,
                              ageSettings = createAgeSettings(includeAge = FALSE),
                              seasonalitySettings = createSeasonalitySettings(includeSeasonality = FALSE),
                              eventDependentObservation = FALSE) {
  start <- Sys.time()
  if (is.null(outcomeId)){
    outcomeId <- sccsData$metaData$outcomeIds
    if (length(outcomeId) != 1){
      stop("No outcome ID specified, but multiple outcomes found")
    }
  }
  if (eventDependentObservation){
    sccsData$cases$uncensored <- isUncensored(sccsData)
  } else {
    sccsData$cases$uncensored <- ff::ff(FALSE, nrow(sccsData$cases))
  }

  settings <- list()
  settings$metaData <- sccsData$metaData
  settings$metaData$era_call <- match.call()
  settings$metaData$outcomeId <- outcomeId
  settings$covariateRef <- data.frame()
  settings <- addAgeSettings(settings, ageSettings, outcomeId, firstOutcomeOnly, naivePeriod, sccsData)
  settings <- addSeasonalitySettings(settings, seasonalitySettings, sccsData)
  settings <- addEventDependentObservationSettings(settings, eventDependentObservation, outcomeId, naivePeriod, sccsData)
  settings <- addCovariateSettings(settings, covariateSettings, sccsData)
  settings$metaData$covariateSettingsList <- settings$covariateSettingsList

  writeLines("Converting person data to SCCS eras. This might take a while.")
  data <- .convertToSccs(sccsData$cases,
                         sccsData$eras,
                         outcomeId,
                         naivePeriod,
                         firstOutcomeOnly,
                         ageSettings$includeAge,
                         settings$ageOffset,
                         settings$ageDesignMatrix,
                         seasonalitySettings$includeSeasonality,
                         settings$seasonDesignMatrix,
                         settings$covariateSettingsList,
                         eventDependentObservation,
                         settings$censorModel)

  result <- list(outcomes = data$outcomes,
                 covariates = data$covariates,
                 covariateRef = ff::as.ffdf(settings$covariateRef),
                 metaData = settings$metaData)
  open(result$outcomes)
  open(result$covariates)
  open(result$covariateRef)
  class(result) <- "sccsEraData"
  delta <- Sys.time() - start
  writeLines(paste("Analysis took", signif(delta, 3), attr(delta, "units")))
  return(result)
}

isUncensored <- function(sccsData) {
  dates <- as.Date(paste(ff::as.ram(sccsData$cases$observationStartYear),
                         ff::as.ram(sccsData$cases$observationStartMonth),
                         ff::as.ram(sccsData$cases$observationStartDay), sep="-"), format = "%Y-%m-%d")
  dates <- dates + ff::as.ram(sccsData$cases$observationDays)
  studyEndDate <- max(dates)
  return(ff::as.ff(dates == studyEndDate))
}

addAgeSettings <- function(settings, ageSettings, outcomeId, firstOutcomeOnly, naivePeriod, sccsData) {
  if (!ageSettings$includeAge){
    settings$ageOffset <- 0
    settings$ageDesignMatrix <- matrix()
  } else {
    if (length(ageSettings$ageKnots) == 1){
      # Single number, should interpret as number of knots. Spread out knots to data quantiles:
      outcomes <- ffbase::subset.ffdf(sccsData$eras, eraType == "hoi" &  conceptId == outcomeId)
      if (firstOutcomeOnly){
        outcomes <- ff::as.ffdf(aggregate(startDay ~ observationPeriodId, outcomes, min))
      }
      outcomes <- ffbase::subset.ffdf(outcomes, startDay >= naivePeriod)
      outcomes <- merge(outcomes, sccsData$cases)
      outcomeAges <- outcomes$startDay + outcomes$ageInDays
      ageKnots <- ffbase::quantile.ff(outcomeAges, seq(0.01,0.99, length.out = ageSettings$ageKnots))
    } else {
      ageKnots <- ageSettings$ageKnots
    }
    settings$ageOffset <- ageKnots[1]
    ageDesignMatrix <- splines::bs(ageKnots[1]:ageKnots[length(ageKnots)], knots = ageKnots[2:(length(ageKnots)-1)], Boundary.knots = ageKnots[c(1,length(ageKnots))])
    # Fixing first beta to zero, so dropping first column of design matrix:
    settings$ageDesignMatrix <- ageDesignMatrix[,2:ncol(ageDesignMatrix)]
    splineCovariateRef <- data.frame(covariateId = 100:(100 + length(ageKnots) -1), covariateName = "Age spline component", originalCovariateId = 0, originalCovariateName = "")
    settings$covariateRef <- rbind(settings$covariateRef, splineCovariateRef)
    age <- list(ageKnots = ageKnots,
                covariateIds = splineCovariateRef$covariateId,
                allowRegularization = ageSettings$allowRegularization)
    settings$metaData$age <- age
  }
  return(settings)
}

addSeasonalitySettings <- function(settings, seasonalitySettings, sccsData) {
  if (!seasonalitySettings$includeSeasonality){
    settings$seasonDesignMatrix <- matrix()
  } else {
    if (length(seasonalitySettings$seasonKnots) == 1){
      # Single number, should interpret as number of knots. Spread out knots evenly:
      seasonKnots <- seq(1,12, length.out = seasonalitySettings$seasonKnots)
    } else {
      seasonKnots <- seasonalitySettings$seasonKnots
    }
    seasonDesignMatrix <- cyclicSplineDesign(1:12, knots = seasonKnots)
    # Fixing first beta to zero, so dropping first column of design matrix:
    settings$seasonDesignMatrix <- seasonDesignMatrix[,2:ncol(seasonDesignMatrix)]
    splineCovariateRef <- data.frame(covariateId = 200:(200 + length(seasonKnots) - 3), covariateName = "Seasonality spline component", originalCovariateId = 0, originalCovariateName = "")
    settings$covariateRef <- rbind(settings$covariateRef, splineCovariateRef)
    seasonality <- list(seasonKnots = seasonKnots,
                        covariateIds = splineCovariateRef$covariateId,
                        allowRegularization = seasonalitySettings$allowRegularization)
    settings$metaData$seasonality <- seasonality
  }
  return(settings)
}

addEventDependentObservationSettings <- function(settings, eventDependentObservation, outcomeId, naivePeriod, sccsData){
  if (eventDependentObservation) {
    # Pick first outcome per person
    t <- sccsData$eras$eraType == "hoi" & sccsData$eras$conceptId == outcomeId
    rownames(sccsData$eras) <- NULL
    firstOutcomes <- aggregate(startDay ~ observationPeriodId, data = sccsData$eras[ffbase::ffwhich(t, t == TRUE),], min)

    # See who has first event in remaining observation period after applying naive period
    firstOutcomes <- firstOutcomes[firstOutcomes$startDay >= naivePeriod,]
    t <- in.ff(sccsData$cases$observationPeriodId, ff::as.ff(firstOutcomes$observationPeriodId))
    cases <- ff::as.ram(sccsData$cases[ffbase::ffwhich(t, t == TRUE), ])
    cases <- merge(cases, firstOutcomes)

    # Fit censoring models
    data <- data.frame(astart = cases$ageInDays + naivePeriod,
                       aend = cases$ageInDays + cases$observationDays,
                       aevent = cases$ageInDays + cases$startDay + 1,
                       present = cases$uncensored)
    #data$aend[cases$ageInDays + data$aend == data$aevent] <- data$aend[cases$ageInDays + data$aend == data$aevent] + 0.5

    settings$censorModel <- fitModelsAndPickBest(data)
    settings$metaData$censorModel <- settings$censorModel
  } else {
    settings$censorModel <- list(model = 0, p = c(0))
  }
  return(settings)
}

addCovariateSettings <- function(settings, covariateSettings, sccsData) {
  if (is.list(covariateSettings) && class(covariateSettings) != "covariateSettings"){
    covariateSettingsList <- covariateSettings
  } else {
    covariateSettingsList <- list(covariateSettings)
  }
  covariateRef <- ff::as.ram(sccsData$covariateRef)

  # Iterate over different covariate settings. Assign unique IDs, and store in covariateRef:
  outputId <- 1000
  for (i in 1:length(covariateSettingsList)){
    covariateSettings <- covariateSettingsList[[i]]

    if (is.null(covariateSettings$label)){
      covariateSettings$label <- "Covariate"
    }
    if (is.null(covariateSettings$includeCovariateIds) ||
        length(covariateSettings$includeCovariateIds) == 0) {
      covariateSettings$covariateIds <- ff::as.ram(sccsData$covariateRef$covariateId)
    } else {
      covariateSettings$covariateIds <-covariateSettings$includeCovariateIds
    }
    if (!is.null(covariateSettings$excludeCovariateIds) &&
        length(covariateSettings$excludeCovariateIds) != 0) {
      covariateSettings$covariateIds <- covariateSettings$covariateIds[covariateSettings$covariateIds != covariateSettings$excludeCovariateIds]
    }

    if (length(covariateSettings$splitPoints) == 0){
      if (!covariateSettings$stratifyByID){
        # Create a single output ID
        covariateSettings$outputIds <- outputId
        newCovariateRef <- data.frame(covariateId = outputId, covariateName = covariateSettings$label, originalCovariateId = 0, originalCovariateName = "")
        settings$covariateRef <- rbind(settings$covariateRef, newCovariateRef)
        outputId <- outputId + 1
      } else {
        # Create a unique output ID for every covariate ID
        outputIds <- outputId:(outputId + length(covariateSettings$covariateIds) - 1)
        covariateSettings$outputIds <- matrix(outputIds, ncol = 1)
        outputId <- outputId + length(outputIds)
        varNames <- covariateRef[covariateRef$covariateId %in% covariateSettings$covariateIds,]
        names(varNames)[names(varNames) == "covariateId"] <- "originalCovariateId"
        names(varNames)[names(varNames) == "covariateName"] <- "originalCovariateName"
        varNames$covariateName <- paste(covariateSettings$label, varNames$originalCovariateName, sep = ": ")
        newCovariateRef <- data.frame(covariateId = outputIds,
                                      originalCovariateId = covariateSettings$covariateIds)
        newCovariateRef <- merge(newCovariateRef, varNames, by = "originalCovariateId")
        settings$covariateRef <- rbind(settings$covariateRef, newCovariateRef)
      }
    } else {
      startDays <- c(covariateSettings$start, covariateSettings$splitPoints + 1)
      endDays <- c(covariateSettings$splitPoints, NA)
      if (!covariateSettings$stratifyByID){
        outputIds <- outputId:(outputId + length(covariateSettings$splitPoints))
        outputId <- outputId + length(covariateSettings$splitPoints) + 1
        names <- rep(covariateSettings$label, length(covariateSettings$splitPoints) + 1)
        names <- paste(names," day ", startDays,"-", c(endDays[1:length(endDays)-1],"") , sep = "")
        covariateSettings$outputIds <- matrix(outputIds, ncol = 1)
        newCovariateRef <- data.frame(covariateId = outputIds, covariateName = names, originalCovariateId = 0, originalCovariateName = "")
        settings$covariateRef <- rbind(settings$covariateRef, newCovariateRef)
      } else {
        outputIds <- outputId:(outputId + (length(covariateSettings$splitPoint) + 1) * length(covariateSettings$covariateIds) - 1)
        outputId <- max(outputIds) + 1
        names <- paste("Covariate", rep(covariateSettings$covariateIds, each = length(covariateSettings$splitPoints) + 1))
        names <- paste(names,", day ", startDays,"-", c(endDays[1:length(endDays)-1],"") , sep = "")
        covariateSettings$outputIds <- matrix(outputIds, ncol = length(covariateSettings$splitPoints) + 1, byrow = TRUE)
        originalCovariateId <- rep(covariateSettings$covariateIds, each = length(covariateSettings$splitPoints) + 1)
        originalCovariateName <- covariateRef$covariateName[match(originalCovariateId, covariateRef$covariateId)]
        newCovariateRef <- data.frame(covariateId = outputIds,
                                      covariateName = names,
                                      originalCovariateId = originalCovariateId,
                                      originalCovariateName = originalCovariateName)
        settings$covariateRef <- rbind(settings$covariateRef, newCovariateRef)
      }
    }
    covariateSettingsList[[i]] <- covariateSettings
  }
  settings$covariateSettingsList <- covariateSettingsList
  settings$covariateRef$covariateName <- as.factor(settings$covariateRef$covariateName)
  settings$covariateRef$originalCovariateName <- as.factor(settings$covariateRef$originalCovariateName)
  return(settings)
}

#' @export
createCovariateSettings <- function(includeCovariateIds = NULL,
                                    excludeCovariateIds = NULL,
                                    label = NULL,
                                    stratifyByID = TRUE,
                                    start = 0,
                                    addExposedDaysToStart = FALSE,
                                    end = 0,
                                    addExposedDaysToEnd = FALSE,
                                    firstOccurrenceOnly = FALSE,
                                    splitPoints = c(),
                                    allowRegularization = FALSE) {
   return(OhdsiRTools::convertArgsToList(match.call(), "covariateSettings"))
}

#' @export
createAgeSettings <- function(includeAge = FALSE,
                              ageKnots= 5,
                              allowRegularization = FALSE) {
  return(OhdsiRTools::convertArgsToList(match.call(), "ageSettings"))
}

#' @export
createSeasonalitySettings <- function(includeSeasonality = FALSE,
                                      seasonKnots= 5,
                                      allowRegularization = FALSE) {
  return(OhdsiRTools::convertArgsToList(match.call(), "seasonalitySettings"))
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
  writeLines(paste("Exposure concept ID(s):", paste(x$metaData$exposureIds, collapse = ",")))
  writeLines(paste("Outcome concept ID(s):", paste(x$metaData$outcomeIds, collapse = ",")))
}

#' @export
summary.sccsEraData  <- function(object, ...) {
  caseCount <- length(ffbase::unique.ff(object$outcomes$stratumId))
  eraCount <- nrow(object$outcomes)

  outcomeCounts <- data.frame(outcomeConceptId = object$metaData$outcomeIds,
                              eventCount = ffbase::sum.ff(object$outcomes$y),
                              caseCount = caseCount)
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
  writeLines(paste("Exposure concept ID(s):", paste(x$metaData$exposureIds, collapse = ",")))
  writeLines(paste("Outcome concept ID(s):", paste(x$metaData$outcomeIds, collapse = ",")))
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

#' @export
cyclicSplineDesign <- function (x, knots, ord = 4) {
  nk <- length(knots)
  if (ord < 2)
    stop("order too low")
  if (nk < ord)
    stop("too few knots")
  knots <- sort(knots)
  k1 <- knots[1]
  if (min(x) < k1 || max(x) > knots[nk])
    stop("x out of range")
  xc <- knots[nk - ord + 1]
  knots <- c(k1 - (knots[nk] - knots[(nk - ord + 1):(nk - 1)]),
             knots)
  ind <- x > xc
  X1 <- splines::splineDesign(knots, x, ord, outer.ok = TRUE)
  x[ind] <- x[ind] - max(knots) + k1
  if (sum(ind)) {
    X2 <- splines::splineDesign(knots, x[ind], ord, outer.ok = TRUE)
    X1[ind, ] <- X1[ind, ] + X2
  }
  X1
}

