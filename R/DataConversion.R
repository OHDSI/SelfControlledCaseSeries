# @file DataConversion.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
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
#' This function creates covariates based on the data in the \code{sccsData} object, according to the
#' provided settings. It chops patient time into periods during which all covariates remain constant.
#' The output details these periods, their durations, and a sparse representation of the covariate
#' values.
#'
#' @param sccsData                    An object of type \code{sccsData} as created using the
#'                                    \code{\link{getDbSccsData}} function.
#' @param outcomeId                   The outcome to create the era data for. If not specified it is
#'                                    assumed to be the one outcome for which the data was loaded from
#'                                    the database.
#' @param naivePeriod                 The number of days at the start of a patient's observation period
#'                                    that should not be included in the risk calculations. Note that
#'                                    the naive period can be used to determine current covariate
#'                                    status right after the naive period, and whether an outcome is
#'                                    the first one.
#' @param firstOutcomeOnly            Whether only the first occurrence of an outcome should be
#'                                    considered.
#' @param covariateSettings           Either an object of type \code{covariateSettings} as created
#'                                    using the \code{\link{createCovariateSettings}} function, or a
#'                                    list of such objects.
#' @param ageSettings                 An object of type \code{ageSettings} as created using the
#'                                    \code{\link{createAgeSettings}} function.
#' @param seasonalitySettings         An object of type \code{seasonalitySettings} as created using the
#'                                    \code{\link{createSeasonalitySettings}} function.
#' @param minCasesForAgeSeason        Minimum number of cases to use to fit age and season splines. IF
#'                                    needed (and available), cases that are not exposed will be included.#'
#' @param eventDependentObservation   Should the extension proposed by Farrington et al. be used to
#'                                    adjust for event-dependent observation time?
#'
#' @references
#' Farrington, C. P., Anaya-Izquierdo, A., Whitaker, H. J., Hocine, M.N., Douglas, I., and Smeeth, L.
#' (2011). Self-Controlled case series analysis with event-dependent observation periods. Journal of
#' the American Statistical Association 106 (494), 417-426
#'
#' @return
#' An object of type \code{sccsEraData}.
#'
#' @export
createSccsEraData <- function(sccsData,
                              outcomeId = NULL,
                              naivePeriod = 0,
                              firstOutcomeOnly = FALSE,
                              covariateSettings,
                              ageSettings = createAgeSettings(includeAge = FALSE),
                              seasonalitySettings = createSeasonalitySettings(includeSeasonality = FALSE),
                              minCasesForAgeSeason = 10000,
                              eventDependentObservation = FALSE) {
  start <- Sys.time()
  if (is.null(outcomeId)) {
    outcomeId <- sccsData$metaData$outcomeIds
    if (length(outcomeId) != 1) {
      stop("No outcome ID specified, but multiple outcomes found")
    }
  }
  if (sum(sccsData$eras$eraType == "hoi" & sccsData$eras$conceptId == outcomeId) == 0) {
    sccsData$metaData$error <- "Error: No cases left"
    sccsData$metaData$outcomeId <- outcomeId
    result <- list(metaData = sccsData$metaData)
    class(result) <- "sccsEraData"
    return(result)
  }

  if (eventDependentObservation) {
    sccsData$cases$uncensored <- isUncensored(sccsData, ageSettings$maxAge)
  } else {
    sccsData$cases$uncensored <- ff::ff(FALSE, nrow(sccsData$cases))
  }

  settings <- list()
  settings$metaData <- list()
  settings$metaData$getDbCall <- sccsData$metaData$call
  settings$metaData$eraCall <- match.call()
  settings$metaData$outcomeId <- outcomeId
  settings$covariateRef <- data.frame()
  ageSeasonsCases <- numeric(0)
  if (ageSettings$includeAge || seasonalitySettings$includeSeasonality) {
    includedOutcomes <- findIncludedOutcomes(sccsData, outcomeId, firstOutcomeOnly, naivePeriod, ageSettings$minAge, ageSettings$maxAge)$outcomes
    if (nrow(includedOutcomes) > minCasesForAgeSeason) {
      set.seed(0)
      ageSeasonsCases <- sample(includedOutcomes$observationPeriodId, minCasesForAgeSeason, replace = FALSE)
    }
  }
  settings <- addAgeSettings(settings,
                             ageSettings,
                             includedOutcomes)
  settings <- addSeasonalitySettings(settings, seasonalitySettings, sccsData)
  settings <- addEventDependentObservationSettings(settings,
                                                   eventDependentObservation,
                                                   outcomeId,
                                                   naivePeriod,
                                                   sccsData)
  settings <- addCovariateSettings(settings, covariateSettings, sccsData)
  settings$metaData$covariateSettingsList <- settings$covariateSettingsList

  writeLines("Converting person data to SCCS eras. This might take a while.")
  data <- convertToSccs(sccsData$cases,
                        sccsData$eras,
                        outcomeId,
                        naivePeriod,
                        firstOutcomeOnly,
                        ageSettings$includeAge,
                        settings$ageOffset,
                        settings$ageDesignMatrix,
                        settings$minAge,
                        settings$maxAge,
                        seasonalitySettings$includeSeasonality,
                        settings$seasonDesignMatrix,
                        ageSeasonsCases,
                        settings$covariateSettingsList,
                        eventDependentObservation,
                        settings$censorModel)
  if (length(data$outcomes) == 0) {
    warning("Conversion resulted in empty data set. Perhaps no one with the outcome had any exposure of interest?")
    result <- list(outcomes = NULL,
                   covariates = NULL,
                   covariateRef = NULL,
                   metaData = settings$metaData)
  } else {
    result <- list(outcomes = data$outcomes,
                   covariates = data$covariates,
                   covariateRef = ff::as.ffdf(settings$covariateRef),
                   metaData = settings$metaData)
    open(result$outcomes)
    open(result$covariates)
    open(result$covariateRef)
  }
  class(result) <- "sccsEraData"
  delta <- Sys.time() - start
  writeLines(paste("Analysis took", signif(delta, 3), attr(delta, "units")))
  return(result)
}

findIncludedOutcomes <- function(sccsData, outcomeId, firstOutcomeOnly, naivePeriod, minAge, maxAge) {
  outcomes <- ffbase::subset.ffdf(sccsData$eras, eraType == "hoi" & conceptId == outcomeId)
  if (firstOutcomeOnly) {
    outcomes <- ff::as.ffdf(aggregate(startDay ~ observationPeriodId, outcomes, min))
  }
  colnames(outcomes)[colnames(outcomes) == "startDay"] <- "outcomeDay"
  cases <- sccsData$cases
  cases$trueStartAge <- ff::clone.ff(cases$ageInDays)
  idx <- naivePeriod > cases$censoredDays
  if (ffbase::any.ff(idx)) {
    cases$trueStartAge[idx] <- cases$trueStartAge[idx] + (naivePeriod - cases$censoredDays[idx])
  }
  cases$trueEndAge <- cases$ageInDays + cases$observationDays
  if (!is.null(minAge)) {
    minAgeDays <- minAge * 365.25
    idx <- cases$trueStartAge < minAgeDays
    cases$trueStartAge[idx] <- ff::ff(minAgeDays, ffbase::sum.ff(idx))
  }
  if (!is.null(maxAge)) {
    maxAgeDays <- (maxAge + 1) * 365.25
    idx <- cases$trueEndAge > maxAgeDays
    cases$trueEndAge[idx] <- ff::ff(maxAgeDays, ffbase::sum.ff(idx))
  }
  cases <- cases[cases$trueStartAge <= cases$trueEndAge, ]
  outcomes <- merge(outcomes, ffbase::subset.ffdf(cases, select = c("observationPeriodId", "ageInDays", "trueStartAge", "trueEndAge")))
  outcomes$outcomeAge <- outcomes$outcomeDay + outcomes$ageInDays
  outcomes <- outcomes[outcomes$outcomeAge >= outcomes$trueStartAge & outcomes$outcomeAge <= outcomes$trueEndAge, ]
  cases <- cases[ffbase::`%in%`(cases$observationPeriodId, outcomes$observationPeriodId), ]
  return(list(outcomes = outcomes, cases = cases))
}

isUncensored <- function(sccsData, maxAge) {
  dates <- as.Date(paste(ff::as.ram(sccsData$cases$startYear),
                         ff::as.ram(sccsData$cases$startMonth),
                         ff::as.ram(sccsData$cases$startDay),
                         sep = "-"), format = "%Y-%m-%d")
  dates <- dates + ff::as.ram(sccsData$cases$observationDays)
  studyEndDate <- max(dates)
  result <- ff::as.ff(dates == studyEndDate)
  if (!is.null(maxAge)) {
    maxAgeDays <- (maxAge + 1) * 365.25
    truncatedByMaxAge <- sccsData$cases$ageInDays + sccsData$cases$observationDays > maxAgeDays
    result <- result | truncatedByMaxAge
  }
  return(result)
}

addAgeSettings <- function(settings,
                           ageSettings,
                           includedOutcomes) {
  if (is.null(ageSettings$minAge)) {
    settings$minAge <- -1
  } else {
    settings$minAge <- ageSettings$minAge * 365.25
  }
  if (is.null(ageSettings$maxAge)) {
    settings$maxAge <- -1
  } else {
    settings$maxAge <- (ageSettings$maxAge + 1) * 365.25
  }
  if (!ageSettings$includeAge) {
    settings$ageOffset <- 0
    settings$ageDesignMatrix <- matrix()
  } else {
    if (length(ageSettings$ageKnots) == 1) {
      ageKnots <- ffbase::quantile.ff(includedOutcomes$outcomeAge,
                                      seq(0.01, 0.99, length.out = ageSettings$ageKnots))
    } else {
      ageKnots <- ageSettings$ageKnots
    }
    settings$ageOffset <- ageKnots[1]
    ageDesignMatrix <- splines::bs(ageKnots[1]:ageKnots[length(ageKnots)],
                                   knots = ageKnots[2:(length(ageKnots) - 1)],
                                   Boundary.knots = ageKnots[c(1, length(ageKnots))])
    # Fixing first beta to zero, so dropping first column of design matrix:
    settings$ageDesignMatrix <- ageDesignMatrix[, 2:ncol(ageDesignMatrix)]
    splineCovariateRef <- data.frame(covariateId = 100:(100 + length(ageKnots) - 1),
                                     covariateName = paste("Age spline component",
                                                           1:(length(ageKnots))),
                                     originalCovariateId = 0,
                                     originalCovariateName = "")
    settings$covariateRef <- rbind(settings$covariateRef, splineCovariateRef)
    age <- list(ageKnots = ageKnots,
                covariateIds = splineCovariateRef$covariateId,
                allowRegularization = ageSettings$allowRegularization,
                computeConfidenceIntervals = ageSettings$computeConfidenceIntervals)
    settings$metaData$age <- age
  }
  return(settings)
}

addSeasonalitySettings <- function(settings, seasonalitySettings, sccsData) {
  if (!seasonalitySettings$includeSeasonality) {
    settings$seasonDesignMatrix <- matrix()
  } else {
    if (length(seasonalitySettings$seasonKnots) == 1) {
      # Single number, should interpret as number of knots. Spread out knots evenly:
      seasonKnots <- seq(1, 12, length.out = seasonalitySettings$seasonKnots)
    } else {
      seasonKnots <- seasonalitySettings$seasonKnots
    }
    seasonDesignMatrix <- cyclicSplineDesign(1:12, knots = seasonKnots)
    # Fixing first beta to zero, so dropping first column of design matrix:
    settings$seasonDesignMatrix <- seasonDesignMatrix[, 2:ncol(seasonDesignMatrix)]
    splineCovariateRef <- data.frame(covariateId = 200:(200 + length(seasonKnots) - 3),
                                     covariateName = paste("Seasonality spline component",
                                                           1:(length(seasonKnots) - 2)),
                                     originalCovariateId = 0,
                                     originalCovariateName = "")
    settings$covariateRef <- rbind(settings$covariateRef, splineCovariateRef)
    seasonality <- list(seasonKnots = seasonKnots,
                        covariateIds = splineCovariateRef$covariateId,
                        allowRegularization = seasonalitySettings$allowRegularization,
                        computeConfidenceIntervals = seasonalitySettings$computeConfidenceIntervals)
    settings$metaData$seasonality <- seasonality
  }
  return(settings)
}

addEventDependentObservationSettings <- function(settings,
                                                 eventDependentObservation,
                                                 outcomeId,
                                                 naivePeriod,
                                                 sccsData) {
  if (eventDependentObservation) {
    # Pick first outcome per person
    t <- sccsData$eras$eraType == "hoi" & sccsData$eras$conceptId == outcomeId
    rownames(sccsData$eras) <- NULL
    firstOutcomes <- aggregate(startDay ~ observationPeriodId,
                               data = sccsData$eras[ffbase::ffwhich(t, t == TRUE), ],
                               min)
    colnames(firstOutcomes)[colnames(firstOutcomes) == "startDay"] <- "outcomeDay"
    m <- ffbase::ffmatch(ff::as.ff(firstOutcomes$observationPeriodId), sccsData$cases$observationPeriodId)
    firstOutcomes$censoredDays <- ff::as.ram(sccsData$cases$censoredDays[m])

    # See who has first event in remaining observation period after applying naive period
    firstOutcomes <- firstOutcomes[firstOutcomes$outcomeDay >= (naivePeriod - firstOutcomes$censoredDays) & firstOutcomes$outcomeDay >= 0, ]
    rownames(sccsData$cases) <- NULL
    t <- in.ff(sccsData$cases$observationPeriodId, ff::as.ff(firstOutcomes$observationPeriodId))
    cases <- ff::as.ram(sccsData$cases[ffbase::ffwhich(t, t == TRUE), ])
    cases <- merge(cases, firstOutcomes)

    # Fit censoring models
    data <- data.frame(astart = cases$ageInDays + naivePeriod,
                       aend = cases$ageInDays + cases$observationDays,
                       aevent = cases$ageInDays + cases$outcomeDay + 1,
                       present = cases$uncensored)
    # data$aend[cases$ageInDays + data$aend == data$aevent] <- data$aend[cases$ageInDays + data$aend ==
    # data$aevent] + 0.5

    settings$censorModel <- fitModelsAndPickBest(data)
    settings$metaData$censorModel <- settings$censorModel
  } else {
    settings$censorModel <- list(model = 0, p = c(0))
  }
  return(settings)
}

addCovariateSettings <- function(settings, covariateSettings, sccsData) {
  if (is.list(covariateSettings) && class(covariateSettings) != "covariateSettings") {
    covariateSettingsList <- covariateSettings
  } else {
    covariateSettingsList <- list(covariateSettings)
  }
  covariateRef <- ff::as.ram(sccsData$covariateRef)

  # Iterate over different covariate settings. Assign unique IDs, and store in covariateRef:
  outputId <- 1000
  for (i in 1:length(covariateSettingsList)) {
    covariateSettings <- covariateSettingsList[[i]]

    if (is.null(covariateSettings$label)) {
      covariateSettings$label <- "Covariate"
    }
    if (is.null(covariateSettings$includeCovariateIds) || length(covariateSettings$includeCovariateIds) ==
        0) {
      covariateSettings$covariateIds <- ff::as.ram(sccsData$covariateRef$covariateId)
      t <- sccsData$eras$eraType == "hoi"
      t <- ffbase::ffwhich(t, t == FALSE)
      covariateSettings$covariateIds <- ff::as.ram(ffbase::unique.ff(sccsData$eras$conceptId[t]))
    } else {
      covariateSettings$covariateIds <- covariateSettings$includeCovariateIds
    }
    if (!is.null(covariateSettings$excludeCovariateIds) && length(covariateSettings$excludeCovariateIds) !=
        0) {
      covariateSettings$covariateIds <- covariateSettings$covariateIds[!ffbase::`%in%`(covariateSettings$covariateIds,
                                                                         covariateSettings$excludeCovariateIds)]
    }

    if (length(covariateSettings$splitPoints) == 0) {
      if (!covariateSettings$stratifyById) {
        # Create a single output ID
        covariateSettings$outputIds <- as.matrix(outputId)
        newCovariateRef <- data.frame(covariateId = outputId,
                                      covariateName = covariateSettings$label,
                                      originalCovariateId = 0,
                                      originalCovariateName = "")
        settings$covariateRef <- rbind(settings$covariateRef, newCovariateRef)
        outputId <- outputId + 1
      } else {
        # Create a unique output ID for every covariate ID
        outputIds <- outputId:(outputId + length(covariateSettings$covariateIds) - 1)
        covariateSettings$outputIds <- matrix(outputIds, ncol = 1)
        outputId <- outputId + length(outputIds)
        varNames <- covariateRef[covariateRef$covariateId %in% covariateSettings$covariateIds, ]
        if (nrow(varNames) == 0) {
          warning(paste0("Could not find covariate with ID ", covariateSettings$covariateIds, " in data"))
        } else {
          names(varNames)[names(varNames) == "covariateId"] <- "originalCovariateId"
          names(varNames)[names(varNames) == "covariateName"] <- "originalCovariateName"
          varNames$originalCovariateName <- as.character(varNames$originalCovariateName)
          varNames$originalCovariateName[varNames$originalCovariateName == ""] <- varNames$originalCovariateId[varNames$originalCovariateName ==
                                                                                                                 ""]
          varNames$covariateName <- paste(covariateSettings$label,
                                          varNames$originalCovariateName,
                                          sep = ": ")
          newCovariateRef <- data.frame(covariateId = outputIds,
                                        originalCovariateId = covariateSettings$covariateIds)
          newCovariateRef <- merge(newCovariateRef, varNames, by = "originalCovariateId")
          settings$covariateRef <- rbind(settings$covariateRef, newCovariateRef)
        }
      }
    } else {
      startDays <- c(covariateSettings$start, covariateSettings$splitPoints + 1)
      endDays <- c(covariateSettings$splitPoints, NA)
      if (!covariateSettings$stratifyById) {
        outputIds <- outputId:(outputId + length(covariateSettings$splitPoints))
        outputId <- outputId + length(covariateSettings$splitPoints) + 1
        varNames <- rep(covariateSettings$label, length(covariateSettings$splitPoints) + 1)
        varNames <- paste(varNames, " day ", startDays, "-", c(endDays[1:length(endDays) - 1], ""))
        # covariateSettings$outputIds <- matrix(outputIds, ncol = 1)
        covariateSettings$outputIds <- matrix(outputIds,
                                              ncol = length(covariateSettings$splitPoints) + 1,
                                              byrow = TRUE)
        newCovariateRef <- data.frame(covariateId = outputIds,
                                      covariateName = varNames,
                                      originalCovariateId = 0,
                                      originalCovariateName = "")
        settings$covariateRef <- rbind(settings$covariateRef, newCovariateRef)
      } else {
        outputIds <- outputId:(outputId + (length(covariateSettings$splitPoint) + 1) * length(covariateSettings$covariateIds) - 1)
        outputId <- max(outputIds) + 1
        covariateSettings$outputIds <- matrix(outputIds,
                                              ncol = length(covariateSettings$splitPoints) + 1,
                                              byrow = TRUE)
        if (any(covariateSettings$covariateIds %in% covariateRef$covariateId)) {
          originalCovariateId <- rep(covariateSettings$covariateIds,
                                     each = length(covariateSettings$splitPoints) + 1)
          originalCovariateName <- covariateRef$covariateName[match(originalCovariateId,
                                                                    covariateRef$covariateId)]
          originalCovariateName[originalCovariateName == ""] <- originalCovariateId[originalCovariateName == ""]
          varNames <- paste(covariateSettings$label, ": ", originalCovariateName, sep = "")
          varNames <- paste(varNames,
                            ", day ",
                            startDays,
                            "-",
                            c(endDays[1:length(endDays) - 1], ""),
                            sep = "")

          newCovariateRef <- data.frame(covariateId = outputIds,
                                        covariateName = varNames,
                                        originalCovariateId = originalCovariateId,
                                        originalCovariateName = originalCovariateName)
          settings$covariateRef <- rbind(settings$covariateRef, newCovariateRef)
        }
      }
    }
    covariateSettingsList[[i]] <- covariateSettings
  }
  settings$covariateSettingsList <- covariateSettingsList
  settings$covariateRef$covariateName <- as.factor(settings$covariateRef$covariateName)
  settings$covariateRef$originalCovariateName <- as.factor(settings$covariateRef$originalCovariateName)
  return(settings)
}

#' Create covariate settings
#'
#' @details
#' Create an object specifying how to create a (set of) covariates.
#'
#' @param includeCovariateIds     One or more IDs of variables in the \code{sccsData} object that
#'                                should be used to construct this covariate. If no IDs are specified,
#'                                all variables will be used.
#' @param excludeCovariateIds     One or more IDs of variables in the \code{sccsData} object that
#'                                should not be used to construct this covariate.
#' @param label                   A label used to identify the covariates created using these settings.
#' @param stratifyById            Should a single covariate be created for every ID in the
#'                                \code{sccsData} object, or should a single covariate be constructed?
#'                                For example, if the IDs identify exposures to different drugs, should
#'                                a covariate be constructed for every drug, or a single covariate for
#'                                exposure to any of these drugs. Note that overlap will be considered
#'                                a single exposure.
#' @param start                   The start of the risk window in days, relative to the exposure start
#'                                date.
#' @param addExposedDaysToStart   Should the length of exposure be added to the start date?
#' @param end                     The start of the risk window in days, relative to the exposure start
#'                                date.
#' @param addExposedDaysToEnd     Should the length of exposure be added to the end date?
#' @param firstOccurrenceOnly     Should only the first occurrence of the exposure be used?
#' @param splitPoints             To split the risk window into several smaller windows, specify the
#'                                end of each sub- window relative to the start of the main risk
#'                                window. If addExposedDaysToStart is TRUE, the split points will be
#'                                considered to be relative to the end of the main risk window instead.
#' @param allowRegularization     When fitting the model, should the covariates defined here be allowed
#'                                to be regularized?
#'
#' @return
#' An object of type \code{covariateSettings}.
#'
#' @export
createCovariateSettings <- function(includeCovariateIds = NULL,
                                    excludeCovariateIds = NULL,
                                    label = "Covariates",
                                    stratifyById = TRUE,
                                    start = 0,
                                    addExposedDaysToStart = FALSE,
                                    end = 0,
                                    addExposedDaysToEnd = FALSE,
                                    firstOccurrenceOnly = FALSE,
                                    splitPoints = c(),
                                    allowRegularization = FALSE) {
  if (end < start && !addExposedDaysToEnd)
    stop("End day always precedes start day. Either pick a later end day, or set addExposedDaysToEnd to TRUE.")

  # First: get default values:
  analysis <- list()
  for (name in names(formals(createCovariateSettings))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  class(analysis) <- "covariateSettings"
  return(analysis)
}

#' Create age settings
#'
#' @details
#' Create an object specifing whether and how age should be included in the model. Age can be included
#' by splitting patient time into calendar months. During a month, the relative risk attributed to age
#' is assumed to be constant, and the risk from month to month is modeled using a cubic spline.
#'
#' @param includeAge            Should age be included in the model?
#' @param ageKnots              If a single number is provided this is assumed to indicate the number
#'                              of knots to use for the spline, and the knots are automatically spaced
#'                              according to equal percentiles of the data. If more than one number is
#'                              provided these are assumed to be the exact location of the knots in
#'                              age-days
#' @param allowRegularization   When fitting the model, should the covariates defined here be allowed
#'                              to be regularized?
#' @param computeConfidenceIntervals  Should confidence intervals be computed for the covariates defined
#'                                    here? Setting this to FALSE might save computing time when fitting the
#'                                    model. Will be turned to FALSE  automaticaly when \code{allowRegularization = TRUE}.
#' @param minAge                Minimum age at which patient time will be included in the analysis. Note
#'                              that information prior to the min age is still used to determine exposure
#'                              status after the minimum age (e.g. when a prescription was started just prior
#'                              to reaching the minimum age). Also, outcomes occurring before the minimum age
#'                              is reached will be considered as prior outcomes when using first outcomes only.
#'                              Age should be specified in years, but non-integer values are allowed. If not
#'                              specified, no age restriction will be applied.
#' @param maxAge                Maximum age at which patient time will be included in the analysis. Age should
#'                              be specified in years, but non-integer values are allowed. If not
#'                              specified, no age restriction will be applied.
#'
#' @return
#' An object of type \code{ageSettings}.
#'
#' @export
createAgeSettings <- function(includeAge = FALSE,
                              ageKnots = 5,
                              allowRegularization = FALSE,
                              computeConfidenceIntervals = FALSE,
                              minAge = NULL,
                              maxAge = NULL) {
  if (computeConfidenceIntervals && allowRegularization) {
    computeConfidenceIntervals <- FALSE
    warning("computeConfidenceIntervals is set to FALSE because allowRegularization is TRUE")
  }
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createAgeSettings))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  class(analysis) <- "ageSettings"
  return(analysis)
}

#' Create seasonality settings
#'
#' @details
#' Create an object specifing whether and how seasonality should be included in the model. Seasonality
#' can be included by splitting patient time into calendar months. During a month, the relative risk
#' attributed to season is assumed to be constant, and the risk from month to month is modeled using a
#' cyclic cubic spline.
#'
#' @param includeSeasonality    Should seasonlaity be included in the model?
#' @param seasonKnots           If a single number is provided this is assumed to indicate the number
#'                              of knots to use for the spline, and the knots are automatically equaly
#'                              spaced across the year. If more than one number is provided these are
#'                              assumed to be the exact location of the knots in days relative to the
#'                              start of the year.
#' @param allowRegularization   When fitting the model, should the covariates defined here be allowed
#'                              to be regularized?
#' @param computeConfidenceIntervals  Should confidence intervals be computed for the covariates defined
#'                                    here? Setting this to FALSE might save computing time when fitting the
#'                                    model. Will be turned to FALSE  automaticaly when \code{allowRegularization = TRUE}.
#'
#' @return
#' An object of type \code{seasonalitySettings}.
#'
#' @export
createSeasonalitySettings <- function(includeSeasonality = FALSE,
                                      seasonKnots = 5,
                                      allowRegularization = FALSE,
                                      computeConfidenceIntervals = FALSE) {
  if (computeConfidenceIntervals && allowRegularization) {
    computeConfidenceIntervals <- FALSE
    warning("computeConfidenceIntervals is set to FALSE because allowRegularization is TRUE")
  }
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createSeasonalitySettings))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  class(analysis) <- "seasonalitySettings"
  return(analysis)
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
#' @param compress      Should compression be used when saving? IF TRUE, the zip program needs to be
#'                      available on the command prompt.
#'
#' @details
#' The data will be written to a set of files in the specified folder.
#'
#' @export
saveSccsEraData <- function(sccsEraData, folder, compress = FALSE) {
  if (missing(sccsEraData))
    stop("Must specify sccsEraData")
  if (missing(folder))
    stop("Must specify folder")
  if (class(sccsEraData) != "sccsEraData")
    stop("Data not of class sccsEraData")

  dir.create(folder)

  if (is.null(sccsEraData$outcomes)) {
    outcomes <- ff::as.ffdf(data.frame(error = 1))
    covariates <- ff::as.ffdf(data.frame(error = 1))
    covariateRef <- ff::as.ffdf(data.frame(error = 1))
  } else {
    outcomes <- sccsEraData$outcomes
    covariates <- sccsEraData$covariates
    covariateRef <- sccsEraData$covariateRef
  }
  if (compress) {
    saveCompressedFfdf(outcomes, file.path(folder, "outcomes"))
    saveCompressedFfdf(covariates, file.path(folder, "covariates"))
    saveCompressedFfdf(covariateRef, file.path(folder, "covariateRef"))
  } else {
    ffbase::save.ffdf(outcomes, covariates, covariateRef, dir = folder)
    # Open all ffdfs to prevent annoying messages later:
    if (!is.null(sccsEraData$outcomes)) {
      open(sccsEraData$outcomes)
      open(sccsEraData$covariates)
      open(sccsEraData$covariateRef)
    }
  }
  metaData <- sccsEraData$metaData
  save(metaData, file = file.path(folder, "metaData.Rdata"))
  invisible(TRUE)
}

#' Load the SCCS era data from a folder
#'
#' @description
#' \code{loadSccsEraData} loads an object of type sccsEraData from a folder in the file system.
#'
#' @param folder     The name of the folder containing the data.
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
  if (file.exists(file.path(absolutePath, "outcomes.zip"))) {
    outcomes <- loadCompressedFfdf(file.path(absolutePath, "outcomes"))
    covariates <- loadCompressedFfdf(file.path(absolutePath, "covariates"))
    covariateRef <- loadCompressedFfdf(file.path(absolutePath, "covariateRef"))
  } else {
    ffbase::load.ffdf(absolutePath, e)
  }
  load(file.path(absolutePath, "metaData.Rdata"), e)
  result <- list(outcomes = get("outcomes", envir = e),
                 covariates = get("covariates", envir = e),
                 covariateRef = get("covariateRef", envir = e),
                 metaData = get("metaData", envir = e))
  if (!file.exists(file.path(absolutePath, "outcomes.zip"))) {
    # Open all ffdfs to prevent annoying messages later:
    open(result$outcomes, readonly = readOnly)
    open(result$covariates, readonly = readOnly)
    open(result$covariateRef, readonly = readOnly)
  }
  if (!is.null(result$outcomes$error)) {
    result$outcomes <- NULL
    result$covariates <- NULL
    result$covariateRef <- NULL
  }
  class(result) <- "sccsEraData"
  rm(e)
  return(result)
}



#' Force a loaded SCCS era data in RAM
#'
#' @description
#' \code{forceSccsEraDataIntoRam} converts the ffdf components of an sccsEraData object
#' into data.table components
#'
#' @param sccsEraData     Exisiting sccsEraData object.
#'
#' @details
#' Uses ff::as.ram() to move virtual data into data.table objects
#'
#' @return
#' An object of class sccsEraData
#'
#' @export
forceSccsEraDataIntoRam <- function(sccsEraData) {
  if (!inherits(sccsEraData$outcomes , "ffdf")) {
    stop("sccsEraData must contain virtual ffdf objects")
  }
  sccsEraData$outcomes <- ff::as.ram(sccsEraData$outcomes)
  sccsEraData$covariates <- ff::as.ram(sccsEraData$covariates)
  sccsEraData$covariateRef <- ff::as.ram(sccsEraData$covariateRef)

  return (sccsEraData)
}

#' @export
print.sccsEraData <- function(x, ...) {
  writeLines("sccsEraData object")
  writeLines("")
  writeLines(paste("Outcome ID:", paste(x$metaData$outcomeId, collapse = ",")))
}

#' @export
summary.sccsEraData <- function(object, ...) {
  if (is.null(object$outcomes)) {
    outcomeCounts <- data.frame(outcomeConceptId = object$metaData$outcomeId,
                                eventCount = 0,
                                caseCount = 0)

    result <- list(metaData = object$metaData,
                   outcomeCounts = outcomeCounts,
                   covariateCount = 0,
                   covariateValueCount = 0,
                   covariateRef = ff::as.ram(object$covariateRef))
  } else {

    eventCount <- ifelse(inherits(object$outcomes, "ffdf"),
                         ffbase::sum.ff(object$outcomes$y),
                         sum(object$outcomes$y))

    caseCount <- ifelse(inherits(object$outcomes, "ffdf"),
                        length(ffbase::unique.ff(object$outcomes$stratumId)),
                        length(unique(object$outcomes$stratumId)))

    outcomeCounts <- data.frame(outcomeConceptId = object$metaData$outcomeId,
                                eventCount = eventCount,
                                caseCount = caseCount)

    result <- list(metaData = object$metaData,
                   outcomeCounts = outcomeCounts,
                   covariateCount = nrow(object$covariateRef),
                   covariateValueCount = nrow(object$covariates),
                   covariateRef = ff::as.ram(object$covariateRef))

  }

  class(result) <- "summary.sccsEraData"
  return(result)
}

#' @export
print.summary.sccsEraData <- function(x, ...) {
  writeLines("sccsEraData object summary")
  writeLines("")
  writeLines(paste("Outcome ID:", paste(x$metaData$outcomeId, collapse = ",")))
  writeLines("")
  writeLines("Outcome count:")
  outcomeCounts <- x$outcomeCounts
  rownames(outcomeCounts) <- outcomeCounts$outcomeConceptId
  outcomeCounts$outcomeConceptId <- NULL
  colnames(outcomeCounts) <- c("Event count", "Case count")
  printCoefmat(outcomeCounts)
  writeLines("")
  writeLines("Covariates:")
  writeLines(paste("Number of covariates:", x$covariateCount))
  writeLines(paste("Number of covariate eras:", x$covariateValueCount))
  writeLines("")
  covariateRef <- x$covariateRef
  if (nrow(covariateRef) > 10)
    covariateRef <- covariateRef[1:10, ]
  rownames(covariateRef) <- as.character(covariateRef$covariateName)
  covariateRef$covariateName <- NULL
  covariateRef$originalCovariateName <- NULL
  colnames(covariateRef) <- c("Original covariate ID", "Current covariate ID")
  printCoefmat(covariateRef)
}

#' Create a design matrix for a cyclic spline
#'
#' @details
#' This function is used by other functions in this package.
#'
#' @param x       Vector of coordinates of the points to be interpolated.
#' @param knots   Location of the knots.
#' @param ord     Order of the spline function.
#'
#' @export
cyclicSplineDesign <- function(x, knots, ord = 4) {
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
  knots <- c(k1 - (knots[nk] - knots[(nk - ord + 1):(nk - 1)]), knots)
  ind <- x > xc
  X1 <- splines::splineDesign(knots, x, ord, outer.ok = TRUE)
  x[ind] <- x[ind] - max(knots) + k1
  if (sum(ind)) {
    X2 <- splines::splineDesign(knots, x[ind], ord, outer.ok = TRUE)
    X1[ind, ] <- X1[ind, ] + X2
  }
  X1
}

saveCompressedFfdf <- function(ffdf, fileName) {
  dir.create(dirname(fileName), showWarnings = FALSE, recursive = TRUE)
  saveRDS(ffdf, paste0(fileName, ".rds"))
  fileNames <- sapply(bit::physical(ffdf), function(x) bit::physical(x)$filename)
  sourceDir <- dirname(fileNames[1])
  oldWd <- setwd(sourceDir)
  on.exit(setwd(oldWd))
  sourceNames <- basename(fileNames)
  ff::close.ffdf(ffdf)
  DatabaseConnector::createZipFile(zipFile = paste0(fileName, ".zip"), files = sourceNames)
  ff::open.ffdf(ffdf)
}

loadCompressedFfdf <- function(fileName) {
  ffdf <- readRDS(paste0(fileName, ".rds"))
  tempRoot <- ff::fftempfile("temp")
  utils::unzip(zipfile = paste0(fileName, ".zip"), exdir = tempRoot)
  for (ff in bit::physical(ffdf)) {
    newFileName <- ff::fftempfile("")
    file.rename(file.path(tempRoot, basename(bit::physical(ff)$filename)), newFileName)
    bit::physical(ff)$filename <- newFileName
    bit::physical(ff)$finalizer <- "delete"
    ff::open.ff(ff)
    reg.finalizer(attr(ff,"physical"), ff::finalize.ff_pointer, onexit = bit::physical(ff)$finonexit)
  }
  unlink(tempRoot, recursive = TRUE)
  return(ffdf)
}
