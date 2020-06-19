# Copyright 2020 Observational Health Data Sciences and Informatics
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

#' Create SCCS era data
#'
#' @details
#' This function creates covariates based on the data in the `sccsData` argument, according to the
#' provided settings. It chops patient time into periods during which all covariates remain constant.
#' The output details these periods, their durations, and a sparse representation of the covariate
#' values.
#'
#' @template StudyPopulation
#' @template SccsData
#' @param eraCovariateSettings        Either an object of type `covariateSettings` as created
#'                                    using the [createEraCovariateSettings()] function, or a
#'                                    list of such objects.
#' @param ageCovariateSettings        An object of type `ageCovariateSettings` as created using the
#'                                    [createAgeCovariateSettings()] function.
#' @param seasonalityCovariateSettings An object of type `seasonalityCovariateSettings` as created using the
#'                                    [createSeasonalityCovariateSettings()] function.
#' @param minCasesForAgeSeason        Minimum number of cases to use to fit age and season splines. If
#'                                    needed (and available), cases that are not exposed will be included.
#' @param eventDependentObservation   Should the extension proposed by Farrington et al. be used to
#'                                    adjust for event-dependent observation time?
#'
#' @references
#' Farrington, C. P., Anaya-Izquierdo, A., Whitaker, H. J., Hocine, M.N., Douglas, I., and Smeeth, L.
#' (2011). Self-Controlled case series analysis with event-dependent observation periods. Journal of
#' the American Statistical Association 106 (494), 417-426
#'
#' @return
#' An object of type [SccsIntervalData].
#'
#' @export
createSccsIntervalData <- function(studyPopulation,
                                   sccsData,
                                   eraCovariateSettings,
                                   ageCovariateSettings = NULL,
                                   seasonalityCovariateSettings = NULL,
                                   minCasesForAgeSeason = 10000,
                                   eventDependentObservation = FALSE) {
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

  ageSeasonsCases <- numeric(0)
  if (!is.null(ageCovariateSettings) || !is.null(seasonalityCovariateSettings)) {
    if (nrow(studyPopulation$cases) > minCasesForAgeSeason) {
      set.seed(0)
      ageSeasonsCases <- sample(studyPopulation$cases$caseId, minCasesForAgeSeason, replace = FALSE)
    }
  }

  settings <- list()
  settings$metaData <- list()
  settings$covariateRef <- tibble()
  settings <- addEventDependentObservationSettings(settings,
                                                   eventDependentObservation,
                                                   studyPopulation)
  if (eventDependentObservation && settings$metaData$censorModel$model %in% c(1, 3) && !is.null(ageCovariateSettings)) {
    warning("Optimal censoring model adjusts for age, so removing age as separate covariate.")
    ageCovariateSettings <- NULL
  }
  settings <- addAgeSettings(settings, ageCovariateSettings, studyPopulation)
  settings <- addSeasonalitySettings(settings, seasonalityCovariateSettings, sccsData)

  settings <- addEraCovariateSettings(settings, eraCovariateSettings, sccsData)
  settings$metaData$covariateSettingsList <- settings$covariateSettingsList
  metaData <- append(studyPopulation$metaData, settings$metaData)

  ParallelLogger::logInfo("Converting person data to SCCS intervals. This might take a while.")
  # Ensure all sorted bv caseId:
  cases <- studyPopulation$cases[order(studyPopulation$cases$caseId), ]
  outcomes <- studyPopulation$outcomes[order(studyPopulation$outcomes$caseId), ]
  eras <- sccsData$eras %>%
    arrange(.data$caseId)

  data <- convertToSccs(cases,
                        outcomes,
                        eras,
                        !is.null(ageCovariateSettings),
                        settings$ageOffset,
                        settings$ageDesignMatrix,
                        !is.null(seasonalityCovariateSettings),
                        settings$seasonDesignMatrix,
                        ageSeasonsCases,
                        settings$covariateSettingsList,
                        eventDependentObservation,
                        settings$censorModel)

  if (is.null(data$outcomes)) {
    warning("Conversion resulted in empty data set. Perhaps no one with the outcome had any exposure of interest?")
    data <- createEmptySccsIntervalData()
    if (nrow(settings$covariateRef) > 0) {
      data$covariateRef <- settings$covariateRef
    }

  } else {
    metaData$covariateStatistics <- collect(data$covariateStatistics)
    data$covariateStatistics <- NULL
    data$covariateRef <- settings$covariateRef

  }
  attr(data, "metaData") <- metaData
  class(data) <- "SccsIntervalData"
  attr(class(data), "package") <- "SelfControlledCaseSeries"

  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Generating SCCS interval data took", signif(delta, 3), attr(delta, "units")))
  return(data)
}

createEmptySccsIntervalData <- function() {
  sccsIntervalData <- Andromeda::andromeda(outcomes = tibble(rowId = 1,
                                                             stratumId = 1,
                                                             time = 1,
                                                             y = 1)[-1, ],
                                           covariates = tibble(rowId = 1,
                                                               stratumId = 1,
                                                               covariateId = 1,
                                                               covariateValue = 1)[-1, ],
                                           covariateRef = tibble(covariateId = 1,
                                                                 covariateName = "",
                                                                 originalEraId = 1,
                                                                 originalEraName = "",
                                                                 originalEraType = "")[-1, ])
  return(sccsIntervalData)
}

addAgeSettings <- function(settings,
                           ageCovariateSettings,
                           studyPopulation) {
  if (is.null(ageCovariateSettings)) {
    settings$ageOffset <- 0
    settings$ageDesignMatrix <- matrix()
  } else {
    if (length(ageCovariateSettings$ageKnots) == 1) {
      ageKnots <- studyPopulation$outcomes %>%
        inner_join(studyPopulation$cases, by = "caseId") %>%
        transmute(outcomeAge = .data$outcomeDay + .data$ageInDays) %>%
        pull() %>%
        quantile(seq(0.01, 0.99, length.out = ageCovariateSettings$ageKnots))
    } else {
      ageKnots <- ageCovariateSettings$ageKnots
    }
    settings$ageOffset <- ageKnots[1]
    ageDesignMatrix <- splines::bs(ageKnots[1]:ageKnots[length(ageKnots)],
                                   knots = ageKnots[2:(length(ageKnots) - 1)],
                                   Boundary.knots = ageKnots[c(1, length(ageKnots))])
    # Fixing first beta to zero, so dropping first column of design matrix:
    settings$ageDesignMatrix <- ageDesignMatrix[, 2:ncol(ageDesignMatrix)]
    splineCovariateRef <- tibble(covariateId = 100:(100 + length(ageKnots) - 1),
                                 covariateName = paste("Age spline component",
                                                       1:(length(ageKnots))),
                                 originalEraId = 0,
                                 originalEraType = "",
                                 originalEraName = "")
    settings$covariateRef <- bind_rows(settings$covariateRef, splineCovariateRef)
    age <- list(ageKnots = ageKnots,
                covariateIds = splineCovariateRef$covariateId,
                allowRegularization = ageCovariateSettings$allowRegularization,
                computeConfidenceIntervals = ageCovariateSettings$computeConfidenceIntervals)
    settings$metaData$age <- age
  }
  return(settings)
}

addSeasonalitySettings <- function(settings, seasonalityCovariateSettings, sccsData) {
  if (is.null(seasonalityCovariateSettings)) {
    settings$seasonDesignMatrix <- matrix()
  } else {
    if (length(seasonalityCovariateSettings$seasonKnots) == 1) {
      # Single number, should interpret as number of knots. Spread out knots evenly:
      seasonKnots <- seq(1, 12, length.out = seasonalityCovariateSettings$seasonKnots)
    } else {
      seasonKnots <- seasonalityCovariateSettings$seasonKnots
    }
    seasonDesignMatrix <- cyclicSplineDesign(1:12, knots = seasonKnots)
    # Fixing first beta to zero, so dropping first column of design matrix:
    settings$seasonDesignMatrix <- seasonDesignMatrix[, 2:ncol(seasonDesignMatrix)]
    splineCovariateRef <- tibble(covariateId = 200:(200 + length(seasonKnots) - 3),
                                 covariateName = paste("Seasonality spline component",
                                                       1:(length(seasonKnots) - 2)),
                                 originalEraId = 0,
                                 originalEraType = "",
                                 originalEraName = "")
    settings$covariateRef <- bind_rows(settings$covariateRef, splineCovariateRef)
    seasonality <- list(seasonKnots = seasonKnots,
                        covariateIds = splineCovariateRef$covariateId,
                        allowRegularization = seasonalityCovariateSettings$allowRegularization,
                        computeConfidenceIntervals = seasonalityCovariateSettings$computeConfidenceIntervals)
    settings$metaData$seasonality <- seasonality
  }
  return(settings)
}

addEventDependentObservationSettings <- function(settings,
                                                 eventDependentObservation,
                                                 studyPopulation) {
  if (!eventDependentObservation) {
    settings$censorModel <- list(model = 0, p = c(0))
  } else {

    data <- studyPopulation$outcomes %>%
      group_by(.data$caseId) %>%
      summarise(outcomeDay = min(.data$outcomeDay)) %>%
      inner_join(studyPopulation$cases, by = "caseId") %>%
      transmute(astart = .data$ageInDays,
                aend = .data$ageInDays + .data$endDay + 1,
                aevent = .data$ageInDays + .data$outcomeDay + 1,
                present = .data$noninformativeEndCensor == 1)

    settings$censorModel <- fitModelsAndPickBest(data)
    settings$metaData$censorModel <- settings$censorModel
  }
  return(settings)
}

addEraCovariateSettings <- function(settings, eraCovariateSettings, sccsData) {
  if (is.list(eraCovariateSettings) && class(eraCovariateSettings) != "EraCovariateSettings") {
    eraCovariateSettingsList <- eraCovariateSettings
  } else {
    eraCovariateSettingsList <- list(eraCovariateSettings)
  }
  eraRef <- sccsData$eraRef %>%
    collect()

  # Iterate over different covariate settings. Assign unique IDs, and store in covariateRef:
  outputId <- 1000
  for (i in 1:length(eraCovariateSettingsList)) {
    covariateSettings <- eraCovariateSettingsList[[i]]

    if (is.null(covariateSettings$label)) {
      covariateSettings$label <- "Covariate"
    }
    if (is.null(covariateSettings$includeEraIds) || length(covariateSettings$includeEraIds) == 0) {
      covariateSettings$eraIds <- eraRef %>%
        filter(.data$eraType != "hoi") %>%
        select(.data$eraId) %>%
        pull()
    } else {
      covariateSettings$eraIds <- covariateSettings$includeEraIds
    }
    if (!is.null(covariateSettings$excludeEraIds) && length(covariateSettings$excludeEraIds) != 0) {
      covariateSettings$eraIds <- covariateSettings$eraIds[!covariateSettings$eraIds %in% covariateSettings$excludeEraIds]
    }

    if (length(covariateSettings$splitPoints) == 0) {
      if (!covariateSettings$stratifyById) {
        # Create a single output ID
        covariateSettings$outputIds <- as.matrix(outputId)
        newCovariateRef <- tibble(covariateId = outputId,
                                  covariateName = covariateSettings$label,
                                  originalEraId = 0,
                                  originalEraType = "",
                                  originalEraName = "")
        settings$covariateRef <- bind_rows(settings$covariateRef, newCovariateRef)
        outputId <- outputId + 1
      } else {
        # Create a unique output ID for every covariate ID
        outputIds <- outputId:(outputId + length(covariateSettings$eraIds) - 1)
        covariateSettings$outputIds <- matrix(outputIds, ncol = 1)
        outputId <- outputId + length(outputIds)
        varNames <- eraRef[eraRef$eraId %in% covariateSettings$eraIds, ]
        if (nrow(varNames) == 0) {
          warning(paste0("Could not find era with ID ", covariateSettings$eraIds, " in data"))
        } else {
          varNames <- varNames %>%
            transmute(originalEraId = .data$eraId,
                      originalEraType = .data$eraType,
                      originalEraName = .data$eraName,
                      covariateName = paste(covariateSettings$label,
                                            .data$eraName,
                                            sep = ": "))

          newCovariateRef <- tibble(covariateId = outputIds,
                                    originalEraId = covariateSettings$eraIds) %>%
            inner_join(varNames, by = "originalEraId")
          settings$covariateRef <- bind_rows(settings$covariateRef, newCovariateRef)
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
        newCovariateRef <- tibble(covariateId = outputIds,
                                  covariateName = varNames,
                                  originaEraId = 0,
                                  originalEraType = "",
                                  originalEraName = "")
        settings$covariateRef <- bind_rows(settings$covariateRef, newCovariateRef)
      } else {
        outputIds <- outputId:(outputId + (length(covariateSettings$splitPoint) + 1) * length(covariateSettings$eraIds) - 1)
        outputId <- max(outputIds) + 1
        covariateSettings$outputIds <- matrix(outputIds,
                                              ncol = length(covariateSettings$splitPoints) + 1,
                                              byrow = TRUE)
        if (any(covariateSettings$eraIds %in% eraRef$eraId)) {
          originalEraId <- rep(covariateSettings$eraIds,
                               each = length(covariateSettings$splitPoints) + 1)
          originalEraType <- eraRef$eraType[match(originalEraId,
                                                  eraRef$eraId)]
          originalEraName <- eraRef$eraName[match(originalEraId,
                                                  eraRef$eraId)]
          originalEraName[originalEraName == ""] <- originalEraId[originalEraName == ""]
          varNames <- paste(covariateSettings$label, ": ", originalEraName, sep = "")
          varNames <- paste(varNames,
                            ", day ",
                            startDays,
                            "-",
                            c(endDays[1:length(endDays) - 1], ""),
                            sep = "")

          newCovariateRef <- data.frame(covariateId = outputIds,
                                        covariateName = varNames,
                                        originalEraId = originalEraId,
                                        originalEraType = originalEraType,
                                        originalEraName = originalEraName)
          settings$covariateRef <- bind_rows(settings$covariateRef, newCovariateRef)
        }
      }
    }
    eraCovariateSettingsList[[i]] <- covariateSettings
  }
  settings$covariateSettingsList <- eraCovariateSettingsList
  return(settings)
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
