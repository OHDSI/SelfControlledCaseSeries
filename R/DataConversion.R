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
#' @param eraCovariateSettings        Either an object of type `EraCovariateSettings` as created
#'                                    using the [createEraCovariateSettings()] function, or a
#'                                    list of such objects.
#' @param ageCovariateSettings        An object of type `ageCovariateSettings` as created using the
#'                                    [createAgeCovariateSettings()] function.
#' @param seasonalityCovariateSettings An object of type `seasonalityCovariateSettings` as created using the
#'                                    [createSeasonalityCovariateSettings()] function.
#' @param calendarTimeCovariateSettings An object of type `calendarTimeCovariateSettings` as created using the
#'                                    [createCalendarTimeCovariateSettings()] function.
#' @param minCasesForAgeSeason        DEPRECATED: Use `minCasesForTimeCovariates` instead.
#' @param minCasesForTimeCovariates   Minimum number of cases to use to fit age, season and calendar time splines. If
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
                                   calendarTimeCovariateSettings = NULL,
                                   minCasesForAgeSeason = NULL,
                                   minCasesForTimeCovariates = 10000,
                                   eventDependentObservation = FALSE) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(studyPopulation, min.len = 1, add = errorMessages)
  checkmate::assertClass(sccsData, "SccsData", add = errorMessages)
  checkmate::assertList(studyPopulation, min.len = 1, add = errorMessages)
  if (is.list(eraCovariateSettings) && class(eraCovariateSettings) != "EraCovariateSettings") {
    for (i in 1:length(eraCovariateSettings)) {
      checkmate::assertClass(eraCovariateSettings[[i]], "EraCovariateSettings", add = errorMessages)
    }
  } else {
    checkmate::assertClass(eraCovariateSettings, "EraCovariateSettings", add = errorMessages)
  }
  checkmate::assertClass(ageCovariateSettings, "ageSettings", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(seasonalityCovariateSettings, "SeasonalityCovariateSettings", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(calendarTimeCovariateSettings, "CalendarTimeCovariateSettings", null.ok = TRUE, add = errorMessages)
  checkmate::assertInt(minCasesForAgeSeason, lower = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertInt(minCasesForTimeCovariates, lower = 1, add = errorMessages)
  checkmate::assertLogical(eventDependentObservation, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (!is.null(minCasesForAgeSeason)) {
    warning("Argument 'minCasesForAgeSeason' in 'createSccsIntervalData()' is deprecated. Use 'minCasesForTimeCovariates' instead.")
    minCasesForTimeCovariates <- minCasesForAgeSeason
  }

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

  timeCovariateCases <- numeric(0)
  if (!is.null(ageCovariateSettings) ||
    !is.null(seasonalityCovariateSettings) ||
    !is.null(calendarTimeCovariateSettings)) {
    if (nrow(studyPopulation$cases) > minCasesForTimeCovariates) {
      set.seed(0)
      timeCovariateCases <- sample(studyPopulation$cases$caseId, minCasesForTimeCovariates, replace = FALSE)
    }
  }

  settings <- list()
  settings$metaData <- list()
  settings$covariateRef <- tibble()
  settings <- addEventDependentObservationSettings(
    settings,
    eventDependentObservation,
    studyPopulation
  )
  if (eventDependentObservation && settings$metaData$censorModel$model %in% c(1, 3) && !is.null(ageCovariateSettings)) {
    warning("Optimal censoring model adjusts for age, so removing age as separate covariate.")
    ageCovariateSettings <- NULL
  }
  settings <- addAgeSettings(settings, ageCovariateSettings, studyPopulation)
  settings <- addSeasonalitySettings(settings, seasonalityCovariateSettings, sccsData)
  settings <- addCalendarTimeSettings(settings, calendarTimeCovariateSettings, studyPopulation)

  settings <- addEraCovariateSettings(settings, eraCovariateSettings, sccsData)
  settings$metaData$covariateSettingsList <- cleanCovariateSettingsList(settings$covariateSettingsList)
  metaData <- append(studyPopulation$metaData, settings$metaData)

  ParallelLogger::logInfo("Converting person data to SCCS intervals. This might take a while.")
  # Ensure all sorted bv caseId:
  cases <- studyPopulation$cases[order(studyPopulation$cases$caseId), ]
  outcomes <- studyPopulation$outcomes[order(studyPopulation$outcomes$caseId), ]
  eras <- sccsData$eras %>%
    arrange(.data$caseId)

  data <- convertToSccs(
    cases = cases,
    outcomes = outcomes,
    eras = eras,
    includeAge = !is.null(ageCovariateSettings),
    ageOffset = settings$ageOffset,
    ageDesignMatrix = settings$ageDesignMatrix,
    includeSeason = !is.null(seasonalityCovariateSettings),
    seasonDesignMatrix = settings$seasonDesignMatrix,
    includeCalendarTime = !is.null(calendarTimeCovariateSettings),
    calendarTimeOffset = settings$calendarTimeOffset,
    calendarTimeDesignMatrix = settings$calendarTimeDesignMatrix,
    timeCovariateCases = timeCovariateCases,
    covariateSettingsList = settings$covariateSettingsList,
    eventDependentObservation = eventDependentObservation,
    censorModel = settings$censorModel,
    scri = FALSE,
    controlIntervalId = 0
  )

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
  ParallelLogger::logInfo(paste("Generating SCCS interval data took", signif(delta, 3), attr(delta, "units")))
  return(data)
}

createEmptySccsIntervalData <- function() {
  sccsIntervalData <- Andromeda::andromeda(
    outcomes = tibble(
      rowId = 1,
      stratumId = 1,
      time = 1,
      y = 1
    )[-1, ],
    covariates = tibble(
      rowId = 1,
      stratumId = 1,
      covariateId = 1,
      covariateValue = 1
    )[-1, ],
    covariateRef = tibble(
      covariateId = 1,
      covariateName = "",
      originalEraId = 1,
      originalEraName = "",
      originalEraType = ""
    )[-1, ]
  )
  return(sccsIntervalData)
}

addAgeSettings <- function(settings,
                           ageCovariateSettings,
                           studyPopulation) {
  if (is.null(ageCovariateSettings)) {
    settings$ageOffset <- 0
    settings$ageDesignMatrix <- matrix()
    return(settings)
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
    if (length(ageKnots) > nrow(studyPopulation$outcomes)) {
      warning("There are more age knots than cases. Removing age from model")
      settings$ageOffset <- 0
      settings$ageDesignMatrix <- matrix()
      return(settings)
    }
    settings$ageOffset <- ageKnots[1]
    ageDesignMatrix <- splines::bs(ageKnots[1]:ageKnots[length(ageKnots)],
      knots = ageKnots[2:(length(ageKnots) - 1)],
      Boundary.knots = ageKnots[c(1, length(ageKnots))]
    )
    # Fixing first beta to zero, so dropping first column of design matrix:
    settings$ageDesignMatrix <- ageDesignMatrix[, 2:ncol(ageDesignMatrix)]
    splineCovariateRef <- tibble(
      covariateId = 100:(100 + length(ageKnots) - 1),
      covariateName = paste(
        "Age spline component",
        1:(length(ageKnots))
      ),
      originalEraId = 0,
      originalEraType = "",
      originalEraName = ""
    )
    settings$covariateRef <- bind_rows(settings$covariateRef, splineCovariateRef)
    age <- list(
      ageKnots = ageKnots,
      covariateIds = splineCovariateRef$covariateId,
      allowRegularization = ageCovariateSettings$allowRegularization,
      computeConfidenceIntervals = ageCovariateSettings$computeConfidenceIntervals
    )
    settings$metaData$age <- age
    return(settings)
  }
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
    splineCovariateRef <- tibble(
      covariateId = 200:(200 + length(seasonKnots) - 3),
      covariateName = paste(
        "Seasonality spline component",
        1:(length(seasonKnots) - 2)
      ),
      originalEraId = 0,
      originalEraType = "",
      originalEraName = ""
    )
    settings$covariateRef <- bind_rows(settings$covariateRef, splineCovariateRef)
    seasonality <- list(
      seasonKnots = seasonKnots,
      covariateIds = splineCovariateRef$covariateId,
      allowRegularization = seasonalityCovariateSettings$allowRegularization,
      computeConfidenceIntervals = seasonalityCovariateSettings$computeConfidenceIntervals
    )
    settings$metaData$seasonality <- seasonality
  }
  return(settings)
}

addCalendarTimeSettings <- function(settings,
                                    calendarTimeCovariateSettings,
                                    studyPopulation) {
  if (is.null(calendarTimeCovariateSettings)) {
    settings$calendarTimeOffset <- 0
    settings$calendarTimeDesignMatrix <- matrix()
    return(settings)
  } else {
    if (length(calendarTimeCovariateSettings$calendarTimeKnots) == 1) {
      observationPeriodCounts <- computeObservedPerMonth(studyPopulation) %>%
        arrange(.data$month) %>%
        mutate(cumCount = cumsum(.data$observationPeriodCount))

      total <- observationPeriodCounts %>%
        tail(1) %>%
        pull(.data$cumCount)

      cutoffs <- total * seq(0.01, 0.99, length.out = calendarTimeCovariateSettings$calendarTimeKnots)
      calendarTimeKnots <- rep(0, calendarTimeCovariateSettings$calendarTimeKnots)
      for (i in 1:calendarTimeCovariateSettings$calendarTimeKnots) {
        calendarTimeKnots[i] <- min(observationPeriodCounts$month[observationPeriodCounts$cumCount >= cutoffs[i]])
      }
    } else {
      knotDates <- calendarTimeCovariateSettings$calendarTimeKnots
      calendarTimeKnots <- convertDateToMonth(knotDates)
    }
    if (length(calendarTimeKnots) > nrow(studyPopulation$outcomes)) {
      warning("There are more calendar time knots than cases. Removing calendar time from model")
      settings$calendarTimeOffset <- 0
      settings$calendarTimeDesignMatrix <- matrix()
      return(settings)
    }
    settings$calendarTimeOffset <- calendarTimeKnots[1]
    calendarTimeDesignMatrix <- splines::bs(calendarTimeKnots[1]:calendarTimeKnots[length(calendarTimeKnots)],
      knots = calendarTimeKnots[2:(length(calendarTimeKnots) - 1)],
      Boundary.knots = calendarTimeKnots[c(1, length(calendarTimeKnots))]
    )
    # Fixing first beta to zero, so dropping first column of design matrix:
    settings$calendarTimeDesignMatrix <- calendarTimeDesignMatrix[, 2:ncol(calendarTimeDesignMatrix)]
    splineCovariateRef <- tibble(
      covariateId = 300:(300 + length(calendarTimeKnots) - 1),
      covariateName = paste(
        "Calendar time spline component",
        1:(length(calendarTimeKnots))
      ),
      originalEraId = 0,
      originalEraType = "",
      originalEraName = ""
    )
    settings$covariateRef <- bind_rows(settings$covariateRef, splineCovariateRef)
    calendarTime <- list(
      calendarTimeKnots = calendarTimeKnots,
      covariateIds = splineCovariateRef$covariateId,
      allowRegularization = calendarTimeCovariateSettings$allowRegularization,
      computeConfidenceIntervals = calendarTimeCovariateSettings$computeConfidenceIntervals
    )
    settings$metaData$calendarTime <- calendarTime
    return(settings)
  }
}

convertDateToMonth <- function(date) {
  return(as.numeric(format(date, "%Y")) * 12 + as.numeric(format(date, "%m")) - 1)
}

convertMonthToStartDate <- function(month) {
  year <- floor(month / 12)
  month <- floor(month %% 12) + 1
  return(as.Date(sprintf(
    "%s-%s-%s",
    year,
    month,
    1
  )))
}

convertMonthToEndDate <- function(month) {
  year <- floor(month / 12)
  month <- floor(month %% 12) + 1
  year <- if_else(month == 12, year + 1, year)
  month <- if_else(month == 12, 1, month + 1)
  return(as.Date(sprintf(
    "%s-%s-%s",
    year,
    month,
    1
  )) - 1)
}

computeObservedPerMonth <- function(studyPopulation) {
  observationPeriods <- studyPopulation$cases %>%
    mutate(endDate = .data$startDate + .data$endDay) %>%
    mutate(
      startMonth = convertDateToMonth(.data$startDate),
      endMonth = convertDateToMonth(.data$endDate) + 1
    ) %>%
    select(.data$startMonth, .data$endMonth)

  months <- full_join(
    observationPeriods %>%
      group_by(.data$startMonth) %>%
      summarise(startCount = n()) %>%
      rename(month = .data$startMonth),
    observationPeriods %>%
      group_by(.data$endMonth) %>%
      summarise(endCount = n()) %>%
      rename(month = .data$endMonth),
    by = "month"
  ) %>%
    mutate(
      startCount = ifelse(is.na(.data$startCount), 0, .data$startCount),
      endCount = ifelse(is.na(.data$endCount), 0, .data$endCount)
    )

  # Adding months with no starts and ends:
  months <- months %>%
    full_join(tibble(month = min(months$month):max(months$month)), by = "month") %>%
    mutate(
      startCount = if_else(is.na(.data$startCount), 0, .data$startCount),
      endCount = if_else(is.na(.data$endCount), 0, .data$endCount)
    )

  months <- months %>%
    arrange(.data$month) %>%
    mutate(
      cumStarts = cumsum(.data$startCount),
      cumEnds = cumsum(.data$endCount)
    ) %>%
    mutate(observationPeriodCount = .data$cumStarts - .data$cumEnds) %>%
    select(.data$month, .data$observationPeriodCount) %>%
    head(-1)

  return(months)
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
      transmute(
        astart = .data$ageInDays,
        aend = .data$ageInDays + .data$endDay + 1,
        aevent = .data$ageInDays + .data$outcomeDay + 1,
        present = .data$noninformativeEndCensor == 1
      )

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
        newCovariateRef <- tibble(
          covariateId = outputId,
          covariateName = covariateSettings$label,
          originalEraId = 0,
          originalEraType = "",
          originalEraName = "",
          isControlInterval = covariateSettings$isControlInterval
        )
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
            transmute(
              originalEraId = .data$eraId,
              originalEraType = .data$eraType,
              originalEraName = .data$eraName,
              covariateName = paste(covariateSettings$label,
                .data$eraName,
                sep = ": "
              ),
              isControlInterval = FALSE
            )

          newCovariateRef <- tibble(
            covariateId = outputIds,
            originalEraId = covariateSettings$eraIds
          ) %>%
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
          byrow = TRUE
        )
        newCovariateRef <- tibble(
          covariateId = outputIds,
          covariateName = varNames,
          originaEraId = 0,
          originalEraType = "",
          originalEraName = "",
          isControlInterval = FALSE
        )
        settings$covariateRef <- bind_rows(settings$covariateRef, newCovariateRef)
      } else {
        outputIds <- outputId:(outputId + (length(covariateSettings$splitPoint) + 1) * length(covariateSettings$eraIds) - 1)
        outputId <- max(outputIds) + 1
        covariateSettings$outputIds <- matrix(outputIds,
          ncol = length(covariateSettings$splitPoints) + 1,
          byrow = TRUE
        )
        if (any(covariateSettings$eraIds %in% eraRef$eraId)) {
          originalEraId <- rep(covariateSettings$eraIds,
            each = length(covariateSettings$splitPoints) + 1
          )
          originalEraType <- eraRef$eraType[match(
            originalEraId,
            eraRef$eraId
          )]
          originalEraName <- eraRef$eraName[match(
            originalEraId,
            eraRef$eraId
          )]
          originalEraName[originalEraName == ""] <- originalEraId[originalEraName == ""]
          varNames <- paste(covariateSettings$label, ": ", originalEraName, sep = "")
          varNames <- paste(varNames,
            ", day ",
            startDays,
            "-",
            c(endDays[1:length(endDays) - 1], ""),
            sep = ""
          )

          newCovariateRef <- tibble(
            covariateId = outputIds,
            covariateName = varNames,
            originalEraId = originalEraId,
            originalEraType = originalEraType,
            originalEraName = originalEraName,
            isControlInterval = FALSE
          )
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
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertNumeric(x, min.len = 1, add = errorMessages)
  checkmate::assertNumeric(knots, min.len = 1, add = errorMessages)
  checkmate::assertInt(ord, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  nk <- length(knots)
  if (ord < 2) {
    stop("order too low")
  }
  if (nk < ord) {
    stop("too few knots")
  }
  knots <- sort(knots)
  k1 <- knots[1]
  if (min(x) < k1 || max(x) > knots[nk]) {
    stop("x out of range")
  }
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
