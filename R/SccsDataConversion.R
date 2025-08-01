# Copyright 2025 Observational Health Data Sciences and Informatics
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
#' @param createSccsIntervalDataArgs An object of type `CreateSccsIntervalDataArgs` as created by the
#'                                   `createCreateSccsIntervalDataArgs` function.
#'
#' @return
#' An object of type `SccsIntervalData`.
#'
#' @export
createSccsIntervalData <- function(studyPopulation,
                                   sccsData,
                                   createSccsIntervalDataArgs) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(studyPopulation, min.len = 1, add = errorMessages)
  checkmate::assertClass(sccsData, "SccsData", add = errorMessages)
  checkmate::assertR6(createSccsIntervalDataArgs, "CreateSccsIntervalDataArgs", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  start <- Sys.time()

  timeCovariateCases <- numeric(0)
  if (!is.null(createSccsIntervalDataArgs$ageCovariateSettings) ||
      !is.null(createSccsIntervalDataArgs$seasonalityCovariateSettings) ||
      !is.null(createSccsIntervalDataArgs$calendarTimeCovariateSettings)) {
    if (nrow(studyPopulation$cases) > createSccsIntervalDataArgs$minCasesForTimeCovariates) {
      # Select uniformly. Don't want to use random sampleing because not reproducible:
      idx <- uniformSelect(createSccsIntervalDataArgs$minCasesForTimeCovariates, nrow(studyPopulation$cases))
      timeCovariateCases <- studyPopulation$cases$caseId[idx]
    }
  }

  settings <- list()
  settings$metaData <- list()
  settings$covariateRef <- tibble()
  settings <- addEventDependentObservationSettings(
    settings,
    createSccsIntervalDataArgs$eventDependentObservation,
    studyPopulation,
    createSccsIntervalDataArgs$endOfObservationEraLength
  )
  if (createSccsIntervalDataArgs$eventDependentObservation && settings$metaData$censorModel$model %in% c(1, 3) && !is.null(createSccsIntervalDataArgs$ageCovariateSettings)) {
    warning("Optimal censoring model adjusts for age, so removing age as separate covariate.")
    createSccsIntervalDataArgs$ageCovariateSettings <- NULL
  }
  settings <- addAgeSettings(settings, createSccsIntervalDataArgs$ageCovariateSettings, studyPopulation)
  settings <- addSeasonalitySettings(settings, createSccsIntervalDataArgs$seasonalityCovariateSettings, sccsData)
  settings <- addCalendarTimeSettings(settings, createSccsIntervalDataArgs$calendarTimeCovariateSettings, studyPopulation, sccsData)
  settings <- addEraCovariateSettings(settings, createSccsIntervalDataArgs$eraCovariateSettings, sccsData)
  settings$metaData$covariateSettingsList <- settings$covariateSettingsList
  metaData <- append(studyPopulation$metaData, settings$metaData)
  metaData$design <- "SCCS"

  if (nrow(studyPopulation$outcomes) == 0) {
    sccsIntervalData <- createEmptySccsIntervalData()
    metaData$error <- "Error: No cases left"
    attr(sccsIntervalData, "metaData") <- metaData
    class(sccsIntervalData) <- "SccsIntervalData"
    attr(class(sccsIntervalData), "package") <- "SelfControlledCaseSeries"
    return(sccsIntervalData)
  }

  message("Converting person data to SCCS intervals. This might take a while.")
  # Ensure all sorted bv caseId:
  cases <- studyPopulation$cases[order(studyPopulation$cases$caseId), ]
  outcomes <- studyPopulation$outcomes[order(studyPopulation$outcomes$caseId), ]
  data <- Andromeda::andromeda()

  convertToSccs(
    cases = cases,
    outcomes = outcomes,
    eras = sccsData$eras,
    includeAge = settings$includeAge,
    ageOffset = settings$ageOffset,
    ageDesignMatrix = settings$ageDesignMatrix,
    includeSeason = settings$includeSeason,
    seasonDesignMatrix = settings$seasonDesignMatrix,
    includeCalendarTime = settings$includeCalendarTime,
    calendarTimeOffset = settings$calendarTimeOffset,
    calendarTimeDesignMatrix = settings$calendarTimeDesignMatrix,
    timeCovariateCases = timeCovariateCases,
    covariateSettingsList = settings$covariateSettingsList,
    endOfObservationEraLength = settings$endOfObservationEraLength,
    endOfObservationCovariateId = settings$endOfObservationCovariateId,
    eventDependentObservation = createSccsIntervalDataArgs$eventDependentObservation,
    censorModel = settings$censorModel,
    scri = FALSE,
    controlIntervalId = 0,
    resultAndromeda = data
  )

  if (is.null(data$outcomes) || is.null(data$covariates)) {
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
  metaData$attrition <- bind_rows(
    metaData$attrition,
    countOutcomesIntervalData(data, sccsData, metaData$outcomeId)
  )
  attr(data, "metaData") <- metaData
  class(data) <- "SccsIntervalData"
  attr(class(data), "package") <- "SelfControlledCaseSeries"

  delta <- Sys.time() - start
  message(paste("Generating SCCS interval data took", signif(delta, 3), attr(delta, "units")))
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
      originalEraType = "",
      covariateAnalysisId = 1
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
    settings$includeAge <- FALSE
    return(settings)
  } else {
    if (length(ageCovariateSettings$ageKnots) == 1) {
      ageKnots <- studyPopulation$outcomes |>
        inner_join(studyPopulation$cases, by = join_by("caseId")) |>
        transmute(outcomeAge = .data$outcomeDay + .data$ageAtObsStart) |>
        pull() |>
        quantile(seq(0.01, 0.99, length.out = ageCovariateSettings$ageKnots))
      ageKnots <- ageKnots[!duplicated(ageKnots)]
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
    settings$ageDesignMatrix <- splines::bs(ageKnots[1]:ageKnots[length(ageKnots)],
                                            knots = ageKnots[2:(length(ageKnots) - 1)],
                                            Boundary.knots = ageKnots[c(1, length(ageKnots))],
                                            degree = 2)
    settings$includeAge <- TRUE
    splineCovariateRef <- tibble(
      covariateId = 100:(100 + length(ageKnots) - 1),
      covariateName = paste(
        "Age spline component",
        1:(length(ageKnots))
      ),
      originalEraId = 0,
      originalEraType = "",
      originalEraName = "",
      isControlInterval = FALSE,
      preExposure = FALSE
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
    settings$includeSeason <- FALSE
  } else {
    if (length(seasonalityCovariateSettings$seasonKnots) == 1) {
      # Single number, should interpret as number of knots. Spread out knots evenly:
      seasonKnots <- 0.5 + seq(0, 12, length.out = seasonalityCovariateSettings$seasonKnots)
    } else {
      seasonKnots <- seasonalityCovariateSettings$seasonKnots
    }
    settings$seasonDesignMatrix <- cyclicSplineDesign(1:12, knots = seasonKnots)
    settings$includeSeason <- TRUE
    splineCovariateRef <- tibble(
      covariateId = 200:(200 + length(seasonKnots) - 2),
      covariateName = paste(
        "Seasonality spline component",
        1:(length(seasonKnots) - 1)
      ),
      originalEraId = 0,
      originalEraType = "",
      originalEraName = "",
      isControlInterval = FALSE,
      preExposure = FALSE
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
                                    studyPopulation,
                                    sccsData) {
  if (is.null(calendarTimeCovariateSettings)) {
    settings$calendarTimeOffset <- 0
    settings$calendarTimeDesignMatrix <- matrix()
    settings$includeCalendarTime <- FALSE
    return(settings)
  } else if ((length(calendarTimeCovariateSettings$calendarTimeKnots) == 1 &&
              calendarTimeCovariateSettings$calendarTimeKnots > nrow(studyPopulation$outcomes)) ||
             (length(calendarTimeCovariateSettings$calendarTimeKnots) > nrow(studyPopulation$outcomes))) {
    warning("There are more calendar time knots than cases. Removing calendar time from model")
    settings$calendarTimeOffset <- 0
    settings$calendarTimeDesignMatrix <- matrix()
    settings$includeCalendarTime <- FALSE
    return(settings)
  } else {
    if (length(calendarTimeCovariateSettings$calendarTimeKnots) == 1) {
      observationPeriodCounts <- computeObservedPerMonth(studyPopulation) |>
        arrange(.data$month)
      total <- sum(observationPeriodCounts$observationPeriodCount)
      studyPeriods <- attr(sccsData, "metaData")$studyPeriods
      if (is.null(studyPeriods)) {
        studyPeriods <- tibble(studyStartMonth = -Inf, studyEndMonth = Inf)
      } else {
        studyPeriods <- studyPeriods |>
          transmute(studyStartMonth = convertDateToMonth(.data$studyStartDate),
                    studyEndMonth =  convertDateToMonth(.data$studyEndDate)) |>
          arrange(.data$studyStartMonth)
      }
      calendarTimeKnotsInPeriods <- list()
      for (i in seq_len(nrow(studyPeriods))) {
        countsInPeriod <- observationPeriodCounts |>
          filter(.data$month >= studyPeriods$studyStartMonth[i],
                 .data$month <= studyPeriods$studyEndMonth[i]) |>
          mutate(cumCount = cumsum(.data$observationPeriodCount))
        totalInPeriod <- sum(countsInPeriod$observationPeriodCount)
        if (totalInPeriod > 0) {
          # Evenly divide free knots (knots not at boundary) over periods:
          freeKnotsInPeriod <- round((calendarTimeCovariateSettings$calendarTimeKnots - 2) * (totalInPeriod / total))
          knotsInPeriod <- freeKnotsInPeriod + 2
          cutoffs <- totalInPeriod * seq(0.005, 0.995, length.out = knotsInPeriod)
          calendarTimeKnots <- rep(0, knotsInPeriod)
          for (j in seq_len(knotsInPeriod)) {
            calendarTimeKnots[j] <- min(countsInPeriod$month[countsInPeriod$cumCount >= cutoffs[j]])
          }
          calendarTimeKnots <- calendarTimeKnots[!duplicated(calendarTimeKnots)]
          calendarTimeKnotsInPeriods[[length(calendarTimeKnotsInPeriods) + 1]] <- calendarTimeKnots
        }
      }
    } else {
      knotDates <- calendarTimeCovariateSettings$calendarTimeKnots
      calendarTimeKnotsInPeriods <- list(sort(convertDateToMonth(knotDates)))
    }
    if (length(calendarTimeKnotsInPeriods) == 0) {
      warning("All outcomes fall outside of all study periods. Unable to model calendar time")
      settings$calendarTimeOffset <- 0
      settings$calendarTimeDesignMatrix <- matrix()
      settings$includeCalendarTime <- FALSE
      return(settings)
    }
    firstKnot <- calendarTimeKnotsInPeriods[[1]][1]
    lastKnot <- last(calendarTimeKnotsInPeriods[[length(calendarTimeKnotsInPeriods)]])
    settings$calendarTimeOffset <- firstKnot
    settings$calendarTimeDesignMatrix <- createMultiSegmentDesignMatrix(
      x = seq(firstKnot, lastKnot),
      knotsPerSegment = calendarTimeKnotsInPeriods
    )
    settings$includeCalendarTime <- TRUE
    splineCovariateRef <- tibble(
      covariateId = 300 + seq_len(ncol(settings$calendarTimeDesignMatrix)) - 1,
      covariateName = paste(
        "Calendar time spline component",
        seq_len(ncol(settings$calendarTimeDesignMatrix))
      ),
      originalEraId = 0,
      originalEraType = "",
      originalEraName = "",
      isControlInterval = FALSE,
      preExposure = FALSE
    )
    settings$covariateRef <- bind_rows(settings$covariateRef, splineCovariateRef)
    calendarTime <- list(
      calendarTimeKnotsInPeriods = calendarTimeKnotsInPeriods,
      covariateIds = splineCovariateRef$covariateId,
      allowRegularization = calendarTimeCovariateSettings$allowRegularization,
      computeConfidenceIntervals = calendarTimeCovariateSettings$computeConfidenceIntervals
    )
    settings$metaData$calendarTime <- calendarTime
    return(settings)
  }
}

createMultiSegmentDesignMatrix <- function(x, knotsPerSegment) {
  for (i in seq_along(knotsPerSegment)) {
    knots <- knotsPerSegment[[i]]
    boundaryKnots <- knots[c(1, length(knots))]
    innerKnots <- setdiff(knots, boundaryKnots)
    designMatrix <- splines::bs(x = x[x >= boundaryKnots[1] & x <= boundaryKnots[2]],
                                knots = innerKnots,
                                Boundary.knots = boundaryKnots,
                                degree = if(length(innerKnots) == 0) 1 else 2)
    if (i == 1) {
      rowsToFillWithNew <- sum(x < boundaryKnots[1])
      if (rowsToFillWithNew == 0) {
        fullDesignMatrix <- designMatrix
      } else {
        filler <- designMatrix[rep(1, rowsToFillWithNew), ]
        fullDesignMatrix <- rbind(filler,
                                  designMatrix)
      }
    } else {
      priorMatrix <- cbind(fullDesignMatrix,
                           matrix(0, nrow = nrow(fullDesignMatrix), ncol = ncol(designMatrix)))
      newMatrix <- cbind(matrix(0, nrow = nrow(designMatrix), ncol = ncol(fullDesignMatrix)),
                         designMatrix)
      xToFill <- x[x > lastBoundary & x < boundaryKnots[1]]
      rowsToFillWithPrior <- sum(xToFill - lastBoundary < boundaryKnots[1] - xToFill)
      rowsToFillWithNew <- length(xToFill) - rowsToFillWithPrior
      filler <- rbind(priorMatrix[rep(nrow(priorMatrix), rowsToFillWithPrior), ],
                      newMatrix[rep(1, rowsToFillWithNew), ])
      fullDesignMatrix <- rbind(priorMatrix,
                                filler,
                                newMatrix)
    }
    lastBoundary <- boundaryKnots[2]
  }
  rowsToFillWithPrior <- sum(x > lastBoundary)
  if (rowsToFillWithPrior != 0) {
    filler <- fullDesignMatrix[rep(nrow(fullDesignMatrix), rowsToFillWithPrior), ]
    fullDesignMatrix <- rbind(fullDesignMatrix, filler)
  }
  return(fullDesignMatrix)
}

convertDateToMonth <- function(date) {
  return(as.numeric(format(date, "%Y")) * 12 + as.numeric(format(date, "%m")) - 1)
}

computeMonthFraction <- function(date, post = FALSE) {
  # For simplicity assume all months have 31 days. Could use lubridate package to be more precise.
  # Assume day itself is included
  if (post) {
    return((31-as.numeric(format(date, "%d")) + 1)/31)
  } else {
    return(as.numeric(format(date, "%d"))/31)
  }
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
  observationPeriods <- studyPopulation$cases |>
    mutate(
      startDate = .data$observationPeriodStartDate + .data$startDay,
      endDate = .data$observationPeriodStartDate + .data$endDay
    ) |>
    mutate(
      startMonth = convertDateToMonth(.data$startDate),
      endMonth = convertDateToMonth(.data$endDate) + 1
    ) |>
    select("startMonth", "endMonth")

  months <- full_join(
    observationPeriods |>
      group_by(.data$startMonth) |>
      summarise(startCount = n()) |>
      rename(month = "startMonth"),
    observationPeriods |>
      group_by(.data$endMonth) |>
      summarise(endCount = n()) |>
      rename(month = "endMonth"),
    by = "month"
  ) |>
    mutate(
      startCount = ifelse(is.na(.data$startCount), 0, .data$startCount),
      endCount = ifelse(is.na(.data$endCount), 0, .data$endCount)
    )

  # Adding months with no starts and ends:
  if (nrow(months) > 0) {
    months <- months |>
      full_join(tibble(month = min(months$month):max(months$month)), by = "month") |>
      mutate(
        startCount = if_else(is.na(.data$startCount), 0, .data$startCount),
        endCount = if_else(is.na(.data$endCount), 0, .data$endCount)
      )
  }
  months <- months |>
    arrange(.data$month) |>
    mutate(
      cumStarts = cumsum(.data$startCount),
      cumEnds = cumsum(.data$endCount)
    ) |>
    mutate(observationPeriodCount = .data$cumStarts - .data$cumEnds) |>
    select("month", "observationPeriodCount") |>
    head(-1)

  return(months)
}

addEventDependentObservationSettings <- function(settings,
                                                 eventDependentObservation,
                                                 studyPopulation,
                                                 endOfObservationEraLength) {
  if (!eventDependentObservation) {
    settings$censorModel <- list(model = 0, p = c(0))
  } else {
    data <- studyPopulation$outcomes |>
      group_by(.data$caseId) |>
      summarise(outcomeDay = min(.data$outcomeDay)) |>
      inner_join(studyPopulation$cases, by = join_by("caseId")) |>
      transmute(
        astart = .data$ageAtObsStart + .data$startDay,
        aend = .data$ageAtObsStart + .data$endDay + 1,
        aevent = .data$ageAtObsStart + .data$outcomeDay + 1,
        present = .data$noninformativeEndCensor == 1
      )
    settings$censorModel <- fitModelsAndPickBest(data)
    settings$metaData$censorModel <- settings$censorModel
  }
  settings$endOfObservationEraLength <- endOfObservationEraLength
  settings$endOfObservationCovariateId <- 99
  settings$endOfObservationAnalysisId <- 99
  newCovariateRef <- tibble(
    covariateId = settings$endOfObservationCovariateId,
    covariateName = "End of observation period",
    covariateAnalysisId = NA,
    originalEraId = 0,
    originalEraType = "",
    originalEraName = "",
    isControlInterval = FALSE,
    preExposure = FALSE
  )
  settings$covariateRef <- bind_rows(settings$covariateRef, newCovariateRef)
  settings$metaData$endOfObservationEra <- list(
    endOfObservationEraLength = settings$endOfObservationEraLength,
    endOfObservationCovariateId = settings$endOfObservationCovariateId)
  return(settings)
}

addEraCovariateSettings <- function(settings, eraCovariateSettings, sccsData) {
  if (is.list(eraCovariateSettings) && !is(eraCovariateSettings, "EraCovariateSettings")) {
    eraCovariateSettingsList <- eraCovariateSettings
  } else {
    eraCovariateSettingsList <- list(eraCovariateSettings)
  }
  eraRef <- sccsData$eraRef |>
    collect()

  # Iterate over different covariate settings. Assign unique IDs, and store in covariateRef:
  outputId <- 1000
  for (i in 1:length(eraCovariateSettingsList)) {
    covariateSettings <- eraCovariateSettingsList[[i]]$toList()
    isControlInterval <- is(eraCovariateSettingsList[[i]], "ControlIntervalSettings")
    covariateSettings$covariateAnalysisId <- 1

    if (is.null(covariateSettings$label)) {
      covariateSettings$label <- "Covariate"
    }
    if (is.null(covariateSettings$includeEraIds) || length(covariateSettings$includeEraIds) == 0) {
      covariateSettings$eraIds <- eraRef |>
        filter(.data$eraType != "hoi") |>
        select("eraId") |>
        pull()
    } else {
      covariateSettings$eraIds <- covariateSettings$includeEraIds
    }
    if (!is.null(covariateSettings$excludeEraIds) && length(covariateSettings$excludeEraIds) != 0) {
      covariateSettings$eraIds <- covariateSettings$eraIds[!covariateSettings$eraIds %in% covariateSettings$excludeEraIds]
    }

    if (isControlInterval || !covariateSettings$stratifyById) {
      # stratifyById == FALSE
      covariateSettings$outputIds <- as.matrix(outputId)
      if (isControlInterval) {
        covariateSettings$stratifyById <- FALSE
        covariateSettings$allowRegularization <- FALSE
        covariateSettings$exposureOfInterest <- FALSE
        covariateSettings$profileLikelihood <- FALSE
      } else {
        if (length(covariateSettings$eraIds) == 1) {
          originalEraId <- covariateSettings$eraIds
        } else {
          originalEraId <- 0
        }
        exposureId <- covariateSettings$eraIds
        newCovariateRef <- tibble(
          covariateId = outputId,
          covariateName = covariateSettings$label,
          covariateAnalysisId = i,
          originalEraId = originalEraId,
          originalEraType = "",
          originalEraName = "",
          isControlInterval = isControlInterval,
          preExposure = covariateSettings$preExposure
        )
        settings$covariateRef <- bind_rows(settings$covariateRef, newCovariateRef)
      }
      outputId <- outputId + 1
    } else {
      # stratifyById == TRUE
      # Create a unique output ID for every covariate ID:
      outputIds <- outputId:(outputId + length(covariateSettings$eraIds) - 1)
      covariateSettings$outputIds <- matrix(outputIds, ncol = 1)
      outputId <- outputId + length(outputIds)
      varNames <- eraRef[eraRef$eraId %in% covariateSettings$eraIds, ]
      if (nrow(varNames) == 0) {
        warning(paste0("Could not find era with ID ", covariateSettings$eraIds, " in data"))
      } else {
        varNames <- varNames |>
          transmute(
            originalEraId = .data$eraId,
            originalEraType = .data$eraType,
            originalEraName = .data$eraName,
            covariateName = paste(covariateSettings$label,
                                  .data$eraName,
                                  sep = ": "
            ),
            isControlInterval = FALSE,
            preExposure = covariateSettings$preExposure
          )

        newCovariateRef <- tibble(
          covariateId = outputIds,
          covariateAnalysisId = i,
          originalEraId = covariateSettings$eraIds
        ) |>
          left_join(varNames, by = join_by("originalEraId"))
        settings$covariateRef <- bind_rows(settings$covariateRef, newCovariateRef)
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
#' @param ord     Order of the spline function. `ord = 3` implies quadratic.
#'
#' @export
cyclicSplineDesign <- function(x, knots, ord = 3) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertNumeric(x, min.len = 1, add = errorMessages)
  checkmate::assertNumeric(knots, min.len = 1, add = errorMessages)
  checkmate::assertInt(ord, lower = 2, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  # Borrowed from mgcv::cSplineDes
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

countOutcomesIntervalData <- function(data, sccsData, outcomeId) {
  counts <- data$outcomes |>
    inner_join(select(sccsData$cases, stratumId = "caseId", "personId"), by = join_by("stratumId"), copy = TRUE) |>
    summarize(
      outcomeSubjects = n_distinct(.data$personId),
      outcomeEvents = sum(.data$y, na.rm = TRUE),
      outcomeObsPeriods = n_distinct(.data$stratumId),
      observedDays = sum(.data$time, na.rm = TRUE)
    ) |>
    collect() |>
    mutate(
      outcomeId = !!outcomeId,
      description = "Having at least one covariate",
      outcomeEvents = ifelse(is.na(.data$outcomeEvents), 0, .data$outcomeEvents),
      observedDays = ifelse(is.na(.data$observedDays), 0, .data$observedDays)
    )
  return(counts)
}

uniformSelect <- function(size, totalSize) {
  if (size == 1) {
    return(floor((totalSize + 1) / 2))
  } else {
    return(floor(1 + (0:(size - 1)) * (totalSize - 1) / (size - 1)))
  }
}
