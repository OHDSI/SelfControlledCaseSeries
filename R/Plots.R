# Copyright 2023 Observational Health Data Sciences and Informatics
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

#' Plot the age ranges spanned by each observation period.
#'
#' @template StudyPopulation
#' @param maxPersons  The maximum number of persons to plot. If there are more than this number of persons
#'                    a random sample will be taken to avoid visual clutter.
#' @param fileName    Name of the file where the plot should be saved, for example 'plot.png'.
#'                    See the function [ggplot2::ggsave()] for supported file formats.
#' @param title       Optional: the main title for the plot
#'
#' @details
#' Plots a line per patient from their age at observation start to their age at observation end.
#'
#' @return
#' A ggplot object. Use the [ggplot2::ggsave()] function to save to file in a different
#' format.
#'
#' @export
plotAgeSpans <- function(studyPopulation,
                         maxPersons = 10000,
                         title = NULL,
                         fileName = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(studyPopulation, min.len = 1, add = errorMessages)
  checkmate::assertInt(maxPersons, lower = 1, add = errorMessages)
  checkmate::assertCharacter(title, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(fileName, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  cases <- studyPopulation$cases %>%
    transmute(startAge = .data$ageAtObsStart + .data$startDay,
              endAge = .data$ageAtObsStart + .data$endDay) %>%
    arrange(.data$startAge, .data$endAge) %>%
    mutate(rank = row_number())

  ageLabels <- floor(min(cases$startAge) / 365.25):ceiling(max(cases$endAge) / 365.25)
  if (length(ageLabels) > 10) {
    ageLabels <- 10 * (floor(min(cases$startAge) / 3652.5):floor(max(cases$endAge) / 3652.5))
  }
  ageBreaks <- ageLabels * 365.25
  if (nrow(cases) > maxPersons) {
    warning("There are ", nrow(cases), " cases. Random sampling ", maxPersons, " cases.")
    cases <- cases[sample.int(nrow(cases), maxPersons), ]
  }
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(cases, ggplot2::aes(x = .data$startAge, xmin = .data$startAge, xmax = .data$endAge, y = rank)) +
    ggplot2::geom_vline(xintercept = ageBreaks, colour = "#AAAAAA", lty = 1, linewidth = 0.2) +
    ggplot2::geom_errorbarh(color = rgb(0, 0, 0.8), alpha = 0.8) +
    ggplot2::scale_x_continuous("Age (years)", breaks = ageBreaks, labels = ageLabels) +
    ggplot2::scale_y_continuous("Case rank") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
      panel.grid.major = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      strip.text.x = theme,
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_blank(),
      legend.position = "top"
    )
  if (!is.null(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  # fileName <- "S:/temp/plot.png"
  if (!is.null(fileName)) {
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  }
  return(plot)
}

computeTimeToObsEnd <- function(studyPopulation) {
  outcomes <- studyPopulation$outcomes %>%
    group_by(.data$caseId) %>%
    summarise(outcomeDay = min(.data$outcomeDay), .groups = "drop_last") %>%
    inner_join(studyPopulation$cases, by = join_by("caseId")) %>%
    transmute(
      daysFromEvent = .data$endDay - .data$outcomeDay,
      censoring = case_when(
        .data$noninformativeEndCensor == 1 ~ "Uncensored",
        TRUE ~ "Censored"
      )
    )
  return(outcomes)
}

#' Plot time from event to observation end for censored and uncensored time.
#'
#' @template StudyPopulation
#' @param fileName          Name of the file where the plot should be saved, for example 'plot.png'.
#'                          See the function [ggplot2::ggsave()] for supported file formats.
#' @param title             Optional: the main title for the plot
#'
#' @details
#' This plot shows whether there is a difference in time between (first) event and the observation period end for periods that are '
#' censored' and those that are 'uncensored'. By 'censored' we mean periods that end before we would normally expect. Here, we define
#' periods to be uncensored if they end at either the study end date (if specified), database end date (i.e. the date after which no
#' data is captured in the database), or maximum age (if specified). All other periods are assumed to be censored.
#'
#' As proposed by Farrington et al., by comparing the two plots, we can gain some insight into whether the censoring is dependent
#' on the occurrence of the event.
#'
#' @references
#' Farrington P, Whitaker H, Ghebremichael Weldeselassie Y (2018), Self-controlled case series studies: A modelling guide with R, Taylor &
#' Francis
#'
#' @return
#' A ggplot object. Use the [ggplot2::ggsave()] function to save to file in a different
#' format.
#'
#' @export
plotEventObservationDependence <- function(studyPopulation,
                                           title = NULL,
                                           fileName = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(studyPopulation, min.len = 1, add = errorMessages)
  checkmate::assertCharacter(title, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(fileName, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  outcomes <- computeTimeToObsEnd(studyPopulation)

  ageLabels <- 0:ceiling(max(outcomes$daysFromEvent) / 365.25)

  ageBreaks <- ageLabels * 365.25

  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(outcomes, ggplot2::aes(x = .data$daysFromEvent)) +
    ggplot2::geom_vline(xintercept = ageBreaks, colour = "#AAAAAA", lty = 1, linewidth = 0.2) +
    ggplot2::geom_histogram(binwidth = 30.5, fill = rgb(0, 0, 0.8), alpha = 0.8) +
    ggplot2::scale_x_continuous("Years from event", breaks = ageBreaks, labels = ageLabels) +
    ggplot2::scale_y_continuous("Frequency") +
    ggplot2::facet_grid(censoring ~ ., scales = "free_y") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
      panel.grid.major = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      strip.text.y = theme,
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_blank(),
      legend.position = "top"
    )
  if (!is.null(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  if (!is.null(fileName)) {
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  }
  return(plot)
}

computeTimeToEvent <- function(studyPopulation,
                               sccsData,
                               exposureEraId) {
  cases <- studyPopulation$cases %>%
    select("caseId", "startDay", "endDay")

  exposures <- sccsData$eras %>%
    filter(.data$eraId == exposureEraId & .data$eraType == "rx") %>%
    group_by(.data$caseId) %>%
    inner_join(cases,
               by = join_by("caseId", "eraStartDay" >= "startDay", "eraStartDay" < "endDay"),
               copy = TRUE) %>%
    collect()

  if (nrow(exposures) == 0) {
    warning("No exposures found with era ID ", exposureEraId)
    tibble(
      number = 1,
      start = 1.0,
      end = 1.0,
      observed = 1,
      eventsExposed = 1.0,
      eventsUnexposed = 1.0
    ) %>%
      filter(.data$number == -1) %>%
      return()
  }
  firstExposures <- exposures %>%
    group_by(.data$caseId, .data$startDay, .data$endDay) %>%
    summarise(
      eraStartDay = min(.data$eraStartDay, na.rm = TRUE),
      eraEndDay = min(.data$eraEndDay, na.rm = TRUE),
      .groups = "drop_last"
    )

  outcomes <- studyPopulation$outcomes %>%
    inner_join(firstExposures, by = join_by("caseId")) %>%
    mutate(delta = .data$outcomeDay - .data$eraStartDay) %>%
    select("caseId", "outcomeDay", "delta")

  exposedoutcomes <- exposures %>%
    inner_join(outcomes, by = join_by("caseId"), relationship = "many-to-many") %>%
    filter(
      .data$outcomeDay >= .data$eraStartDay,
      .data$outcomeDay <= .data$eraEndDay
    ) %>%
    select("caseId", "delta") %>%
    mutate(exposed = 1)

  outcomes <- outcomes %>%
    left_join(exposedoutcomes, by = join_by("caseId", "delta"), relationship = "many-to-many") %>%
    mutate(exposed = coalesce(.data$exposed, 0))

  weeks <- dplyr::tibble(number = -26:25) %>%
    mutate(
      start = .data$number * 7,
      end = .data$number * 7 + 7
    )

  events <- weeks %>%
    cross_join(select(outcomes, "delta", "exposed")) %>%
    filter(.data$delta >= .data$start, .data$delta < .data$end) %>%
    group_by(.data$number, .data$start, .data$end) %>%
    summarise(
      eventsExposed = sum(.data$exposed),
      eventsUnexposed = n() - sum(.data$exposed),
      .groups = "drop"
    )

  observed <- weeks %>%
    cross_join(transmute(firstExposures, startDelta = .data$startDay -.data$eraStartDay , endDelta = .data$endDay - .data$eraStartDay)) %>%
    filter(.data$endDelta >= .data$start, .data$startDelta < .data$end) %>%
    group_by(.data$number, .data$start, .data$end) %>%
    summarise(
      observed = n(),
      .groups = "drop"
    )

  result <- observed %>%
    left_join(events, by = join_by("number", "start", "end")) %>%
    mutate(
      eventsExposed = if_else(is.na(.data$eventsExposed), 0, .data$eventsExposed),
      eventsUnexposed = if_else(is.na(.data$eventsUnexposed), 0, .data$eventsUnexposed)
    )

  return(result)
}

#' Plot information centered around the start of exposure
#'
#' @param exposureEraId       The exposure to create the era data for. If not specified it is
#'                            assumed to be the one exposure for which the data was loaded from
#'                            the database.
#' @template StudyPopulation
#' @template SccsData
#' @param highlightExposedEvents Highlight events that occurred during the exposure era using a different color?
#' @param fileName            Name of the file where the plot should be saved, for example 'plot.png'.
#'                            See the function [ggplot2::ggsave()] for supported file formats.
#' @param title               Optional: the main title for the plot
#'
#' @details
#' This plot shows the number of events and the number of subjects under observation in week-sized intervals relative to the start
#' of the first exposure.
#'
#' @return
#' A ggplot object. Use the [ggplot2::ggsave()] function to save to file in a different
#' format.
#'
#' @export
plotExposureCentered <- function(studyPopulation,
                                 sccsData,
                                 exposureEraId = NULL,
                                 highlightExposedEvents = TRUE,
                                 title = NULL,
                                 fileName = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(studyPopulation, min.len = 1, add = errorMessages)
  checkmate::assertClass(sccsData, "SccsData", add = errorMessages)
  checkmate::assertInt(exposureEraId, null.ok = TRUE, add = errorMessages)
  checkmate::assertLogical(highlightExposedEvents, len = 1, add = errorMessages)
  checkmate::assertCharacter(title, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(fileName, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (is.null(exposureEraId)) {
    exposureEraId <- attr(sccsData, "metaData")$exposureIds
    if (length(exposureEraId) != 1) {
      stop("No exposure ID specified, but multiple exposures found")
    }
  }

  data <- computeTimeToEvent(studyPopulation, sccsData, exposureEraId)
  if (nrow(data) == 0) {
    return(NULL)
  }

  if (highlightExposedEvents) {
    events <- data %>%
      transmute(.data$start,
        .data$end,
        type = "Events",
        count1 = .data$eventsUnexposed,
        count2 = .data$eventsExposed
      )
  } else {
    events <- data %>%
      transmute(.data$start,
        .data$end,
        type = "Events",
        count1 = .data$eventsUnexposed + .data$eventsExposed,
        count2 = NA
      )
  }
  observed <- data %>%
    transmute(.data$start,
      .data$end,
      type = "Subjects under observation",
      count1 = .data$observed,
      count2 = NA
    )
  data <- bind_rows(events, observed)

  breaks <- seq(-150, 150, 30)
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data$start, xmin = .data$start, xmax = .data$end, ymax = .data$count1, ymin = 0)) +
    ggplot2::geom_rect(fill = rgb(0, 0, 0.8), alpha = 0.8) +
    ggplot2::geom_rect(ggplot2::aes(ymax = .data$count1 + .data$count2, ymin = .data$count1), fill = rgb(0.8, 0, 0), alpha = 0.8) +
    ggplot2::geom_vline(xintercept = 0, colour = "#000000", lty = 1, linewidth = 1) +
    ggplot2::scale_x_continuous("Days since first exposure start", breaks = breaks, labels = breaks) +
    ggplot2::scale_y_continuous("Count") +
    ggplot2::facet_grid(type ~ ., scales = "free_y") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = "#AAAAAA"),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      strip.text.y = theme,
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_blank(),
      legend.position = "top"
    )
  if (!is.null(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  if (!is.null(fileName)) {
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  }
  return(plot)
}

#' Plot the count of events over calendar time.
#'
#' @template StudyPopulation
#' @param sccsModel         Optional: A fitted SCCS model as created using [fitSccsModel()]. If the
#'                          model contains splines for seasonality and or calendar time a panel will
#'                          be added with outcome counts adjusted for these splines.
#' @param fileName          Name of the file where the plot should be saved, for example 'plot.png'.
#'                          See the function [ggplot2::ggsave()] for supported file formats.
#' @param title             Optional: the main title for the plot
#'
#' @return
#' A ggplot object. Use the [ggplot2::ggsave()] function to save to file in a different
#' format.
#'
#' @export
plotEventToCalendarTime <- function(studyPopulation,
                                    sccsModel = NULL,
                                    title = NULL,
                                    fileName = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(studyPopulation, min.len = 1, add = errorMessages)
  checkmate::assertClass(sccsModel, "SccsModel", null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(title, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(fileName, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  data <- computeOutcomeRatePerMonth(studyPopulation)
  plotData <- bind_rows(
    select(data, "month", "monthStartDate", "monthEndDate", value = "rate") %>%
      mutate(type = "Outcomes per person"),
    select(data, "month", "monthStartDate", "monthEndDate", value = "observationPeriodCount") %>%
      mutate(type = "Observed persons"),
  )
  levels <- c("Observed persons", "Outcomes per person")

  if (!is.null(sccsModel)) {
    data <- adjustOutcomeRatePerMonth(data, sccsModel)
    plotData <- bind_rows(
      plotData,
      select(data, "month", "monthStartDate", "monthEndDate", value = "adjustedRate") %>%
        mutate(type = "Adj. outcomes per person"),
    )
    levels <- c(levels, "Adj. outcomes per person")
  }

  plotData$type <- factor(plotData$type, levels = rev(levels))
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(plotData, ggplot2::aes(xmin = .data$monthStartDate, xmax = .data$monthEndDate + 1)) +
    ggplot2::geom_rect(ggplot2::aes(ymax = .data$value), ymin = 0, fill = rgb(0, 0, 0.8), alpha = 0.8, linewidth = 0) +
    ggplot2::scale_x_date("Calendar time") +
    ggplot2::scale_y_continuous("Count", limits = c(0, NA)) +
    ggplot2::facet_grid(.data$type ~ ., scales = "free_y") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = "#AAAAAA"),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      strip.text.y = theme,
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_blank(),
      legend.position = "top"
    )
  # plot
  if (!is.null(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  if (!is.null(fileName)) {
    ggplot2::ggsave(fileName, plot, width = 7, height = 1 + (2 * length(levels)), dpi = 400)
  }
  return(plot)
}

#' Plot the age effect
#'
#' @details
#' Plot the spline curve of the age effect.
#'
#' @param sccsModel   An object of type \code{sccsModel} as created using the
#'                    \code{\link{fitSccsModel}} function.
#' @param rrLim       The limits on the incidence rate ratio scale in the plot.
#' @param fileName    Name of the file where the plot should be saved, for example 'plot.png'. See the
#'                    function \code{ggsave} in the ggplot2 package for supported file formats.
#' @param title       Optional: the main title for the plot
#'
#' @return
#' A Ggplot object. Use the ggsave function to save to file.
#'
#' @export
plotAgeEffect <- function(sccsModel,
                          rrLim = c(0.1, 10),
                          title = NULL,
                          fileName = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsModel, "SccsModel", add = errorMessages)
  checkmate::assertNumeric(rrLim, len = 2, add = errorMessages)
  checkmate::assertCharacter(title, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(fileName, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (!hasAgeEffect(sccsModel)) {
    stop("The model does not contain an age effect.")
  }

  estimates <- sccsModel$estimates
  estimates <- estimates[estimates$covariateId >= 100 & estimates$covariateId < 200, ]
  splineCoefs <- c(0, estimates$logRr)
  ageKnots <- sccsModel$metaData$age$ageKnots
  age <- seq(min(ageKnots), max(ageKnots), length.out = 100)
  ageDesignMatrix <- splines::bs(age,
    knots = ageKnots[2:(length(ageKnots) - 1)],
    Boundary.knots = ageKnots[c(1, length(ageKnots))]
  )
  logRr <- apply(ageDesignMatrix %*% splineCoefs, 1, sum)
  logRr <- logRr - mean(logRr)
  rr <- exp(logRr)
  data <- data.frame(age = age, rr = rr)
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  ageLabels <- floor(min(ageKnots) / 365.25):floor(max(ageKnots) / 365.25)
  if (length(ageLabels) > 10) {
    ageLabels <- 10 * (floor(min(ageKnots) / 3652.5):floor(max(ageKnots) / 3652.5))
  }
  ageBreaks <- ageLabels * 365.25
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data$age, y = .data$rr)) +
    ggplot2::geom_hline(yintercept = breaks, colour = "#AAAAAA", lty = 1, linewidth = 0.2) +
    ggplot2::geom_line(color = rgb(0, 0, 0.8), alpha = 0.8, linewidth = 1) +
    ggplot2::scale_x_continuous("Age", breaks = ageBreaks, labels = ageLabels) +
    ggplot2::scale_y_continuous("Relative risk",
      limits = rrLim,
      trans = "log10",
      breaks = breaks,
      labels = breaks
    ) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
      panel.grid.major = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      strip.text.x = theme,
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_blank(),
      legend.position = "top"
    )
  if (!is.null(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  if (!is.null(fileName)) {
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  }
  return(plot)
}

#' Plot the seasonality effect
#'
#' @details
#' Plot the spline curve of the seasonality effect.
#'
#' @param sccsModel   An object of type \code{sccsModel} as created using the
#'                    \code{\link{fitSccsModel}} function.
#' @param rrLim       The limits on the incidence rate ratio scale in the plot.
#' @param fileName    Name of the file where the plot should be saved, for example 'plot.png'. See the
#'                    function \code{ggsave} in the ggplot2 package for supported file formats.
#' @param title       Optional: the main title for the plot
#'
#' @return
#' A Ggplot object. Use the ggsave function to save to file.
#'
#' @export
plotSeasonality <- function(sccsModel,
                            rrLim = c(0.1, 10),
                            title = NULL,
                            fileName = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsModel, "SccsModel", add = errorMessages)
  checkmate::assertNumeric(rrLim, len = 2, add = errorMessages)
  checkmate::assertCharacter(title, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(fileName, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (!hasSeasonality(sccsModel)) {
    stop("The model does not contain seasonality.")
  }

  estimates <- sccsModel$estimates
  estimates <- estimates[estimates$covariateId >= 200 & estimates$covariateId < 300, ]
  splineCoefs <- c(0, estimates$logRr)
  seasonKnots <- sccsModel$metaData$seasonality$seasonKnots
  season <- unique(c(seq(min(seasonKnots), max(seasonKnots), length.out = 100),
                     seasonKnots))
  seasonDesignMatrix <- cyclicSplineDesign(season, seasonKnots)
  logRr <- apply(seasonDesignMatrix %*% splineCoefs, 1, sum)
  logRr <- logRr - mean(logRr)
  rr <- exp(logRr)
  data <- tibble(season = season, rr = rr)
  knotData <- data %>%
    filter(.data$season %in% seasonKnots)


  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  seasonBreaks <- 1:12
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = season, y = rr)) +
    ggplot2::geom_hline(yintercept = breaks, colour = "#AAAAAA", lty = 1, linewidth = 0.2) +
    ggplot2::geom_line(color = rgb(0, 0, 0.8), alpha = 0.8, linewidth = 1) +
    ggplot2::geom_point(data = knotData) +
    ggplot2::scale_x_continuous("Month", breaks = seasonBreaks, labels = seasonBreaks) +
    ggplot2::scale_y_continuous("Relative risk",
      limits = rrLim,
      trans = "log10",
      breaks = breaks,
      labels = breaks
    ) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
      panel.grid.major = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      strip.text.x = theme,
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_blank(),
      legend.position = "top"
    )
  if (!is.null(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  if (!is.null(fileName)) {
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  }
  return(plot)
}

#' Plot the calendar time ranges spanned by each observation period.
#'
#' @template StudyPopulation
#' @param maxPersons  The maximum number of persons to plot. If there are more than this number of persons
#'                    a random sample will be taken to avoid visual clutter.
#' @param fileName    Name of the file where the plot should be saved, for example 'plot.png'.
#'                    See the function [ggplot2::ggsave()] for supported file formats.
#' @param title       Optional: the main title for the plot
#'
#' @details
#' Plots a line per patient from their observation start to their observation end.
#'
#' @return
#' A ggplot object. Use the [ggplot2::ggsave()] function to save to file in a different
#' format.
#'
#' @export
plotCalendarTimeSpans <- function(studyPopulation,
                                  maxPersons = 10000,
                                  title = NULL,
                                  fileName = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(studyPopulation, add = errorMessages)
  checkmate::assertInt(maxPersons, lower = 1, add = errorMessages)
  checkmate::assertCharacter(title, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(fileName, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  cases <- studyPopulation$cases %>%
    mutate(startDate = .data$observationPeriodStartDate + .data$startDay,
           endDate = .data$observationPeriodStartDate + .data$endDay) %>%
    select("startDate", "endDate") %>%
    arrange(.data$startDate, .data$endDate) %>%
    mutate(rank = row_number())
  if (nrow(cases) > maxPersons) {
    warning("There are ", nrow(cases), " cases. Random sampling ", maxPersons, " cases.")
    cases <- cases[sample.int(nrow(cases), maxPersons), ]
  }
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(cases, ggplot2::aes(x = .data$startDate, xmin = .data$startDate, xmax = .data$endDate, y = rank)) +
    ggplot2::geom_errorbarh(color = rgb(0, 0, 0.8)) +
    ggplot2::scale_x_date("Calendar time") +
    ggplot2::scale_y_continuous("Case rank") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
      panel.grid.major.x = ggplot2::element_line(colour = "#AAAAAA", linewidth = 0.2),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      strip.text.x = theme,
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_blank(),
      legend.position = "top"
    )
  if (!is.null(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  # fileName <- "S:/temp/plot.png"
  if (!is.null(fileName)) {
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  }
  return(plot)
}

#' Plot the calendar time effect
#'
#' @details
#' Plot the spline curve of the calendar time effect.
#'
#' @param sccsModel   An object of type \code{sccsModel} as created using the
#'                    \code{\link{fitSccsModel}} function.
#' @param rrLim       The limits on the incidence rate ratio scale in the plot.
#' @param fileName    Name of the file where the plot should be saved, for example 'plot.png'. See the
#'                    function \code{ggsave} in the ggplot2 package for supported file formats.
#' @param title       Optional: the main title for the plot
#'
#' @return
#' A Ggplot object. Use the ggsave function to save to file.
#'
#' @export
plotCalendarTimeEffect <- function(sccsModel,
                                   rrLim = c(0.1, 10),
                                   title = NULL,
                                   fileName = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsModel, "SccsModel", add = errorMessages)
  checkmate::assertNumeric(rrLim, len = 2, add = errorMessages)
  checkmate::assertCharacter(title, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(fileName, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (!hasCalendarTimeEffect(sccsModel)) {
    stop("The model does not contain a calendar time effect.")
  }

  estimates <- sccsModel$estimates
  estimates <- estimates[estimates$covariateId >= 300 & estimates$covariateId < 400, ]
  splineCoefs <- c(0, estimates$logRr)
  calendarTimeKnots <- sccsModel$metaData$calendarTime$calendarTimeKnots
  calendarTime <- unique(c(seq(min(calendarTimeKnots), max(calendarTimeKnots), length.out = 100),
                           calendarTimeKnots))
  calendarTimeDesignMatrix <- splines::bs(calendarTime,
    knots = calendarTimeKnots[2:(length(calendarTimeKnots) - 1)],
    Boundary.knots = calendarTimeKnots[c(1, length(calendarTimeKnots))]
  )
  logRr <- apply(calendarTimeDesignMatrix %*% splineCoefs, 1, sum)
  logRr <- logRr - mean(logRr)
  rr <- exp(logRr)
  data <- tibble(date = convertMonthToStartDate(calendarTime) + 14,
                 calendarTime = calendarTime,
                 rr = rr)
  knotData <- data %>%
    filter(.data$calendarTime %in% calendarTimeKnots)
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = date, y = rr)) +
    ggplot2::geom_line(color = rgb(0, 0, 0.8), alpha = 0.8, linewidth = 1) +
    ggplot2::geom_point(data = knotData) +
    ggplot2::scale_x_date("Calendar Time") +
    ggplot2::scale_y_continuous("Relative risk",
      limits = rrLim,
      trans = "log10",
      breaks = breaks,
      labels = breaks
    ) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = "#AAAAAA", linewidth = 0.2),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      strip.text.x = theme,
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_blank(),
      legend.position = "top"
    )
  if (!is.null(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  if (!is.null(fileName)) {
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  }
  return(plot)
}
