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

#' Plot the age ranges spanned by each observation period.
#'
#' @template StudyPopulation
#' @param maxPersons  The maximum number of persons to plot. If there are more than this number of persons
#'                    a random sample will be taken to avoid visual clutter.
#' @param fileName          Name of the file where the plot should be saved, for example 'plot.png'.
#'                          See the function [ggplot2::ggsave()] for supported file formats.
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
                         fileName = NULL) {
  cases <- studyPopulation$cases %>%
    transmute(startAge = .data$ageInDays, endAge = .data$ageInDays + .data$endDay) %>%
    arrange(.data$startAge, .data$endAge) %>%
    mutate(rank = row_number())

  ageLabels <- floor(min(cases$startAge)/365.25):ceiling(max(cases$endAge)/365.25)
  if (length(ageLabels) > 10) {
    ageLabels <- 10 * (floor(min(cases$startAge)/3652.5):floor(max(cases$endAge)/3652.5))
  }
  ageBreaks <- ageLabels * 365.25
  if (nrow(cases) > maxPersons) {
    warning("There are ", nrow(cases), " cases. Random sampling ", maxPersons, " cases.")
    cases <- cases[sample.int(nrow(cases), maxPersons), ]
  }
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(cases, ggplot2::aes(x = .data$startAge, xmin = .data$startAge, xmax = .data$endAge, y = rank)) +
    ggplot2::geom_vline(xintercept = ageBreaks, colour = "#AAAAAA", lty = 1, size = 0.2) +
    ggplot2::geom_errorbarh(color = rgb(0, 0, 0.8)) +
    ggplot2::scale_x_continuous("Age (years)", breaks = ageBreaks, labels = ageLabels) +
    ggplot2::scale_y_continuous("Case rank") +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = themeRA,
                   axis.text.x = theme,
                   strip.text.x = theme,
                   strip.background = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   legend.position = "top")
  # fileName <- "S:/temp/plot.png"
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  return(plot)
}

#' Plot time from event to observation end for censored and uncensored time.
#'
#' @template StudyPopulation
#' @param fileName          Name of the file where the plot should be saved, for example 'plot.png'.
#'                          See the function [ggplot2::ggsave()] for supported file formats.
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
                                           fileName = NULL) {


  outcomes <- studyPopulation$outcomes %>%
    group_by(.data$caseId) %>%
    summarise(outcomeDay = min(.data$outcomeDay), .groups = "drop_last") %>%
    inner_join(studyPopulation$cases, by = "caseId") %>%
    transmute(daysFromEvent = .data$endDay - .data$outcomeDay,
              censoring = case_when(.data$noninformativeEndCensor == 1 ~ "Uncensored",
                                    TRUE ~ "Censored"))

  ageLabels <- 0:ceiling(max(outcomes$daysFromEvent)/365.25)

  ageBreaks <- ageLabels * 365.25

  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(outcomes, ggplot2::aes(x = .data$daysFromEvent)) +
    ggplot2::geom_vline(xintercept = ageBreaks, colour = "#AAAAAA", lty = 1, size = 0.2) +
    ggplot2::geom_histogram(binwidth = 30.5, fill = rgb(0, 0, 0.8), alpha = 0.8) +
    ggplot2::scale_x_continuous("Years from event", breaks = ageBreaks, labels = ageLabels) +
    ggplot2::scale_y_continuous("Frequency") +
    ggplot2::facet_grid(censoring~., scales = "free_y") +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = themeRA,
                   axis.text.x = theme,
                   strip.text.y = theme,
                   strip.background = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   legend.position = "top")
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  return(plot)
}

#' Plot information centered around the start of exposure
#'
#' @param exposureEraId               The exposure to create the era data for. If not specified it is
#'                                    assumed to be the one exposure for which the data was loaded from
#'                                    the database.
#' @template StudyPopulation
#' @template SccsData
#' @param fileName          Name of the file where the plot should be saved, for example 'plot.png'.
#'                          See the function [ggplot2::ggsave()] for supported file formats.
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
                                 fileName = NULL) {

  if (is.null(exposureEraId)) {
    exposureEraId <- attr(sccsData, "metaData")$exposureEraIds
    if (length(exposureEraId) != 1) {
      stop("No exposure ID specified, but multiple exposures found")
    }
  }

  cases <- studyPopulation$cases %>%
    select(.data$caseId, caseEndDay = .data$endDay, .data$offset)

  exposures <- sccsData$eras %>%
    filter(.data$eraId == exposureEraId & .data$eraType == "rx") %>%
    group_by(.data$caseId) %>%
    inner_join(cases, by = "caseId", copy = TRUE) %>%
    mutate(startDay = .data$startDay - .data$offset,
           endDay = .data$endDay - .data$offset) %>%
    filter(.data$startDay >= 0, .data$startDay < .data$caseEndDay) %>%
    collect()

  firstExposures <- exposures %>%
    group_by(.data$caseId, .data$caseEndDay) %>%
    summarise(startDay = min(.data$startDay, na.rm = TRUE),
              endDay = min(.data$endDay, na.rm = TRUE),
              .groups = "drop_last")

  outcomes <- studyPopulation$outcomes %>%
    inner_join(firstExposures, by = "caseId") %>%
    mutate(delta = .data$outcomeDay - .data$startDay) %>%
    select(.data$caseId, .data$outcomeDay, .data$delta)

  exposedoutcomes <- exposures %>%
    inner_join(outcomes, by = "caseId") %>%
    filter(.data$outcomeDay >= .data$startDay,
           .data$outcomeDay <= .data$endDay) %>%
    select(.data$caseId, .data$delta) %>%
    mutate(exposed = 1)

  outcomes <- outcomes %>%
    left_join(exposedoutcomes, by = c("caseId", "delta")) %>%
    mutate(exposed = coalesce(.data$exposed, 0))

  weeks <- dplyr::tibble(number = -26:25) %>%
    mutate(start = .data$number*7,
           end = .data$number*7 + 7)

  events <- weeks %>%
    full_join(select(outcomes, .data$delta, .data$exposed), by = character()) %>%
    filter(.data$delta >= .data$start, .data$delta < .data$end) %>%
    group_by(.data$number, .data$start, .data$end) %>%
    summarise(eventsExposed = sum(.data$exposed),
              eventsUnexposed = n() - sum(.data$exposed),
              .groups = "drop_last")

  observed <- weeks %>%
    full_join(transmute(firstExposures, startDelta = -.data$startDay, endDelta = .data$caseEndDay - .data$startDay), by = character()) %>%
    filter(.data$endDelta >= .data$start, .data$startDelta < .data$end) %>%
    group_by(.data$number, .data$start, .data$end) %>%
    summarise(observed = n(),
              .groups = "drop_last")

  events <- events %>%
    transmute(.data$start,
              .data$end,
              type = "Events",
              count1 = .data$eventsUnexposed,
              count2 = .data$eventsExposed)

  observed <- observed %>%
    transmute(.data$start,
              .data$end,
              type = "Subjects under observation",
              count1 = .data$observed,
              count2 = NA)

  data <- bind_rows(events, observed)

  breaks <- seq(-150,150, 30)
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data$start, xmin = .data$start, xmax = .data$end, ymax = .data$count1, ymin = 0)) +
    ggplot2::geom_rect(fill = rgb(0, 0, 0.8), alpha = 0.8) +
    ggplot2::geom_rect(ggplot2::aes(ymax = .data$count1 + .data$count2, ymin = .data$count1), fill = rgb(0.8, 0, 0), alpha = 0.8) +
    ggplot2::geom_vline(xintercept = 0, colour = "#000000", lty = 1, size = 1) +
    ggplot2::scale_x_continuous("Days since first exposure start", breaks = breaks, labels = breaks) +
    ggplot2::scale_y_continuous("Count") +
    ggplot2::facet_grid(type~., scales = "free_y") +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
                   panel.grid.major = ggplot2::element_line(colour = "#AAAAAA"),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = themeRA,
                   axis.text.x = theme,
                   strip.text.y = theme,
                   strip.background = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   legend.position = "top")
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  return(plot)
}

#' Plot the count of events over calendar time.
#'
#' @template StudyPopulation
#' @param fileName          Name of the file where the plot should be saved, for example 'plot.png'.
#'                          See the function [ggplot2::ggsave()] for supported file formats.
#'
#' @return
#' A ggplot object. Use the [ggplot2::ggsave()] function to save to file in a different
#' format.
#'
#' @export
plotEventToCalendarTime <- function(studyPopulation,
                                    fileName = NULL) {
  dates <- studyPopulation$outcomes %>%
    inner_join(studyPopulation$cases , by = "caseId") %>%
    transmute(outcomeDate = .data$startDate  + .data$outcomeDay)

  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(dates, ggplot2::aes(x = .data$outcomeDate)) +
    ggplot2::geom_histogram(binwidth = 30.5, fill = rgb(0, 0, 0.8), alpha = 0.8) +
    ggplot2::scale_x_date("Calendar time") +
    ggplot2::scale_y_continuous("Frequency") +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
                   panel.grid.major = ggplot2::element_line(colour = "#AAAAAA"),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = themeRA,
                   axis.text.x = theme,
                   strip.text.y = theme,
                   strip.background = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   legend.position = "top")
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
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
#'
#' @return
#' A Ggplot object. Use the ggsave function to save to file.
#'
#' @export
plotAgeEffect <- function(sccsModel, rrLim = c(0.1, 10), fileName = NULL) {
  estimates <- sccsModel$estimates
  estimates <- estimates[estimates$covariateId >= 100 & estimates$covariateId < 200, ]
  splineCoefs <- c(0, estimates$logRr)
  ageKnots <- sccsModel$metaData$age$ageKnots
  age <- seq(min(ageKnots), max(ageKnots), length.out = 100)
  ageDesignMatrix <- splines::bs(age,
                                 knots = ageKnots[2:(length(ageKnots) - 1)],
                                 Boundary.knots = ageKnots[c(1, length(ageKnots))])
  logRr <- apply(ageDesignMatrix %*% splineCoefs, 1, sum)
  logRr <- logRr - mean(logRr)
  rr <- exp(logRr)
  data <- data.frame(age = age, rr = rr)
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  ageLabels <- floor(min(ageKnots)/365.25):floor(max(ageKnots)/365.25)
  if (length(ageLabels) > 10) {
    ageLabels <- 10 * (floor(min(ageKnots)/3652.5):floor(max(ageKnots)/3652.5))
  }
  ageBreaks <- ageLabels * 365.25
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data$age, y = .data$rr)) +
    ggplot2::geom_hline(yintercept = breaks, colour = "#AAAAAA", lty = 1, size = 0.2) +
    ggplot2::geom_line(color = rgb(0, 0, 0.8), alpha = 0.8, lwd = 1) +
    ggplot2::scale_x_continuous("Age", breaks = ageBreaks, labels = ageLabels) +
    ggplot2::scale_y_continuous("Relative risk",
                                lim = rrLim,
                                trans = "log10",
                                breaks = breaks,
                                labels = breaks) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = themeRA,
                   axis.text.x = theme,
                   strip.text.x = theme,
                   strip.background = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   legend.position = "top")
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
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
#'
#' @return
#' A Ggplot object. Use the ggsave function to save to file.
#'
#' @export

plotSeasonality <- function(sccsModel, rrLim = c(0.1, 10), fileName = NULL) {
  estimates <- sccsModel$estimates
  estimates <- estimates[estimates$covariateId >= 200 & estimates$covariateId < 300, ]
  splineCoefs <- c(0, estimates$logRr)
  seasonKnots <- sccsModel$metaData$seasonality$seasonKnots
  season <- seq(min(seasonKnots), max(seasonKnots), length.out = 100)
  seasonDesignMatrix <- cyclicSplineDesign(season, seasonKnots)
  logRr <- apply(seasonDesignMatrix %*% splineCoefs, 1, sum)
  logRr <- logRr - mean(logRr)
  rr <- exp(logRr)
  data <- data.frame(season = season, rr = rr)

  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  seasonBreaks <- 1:12
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = season, y = rr)) +
    ggplot2::geom_hline(yintercept = breaks, colour = "#AAAAAA", lty = 1, size = 0.2) +
    ggplot2::geom_line(color = rgb(0, 0, 0.8), alpha = 0.8, lwd = 1) +
    ggplot2::scale_x_continuous("Month", breaks = seasonBreaks, labels = seasonBreaks) +
    ggplot2::scale_y_continuous("Relative risk",
                                lim = rrLim,
                                trans = "log10",
                                breaks = breaks,
                                labels = breaks) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = themeRA,
                   axis.text.x = theme,
                   strip.text.x = theme,
                   strip.background = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   legend.position = "top")
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  return(plot)
}

