# @file Plots.R
#
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

#' Plot the age ranges spanned by each observation period.
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
#' @param fileName    Name of the file where the plot should be saved, for example 'plot.png'. See the
#'                    function \code{ggsave} in the ggplot2 package for supported file formats.
#'
#' @details
#' If parameters such as naivePeriod, minAge, and maxAge are provided, these will first be applied to curtail the observation period
#' prior to plotting. Similarly, firstOutcomeOnly can be provided so subjects where the first outcome falls before the true observation
#' start are removed before plotting.
#'
#' @return
#' A Ggplot object. Use the ggsave function to save to file.
#'
#' @export
plotAgeSpans <- function(sccsData,
                         outcomeId = NULL,
                         firstOutcomeOnly = FALSE,
                         naivePeriod = 0,
                         minAge = NULL,
                         maxAge = NULL,
                         fileName = NULL) {
  if (is.null(outcomeId)) {
    outcomeId <- sccsData$metaData$outcomeIds
    if (length(outcomeId) != 1) {
      stop("No outcome ID specified, but multiple outcomes found")
    }
  }
  data <- findIncludedOutcomes(sccsData = sccsData,
                               outcomeId = outcomeId,
                               firstOutcomeOnly = firstOutcomeOnly,
                               naivePeriod = naivePeriod,
                               minAge = minAge,
                               maxAge = maxAge)
  cases <- data$cases[, c("trueStartAge", "trueEndAge")]
  cases <- cases[order(cases$trueStartAge, cases$trueEndAge), ]
  cases$rank <- 1:nrow(cases)

  ageLabels <- floor(min(cases$trueStartAge)/365.25):ceiling(max(cases$trueEndAge)/365.25)
  if (length(ageLabels) > 10) {
    ageLabels <- 10 * (floor(min(cases$trueStartAge)/3652.5):floor(max(cases$trueEndAge)/3652.5))
  }
  ageBreaks <- ageLabels * 365.25
  if (nrow(cases) > 10000) {
    warning(paste("There are", nrow(cases), "cases, but can only reasonably show 10,000. Random sampling 10,000 cases."))
    cases <- cases[sample.int(nrow(cases), 10000), ]
  }
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(cases, ggplot2::aes(x = trueStartAge, xmin = trueStartAge, xmax = trueEndAge, y = rank)) +
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
#' @param fileName    Name of the file where the plot should be saved, for example 'plot.png'. See the
#'                    function \code{ggsave} in the ggplot2 package for supported file formats.
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
#' If parameters such as naivePeriod, minAge, and maxAge are provided, these will first be applied to curtail the observation period
#' prior to plotting. Similarly, firstOutcomeOnly can be provided so subjects where the first outcome falls before the true observation
#' start are removed before plotting.
#'
#' @references
#' Farrington P, Whitaker H, Ghebremichael Weldeselassie Y (2018), Self-controlled case series studies: A modelling guide with R, Taylor &
#' Francis
#'
#' @return
#' A Ggplot object. Use the ggsave function to save to file.
#'
#' @export
plotEventObservationDependence <- function(sccsData,
                                           outcomeId = NULL,
                                           naivePeriod = 0,
                                           minAge = NULL,
                                           maxAge = NULL,
                                           fileName = NULL) {
  if (is.null(outcomeId)) {
    outcomeId <- sccsData$metaData$outcomeIds
    if (length(outcomeId) != 1) {
      stop("No outcome ID specified, but multiple outcomes found")
    }
  }
  data <- findIncludedOutcomes(sccsData = sccsData,
                               outcomeId = outcomeId,
                               firstOutcomeOnly = TRUE,
                               naivePeriod = naivePeriod,
                               minAge = minAge,
                               maxAge = maxAge)
  uncensored <- isUncensored(sccsData = sccsData, maxAge = maxAge)
  cases <- data$cases[, c("trueStartAge", "trueEndAge")]
  outcomes <- data$outcomes[, c("observationPeriodId", "trueEndAge", "outcomeAge")]
  outcomes$censoring <- "Censored"
  outcomes$censoring[outcomes$observationPeriodId %in% ff::as.ram(sccsData$cases$observationPeriodId[uncensored])] <- "Uncensored"
  outcomes$daysFromEvent <- outcomes$trueEndAge - outcomes$outcomeAge

  ageLabels <- 0:ceiling(max(outcomes$daysFromEvent)/365.25)

  ageBreaks <- ageLabels * 365.25

  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(outcomes, ggplot2::aes(x = daysFromEvent)) +
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
  # fileName <- "S:/temp/plot2.png"
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  return(plot)
}

#' Plot information centered around the start of exposure
#'
#' @param sccsData                    An object of type \code{sccsData} as created using the
#'                                    \code{\link{getDbSccsData}} function.
#' @param outcomeId                   The outcome to create the era data for. If not specified it is
#'                                    assumed to be the one outcome for which the data was loaded from
#'                                    the database.
#' @param exposureId                  The exposure to create the era data for. If not specified it is
#'                                    assumed to be the one exposure for which the data was loaded from
#'                                    the database.
#' @param naivePeriod                 The number of days at the start of a patient's observation period
#'                                    that should not be included in the risk calculations. Note that
#'                                    the naive period can be used to determine current covariate
#'                                    status right after the naive period, and whether an outcome is
#'                                    the first one.
#' @param firstOutcomeOnly            Whether only the first occurrence of an outcome should be
#'                                    considered.
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
#' @param fileName    Name of the file where the plot should be saved, for example 'plot.png'. See the
#'                    function \code{ggsave} in the ggplot2 package for supported file formats.
#'
#' @details
#' This plot shows the number of events and the number of subjects under observation in week-sized intervals relative to the start
#' of the first exposure.
#'
#' If parameters such as naivePeriod, minAge, and maxAge are provided, these will first be applied to curtail the observation period
#' prior to plotting. Similarly, firstOutcomeOnly can be provided so subjects where the first outcome falls before the true observation
#' start are removed before plotting.
#'
#' @return
#' A Ggplot object. Use the ggsave function to save to file.
#'
#' @export
plotExposureCentered <- function(sccsData,
                                 outcomeId = NULL,
                                 exposureId = NULL,
                                 naivePeriod = 0,
                                 firstOutcomeOnly = FALSE,
                                 minAge = NULL,
                                 maxAge = NULL,
                                 fileName = NULL) {
  if (is.null(outcomeId)) {
    outcomeId <- sccsData$metaData$outcomeIds
    if (length(outcomeId) != 1) {
      stop("No outcome ID specified, but multiple outcomes found")
    }
  }
  if (is.null(exposureId)) {
    exposureId <- sccsData$metaData$exposureIds
    if (length(exposureId) != 1) {
      stop("No exposure ID specified, but multiple exposures found")
    }
  }
  data <- findIncludedOutcomes(sccsData = sccsData,
                               outcomeId = outcomeId,
                               firstOutcomeOnly = firstOutcomeOnly,
                               naivePeriod = naivePeriod,
                               minAge = minAge,
                               maxAge = maxAge)
  cases <- data$cases[, c("observationPeriodId", "ageInDays", "trueStartAge", "trueEndAge")]
  cases$trueStartDay <- cases$trueStartAge - cases$ageInDays
  cases$trueEndDay <- cases$trueEndAge - cases$ageInDays
  exposures <- sccsData$eras[sccsData$eras$conceptId == exposureId & sccsData$eras$eraType == "hei", ]
  exposures <- merge(exposures[, c("observationPeriodId", "startDay", "endDay")], cases[, c("observationPeriodId", "ageInDays", "trueStartDay", "trueEndDay")])
  exposures <- exposures[exposures$startDay <= exposures$trueEndDay & exposures$endDay >= exposures$trueStartDay, ]
  exposures$startDay[exposures$startDay < exposures$trueStartDay] <- exposures$trueStartDay[exposures$startDay < exposures$trueStartDay]
  exposures$endDay[exposures$endDay > exposures$trueEndDay] <- exposures$trueEndDay[exposures$endDay > exposures$trueEndDay]
  firstExposures <- aggregate(startDay ~ observationPeriodId, exposures, min)

  firstExposures <- merge(firstExposures, cases)
  firstExposures$exposureAge <- firstExposures$startDay + firstExposures$ageInDays
  firstExposures$startDelta <- firstExposures$trueStartAge - firstExposures$exposureAge
  firstExposures$endDelta <- firstExposures$trueEndAge - firstExposures$exposureAge
  outcomes <- data$outcomes[, c("observationPeriodId", "outcomeAge")]
  outcomes <- merge(outcomes, firstExposures[, c("observationPeriodId", "exposureAge")])
  outcomes$delta <- outcomes$outcomeAge - outcomes$exposureAge

  exposedOutcomes <- merge(outcomes[, c("observationPeriodId", "outcomeAge", "delta")],
                    exposures[, c("observationPeriodId", "startDay", "endDay", "ageInDays")])
  exposedOutcomes$outcomeDay <- exposedOutcomes$outcomeAge - exposedOutcomes$ageInDays
  exposedOutcomes <- exposedOutcomes[exposedOutcomes$outcomeDay >= exposedOutcomes$startDay & exposedOutcomes$outcomeDay <= exposedOutcomes$endDay, ]
  exposedOutcomes <- exposedOutcomes[, c("observationPeriodId", "outcomeAge")]
  exposedOutcomes$exposed <- TRUE

  outcomes <- merge(outcomes, exposedOutcomes, all.x = TRUE)
  outcomes$exposed[is.na(outcomes$exposed)] <- FALSE

  weeks <- data.frame(number = -26:25)
  weeks$start <- weeks$number*7
  weeks$end <- weeks$number*7 + 7
  weeks$eventsExposed <- 0
  weeks$eventsUnexposed <- 0
  weeks$observed <- 0
  for (i in 1:nrow(weeks)) {
    weeks$eventsExposed[i] <- sum(outcomes$delta >= weeks$start[i] & outcomes$delta < weeks$end[i] & outcomes$exposed)
    weeks$eventsUnexposed[i] <- sum(outcomes$delta >= weeks$start[i] & outcomes$delta < weeks$end[i] & !outcomes$exposed)
    weeks$observed[i] <- sum(firstExposures$startDelta <= weeks$start[i] & firstExposures$endDelta >= weeks$end[i])
  }
  events <- weeks
  events$type <- "Events"
  events$count1 <- events$eventsUnexposed
  events$count2 <- events$eventsExposed
  observed <- weeks
  observed$type <- "Subjects under observation"
  observed$count1 <- observed$observed
  observed$count2 <- rep(NA, nrow(observed))
  data <- rbind(events, observed)
  breaks <- seq(-150,150, 30)
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = start, xmin = start, xmax = end, ymax = count1, ymin = 0)) +
    ggplot2::geom_rect(fill = rgb(0, 0, 0.8), alpha = 0.8) +
    ggplot2::geom_rect(ggplot2::aes(ymax = count1 + count2, ymin = count1), fill = rgb(0.8, 0, 0), alpha = 0.8) +
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
  # fileName <- "S:/temp/plot4b.png"
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  return(plot)
}

#' Plot exposures and outcomes per person
#'
#' @param sccsData                    An object of type \code{sccsData} as created using the
#'                                    \code{\link{getDbSccsData}} function.
#' @param outcomeId                   The outcome to create the era data for. If not specified it is
#'                                    assumed to be the one outcome for which the data was loaded from
#'                                    the database.
#' @param exposureId                  The exposure to create the era data for. If not specified it is
#'                                    assumed to be the one exposure for which the data was loaded from
#'                                    the database.
#' @param naivePeriod                 The number of days at the start of a patient's observation period
#'                                    that should not be included in the risk calculations. Note that
#'                                    the naive period can be used to determine current covariate
#'                                    status right after the naive period, and whether an outcome is
#'                                    the first one.
#' @param firstOutcomeOnly            Whether only the first occurrence of an outcome should be
#'                                    considered.
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
#' @param fileName    Name of the file where the plot should be saved, for example 'plot.png'. See the
#'                    function \code{ggsave} in the ggplot2 package for supported file formats.
#'
#' @details
#' This plot shows the observation time (black), exposures (red), and outcomes (yellow) per person.
#'
#' If parameters such as naivePeriod, minAge, and maxAge are provided, these will first be applied to curtail the observation period
#' prior to plotting. Similarly, firstOutcomeOnly can be provided so subjects where the first outcome falls before the true observation
#' start are removed before plotting.
#'
#' @return
#' A Ggplot object. Use the ggsave function to save to file.
#'
#' @export
plotPerPersonData <- function(sccsData,
                              outcomeId = NULL,
                              exposureId = NULL,
                              naivePeriod = 0,
                              firstOutcomeOnly = FALSE,
                              minAge = NULL,
                              maxAge = NULL,
                              fileName = NULL) {
  if (is.null(outcomeId)) {
    outcomeId <- sccsData$metaData$outcomeIds
    if (length(outcomeId) != 1) {
      stop("No outcome ID specified, but multiple outcomes found")
    }
  }
  if (is.null(exposureId)) {
    exposureId <- sccsData$metaData$exposureIds
    if (length(exposureId) != 1) {
      stop("No exposure ID specified, but multiple exposures found")
    }
  }
  data <- findIncludedOutcomes(sccsData = sccsData,
                               outcomeId = outcomeId,
                               firstOutcomeOnly = firstOutcomeOnly,
                               naivePeriod = naivePeriod,
                               minAge = minAge,
                               maxAge = maxAge)

  cases <- data$cases[, c("observationPeriodId", "ageInDays", "trueStartAge", "trueEndAge")]

  cases$trueStartDay <- cases$trueStartAge - cases$ageInDays
  cases$trueEndDay <- cases$trueEndAge - cases$ageInDays
  exposures <- sccsData$eras[sccsData$eras$conceptId == exposureId & sccsData$eras$eraType == "hei", ]
  exposures <- merge(exposures[, c("observationPeriodId", "startDay", "endDay")], cases[, c("observationPeriodId", "ageInDays", "trueStartDay", "trueEndDay")])
  exposures <- exposures[exposures$startDay <= exposures$trueEndDay & exposures$endDay >= exposures$trueStartDay, ]
  exposures$startDay[exposures$startDay < exposures$trueStartDay] <- exposures$trueStartDay[exposures$startDay < exposures$trueStartDay]
  exposures$endDay[exposures$endDay > exposures$trueEndDay] <- exposures$trueEndDay[exposures$endDay > exposures$trueEndDay]
  firstExposureStarts <- aggregate(startDay ~ observationPeriodId, exposures, min)
  firstExposureEnds <- aggregate(endDay ~ observationPeriodId, exposures, min)
  cases <- merge(firstExposureStarts, cases)
  cases <- merge(firstExposureEnds, cases)
  cases$endDay <- cases$endDay - cases$startDay
  cases$trueStartDay <- cases$trueStartDay - cases$startDay
  cases$trueEndDay <- cases$trueEndDay - cases$startDay
  if (nrow(cases) > 100) {
    warning(paste("There are", nrow(cases), "cases, but can only reasonably show 100. Random sampling 100 cases."))
    cases <- cases[sample.int(nrow(cases), 100), ]
  }
  cases <- cases[order(cases$endDay), ]
  cases$rank <- 1:nrow(cases)

  exposures <- merge(exposures[, c("observationPeriodId", "startDay", "endDay")], cases[, c("observationPeriodId", "rank","trueStartDay")])
  exposures$startDay <- exposures$startDay + exposures$trueStartDay
  exposures$endDay <- exposures$endDay + exposures$trueStartDay

  outcomes <- data$outcomes[, c("observationPeriodId", "outcomeAge")]
  outcomes <- merge(outcomes, cases[, c("observationPeriodId", "rank", "ageInDays", "trueStartDay")])
  outcomes$day <- outcomes$outcomeAge - outcomes$ageInDays + outcomes$trueStartDay

  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(cases, ggplot2::aes(x = trueStartDay, y = rank, yend = rank)) +
    ggplot2::geom_segment(ggplot2::aes(x = trueStartDay, xend = trueEndDay), colour = rgb(0, 0, 0), size = 2) +
    ggplot2::geom_segment(ggplot2::aes(x = startDay, xend = endDay), colour = rgb(0.8, 0, 0), data = exposures, size = 2) +
    ggplot2::geom_point(ggplot2::aes(x = day), colour = rgb(0.8, 0.8, 0), data = outcomes, size = 1) +
    ggplot2::scale_x_continuous("Days since first exposure start") +
    ggplot2::scale_y_continuous("Case rank") +
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
  # fileName <- "S:/temp/plot5b.png"
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 10, height = 10, dpi = 400)
  return(plot)
}


#' Plot the count of events over calendar time.
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
#' @param fileName    Name of the file where the plot should be saved, for example 'plot.png'. See the
#'                    function \code{ggsave} in the ggplot2 package for supported file formats.
#'
#' @return
#' A Ggplot object. Use the ggsave function to save to file.
#'
#' @export
plotEventToCalendarTime <- function(sccsData,
                                    outcomeId = NULL,
                                    naivePeriod = 0,
                                    minAge = NULL,
                                    maxAge = NULL,
                                    fileName = NULL) {
  if (is.null(outcomeId)) {
    outcomeId <- sccsData$metaData$outcomeIds
    if (length(outcomeId) != 1) {
      stop("No outcome ID specified, but multiple outcomes found")
    }
  }
  data <- findIncludedOutcomes(sccsData = sccsData,
                               outcomeId = outcomeId,
                               firstOutcomeOnly = TRUE,
                               naivePeriod = naivePeriod,
                               minAge = minAge,
                               maxAge = maxAge)
  outcomes <- merge(ffbase::subset.ffdf(data$cases, select = c("observationPeriodId", "startYear", "startMonth", "startDay")),
                    ffbase::subset.ffdf(data$outcomes, select = c("observationPeriodId", "outcomeDay")))

  dates <- as.Date(paste(ff::as.ram(outcomes$startYear),
                         ff::as.ram(outcomes$startMonth),
                         ff::as.ram(outcomes$startDay),
                         sep = "-"), format = "%Y-%m-%d")
  dates <- dates + ff::as.ram(outcomes$outcomeDay)

  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(data.frame(dates = dates), ggplot2::aes(x = dates)) +
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
  # fileName <- "S:/temp/plot3.png"
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
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = age, y = rr)) +
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

