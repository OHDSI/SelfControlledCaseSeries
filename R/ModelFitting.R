# @file ModelFitting.R
#
# Copyright 2014 Observational Health Data Sciences and Informatics
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

#' @export
fitSccsModel <- function(sccsEraData,
                         exposureId,
                         prior = createPrior("laplace", useCrossValidation = TRUE),
                         control = createControl(cvType = "auto",
                                                 startingVariance = 0.1,
                                                 noiseLevel = "quiet")) {
  covariateIds <- sccsEraData$metaData$exposureRef$covariateId[sccsEraData$metaData$exposureRef$conceptId %in% exposureId]
  prior$exclude <- covariateIds
  cyclopsData <- Cyclops::convertToCyclopsData(sccsEraData$outcomes,
                                               sccsEraData$covariates,
                                               modelType = "cpr",
                                               addIntercept = FALSE,
                                               checkRowIds = FALSE,
                                               quiet = TRUE)
  fit <- tryCatch({
    Cyclops::fitCyclopsModel(cyclopsData, prior = prior, control = control)
  }, error = function(e) {
    e$message
  })
  if (is.character(fit)) {
    coefficients <- c(0)
    treatmentEstimate <- data.frame(logRr = 0, logLb95 = -Inf, logUb95 = Inf, seLogRr = Inf)
    priorVariance <- 0
    status <- fit
  } else if (fit$return_flag == "ILLCONDITIONED") {
    coefficients <- c(0)
    treatmentEstimate <- data.frame(logRr = 0, logLb95 = -Inf, logUb95 = Inf, seLogRr = Inf)
    priorVariance <- 0
    status <- "ILL CONDITIONED, CANNOT FIT"
  } else {
    status <- "OK"
    coefficients <- coef(fit)
    logRr <- coef(fit)[names(coef(fit)) == covariateIds]
    ci <- tryCatch({
      confint(fit, parm = covariateIds, includePenalty = TRUE)
    }, error = function(e) {
      missing(e)  # suppresses R CMD check note
      c(0, -Inf, Inf)
    })
    if (identical(ci, c(0, -Inf, Inf)))
      status <- "ERROR COMPUTING CI"
    seLogRr <- (ci[,3] - logRr)/qnorm(0.975)
    treatmentEstimate <- data.frame(logRr = logRr,
                                    logLb95 = ci[,2],
                                    logUb95 = ci[,3],
                                    seLogRr = seLogRr)
    priorVariance <- fit$variance[1]
  }
  result <- list(exposureId = exposureId,
                 outcomeId = sccsEraData$metaData$outcomeId,
                 coefficients = coefficients,
                 priorVariance = priorVariance,
                 treatmentEstimate = treatmentEstimate,
                 status = status)
  if (!is.null(sccsEraData$metaData$ageKnots)){
    result$ageKnots <- sccsEraData$metaData$ageKnots
  }
  if (!is.null(sccsEraData$metaData$seasonKnots)){
    result$seasonKnots <- sccsEraData$metaData$seasonKnots
  }
  class(result) <- "sccsModel"
  return(result)
}

#' @export
getModel <- function(sccsModel, sccsEraData) {
  cfs <- sccsModel$coefficients
  cfs <- data.frame(coefficient = cfs, id = as.numeric(names(cfs)))
  cfs <- merge(ff::as.ffdf(cfs),
               sccsEraData$covariateRef,
               by.x = "id",
               by.y = "covariateId",
               all.x = TRUE)
  cfs <- ff::as.ram(cfs[, c("coefficient", "id", "covariateName")])
  cfs$conceptName <- as.character(cfs$conceptName)
  cfs <- cfs[order(-abs(cfs$coefficient)), ]
  cfs
}

#' @export
plotAgeEffect <- function(sccsModel, rrLim = c(0.1,10), fileName = NULL){
  allCoefs <- sccsModel$coefficients
  coefId <- as.numeric(names(allCoefs))
  splineCoefs <- allCoefs[coefId >= 100 & coefId < 130]
  splineCoefs <- c(0, splineCoefs)
  ageKnots <- sccsModel$ageKnots
  age <- seq(min(ageKnots), max(ageKnots), length.out = 100)
  ageDesignMatrix <- splines::bs(age, knots = ageKnots[2:(length(ageKnots)-1)], Boundary.knots = ageKnots[c(1,length(ageKnots))])
  logRr <- apply(ageDesignMatrix %*% splineCoefs, 1, sum)
  logRr <- logRr - mean(logRr)
  rr <- exp(logRr)
  data <- data.frame(age = age, rr = rr)
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  ageLabels <- floor(min(ageKnots)/365.25) : floor(max(ageKnots)/365.25)
  if (length(ageLabels) > 10){
    ageLabels <- 10*(floor(min(ageKnots)/3652.5) : floor(max(ageKnots)/3652.5))
  }
  ageBreaks <- ageLabels * 365.25
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = age, y = rr)) +
    ggplot2::geom_hline(yintercept = breaks, colour = "#AAAAAA", lty = 1,lw = 0.2) +
    ggplot2::geom_line(color = rgb(0,0,0.8), alpha = 0.8, lwd = 1) +
    ggplot2::scale_x_continuous("Age", breaks = ageBreaks, labels = ageLabels) +
    ggplot2::scale_y_continuous("Relative risk", lim = rrLim, trans = "log10", breaks = breaks, labels = breaks) +
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

#' @export
plotSeasonality <- function(sccsModel, rrLim = c(0.1,10), fileName = NULL){
  allCoefs <- sccsModel$coefficients
  coefId <- as.numeric(names(allCoefs))
  splineCoefs <- allCoefs[coefId >= 200 & coefId < 220]
  splineCoefs <- c(0, splineCoefs)
  seasonKnots <- sccsModel$seasonKnots
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
    ggplot2::geom_hline(yintercept = breaks, colour = "#AAAAAA", lty = 1,lw = 0.2) +
    ggplot2::geom_line(color = rgb(0,0,0.8), alpha = 0.8, lwd = 1) +
    ggplot2::scale_x_continuous("Month", breaks = seasonBreaks, labels = seasonBreaks) +
    ggplot2::scale_y_continuous("Relative risk", lim = rrLim, trans = "log10", breaks = breaks, labels = breaks) +
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
