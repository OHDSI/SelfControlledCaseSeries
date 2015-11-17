# @file ModelFitting.R
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

#' Fit the SCCS model
#'
#' @details
#' Fits the SCCS model as a conditional Poisson regression. When allowed, coefficients for
#' some or all covariates can be regularized.
#'
#' @param sccsEraData  An object of type \code{sccsEraData} as created using the
#' \code{\link{createSccsEraData}} function.
#' @param prior                  The prior used to fit the model. See
#'                               \code{\link[Cyclops]{createPrior}} for details.
#' @param control                The control object used to control the cross-validation used to
#'                               determine the hyperparameters of the prior (if applicable). See
#'                               \code{\link[Cyclops]{createControl}} for details.
#'
#' @return
#' An object of type \code{sccsModel}. eneric function \code{summary}, \code{coef}, and
#' \code{confint} are available.
#'
#' @export
fitSccsModel <- function(sccsEraData,
                         prior = createPrior("laplace", useCrossValidation = TRUE),
                         control = createControl(cvType = "auto",
                                                 selectorType = "byPid",
                                                 startingVariance = 0.1,
                                                 noiseLevel = "quiet")) {
  # Build list of IDs that should not be regularized, and see if there is anything that needs regularization:
  nonRegularized <- c()
  needRegularization <- FALSE
  covariateSettingsList <- sccsEraData$metaData$covariateSettingsList
  for (i in 1:length(covariateSettingsList)){
    if (covariateSettingsList[[i]]$allowRegularization) {
      needRegularization <- TRUE
    } else {
      nonRegularized <- c(nonRegularized, covariateSettingsList[[i]]$outputIds)
    }
  }
  if (!is.null(sccsEraData$metaData$age)){
    if (sccsEraData$metaData$age$allowRegularization){
      needRegularization <- TRUE
    } else {
      nonRegularized <- c(nonRegularized, sccsEraData$metaData$age$covariateIds)
    }
  }
  if (!is.null(sccsEraData$metaData$seasonality)){
    if (sccsEraData$metaData$seasonality$allowRegularization){
      needRegularization <- TRUE
    } else {
      nonRegularized <- c(nonRegularized, sccsEraData$metaData$seasonality$covariateIds)
    }
  }

  if (!needRegularization){
    prior = createPrior("none")
  } else {
    prior$exclude <- nonRegularized
  }
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
    estimates <- coef(fit)
    estimates <- data.frame(logRr = estimates, covariateId = as.numeric(names(estimates)))
    estimates <- merge(estimates, ff::as.ram(sccsEraData$covariateRef), all.x = TRUE)
    tryCatch({
      ci <- as.data.frame(confint(fit, parm = nonRegularized, includePenalty = TRUE))
      rownames(ci) <- NULL
    }, error = function(e) {
      missing(e)  # suppresses R CMD check note
      ci <- data.frame(covariateId = 0, logLb95 = 0, logUb95 = 0)
    })
    names(ci)[names(ci) == "2.5 %"] <- "logLb95"
    names(ci)[names(ci) == "97.5 %"] <- "logUb95"
    ci$evaluations <- NULL
    estimates <- merge(estimates, ci, by.x = "covariateId", by.y = "covariate", all.x = TRUE)
    priorVariance <- fit$variance[1]
  }
  result <- list(outcomeId = sccsEraData$metaData$outcomeId,
                 estimates = estimates,
                 priorVariance = priorVariance,
                 status = status)
  if (!is.null(sccsEraData$metaData$age)){
    result$age <- sccsEraData$metaData$age
  }
  if (!is.null(sccsEraData$metaData$seasonanlity)){
    result$seasononality <- sccsEraData$metaData$seasononality
  }
  class(result) <- "sccsModel"
  return(result)
}


#' @export
coef.sccsModel <- function(object, ...) {
  return(object$estimates$logRr)
}

#' Output the full model
#'
#' @param sccsModel  An object of type \code{sccsModel} as created using the \code{\link{fitSccsModel}}
#' function.
#'
#' @return
#' A data frame with the coefficients and confidence intervals (when not-regularized) for all
#' covariates in the model.
#'
#' @export
getModel <- function(sccsModel) {
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

#' Plot the age effect
#'
#' @details
#' Plot the spline curve of the age effect.
#'
#' @param sccsModel An object of type \code{sccsModel} as created using the \code{\link{fitSccsModel}}
#' function.
#' @param rrLim  The limits on the incidence rate ratio scale in the plot.
#' @param fileName  Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
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

#' Plot the seasonality effect
#'
#' @details
#' Plot the spline curve of the seasonality effect.
#'
#' @param sccsModel An object of type \code{sccsModel} as created using the \code{\link{fitSccsModel}}
#' function.
#' @param rrLim  The limits on the incidence rate ratio scale in the plot.
#' @param fileName  Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
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
