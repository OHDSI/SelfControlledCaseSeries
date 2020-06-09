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

#' Create era covariate settings
#'
#' @details
#' Create an object specifying how to create a (set of) era-based covariates.
#'
#' @param includeEraIds           One or more IDs of variables in the [SccsData] object that
#'                                should be used to construct this covariate. If no IDs are specified,
#'                                all variables will be used.
#' @param excludeEraIds           One or more IDs of variables in the \[SccsData] object that
#'                                should not be used to construct this covariate.
#' @param label                   A label used to identify the covariates created using these settings.
#' @param stratifyById            Should a single covariate be created for every ID in the
#'                                [SccsData] object, or should a single covariate be constructed?
#'                                For example, if the IDs identify exposures to different drugs, should
#'                                a covariate be constructed for every drug, or a single covariate for
#'                                exposure to any of these drugs. Note that overlap will be considered
#'                                a single exposure.
#' @param start                   The start of the risk window (in days) relative to the `startAnchor`.
#' @param startAnchor             The anchor point for the start of the risk window. Can be `"era start"`
#'                                or `"era end"`.
#' @param end                     The end of the risk window (in days) relative to the `endAnchor`.
#' @param endAnchor               The anchor point for the end of the risk window. Can be `"era start"`
#'                                or `"era end"`.
#' @param firstOccurrenceOnly     Should only the first occurrence of the exposure be used?
#' @param splitPoints             To split the risk window into several smaller windows, specify the
#'                                end of each sub- window relative to the start of the main risk
#'                                window. If add ExposedDaysToStart is TRUE, the split points will be
#'                                considered to be relative to the end of the main risk window instead.
#' @param allowRegularization     When fitting the model, should the covariates defined here be allowed
#'                                to be regularized?
#'
#' @return
#' An object of type `EraCovariateSettings`.
#'
#' @export
createEraCovariateSettings <- function(includeEraIds = NULL,
                                       excludeEraIds = NULL,
                                       label = "Covariates",
                                       stratifyById = TRUE,
                                       start = 0,
                                       startAnchor = "era start",
                                       end = 0,
                                       endAnchor = "era end",
                                       firstOccurrenceOnly = FALSE,
                                       splitPoints = c(),
                                       allowRegularization = FALSE) {
  if (!grepl("start$|end$", startAnchor, ignore.case = TRUE)) {
    stop("startAnchor should have value 'era start' or 'era end'")
  }
  if (!grepl("start$|end$", endAnchor, ignore.case = TRUE)) {
    stop("endAnchor should have value 'era start' or 'era end'")
  }
  isEnd <- function(anchor) {
    return(grepl("end$", anchor, ignore.case = TRUE))
  }
  if (end < start && !isEnd(endAnchor))
    stop("End day always precedes start day. Either pick a later end day, or set endAnchor to 'era end'.")

  # Make sure string is exact:
  if (isEnd(startAnchor)) {
    startAnchor <- "era end"
  } else {
    startAnchor <- "era start"
  }
  if (isEnd(endAnchor)) {
    endAnchor <- "era end"
  } else {
    endAnchor <- "era start"
  }
  analysis <- list()
  for (name in names(formals(createEraCovariateSettings))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "EraCovariateSettings"
  return(analysis)
}

#' Create age covariate settings
#'
#' @details
#' Create an object specifying whether and how age should be included in the model. Age can be included
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
#'                                    model. Will be turned to FALSE  automatically when `allowRegularization = TRUE`.
#'
#' @return
#' An object of type `AgeCovariateSettings`.
#'
#' @export
createAgeCovariateSettings <- function(ageKnots = 5,
                                       allowRegularization = FALSE,
                                       computeConfidenceIntervals = FALSE) {
  if (computeConfidenceIntervals && allowRegularization) {
    computeConfidenceIntervals <- FALSE
    warning("computeConfidenceIntervals is set to FALSE because allowRegularization is TRUE")
  }
  analysis <- list()
  for (name in names(formals(createAgeCovariateSettings))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "ageSettings"
  return(analysis)
}

#' Create seasonality settings
#'
#' @details
#' Create an object specifying whether and how seasonality should be included in the model. Seasonality
#' can be included by splitting patient time into calendar months. During a month, the relative risk
#' attributed to season is assumed to be constant, and the risk from month to month is modeled using a
#' cyclic cubic spline.
#'
#' @param includeSeasonality    Should seasonality be included in the model?
#' @param seasonKnots           If a single number is provided this is assumed to indicate the number
#'                              of knots to use for the spline, and the knots are automatically equally
#'                              spaced across the year. If more than one number is provided these are
#'                              assumed to be the exact location of the knots in days relative to the
#'                              start of the year.
#' @param allowRegularization   When fitting the model, should the covariates defined here be allowed
#'                              to be regularized?
#' @param computeConfidenceIntervals  Should confidence intervals be computed for the covariates defined
#'                                    here? Setting this to FALSE might save computing time when fitting the
#'                                    model. Will be turned to FALSE  automatically when `allowRegularization = TRUE`.
#'
#' @return
#' An object of type `seasonalitySettings`.
#'
#' @export
createSeasonalityCovariateSettings <- function(seasonKnots = 5,
                                               allowRegularization = FALSE,
                                               computeConfidenceIntervals = FALSE) {
  if (computeConfidenceIntervals && allowRegularization) {
    computeConfidenceIntervals <- FALSE
    warning("computeConfidenceIntervals is set to FALSE because allowRegularization is TRUE")
  }
  analysis <- list()
  for (name in names(formals(createSeasonalityCovariateSettings))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "SeasonalityCovariateSettings"
  return(analysis)
}
