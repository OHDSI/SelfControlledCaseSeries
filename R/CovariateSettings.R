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

#' Create era covariate settings
#'
#' @details
#' Create an object specifying how to create a (set of) era-based covariates.
#'
#' @param includeEraIds         One or more IDs of variables in the [SccsData] object that should be
#'                              used to construct this covariate. If set to NULL, all
#'                              variables will be used.
#' @param excludeEraIds         One or more IDs of variables in the \[SccsData] object that should not
#'                              be used to construct this covariate.
#' @param label                 A label used to identify the covariates created using these settings.
#' @param stratifyById          Should a single covariate be created for every ID in the [SccsData]
#'                              object, or should a single covariate be constructed? For example, if
#'                              the IDs identify exposures to different drugs, should a covariate be
#'                              constructed for every drug, or a single covariate for exposure to any
#'                              of these drugs. Note that overlap will be considered a single exposure.
#' @param start                 The start of the risk window (in days) relative to the `startAnchor`.
#' @param startAnchor           The anchor point for the start of the risk window. Can be `"era start"`
#'                              or `"era end"`.
#' @param end                   The end of the risk window (in days) relative to the `endAnchor`.
#' @param endAnchor             The anchor point for the end of the risk window. Can be `"era start"`
#'                              or `"era end"`.
#' @param firstOccurrenceOnly   Should only the first occurrence of the exposure be used?
#' @param allowRegularization   When fitting the model, should the covariates defined here be allowed
#'                              to be regularized?
#' @param profileLikelihood     When fitting the model, should the likelihood profile be computed for
#'                              the covariate defined here? The likelihood profile can be used to
#'                              avoid making normal approximations on the likelihood and can be used in
#'                              methods specifically designed to make use of the profile, but may take a
#'                              while to compute.
#' @param exposureOfInterest    If TRUE, the fitted coefficient for this variable will be reported when
#'                              using `runSccsAnalyses()`. Requires `includeEraIds` to be a exposure
#'                              reference ID as defined in `createExposure()`.
#' @param preExposure           If TRUE, this variable will be used for the pre-exposure diagnostic.
#'
#' @return
#' An object of type `EraCovariateSettings`.
#'
#' @export
createEraCovariateSettings <- function(includeEraIds,
                                       excludeEraIds = NULL,
                                       label = "Covariates",
                                       stratifyById = FALSE,
                                       start = 0,
                                       startAnchor = "era start",
                                       end = 0,
                                       endAnchor = "era end",
                                       firstOccurrenceOnly = FALSE,
                                       allowRegularization = FALSE,
                                       profileLikelihood = FALSE,
                                       exposureOfInterest = FALSE,
                                       preExposure = start < 0) {
  errorMessages <- checkmate::makeAssertCollection()
  if (is.character(includeEraIds)) {
    checkmate::assertCharacter(includeEraIds, add = errorMessages)
  } else {
    checkmate::assertIntegerish(includeEraIds, null.ok = TRUE, add = errorMessages)
  }
  if (is.character(excludeEraIds)) {
    checkmate::assertCharacter(excludeEraIds, add = errorMessages)
  } else {
    checkmate::assertIntegerish(excludeEraIds, null.ok = TRUE, add = errorMessages)
  }
  checkmate::assertCharacter(label, len = 1, add = errorMessages)
  checkmate::assertLogical(stratifyById, len = 1, add = errorMessages)
  checkmate::assertInt(start, add = errorMessages)
  checkmate::assertChoice(startAnchor, c("era start", "era end"), add = errorMessages)
  checkmate::assertInt(end, add = errorMessages)
  checkmate::assertChoice(endAnchor, c("era start", "era end"), add = errorMessages)
  checkmate::assertLogical(firstOccurrenceOnly, len = 1, add = errorMessages)
  checkmate::assertLogical(allowRegularization, len = 1, add = errorMessages)
  checkmate::assertLogical(profileLikelihood, len = 1, add = errorMessages)
  checkmate::assertLogical(exposureOfInterest, len = 1, add = errorMessages)
  checkmate::assertLogical(exposureOfInterest, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (allowRegularization && profileLikelihood) {
    stop("Cannot profile the likelihood of regularized covariates")
  }
  if (end < start && endAnchor == "era start") {
    stop("End day always precedes start day. Either pick a later end day, or set endAnchor to 'era end'.")
  }
  if (exposureOfInterest && !(is.character(includeEraIds) && length(includeEraIds) == 1)) {
    stop(
      "The exposureOfInterest argument can only be set to TRUE if includeEraIds ",
      "contains a single exposure reference ID as defined in createExposure()."
    )
  }
  analysis <- list()
  for (name in names(formals(createEraCovariateSettings))) {
    analysis[[name]] <- get(name)
  }
  analysis$isControlInterval <- FALSE
  class(analysis) <- "EraCovariateSettings"
  return(analysis)
}

#' Create age covariate settings
#'
#' @details
#' Create an object specifying whether and how age should be included in the model. Age can be
#' included by splitting patient time into calendar months. During a month, the relative risk
#' attributed to age is assumed to be constant, and the risk from month to month is modeled using a
#' quadratic spline.
#'
#' @param ageKnots                     If a single number is provided this is assumed to indicate the
#'                                     number of knots to use for the spline, and the knots are
#'                                     automatically spaced according to equal percentiles of the data.
#'                                     If more than one number is provided these are assumed to be the
#'                                     exact location of the knots in age-days
#' @param allowRegularization          When fitting the model, should the covariates defined here be
#'                                     allowed to be regularized?
#' @param computeConfidenceIntervals   Should confidence intervals be computed for the covariates
#'                                     defined here? Setting this to FALSE might save computing time
#'                                     when fitting the model. Will be turned to FALSE automatically
#'                                     when `allowRegularization = TRUE`.
#'
#' @return
#' An object of type `AgeCovariateSettings`.
#'
#' @export
createAgeCovariateSettings <- function(ageKnots = 5,
                                       allowRegularization = FALSE,
                                       computeConfidenceIntervals = FALSE) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertInt(ageKnots, lower = 1, add = errorMessages)
  checkmate::assertLogical(allowRegularization, len = 1, add = errorMessages)
  checkmate::assertLogical(computeConfidenceIntervals, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
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
#' Create an object specifying whether and how seasonality should be included in the model.
#' Seasonality can be included by splitting patient time into calendar months. During a month, the
#' relative risk attributed to season is assumed to be constant, and the risk from month to month is
#' modeled using a cyclic quadratic spline.
#'
#' @param seasonKnots                  If a single number is provided this is assumed to indicate the
#'                                     number of knots to use for the spline, and the knots are
#'                                     automatically equally spaced across the year. If more than one
#'                                     number is provided these are assumed to be the exact location of
#'                                     the knots in days relative to the start of the year.
#' @param allowRegularization          When fitting the model, should the covariates defined here be
#'                                     allowed to be regularized?
#' @param computeConfidenceIntervals   Should confidence intervals be computed for the covariates
#'                                     defined here? Setting this to FALSE might save computing time
#'                                     when fitting the model. Will be turned to FALSE automatically
#'                                     when `allowRegularization = TRUE`.
#'
#' @return
#' An object of type `seasonalitySettings`.
#'
#' @export
createSeasonalityCovariateSettings <- function(seasonKnots = 5,
                                               allowRegularization = FALSE,
                                               computeConfidenceIntervals = FALSE) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertInt(seasonKnots, lower = 1, add = errorMessages)
  checkmate::assertLogical(allowRegularization, len = 1, add = errorMessages)
  checkmate::assertLogical(computeConfidenceIntervals, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
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

#' Create calendar time settings
#'
#' @details
#' Create an object specifying whether and how calendar time should be included in the model.
#' Calendar time can be included by splitting patient time into calendar months. During a month, the
#' relative risk attributed to calendar time is assumed to be constant, and the risk from month to month is
#' modeled using a quadratic spline.
#'
#' Whereas the seasonality covariate uses a cyclic spline, repeating every year, this calendar time covariate
#' can model trends over years.
#'
#' @param calendarTimeKnots            If a single number is provided this is assumed to indicate the
#'                                     number of knots to use for the spline. See details on how knots are placed.
#'                                     If a series of dates is provided these are assumed to be the exact location of
#'                                     the knots.
#' @param allowRegularization          When fitting the model, should the covariates defined here be
#'                                     allowed to be regularized?
#' @param computeConfidenceIntervals   Should confidence intervals be computed for the covariates
#'                                     defined here? Setting this to FALSE might save computing time
#'                                     when fitting the model. Will be turned to FALSE automatically
#'                                     when `allowRegularization = TRUE`.
#'
#' @details
#' If a number of knots is specified, knots are automatically spaced according to equal percentiles of the data (people
#' observed). If more than one study period is provided, two more knots (start and end) are automatically added for each
#' additional study period. So if `calendarTimeKnots = 5` and there are 3 study periods, the total number of knots will
#' be 5 + 2 * (3 - 1) = 9.#'
#'
#' @return
#' An object of type `seasonalitySettings`.
#'
#' @export
createCalendarTimeCovariateSettings <- function(calendarTimeKnots = 5,
                                                allowRegularization = FALSE,
                                                computeConfidenceIntervals = FALSE) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertInt(calendarTimeKnots, lower = 1, add = errorMessages)
  checkmate::assertLogical(allowRegularization, len = 1, add = errorMessages)
  checkmate::assertLogical(computeConfidenceIntervals, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (length(calendarTimeKnots) != 1 && !is(calendarTimeKnots, "Date")) {
    stop("The 'calendarTimeKnots' should be either a single integer or a vector of dates.")
  }
  if (computeConfidenceIntervals && allowRegularization) {
    computeConfidenceIntervals <- FALSE
    warning("computeConfidenceIntervals is set to FALSE because allowRegularization is TRUE")
  }
  analysis <- list()
  for (name in names(formals(createCalendarTimeCovariateSettings))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "CalendarTimeCovariateSettings"
  return(analysis)
}

#' Create control interval settings
#'
#' @details
#' Create an object specifying how to create a control interval for the self-controlled risk interval (SCRI)
#' design.
#'
#' @param includeEraIds         One or more IDs of variables in the [SccsData] object that should be
#'                              used to construct this covariate. If no IDs are specified, all
#'                              variables will be used.
#' @param excludeEraIds         One or more IDs of variables in the \[SccsData] object that should not
#'                              be used to construct this covariate.
#' @param start                 The start of the control interval (in days) relative to the `startAnchor`.
#' @param startAnchor           The anchor point for the start of the control interval. Can be `"era start"`
#'                              or `"era end"`.
#' @param end                   The end of the control interval (in days) relative to the `endAnchor`.
#' @param endAnchor             The anchor point for the end of the control interval. Can be `"era start"`
#'                              or `"era end"`.
#' @param firstOccurrenceOnly   Should only the first occurrence of the exposure be used?
#'
#' @return
#' An object of type `ControlSettings`.
#'
#' @export
createControlIntervalSettings <- function(includeEraIds = NULL,
                                          excludeEraIds = NULL,
                                          start = 0,
                                          startAnchor = "era start",
                                          end = 0,
                                          endAnchor = "era end",
                                          firstOccurrenceOnly = FALSE) {
  errorMessages <- checkmate::makeAssertCollection()
  if (is.character(includeEraIds)) {
    checkmate::assertCharacter(includeEraIds, add = errorMessages)
  } else {
    checkmate::assertIntegerish(includeEraIds, null.ok = TRUE, add = errorMessages)
  }
  if (is.character(excludeEraIds)) {
    checkmate::assertCharacter(excludeEraIds, add = errorMessages)
  } else {
    checkmate::assertIntegerish(excludeEraIds, null.ok = TRUE, add = errorMessages)
  }
  checkmate::assertInt(start, add = errorMessages)
  checkmate::assertCharacter(startAnchor, len = 1, add = errorMessages)
  checkmate::assertInt(end, add = errorMessages)
  checkmate::assertCharacter(endAnchor, len = 1, add = errorMessages)
  checkmate::assertLogical(firstOccurrenceOnly, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (!grepl("start$|end$", startAnchor, ignore.case = TRUE)) {
    stop("startAnchor should have value 'era start' or 'era end'")
  }
  if (!grepl("start$|end$", endAnchor, ignore.case = TRUE)) {
    stop("endAnchor should have value 'era start' or 'era end'")
  }
  isEnd <- function(anchor) {
    return(grepl("end$", anchor, ignore.case = TRUE))
  }
  if (end < start && !isEnd(endAnchor)) {
    stop("End day always precedes start day. Either pick a later end day, or set endAnchor to 'era end'.")
  }

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
  analysis <- createEraCovariateSettings(
    includeEraIds = includeEraIds,
    excludeEraIds = excludeEraIds,
    label = "Control interval",
    stratifyById = FALSE,
    start = start,
    startAnchor = startAnchor,
    end = end,
    endAnchor = endAnchor,
    firstOccurrenceOnly = firstOccurrenceOnly,
    allowRegularization = FALSE,
    preExposure = FALSE
  )
  analysis$isControlInterval <- TRUE
  class(analysis) <- "ControlIntervalSettings"
  return(analysis)
}
