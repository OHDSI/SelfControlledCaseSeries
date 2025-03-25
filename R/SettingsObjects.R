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

# library(R6)
# library(Cyclops)

.recurseToList <- function(x) {
  if ("toList" %in% names(x)) {
    return(x$toList())
  } else if (is.list(x)) {
    for (i in seq_along(x)) {
      # If member is NULL then assigning NULL will remove it from the list altogether.
      if (!is.null(x[[i]])) {
        x[[i]] <- .recurseToList(x[[i]])
      }
    }
    if (is.object(x)) {
      # jsonlite::toJSON throws error on S3 objects
      class(x) <- NULL
    }
    return(x)
  } else {
    return(x)
  }
}

AbstractSerializableSettings <- R6Class(
  "AbstractSerializableSettings",
  public = list(
    initialize = function(typedList, untypedList, json) {
      if (!missing(typedList)) {
        # Initialize with list where objects already have correct type
        args <- typedList
        requireTyping <- FALSE
      } else if (!missing(untypedList)) {
        # Initialize with list where objects are still lists, amd need to be converted to right type.
        args <- untypedList
        requireTyping <- TRUE
      } else if (!missing(json)) {
        # Initialize with a JSON string
        args <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
        requireTyping <- TRUE
      } else {
        stop("Must provide either typedList, untypedList, or json argument to constructor")
      }
      self$fromList(args, requireTyping)
      self$validate()
    },
    toList = function() {
      # Using is(x, "environment") instead of is.environment(x) because former is FALSE for R6 objects.
      asList <- Filter(function(x) !is.function(x) & !is(x, "environment"), as.list(self))
      asList <- .recurseToList(asList)
      return(asList)
    },
    toJson  = function() {
      jsonlite::toJSON(
        self$toList(),
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null"
      )
    },
    fromList = function(list, requireTyping) {
      for (name in names(list)) {
        if (name %in% names(self)) {
          self[[name]] <- list[[name]]
        }
      }
    }
  )
)

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
#'                              using [runSccsAnalyses()]. Requires `includeEraIds` to be a exposure
#'                              reference ID as defined in [createExposure()].
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
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(EraCovariateSettings$new(typedList = args))
}

EraCovariateSettings <- R6Class(
  "EraCovariateSettings",
  inherit = AbstractSerializableSettings,
  public = list(
    includeEraIds = NULL,
    excludeEraIds = NULL,
    label = NULL,
    stratifyById = NULL,
    start = NULL,
    startAnchor = NULL,
    end = NULL,
    endAnchor = NULL,
    firstOccurrenceOnly = NULL,
    allowRegularization = NULL,
    profileLikelihood = NULL,
    exposureOfInterest = NULL,
    preExposure = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      if (is.character(self$includeEraIds)) {
        checkmate::assertCharacter(self$includeEraIds, add = errorMessages)
      } else {
        checkmate::assertIntegerish(self$includeEraIds, null.ok = TRUE, add = errorMessages)
      }
      if (is.character(self$excludeEraIds)) {
        checkmate::assertCharacter(self$excludeEraIds, add = errorMessages)
      } else {
        checkmate::assertIntegerish(self$excludeEraIds, null.ok = TRUE, add = errorMessages)
      }
      checkmate::assertCharacter(self$label, len = 1, add = errorMessages)
      checkmate::assertLogical(self$stratifyById, len = 1, add = errorMessages)
      checkmate::assertInt(self$start, add = errorMessages)
      checkmate::assertChoice(self$startAnchor, c("era start", "era end"), add = errorMessages)
      checkmate::assertInt(self$end, add = errorMessages)
      checkmate::assertChoice(self$endAnchor, c("era start", "era end"), add = errorMessages)
      checkmate::assertLogical(self$firstOccurrenceOnly, len = 1, add = errorMessages)
      checkmate::assertLogical(self$allowRegularization, len = 1, add = errorMessages)
      checkmate::assertLogical(self$profileLikelihood, len = 1, add = errorMessages)
      checkmate::assertLogical(self$exposureOfInterest, len = 1, add = errorMessages)
      checkmate::assertLogical(self$exposureOfInterest, len = 1, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      if (self$allowRegularization && self$profileLikelihood) {
        stop("Cannot profile the likelihood of regularized covariates")
      }
      if (self$end < self$start && self$endAnchor == "era start") {
        stop("End day always precedes start day. Either pick a later end day, or set endAnchor to 'era end'.")
      }
      if (self$exposureOfInterest && !(is.character(self$includeEraIds) && length(self$includeEraIds) == 1)) {
        stop(
          "The exposureOfInterest argument can only be set to TRUE if includeEraIds ",
          "contains a single exposure reference ID as defined in createExposure()."
        )
      }
    }
  )
)

#' Create age covariate sesettings#' Create age covariate settings
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
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(AgeCovariateSettings$new(typedList = args))
}

AgeCovariateSettings <- R6Class(
  "AgeCovariateSettings",
  inherit = AbstractSerializableSettings,
  public = list(
    ageKnots = NULL,
    allowRegularization = NULL,
    computeConfidenceIntervals = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertInt(self$ageKnots, lower = 1, add = errorMessages)
      checkmate::assertLogical(self$allowRegularization, len = 1, add = errorMessages)
      checkmate::assertLogical(self$computeConfidenceIntervals, len = 1, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      if (self$allowRegularization && self$computeConfidenceIntervals) {
        stop("Cannot have both allowRegularization and computeConfidenceIntervals be TRUE")
      }
    }
  )
)

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
#' An object of type `SeasonalityCovariateSettings`.
#'
#' @export
createSeasonalityCovariateSettings <- function(seasonKnots = 5,
                                               allowRegularization = FALSE,
                                               computeConfidenceIntervals = FALSE) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(SeasonalityCovariateSettings$new(typedList = args))
}

SeasonalityCovariateSettings <- R6Class(
  "SeasonalityCovariateSettings",
  inherit = AbstractSerializableSettings,
  public = list(
    seasonKnots = NULL,
    allowRegularization = NULL,
    computeConfidenceIntervals = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertInt(self$seasonKnots, lower = 1, add = errorMessages)
      checkmate::assertLogical(self$allowRegularization, len = 1, add = errorMessages)
      checkmate::assertLogical(self$computeConfidenceIntervals, len = 1, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      if (self$allowRegularization && self$computeConfidenceIntervals) {
        stop("Cannot have both allowRegularization and computeConfidenceIntervals be TRUE")
      }
    }
  )
)

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
#' An object of type `CalendarTimeCovariateSettings`.
#'
#' @export
createCalendarTimeCovariateSettings <- function(calendarTimeKnots = 5,
                                                allowRegularization = FALSE,
                                                computeConfidenceIntervals = FALSE) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(CalendarTimeCovariateSettings$new(typedList = args))
}

CalendarTimeCovariateSettings <- R6Class(
  "CalendarTimeCovariateSettings",
  inherit = AbstractSerializableSettings,
  public = list(
    calendarTimeKnots = NULL,
    allowRegularization = NULL,
    computeConfidenceIntervals = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertInt(self$calendarTimeKnots, lower = 1, add = errorMessages)
      checkmate::assertLogical(self$allowRegularization, len = 1, add = errorMessages)
      checkmate::assertLogical(self$computeConfidenceIntervals, len = 1, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      if (self$allowRegularization && self$computeConfidenceIntervals) {
        stop("Cannot have both allowRegularization and computeConfidenceIntervals be TRUE")
      }
    }
  )
)

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
#' An object of type `ControlIntervalSettings`.
#'
#' @export
createControlIntervalSettings <- function(includeEraIds = NULL,
                                          excludeEraIds = NULL,
                                          start = 0,
                                          startAnchor = "era start",
                                          end = 0,
                                          endAnchor = "era end",
                                          firstOccurrenceOnly = FALSE) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(ControlIntervalSettings$new(typedList = args))
}

ControlIntervalSettings <- R6Class(
  "ControlIntervalSettings",
  inherit = AbstractSerializableSettings,
  public = list(
    includeEraIds = NULL,
    excludeEraIds = NULL,
    start = NULL,
    startAnchor = NULL,
    end = NULL,
    endAnchor = NULL,
    firstOccurrenceOnly = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      if (is.character(self$includeEraIds)) {
        checkmate::assertCharacter(self$includeEraIds, add = errorMessages)
      } else {
        checkmate::assertIntegerish(self$includeEraIds, null.ok = TRUE, add = errorMessages)
      }
      if (is.character(self$excludeEraIds)) {
        checkmate::assertCharacter(self$excludeEraIds, add = errorMessages)
      } else {
        checkmate::assertIntegerish(self$excludeEraIds, null.ok = TRUE, add = errorMessages)
      }
      checkmate::assertInt(self$start, add = errorMessages)
      checkmate::assertChoice(self$startAnchor, c("era start", "era end"), add = errorMessages)
      checkmate::assertInt(self$end, add = errorMessages)
      checkmate::assertChoice(self$endAnchor, c("era start", "era end"), add = errorMessages)
      checkmate::assertLogical(self$firstOccurrenceOnly, len = 1, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      if (self$end < self$start && self$endAnchor == "era start") {
        stop("End day always precedes start day. Either pick a later end day, or set endAnchor to 'era end'.")
      }
    }
  )
)

#' Create a parameter object for the function getDbSccsData
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param nestingCohortId  A cohort definition ID identifying the records in the nestingCohortTable to use as nesting cohort.
#' @param deleteCovariatesSmallCount  The minimum count for a covariate to appear in the data to be kept.
#' @param studyStartDates  A character object specifying the minimum dates where data is used. Date format is 'yyyymmdd'. Use "" to indicate all time prior. See section for more information.
#' @param studyEndDates  A character object specifying the maximum dates where data is used. Date format is 'yyyymmdd'. Use "" to indicate to the end of observation. See section for more information.
#' @param maxCasesPerOutcome  If there are more than this number of cases for a single outcome cases will be sampled to this size. maxCasesPerOutcome = 0 indicates no maximum size.
#' @param exposureIds  A list of identifiers to extract from the exposure table. If exposureTable = DRUG_ERA, exposureIds should be CONCEPT_ID. If exposureTable = "drug_era", exposureIds is used to select the drug_concept_id. If no exposure IDs are provided, all drugs or cohorts in the exposureTable are included as exposures.
#' @param customCovariateIds  A list of cohort definition IDs identifying the records in the customCovariateTable to use for building custom covariates.
#'
#' @return
#' An object of type `GetDbSccsDataArgs`.
#'
#' @export
createGetDbSccsDataArgs <- function(nestingCohortId = NULL,
                                    deleteCovariatesSmallCount = 0,
                                    studyStartDates = c(),
                                    studyEndDates = c(),
                                    maxCasesPerOutcome = 0,
                                    exposureIds = "exposureId",
                                    customCovariateIds = NULL) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(GetDbSccsDataArgs$new(typedList = args))
}

GetDbSccsDataArgs <- R6Class(
  "GetDbSccsDataArgs",
  inherit = AbstractSerializableSettings,
  public = list(
    nestingCohortId = NULL,
    deleteCovariatesSmallCount = NULL,
    studyStartDates = NULL,
    studyEndDates = NULL,
    maxCasesPerOutcome = NULL,
    exposureIds = NULL,
    customCovariateIds = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertInt(self$nestingCohortId, null.ok = TRUE, add = errorMessages)
      checkmate::assertInt(self$deleteCovariatesSmallCount, lower = 0, add = errorMessages)
      checkmate::assertCharacter(self$studyStartDates, null.ok = TRUE, add = errorMessages)
      checkmate::assertCharacter(self$studyEndDates, null.ok = TRUE, add = errorMessages)
      checkmate::assertInt(self$maxCasesPerOutcome, lower = 0, add = errorMessages)
      if (is.character(self$exposureIds)) {
        checkmate::assertCharacter(self$exposureIds, add = errorMessages)
      } else {
        checkmate::assertIntegerish(self$exposureIds, null.ok = TRUE, add = errorMessages)
      }
      checkmate::assertIntegerish(self$customCovariateIds, null.ok = TRUE, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      if (length(self$estudyStartDates) != length(self$estudyEndDates)) {
        stop("The studyStartDates and studyEndDates arguments must be of equal length")
      }
      for (studyStartDate in self$studyStartDates) {
        if (regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", studyStartDate) == -1) {
          stop("Study start date must have format YYYYMMDD")
        }
      }
      for (studyEndDate in self$studyEndDates) {
        if (regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", studyEndDate) == -1) {
          stop("Study end date must have format YYYYMMDD")
        }
      }
    }
  )
)

#' Create a parameter object for the [createStudyPopulation()] function
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param firstOutcomeOnly  Whether only the first occurrence of an outcome should be considered.
#' @param naivePeriod  The number of days at the start of a patient's observation period that should not be included in the risk calculations. Note that the naive period can be used to determine current covariate status right after the naive period, and whether an outcome is the first one.
#' @param minAge  Minimum age at which patient time will be included in the analysis. Note that information prior to the min age is still used to determine exposure status after the minimum age (e.g. when a prescription was started just prior to reaching the minimum age). Also, outcomes occurring before the minimum age is reached will be considered as prior outcomes when using first outcomes only. Age should be specified in years, but non-integer values are allowed. If not specified, no age restriction will be applied.
#' @param maxAge  Maximum age at which patient time will be included in the analysis. Age should be specified in years, but non-integer values are allowed. If not specified, no age restriction will be applied.
#' @param genderConceptIds  Set of gender concept IDs to restrict the population to. If not specified, no restriction on gender will be applied.
#' @param restrictTimeToEraId  If provided, study time (for all patients) will be restricted to the calender time when that era was observed in the data. For example, if the era ID refers to a drug, study time will be restricted to when the drug was on the market.
#'
#' @return
#' An object of type `CreateStudyPopulationArgs`.
#'
#' @export
createCreateStudyPopulationArgs <- function(firstOutcomeOnly = FALSE,
                                            naivePeriod = 0,
                                            minAge = NULL,
                                            maxAge = NULL,
                                            genderConceptIds = NULL,
                                            restrictTimeToEraId = NULL) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(CreateStudyPopulationArgs$new(typedList = args))
}

CreateStudyPopulationArgs <- R6Class(
  "CreateStudyPopulationArgs",
  inherit = AbstractSerializableSettings,
  public = list(
    firstOutcomeOnly = NULL,
    naivePeriod = NULL,
    minAge = NULL,
    maxAge = NULL,
    genderConceptIds = NULL,
    restrictTimeToEraId = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertLogical(self$firstOutcomeOnly, len = 1, add = errorMessages)
      checkmate::assertInt(self$naivePeriod, lower = 0, add = errorMessages)
      checkmate::assertNumeric(self$minAge, lower = 0, len = 1, null.ok = TRUE, add = errorMessages)
      checkmate::assertNumeric(self$maxAge, lower = 0, len = 1, null.ok = TRUE, add = errorMessages)
      checkmate::assertIntegerish(self$genderConceptIds, lower = 0, null.ok = TRUE, add = errorMessages)
      if (is.character(self$restrictTimeToEraId)) {
        checkmate::assertCharacter(self$restrictTimeToEraId, add = errorMessages)
      } else {
        checkmate::assertIntegerish(self$restrictTimeToEraId, null.ok = TRUE, add = errorMessages)
      }
      checkmate::reportAssertions(collection = errorMessages)
      if (!is.null(self$minAge) && !is.null(self$maxAge) && self$maxAge < self$minAge) {
        stop("Maxinum age should be greater than or equal to minimum age")
      }
    }
  )
)

#' Create a parameter object for the [createSccsIntervalData()] function
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param eraCovariateSettings  Either an object of type `EraCovariateSettings` as created using the [createEraCovariateSettings()] function, or a list of such objects.
#' @param ageCovariateSettings  An object of type `AgeCovariateSettings` as created using the [createAgeCovariateSettings()] function.
#' @param seasonalityCovariateSettings  An object of type `SeasonalityCovariateSettings` as created using the [createSeasonalityCovariateSettings()] function.
#' @param calendarTimeCovariateSettings  An object of type `CalendarTimeCovariateSettings` as created using the [createCalendarTimeCovariateSettings()] function.
#' @param minCasesForTimeCovariates  Minimum number of cases to use to fit age, season and calendar time splines. If needed (and available), cases that are not exposed will be included.
#' @param endOfObservationEraLength   Length in days of the probe that is inserted at the end of a patient's
#'                                    observation time. This probe will be used to test whether there is event-
#'                                    dependent observation end. Set to 0 to not include the probe.
#' @param eventDependentObservation  Should the extension proposed by Farrington et al. be used to adjust for event-dependent observation time?
#'
#' @references
#' Farrington, C. P., Anaya-Izquierdo, A., Whitaker, H. J., Hocine, M.N., Douglas, I., and Smeeth, L.
#' (2011). Self-Controlled case series analysis with event-dependent observation periods. Journal of
#' the American Statistical Association 106 (494), 417-426
#'
#' @return
#' An object of type `CreateSccsIntervalDataArgs`.
#'
#' @export
createCreateSccsIntervalDataArgs <- function(eraCovariateSettings,
                                             ageCovariateSettings = NULL,
                                             seasonalityCovariateSettings = NULL,
                                             calendarTimeCovariateSettings = NULL,
                                             minCasesForTimeCovariates = 10000,
                                             endOfObservationEraLength = 30,
                                             eventDependentObservation = FALSE) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(CreateSccsIntervalDataArgs$new(typedList = args))
}

CreateSccsIntervalDataArgs <- R6Class(
  "CreateSccsIntervalDataArgs",
  inherit = AbstractSerializableSettings,
  public = list(
    eraCovariateSettings = NULL,
    ageCovariateSettings = NULL,
    seasonalityCovariateSettings = NULL,
    calendarTimeCovariateSettings = NULL,
    minCasesForTimeCovariates = NULL,
    endOfObservationEraLength = NULL,
    eventDependentObservation = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      if (is.list(self$eraCovariateSettings)) {
        for (i in seq_along(self$eraCovariateSettings)) {
          checkmate::assertClass(self$eraCovariateSettings[[i]], "EraCovariateSettings", add = errorMessages)
        }
      } else {
        checkmate::assertClass(self$eraCovariateSettings, "EraCovariateSettings", add = errorMessages)
      }
      checkmate::assertClass(self$ageCovariateSettings, "AgeCovariateSettings", null.ok = TRUE, add = errorMessages)
      checkmate::assertClass(self$seasonalityCovariateSettings, "SeasonalityCovariateSettings", null.ok = TRUE, add = errorMessages)
      checkmate::assertClass(self$calendarTimeCovariateSettings, "CalendarTimeCovariateSettings", null.ok = TRUE, add = errorMessages)
      checkmate::assertInt(self$minCasesForTimeCovariates, lower = 1, add = errorMessages)
      checkmate::assertInt(self$minCasesForTimeCovariates, lower = 0, add = errorMessages)
      checkmate::assertLogical(self$eventDependentObservation, len = 1, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
    },
    fromList = function(list, requireTyping) {
      super$fromList(list)
      if (requireTyping) {
        if (is.null(names(self$eraCovariateSettings))) {
          for (i in seq_along(self$eraCovariateSettings)) {
            self$eraCovariateSettings[[i]] <- EraCovariateSettings$new(untypedList = self$eraCovariateSettings[[i]])
          }
        } else {
          self$eraCovariateSettings <- EraCovariateSettings$new(untypedList = self$eraCovariateSettings)
        }
        self$ageCovariateSettings <- if (is.null(self$ageCovariateSettings)) NULL else AgeCovariateSettings$new(untypedList = self$ageCovariateSettings)
        self$seasonalityCovariateSettings <- if (is.null(self$seasonalityCovariateSettings)) NULL else SeasonalityCovariateSettings$new(untypedList = self$seasonalityCovariateSettings)
        self$calendarTimeCovariateSettings <- if (is.null(self$calendarTimeCovariateSettings)) NULL else CalendarTimeCovariateSettings$new(untypedList = self$calendarTimeCovariateSettings)
      }
    }
  )
)

#' Create a parameter object for the [createScriIntervalData()] function
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param eraCovariateSettings  Either an object of type `EraCovariateSettings` as created using the [createEraCovariateSettings()] function, or a list of such objects.
#' @param controlIntervalSettings  An object of type `ControlIntervalSettings` as created using the [createControlIntervalSettings()] function.
#'
#' @return
#' An object of type `CreateScriIntervalDataArgs`.
#'
#' @export
createCreateScriIntervalDataArgs <- function(eraCovariateSettings,
                                             controlIntervalSettings) {

  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(CreateScriIntervalDataArgs$new(typedList = args))
}

CreateScriIntervalDataArgs <- R6Class(
  "CreateScriIntervalDataArgs",
  inherit = AbstractSerializableSettings,
  public = list(
    eraCovariateSettings = NULL,
    controlIntervalSettings = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      if (is.list(self$eraCovariateSettings)) {
        for (i in seq_along(self$eraCovariateSettings)) {
          checkmate::assertClass(self$eraCovariateSettings[[i]], "EraCovariateSettings", add = errorMessages)
        }
      } else {
        checkmate::assertClass(self$eraCovariateSettings, "EraCovariateSettings", add = errorMessages)
      }
      checkmate::assertClass(self$controlIntervalSettings, "ControlIntervalSettings", null.ok = TRUE, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
    },
    fromList = function(list, requireTyping) {
      super$fromList(list)
      if (requireTyping) {
        if (is.null(names(self$eraCovariateSettings))) {
          for (i in seq_along(self$eraCovariateSettings)) {
            self$eraCovariateSettings[[i]] <- EraCovariateSettings$new(untypedList = self$eraCovariateSettings[[i]])
          }
        } else {
          self$eraCovariateSettings <- EraCovariateSettings$new(untypedList = self$eraCovariateSettings)
        }
        self$controlIntervalSettings <- if (is.null(self$controlIntervalSettings)) NULL else ControlIntervalSettings$new(untypedList = self$controlIntervalSettings)
      }
    }
  )
)

#' Create a parameter object for the function fitSccsModel
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param prior  The prior used to fit the model. See Cyclops::createPrior for details.
#' @param control  The control object used to control the cross-validation used to determine the hyperparameters of the prior (if applicable). See Cyclops::createControl for details.
#' @param profileGrid  A one-dimensional grid of points on the log(relative risk) scale where the likelihood for coefficient of variables is sampled. See details.
#' @param profileBounds  The bounds (on the log relative risk scale) for the adaptive sampling of the likelihood function.
#'
#' @return
#' An object of type `FitSccsModelArgs`.
#'
#' @export
createFitSccsModelArgs <- function(prior = createPrior("laplace", useCrossValidation = TRUE),
                                   control = createControl(cvType = "auto", selectorType = "byPid", startingVariance = 0.1, seed = 1, resetCoefficients = TRUE, noiseLevel = "quiet"),
                                   profileGrid = NULL,
                                   profileBounds = c(log(0.1), log(10))) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(FitSccsModelArgs$new(typedList = args))
}

FitSccsModelArgs <- R6Class(
  "FitSccsModelArgs",
  inherit = AbstractSerializableSettings,
  public = list(
    prior = NULL,
    control = NULL,
    profileGrid = NULL,
    profileBounds = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertClass(self$prior, "cyclopsPrior", add = errorMessages)
      checkmate::assertClass(self$control, "cyclopsControl", add = errorMessages)
      checkmate::assertNumeric(self$profileGrid, null.ok = TRUE, add = errorMessages)
      checkmate::assertNumeric(self$profileBounds, null.ok = TRUE, len = 2, sorted = TRUE, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      if (!is.null(self$profileGrid) && !is.null(self$profileBounds)) {
        stop("Specify either profileGrid or profileBounds")
      }
    },
    fromList = function(list, requireTyping) {
      super$fromList(list)
      if (requireTyping) {
        class(self$control) <- "cyclopsControl"
        class(self$prior) <- "cyclopsPrior"
      }
    }
  )
)

#' Create a SelfControlledCaseSeries analysis specification
#'
#' @param analysisId                   An integer that will be used later to refer to this specific set
#'                                     of analysis choices.
#' @param description                  A short description of the analysis.
#' @param getDbSccsDataArgs            An object representing the arguments to be used when calling the
#'                                     [getDbSccsData] function.
#' @param createStudyPopulationArgs    An object representing the arguments to be used when calling the
#'                                     [getDbSccsData] function.
#' @param createIntervalDataArgs       An object representing the arguments to be used when calling the
#'                                     [createSccsIntervalData] or [createScriIntervalData] function.
#' @param fitSccsModelArgs             An object representing the arguments to be used when calling the
#'                                     [fitSccsModel] function.
#'
#' @return
#' An object of type `SccsAnalysis`, to be used with the [runSccsAnalyses] function.
#'
#' @export
createSccsAnalysis <- function(analysisId = 1,
                               description = "",
                               getDbSccsDataArgs,
                               createStudyPopulationArgs,
                               createIntervalDataArgs,
                               fitSccsModelArgs) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(SccsAnalysis$new(typedList = args))
}

SccsAnalysis <- R6Class(
  "SccsAnalysis",
  inherit = AbstractSerializableSettings,
  public = list(
    analysisId = NULL,
    description = NULL,
    getDbSccsDataArgs = NULL,
    createStudyPopulationArgs = NULL,
    createIntervalDataArgs = NULL,
    fitSccsModelArgs = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertInt(self$analysisId, add = errorMessages)
      checkmate::assertCharacter(self$description, len = 1, add = errorMessages)
      checkmate::assertClass(self$getDbSccsDataArgs, "GetDbSccsDataArgs", add = errorMessages)
      checkmate::assertClass(self$createStudyPopulationArgs, "CreateStudyPopulationArgs", add = errorMessages)
      checkmate::assertClass(self$fitSccsModelArgs, "FitSccsModelArgs", add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      if (!is(self$createIntervalDataArgs, "CreateSccsIntervalDataArgs") &&
          !is(self$createIntervalDataArgs, "CreateScriIntervalDataArgs")) {
        stop("The createIntervalDataArgs argument must be either class 'CreateSccsIntervalDataArgs' or 'CreateScriIntervalDataArgs'")
      }
      covariateSettings <- self$createIntervalDataArgs$eraCovariateSettings
      hasExposureOfInterest <- FALSE
      if (is(covariateSettings, "EraCovariateSettings")) {
        covariateSettings <- list(covariateSettings)
      }
      for (covariateSetting in covariateSettings) {
        if (covariateSetting$exposureOfInterest) {
          hasExposureOfInterest <- TRUE
          break
        }
      }
      if (!hasExposureOfInterest) {
        stop("At least one of the era covariates must be the exposure of interest")
      }
    },
    fromList = function(list, requireTyping) {
      super$fromList(list)
      if (requireTyping) {
        self$getDbSccsDataArgs <- GetDbSccsDataArgs$new(untypedList = self$getDbSccsDataArgs)
        self$createStudyPopulationArgs <- CreateStudyPopulationArgs$new(untypedList =self$createStudyPopulationArgs)
        self$createIntervalDataArgs <- CreateSccsIntervalDataArgs$new(untypedList =self$createIntervalDataArgs)
        self$fitSccsModelArgs <- FitSccsModelArgs$new(untypedList =self$fitSccsModelArgs)
      }
    }
  )
)

settings <- createSccsAnalysis(
  analysisId = 1,
  description = "SCCS",
  getDbSccsDataArgs = createGetDbSccsDataArgs(),
  createStudyPopulationArgs = createCreateStudyPopulationArgs(),
  createIntervalDataArgs = createCreateSccsIntervalDataArgs(
    eraCovariateSettings = createEraCovariateSettings(includeEraIds = "exposureId", exposureOfInterest = TRUE)),
  fitSccsModelArgs = createFitSccsModelArgs()
)


#' Create exposure definition
#'
#' @details
#' Create an exposure definition, to be used with the [createExposuresOutcome] function.
#'
#' @param exposureId     An integer used to identify the exposure in the exposure cohort table.
#' @param exposureIdRef  A string used to refer to the exposure when defining covariates using the
#'                       [createEraCovariateSettings()] function.
#' @param trueEffectSize For negative and positive controls: the known true effect size. To be
#'                       used for empirical calibration. Negative controls have
#'                       `trueEffectSize = 1`. If the true effect size is unknown, use
#'                       `trueEffectSize = NA`.
#'
#' @return
#' An object of type `Exposure`.
#'
#' @export
createExposure <- function(exposureId, exposureIdRef = "exposureId", trueEffectSize = NA) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(Exposure$new(typedList = args))
}

Exposure <- R6Class(
  "Exposure",
  inherit = AbstractSerializableSettings,
  public = list(
    exposureId = NULL,
    exposureIdRef = NULL,
    trueEffectSize = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertInt(self$exposureId, add = errorMessages)
      checkmate::assertCharacter(self$exposureIdRef, len = 1, add = errorMessages)
      checkmate::assertNumeric(self$trueEffectSize, len = 1, null.ok = TRUE, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
    },
    fromList = function(list, requireTyping) {
      super$fromList(list)
      if (requireTyping) {
        self$trueEffectSize <- if (is.null(self$trueEffectSize)) NA else self$trueEffectSize
      }
    }
  )
)

#' Create a exposures-outcome combination.
#'
#' @details
#' Create a set of hypotheses of interest, to be used with the [runSccsAnalyses()] function.
#'
#' @param outcomeId       An integer used to identify the outcome in the outcome cohort table.
#' @param exposures       A list of object of type `Exposure` as created by [createExposure()].
#' @param nestingCohortId (Optional) the nesting cohort ID.
#'
#' @return
#' An object of type `ExposuresOutcome`.
#'
#' @export
createExposuresOutcome <- function(outcomeId, exposures, nestingCohortId = NULL) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(ExposuresOutcome$new(typedList = args))
}

ExposuresOutcome <- R6Class(
  "ExposuresOutcome",
  inherit = AbstractSerializableSettings,
  public = list(
    outcomeId = NULL,
    exposures = NULL,
    nestingCohortId = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertInt(self$outcomeId, add = errorMessages)
      checkmate::assertList(self$exposures, min.len = 1, add = errorMessages)
      checkmate::assertInt(self$nestingCohortId, null.ok = TRUE, add = errorMessages)
      for (i in seq_along(self$exposures)) {
        checkmate::assertClass(self$exposures[[i]], "Exposure", add = errorMessages)
      }
      checkmate::reportAssertions(collection = errorMessages)
      uniqueExposureIdRefs <- unique(sapply(self$exposures, function(x) x$exposureIdRef))
      if (length(uniqueExposureIdRefs) != length(self$exposures)) {
        stop("Duplicate exposureIdRefs are not allowed. Please give each exposure a unique exposureIdRef.")
      }
    },
    fromList = function(list, requireTyping) {
      super$fromList(list)
      if (requireTyping) {
        for (i in seq_along(self$exposures)) {
          self$exposures[[i]] <- Exposure$new(untypedList = self$exposures[[i]])
        }
      }
    }
  )
)

#' Create SCCS diagnostics thresholds
#'
#' @description
#' Threshold used when calling [runSccsAnalyses()] to determine if we pass or fail diagnostics.
#'
#' @param mdrrThreshold What is the maximum allowed minimum detectable relative risk (MDRR)?
#' @param easeThreshold What is the maximum allowed expected absolute systematic error (EASE).
#' @param timeTrendMaxRatio The maximum global ratio between the observed and expected count for the
#'                          time stability diagnostic.
#' @param rareOutcomeMaxPrevalence The maximum allowed prevalence (proportion of people with the
#'                                 outcome) allowed when restricting to first outcome only.
#' @param eventObservationDependenceNullBounds The bounds for the null hypothesis for the incidence
#'                                             rate ratio of the end-of-observation probe window.
#' @param eventExposureDependenceNullBounds The bounds for the null hypothesis for the incidence
#'                                          rate of the pre-exposure window.
#'
#' @return
#' An object of type `SccsDiagnosticThresholds`.
#'
#' @export
createSccsDiagnosticThresholds <- function(mdrrThreshold = 10,
                                           easeThreshold = 0.25,
                                           timeTrendMaxRatio = 1.1,
                                           rareOutcomeMaxPrevalence = 0.1,
                                           eventObservationDependenceNullBounds = c(0.5, 2.0),
                                           eventExposureDependenceNullBounds = c(0.8, 1.25)) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(SccsDiagnosticThresholds$new(typedList = args))
}

SccsDiagnosticThresholds <- R6Class(
  "SccsDiagnosticThresholds",
  inherit = AbstractSerializableSettings,
  public = list(
    mdrrThreshold = NULL,
    easeThreshold = NULL,
    timeTrendMaxRatio = NULL,
    rareOutcomeMaxPrevalence = NULL,
    eventObservationDependenceNullBounds = NULL,
    eventExposureDependenceNullBounds = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertNumber(self$mdrrThreshold, lower = 1, add = errorMessages)
      checkmate::assertNumber(self$easeThreshold, lower = 0, add = errorMessages)
      checkmate::assertNumber(self$timeTrendMaxRatio, lower = 1, add = errorMessages)
      checkmate::assertNumber(self$rareOutcomeMaxPrevalence, lower = 0, upper = 1, add = errorMessages)
      checkmate::assertNumeric(self$eventObservationDependenceNullBounds, len = 2, lower = 0, sorted = TRUE, add = errorMessages)
      checkmate::assertNumeric(self$eventExposureDependenceNullBounds, len = 2, lower = 0, sorted = TRUE, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
    }
  )
)

#' Create full SCCS analysis specifications
#'
#' @param sccsAnalysisList               A list of objects of type `SccsAnalysis` as created using the [createSccsAnalysis()] function.
#' @param exposuresOutcomeList           A list of objects of type `ExposuresOutcome` as created using the [createExposuresOutcome()] function.
#' @param analysesToExclude              Analyses to exclude. See the Analyses to Exclude section for details.
#' @param combineDataFetchAcrossOutcomes Should fetching data from the database be done one outcome
#'                                       at a time, or for all outcomes in one fetch? Combining
#'                                       fetches will be more efficient if there is large overlap in
#'                                       the subjects that have the different outcomes.
##' @param sccsDiagnosticThresholds      An object of type `SccsDiagnosticThresholds` as created using
#'                                       createSccsDiagnosticThresholds().
#' @param controlType                    Type of negative (and positive) controls. Can be "outcome" or
#'                                       "exposure". When set to "outcome", controls with the
#'                                       same exposure (and nesting cohort) are grouped together for
#'                                       calibration. When set to "exposure", controls with the same
#'                                       outcome are grouped together.
#'
#' @details
#' ## Analyses to Exclude
#'
#' Normally, `runSccsAnalyses` will run all combinations of exposures-outcome-analyses settings.
#' However, sometimes we may not need all those combinations. Using the `analysesToExclude` argument,
#' we can remove certain items from the full matrix. This argument should be a data frame with at least
#' one of the following columns:
#'
#' - exposureId
#' - outcomeId
#' - nestingCohortId
#' - analysisId
#'
#' This data frame will be joined to the outcome model reference table before executing, and matching rows
#' will be removed. For example, if one specifies only one exposure ID and analysis ID, then any analyses with
#' that exposure and that analysis ID will be skipped.
#'
#' @returns
#' An object of type `SccsAnalysesSpecifications`.
#'
#' @export
createSccsAnalysesSpecifications <- function(sccsAnalysisList,
                                             exposuresOutcomeList,
                                             analysesToExclude = NULL,
                                             combineDataFetchAcrossOutcomes = FALSE,
                                             sccsDiagnosticThresholds = SelfControlledCaseSeries::createSccsDiagnosticThresholds(),
                                             controlType = "outcome") {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(SccsAnalysesSpecifications$new(typedList = args))
}

SccsAnalysesSpecifications <- R6Class(
  "SccsAnalysesSpecifications",
  inherit = AbstractSerializableSettings,
  public = list(
    sccsAnalysisList = NULL,
    exposuresOutcomeList = NULL,
    analysesToExclude = NULL,
    combineDataFetchAcrossOutcomes = NULL,
    sccsDiagnosticThresholds = NULL,
    controlType = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertList(self$sccsAnalysisList, min.len = 1, add = errorMessages)
      for (i in seq_along(self$sccsAnalysisList)) {
        checkmate::assertClass(self$sccsAnalysisList[[i]], "SccsAnalysis", add = errorMessages)
      }
      checkmate::assertList(self$exposuresOutcomeList, min.len = 1, add = errorMessages)
      for (i in 1:length(self$exposuresOutcomeList)) {
        checkmate::assertClass(self$exposuresOutcomeList[[i]], "ExposuresOutcome", add = errorMessages)
      }
      checkmate::assertDataFrame(self$analysesToExclude, null.ok = TRUE, add = errorMessages)
      checkmate::assertLogical(self$combineDataFetchAcrossOutcomes, len = 1, add = errorMessages)
      checkmate::assertR6(self$sccsDiagnosticThresholds, "SccsDiagnosticThresholds", add = errorMessages)
      checkmate::assertChoice(self$controlType, c("outcome", "exposure"), add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      if (!is.null(self$analysesToExclude)) {
        if (!any(c("exposureId", "outcomeId", "analysisId", "nestingCohortId") %in% colnames(self$analysesToExclude))) {
          stop("AnalysesToExclude should have at least one of these columns: 'exposureId', 'outcomeId', 'analysisId', or 'nestingCohortId'")
        }
      }
    },
    fromList = function(list, requireTyping) {
      super$fromList(list)
      if (requireTyping) {
        for (i in seq_along(self$sccsAnalysisList)) {
          self$sccsAnalysisList[[i]] <- SccsAnalysis$new(untypedList = self$sccsAnalysisList[[i]])
        }
        for (i in seq_along(self$exposuresOutcomeList)) {
          self$exposuresOutcomeList[[i]] <- ExposuresOutcome$new(untypedList = self$exposuresOutcomeList[[i]])
        }
        if (!is.null(self$analysesToExclude)) {
          self$analysesToExclude <- as.data.frame(self$analysesToExclude)
        }
        self$sccsDiagnosticThresholds <- SccsDiagnosticThresholds$new(untypedList = self$sccsDiagnosticThresholds)
      }
    }
  )
)

# Loading, saving, conversion ----------------------------------------------------------------------

#' Save a list of SccsAnalysis to file
#'
#' @description
#' Write a list of objects of type `SccsAnalysis` to file. The file is in JSON format.
#'
#' @param sccsAnalysisList   A list of objects of type `SccsAnalysis` as created using the [createSccsAnalysis()] function.
#' @param file               The name of the file where the results will be written
#'
#' @export
saveSccsAnalysisList <- function(sccsAnalysisList, file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(sccsAnalysisList, min.len = 1, add = errorMessages)
  for (i in seq_along(sccsAnalysisList)) {
    checkmate::assertClass(sccsAnalysisList[[i]], "SccsAnalysis", add = errorMessages)
  }
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  for (i in seq_along(sccsAnalysisList)) {
    sccsAnalysisList[[i]] <- sccsAnalysisList[[i]]$toList()
  }
  json <- jsonlite::toJSON(
    sccsAnalysisList,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  file <- normalizePath(file, mustWork = FALSE)
  write(json, file)
}

#' Load a list of sccsAnalysis from file
#'
#' @description
#' Load a list of objects of type `SccsAnalysis` from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type `SccsAnalysis`.
#'
#' @export
loadSccsAnalysisList <- function(file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  file <- normalizePath(file)
  json <- readChar(file, file.info(file)$size)
  sccsAnalysisList <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
  for (i in seq_along(sccsAnalysisList)) {
    sccsAnalysisList[[i]] <- SccsAnalysis$new(untypedList = sccsAnalysisList[[i]])
  }
  return(sccsAnalysisList)
}

#' Save a list of `ExposuresOutcome` to file
#'
#' @description
#' Write a list of objects of type `ExposuresOutcome` to file. The file is in JSON format.
#'
#' @param exposuresOutcomeList  A list of objects of type `ExposuresOutcome` as created using the [createExposuresOutcome()] function.
#' @param file                  The name of the file where the results will be written
#'
#' @export
saveExposuresOutcomeList <- function(exposuresOutcomeList, file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(exposuresOutcomeList, min.len = 1, add = errorMessages)
  for (i in 1:length(exposuresOutcomeList)) {
    checkmate::assertClass(exposuresOutcomeList[[i]], "ExposuresOutcome", add = errorMessages)
  }
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  for (i in seq_along(exposuresOutcomeList)) {
    exposuresOutcomeList[[i]] <- exposuresOutcomeList[[i]]$toList()
  }
  json <- jsonlite::toJSON(
    exposuresOutcomeList,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  file <- normalizePath(file, mustWork = FALSE)
  write(json, file)
}

#' Load a list of `ExposuresOutcome` from file
#'
#' @description
#' Load a list of objects of type `ExposuresOutcome` from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type `ExposuresOutcome`.
#'
#' @export
loadExposuresOutcomeList <- function(file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  file <- normalizePath(file)
  json <- readChar(file, file.info(file)$size)
  exposuresOutcomeList <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
  for (i in seq_along(exposuresOutcomeList)) {
    exposuresOutcomeList[[i]] <- ExposuresOutcome$new(untypedList = exposuresOutcomeList[[i]])
  }
  return(exposuresOutcomeList)
}

#' Convert SccsAnalysesSpecifications to JSON
#'
#' @param sccsAnalysesSpecifications An object of type `SccsAnalysesSpecifications` as created by the [createSccsAnalysesSpecifications()] function.
#'
#' @returns
#' A string containing the JSON representation.
#'
#' @export
convertSccsAnalysesSpecificationsToJson <- function(sccsAnalysesSpecifications) {
  return(sccsAnalysesSpecifications$toJson())
}

#' Convert JSON to SccsAnalysesSpecifications
#'
#' @param json A string containing the JSON representation.
#'
#' @returns
#' An object of type `SccsAnalysesSpecifications`.
#'
#'
#' @export
convertJsonToSccsAnalysesSpecifications <- function(json) {
  return(SccsAnalysesSpecifications$new(json = json))
}
