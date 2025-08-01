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

#' Fit the SCCS model
#'
#' @details
#' Fits the SCCS model as a conditional Poisson regression. When allowed, coefficients for some or all
#' covariates can be regularized.
#'
#' Likelihood profiling is only done for variables for which `profileLikelihood` is set to `TRUE` when
#' calling [createEraCovariateSettings()]. Either specify the `profileGrid` for a completely user-
#' defined grid, or `profileBounds` for an adaptive grid. Both should be defined on the log IRR scale.
#' When both `profileGrid` and `profileGrid` are `NULL` likelihood profiling is disabled.
#'
#' @template SccsIntervalData
#' @param fitSccsModelArgs An object of type `FitSccsModelArgs` as created by the `createFitSccsModelArgs()` function.
#'
#' @return
#' An object of type `SccsModel`. Generic functions `print`, `coef`, and
#' `confint` are available.
#'
#' @references
#' Suchard, M.A., Simpson, S.E., Zorych, I., Ryan, P., and Madigan, D. (2013). Massive parallelization
#' of serial inference algorithms for complex generalized linear models. ACM Transactions on Modeling
#' and Computer Simulation 23, 10
#'
#' @export
fitSccsModel <- function(sccsIntervalData,
                         fitSccsModelArgs) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsIntervalData, "SccsIntervalData", null.ok = TRUE, add = errorMessages)
  checkmate::assertR6(fitSccsModelArgs, "FitSccsModelArgs", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  ParallelLogger::logTrace("Fitting SCCS model")
  metaData <- attr(sccsIntervalData, "metaData")
  metaData$covariateRef <- sccsIntervalData$covariateRef |>
    collect()
  if (!is.null(metaData$error)) {
    result <- list(
      status = metaData$error,
      metaData = metaData
    )
    class(result) <- "SccsModel"
    return(result)
  }
  start <- Sys.time()
  estimates <- NULL
  priorVariance <- 0
  logLikelihood <- NA
  logLikelihoodProfiles <- NULL
  if (sccsIntervalData$outcomes |> count() |> pull() == 0) {
    coefficients <- c(0)
    status <- "Could not estimate because there was no data"
  } else {
    # Build list of IDs that should not be regularized, and see if there is anything that needs
    # regularization:
    nonRegularized <- c()
    needRegularization <- FALSE
    needCi <- c()
    needProfile <- c()
    needTestForEndofObservation <- FALSE
    covariateSettingsList <- metaData$covariateSettingsList
    for (i in 1:length(covariateSettingsList)) {
      if (covariateSettingsList[[i]]$allowRegularization) {
        needRegularization <- TRUE
      } else {
        nonRegularized <- c(nonRegularized, covariateSettingsList[[i]]$outputIds)
        needCi <- c(needCi, covariateSettingsList[[i]]$outputIds)
      }
      if (isTRUE(covariateSettingsList[[i]]$profileLikelihood)) {
        needProfile <- c(needProfile, covariateSettingsList[[i]]$outputIds)
      }
    }
    if (!is.null(metaData$age)) {
      if (metaData$age$allowRegularization) {
        needRegularization <- TRUE
      } else {
        nonRegularized <- c(nonRegularized, metaData$age$covariateIds)
        if (metaData$age$computeConfidenceIntervals) {
          needCi <- c(needCi, metaData$age$covariateIds)
        }
      }
    }
    if (!is.null(metaData$seasonality)) {
      if (metaData$seasonality$allowRegularization) {
        needRegularization <- TRUE
      } else {
        nonRegularized <- c(nonRegularized, metaData$seasonality$covariateIds)
        if (metaData$seasonality$computeConfidenceIntervals) {
          needCi <- c(needCi, metaData$seasonality$covariateIds)
        }
      }
    }
    if (!is.null(metaData$calendarTime$allowRegularization)) {
      if (metaData$calendarTime$allowRegularization) {
        needRegularization <- TRUE
      } else {
        nonRegularized <- c(nonRegularized, metaData$calendarTime$covariateIds)
        if (metaData$calendarTime$computeConfidenceIntervals) {
          needCi <- c(needCi, metaData$calendarTime$covariateIds)
        }
      }
    }
    if (!is.null(metaData$endOfObservationEra) && metaData$endOfObservationEra$endOfObservationEraLength > 0) {
      needTestForEndofObservation <- TRUE
      needCi <- c(needCi, metaData$endOfObservationEra$endOfObservationCovariateId)
      nonRegularized <- c(nonRegularized, metaData$endOfObservationEra$endOfObservationCovariateId)
    }

    if (!needRegularization) {
      prior <- createPrior("none")
    } else {
      covariateIds <- sccsIntervalData$covariates |>
        distinct(.data$covariateId) |>
        pull()
      prior <- fitSccsModelArgs$prior
      prior$exclude <- intersect(nonRegularized, covariateIds)
    }
    cyclopsData <- Cyclops::convertToCyclopsData(sccsIntervalData$outcomes,
                                                 sccsIntervalData$covariates,
                                                 modelType = "cpr",
                                                 addIntercept = FALSE,
                                                 checkRowIds = FALSE,
                                                 quiet = TRUE
    )
    fit <- tryCatch(
      {
        Cyclops::fitCyclopsModel(cyclopsData, prior = prior, control = fitSccsModelArgs$control)
      },
      error = function(e) {
        e$message
      }
    )
    if (is.character(fit)) {
      coefficients <- c(0)
      estimates <- NULL
      priorVariance <- 0
      status <- fit
    } else {
      if (!is.null(fitSccsModelArgs$profileGrid) || !is.null(fitSccsModelArgs$profileBounds)) {
        covariateIds <- intersect(needProfile, as.numeric(Cyclops::getCovariateIds(cyclopsData)))
        getLikelihoodProfile <- function(covariateId) {
          logLikelihoodProfile <- Cyclops::getCyclopsProfileLogLikelihood(
            object = fit,
            parm = covariateId,
            x = fitSccsModelArgs$profileGrid,
            bounds = fitSccsModelArgs$profileBounds,
            tolerance = 0.1,
            includePenalty = TRUE
          )
          return(logLikelihoodProfile)
        }
        logLikelihoodProfiles <- lapply(covariateIds, getLikelihoodProfile)
        names(logLikelihoodProfiles) <- covariateIds
      }
      if (fit$return_flag != "SUCCESS") {
        coefficients <- c(0)
        estimates <- NULL
        priorVariance <- 0
        status <- fit$return_flag
      } else {
        status <- "OK"
        estimates <- coef(fit)
        estimates <- tibble(logRr = estimates, covariateId = as.numeric(names(estimates))) |>
          left_join(
            sccsIntervalData$covariateRef |>
              collect(),
            by = join_by("covariateId")
          )
        if (length(needCi) == 0) {
          estimates$logLb95 <- NA
          estimates$logUb95 <- NA
          estimates$seLogRr <- NA
        } else {
          ci <- tryCatch(
            {
              result <- confint(fit, parm = intersect(needCi, estimates$covariateId), includePenalty = TRUE)
              attr(result, "dimnames")[[1]] <- 1:length(attr(result, "dimnames")[[1]])
              result <- as.data.frame(result)
              rownames(result) <- NULL
              result
            },
            error = function(e) {
              missing(e) # suppresses R CMD check note
              data.frame(covariate = 0, logLb95 = 0, logUb95 = 0)
            }
          )
          names(ci)[names(ci) == "2.5 %"] <- "logLb95"
          names(ci)[names(ci) == "97.5 %"] <- "logUb95"
          ci$evaluations <- NULL
          estimates <- merge(estimates, ci, by.x = "covariateId", by.y = "covariate", all.x = TRUE)
          estimates$seLogRr <- (estimates$logUb95 - estimates$logLb95) / (2 * qnorm(0.975))
          for (param in intersect(needCi, estimates$covariateId)) {
            llNull <- Cyclops::getCyclopsProfileLogLikelihood(
              object = fit,
              parm = param,
              x = 0,
              includePenalty = TRUE
            )$value
            estimates$llr[estimates$covariateId == param] <- fit$log_likelihood - llNull
          }
        }
        # Remove regularized estimates with logRr = 0:
        estimates <- estimates[estimates$logRr != 0 | !is.na(estimates$seLogRr) | estimates$covariateId < 1000, ]
        priorVariance <- fit$variance[1]
        logLikelihood <- fit$log_likelihood
      }
    }
  }
  result <- list(
    estimates = estimates,
    priorVariance = priorVariance,
    logLikelihood = logLikelihood,
    logLikelihoodProfiles = logLikelihoodProfiles,
    status = status,
    metaData = metaData
  )
  class(result) <- "SccsModel"
  delta <- Sys.time() - start
  message(paste("Fitting the model took", signif(delta, 3), attr(delta, "units")))
  ParallelLogger::logDebug("Model fitting status is: ", status)
  return(result)
}


#' @export
coef.SccsModel <- function(object, ...) {
  return(object$estimates$logRr)
}

#' @export
confint.SccsModel <- function(object, ...) {
  return(object$estimates |> select("covariateId", "logLb95", "logUb95"))
}

#' @export
print.SccsModel <- function(x, ...) {
  writeLines("SccsModel object")
  writeLines("")
  writeLines(paste("Outcome ID:", paste(x$metaData$outcomeId, collapse = ",")))
  writeLines("")
  writeLines("Outcome count:")
  attrition <- as.data.frame(x$metaData$attrition)
  attrition <- attrition[nrow(attrition), ]
  rownames(attrition) <- attrition$outcomeId
  attrition$outcomeId <- NULL
  attrition$description <- NULL
  printCoefmat(attrition)
  writeLines("")
  if (x$status != "OK") {
    writeLines(paste("Status:", x$status))
  } else {
    writeLines("Estimates:")
    d <- x$estimates
    output <- tibble(
      d$covariateName,
      d$covariateId,
      exp(d$logRr),
      exp(d$logLb95),
      exp(d$logUb95),
      d$logRr,
      d$seLogRr
    )

    colnames(output) <- c("Name", "ID", "Estimate", "LB95CI", "UB95CI", "LogRr", "SeLogRr")
    print(output, n = 25)
  }
}

#' Output the full model
#'
#' @template SccsModel
#'
#' @return
#' A `tibble` with the coefficients and confidence intervals (when not-regularized) for all
#' covariates in the model.
#'
#' @export
getModel <- function(sccsModel) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsModel, "SccsModel", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  d <- sccsModel$estimates
  output <- tibble(
    d$covariateName,
    d$covariateId,
    exp(d$logRr),
    exp(d$logLb95),
    exp(d$logUb95),
    d$logRr,
    d$seLogRr,
    d$originalEraId,
    d$originalEraType,
    d$originalEraName
  )
  colnames(output) <- c(
    "name",
    "id",
    "estimate",
    "lb95Ci",
    "ub95Ci",
    "logRr",
    "seLogRr",
    "originalEraId",
    "originalEraType",
    "originalEraName"
  )
  return(output)
}

#' Does the model contain an age effect?
#'
#' @template SccsModel
#'
#' @return
#' TRUE if the model contains an age effect, otherwise FALSE.
#'
#' @export
hasAgeEffect <- function(sccsModel) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsModel, "SccsModel", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  estimates <- sccsModel$estimates
  return(any(estimates$covariateId >= 100 & estimates$covariateId < 200))
}

#' Does the model contain an age effect?
#'
#' @template SccsModel
#'
#' @return
#' TRUE if the model contains an age effect, otherwise FALSE.
#'
#' @export
hasSeasonality <- function(sccsModel) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsModel, "SccsModel", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  estimates <- sccsModel$estimates
  return(any(estimates$covariateId >= 200 & estimates$covariateId < 300))
}

#' Does the model contain an age effect?
#'
#' @template SccsModel
#'
#' @return
#' TRUE if the model contains an age effect, otherwise FALSE.
#'
#' @export
hasCalendarTimeEffect <- function(sccsModel) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsModel, "SccsModel", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  estimates <- sccsModel$estimates
  return(any(estimates$covariateId >= 300 & estimates$covariateId < 400))
}
