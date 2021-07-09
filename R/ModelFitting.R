# @file ModelFitting.R
#
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
#' @param prior         The prior used to fit the model. See [Cyclops::createPrior] for
#'                      details.
#' @param control       The control object used to control the cross-validation used to determine the
#'                      hyperparameters of the prior (if applicable). See
#'                      [Cyclops::createControl] for details.
#' @param profileGrid           A one-dimensional grid of points on the log(relative risk) scale where
#'                              the likelihood for coefficient of variables is sampled. See details.
#' @param profileBounds         The bounds (on the log relative risk scale) for the adaptive sampling
#'                              of the likelihood function.
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
                         prior = createPrior("laplace", useCrossValidation = TRUE),
                         control = createControl(cvType = "auto",
                                                 selectorType = "byPid",
                                                 startingVariance = 0.1,
                                                 noiseLevel = "quiet"),
                         profileGrid = NULL,
                         profileBounds = c(log(0.1), log(10))) {
  if (!is.null(profileGrid) && !is.null(profileBounds))
    stop("Specify either profileGrid or profileBounds")

  ParallelLogger::logTrace("Fitting SCCS model")
  metaData <- attr(sccsIntervalData, "metaData")
  if (!is.null(metaData$error)) {
    result <- list(status = metaData$error,
                   metaData = metaData)
    class(result) <- "sccsModel"
    return(result)
  }
  start <- Sys.time()
  estimates <- NULL
  priorVariance <- 0
  logLikelihood <- NA
  logLikelihoodProfiles <- NULL
  if (sccsIntervalData$outcomes %>% count() %>% pull() == 0) {
    coefficients <- c(0)
    status <- "Could not estimate because there was no data"
  } else {
    # Build list of IDs that should not be regularized, and see if there is anything that needs
    # regularization:
    nonRegularized <- c()
    needRegularization <- FALSE
    needCi <- c()
    needProfile <- c()
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

    if (!needRegularization) {
      prior <- createPrior("none")
    } else {
      covariateIds <- sccsIntervalData$covariates %>%
        distinct(.data$covariateId) %>%
        pull()
      prior$exclude <- intersect(nonRegularized, covariateIds)
    }
    cyclopsData <- Cyclops::convertToCyclopsData(sccsIntervalData$outcomes,
                                                 sccsIntervalData$covariates,
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
      estimates <- NULL
      priorVariance <- 0
      status <- fit
    } else {
      if (!is.null(profileGrid) || !is.null(profileBounds)) {
        covariateIds <- intersect(needProfile, as.numeric(Cyclops::getCovariateIds(cyclopsData)))
        getLikelihoodProfile <- function(covariateId) {
          logLikelihoodProfile <- Cyclops::getCyclopsProfileLogLikelihood(object = fit,
                                                                          parm = covariateId,
                                                                          x = profileGrid,
                                                                          bounds = profileBounds,
                                                                          tolerance = 0.1,
                                                                          includePenalty = TRUE)
          return(logLikelihoodProfile)
        }
        logLikelihoodProfiles <- lapply(covariateIds, getLikelihoodProfile)
        names(logLikelihoodProfiles) <- covariateIds
      }
      if (fit$return_flag == "ILLCONDITIONED") {
        coefficients <- c(0)
        estimates <- NULL
        priorVariance <- 0
        status <- "ILL CONDITIONED, CANNOT FIT"
      } else if (fit$return_flag == "MAX_ITERATIONS") {
        coefficients <- c(0)
        estimates <- NULL
        priorVariance <- 0
        status <- "REACHED MAXIMUM NUMBER OF ITERATIONS, CANNOT FIT"
      } else {
        status <- "OK"
        estimates <- coef(fit)
        estimates <- data.frame(logRr = estimates, covariateId = as.numeric(names(estimates)))
        estimates <- merge(estimates, collect(sccsIntervalData$covariateRef), all.x = TRUE)
        if (length(needCi) == 0) {
          estimates$logLb95 <- NA
          estimates$logUb95 <- NA
          estimates$seLogRr <- NA
        } else {
          ci <- tryCatch({
            result <- confint(fit, parm = intersect(needCi, estimates$covariateId), includePenalty = TRUE)
            attr(result, "dimnames")[[1]] <- 1:length(attr(result, "dimnames")[[1]])
            result <- as.data.frame(result)
            rownames(result) <- NULL
            result
          }, error = function(e) {
            missing(e)  # suppresses R CMD check note
            data.frame(covariate = 0, logLb95 = 0, logUb95 = 0)
          })
          names(ci)[names(ci) == "2.5 %"] <- "logLb95"
          names(ci)[names(ci) == "97.5 %"] <- "logUb95"
          ci$evaluations <- NULL
          estimates <- merge(estimates, ci, by.x = "covariateId", by.y = "covariate", all.x = TRUE)
          estimates$seLogRr <- (estimates$logUb95 - estimates$logLb95)/(2*qnorm(0.975))
          for (param in intersect(needCi, estimates$covariateId)) {
            llNull <- Cyclops::getCyclopsProfileLogLikelihood(object = fit,
                                                              parm = param,
                                                              x = 0,
                                                              includePenalty = FALSE)$value
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
  result <- list(estimates = estimates,
                 priorVariance = priorVariance,
                 logLikelihood = logLikelihood,
                 logLikelihoodProfiles = logLikelihoodProfiles,
                 status = status,
                 metaData = metaData)
  class(result) <- "SccsModel"
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Fitting the model took", signif(delta, 3), attr(delta, "units")))
  ParallelLogger::logDebug("Model fitting status is: ", status)
  return(result)
}


#' @export
coef.SccsModel <- function(object, ...) {
  return(object$estimates$logRr)
}

#' @export
confint.SccsModel <- function(object, ...) {
  return(object$estimates %>% select(.data$covariateId, .data$logLb95, .data$logUb95))
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
    output <- tibble(d$covariateName,
                     d$covariateId,
                     exp(d$logRr),
                     exp(d$logLb95),
                     exp(d$logUb95),
                     d$logRr,
                     d$seLogRr)

    colnames(output) <- c("Name", "ID", "Estimate", "LB95CI", "UB95CI", "LogRr", "SeLogRr")
    print(output, n = 25)
  }
}

#' Output the full model
#'
#' @param sccsModel   An object of type \code{sccsModel} as created using the
#'                    \code{\link{fitSccsModel}} function.
#'
#' @return
#' A data frame with the coefficients and confidence intervals (when not-regularized) for all
#' covariates in the model.
#'
#' @export
getModel <- function(sccsModel) {
  d <- sccsModel$estimates
  # d$seLogRr <- (d$logUb95 - d$logRr)/qnorm(0.975)
  output <- tibble(d$covariateName,
                   d$covariateId,
                   exp(d$logRr),
                   exp(d$logLb95),
                   exp(d$logUb95),
                   d$logRr,
                   d$seLogRr,
                   d$originalEraId,
                   d$originalEraType,
                   d$originalEraName)
  colnames(output) <- c("name",
                        "id",
                        "estimate",
                        "lb95Ci",
                        "ub95Ci",
                        "logRr",
                        "seLogRr",
                        "originalEraId",
                        "originalEraType",
                        "originalEraName")
  return(output)
}
