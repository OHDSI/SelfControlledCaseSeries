# @file ModelFitting.R
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

#' Fit the SCCS model
#'
#' @details
#' Fits the SCCS model as a conditional Poisson regression. When allowed, coefficients for some or all
#' covariates can be regularized.
#'
#' @param sccsEraData   An object of type \code{sccsEraData} as created using the
#'                      \code{\link{createSccsEraData}} function.
#' @param prior         The prior used to fit the model. See \code{\link[Cyclops]{createPrior}} for
#'                      details.
#' @param control       The control object used to control the cross-validation used to determine the
#'                      hyperparameters of the prior (if applicable). See
#'                      \code{\link[Cyclops]{createControl}} for details.
#'
#' @return
#' An object of type \code{sccsModel}. Generic functions \code{summary}, \code{coef}, and
#' \code{confint} are available.
#'
#' @references
#' Suchard, M.A., Simpson, S.E., Zorych, I., Ryan, P., and Madigan, D. (2013). Massive parallelization
#' of serial inference algorithms for complex generalized linear models. ACM Transactions on Modeling
#' and Computer Simulation 23, 10
#'
#' @export
fitSccsModel <- function(sccsEraData,
                         prior = createPrior("laplace", useCrossValidation = TRUE),
                         control = createControl(cvType = "auto",
                                                 selectorType = "byPid",
                                                 startingVariance = 0.1,
                                                 noiseLevel = "quiet")) {
  ParallelLogger::logTrace("Fitting SCCS model")
  if (!is.null(sccsEraData$metaData$error)) {
    result <- list(status = sccsEraData$metaData$error,
                   metaData = sccsEraData$metaData)
    class(result) <- "sccsModel"
    return(result)
  }
  start <- Sys.time()
  if (is.null(sccsEraData$outcomes)) {
    coefficients <- c(0)
    estimates <- NULL
    priorVariance <- 0
    status <- "Could not estimate because there was no data"
  } else {
    # Build list of IDs that should not be regularized, and see if there is anything that needs
    # regularization:
    nonRegularized <- c()
    needRegularization <- FALSE
    needCi <- c()
    covariateSettingsList <- sccsEraData$metaData$covariateSettingsList
    for (i in 1:length(covariateSettingsList)) {
      if (covariateSettingsList[[i]]$allowRegularization) {
        needRegularization <- TRUE
      } else {
        nonRegularized <- c(nonRegularized, covariateSettingsList[[i]]$outputIds)
        needCi <- c(needCi, covariateSettingsList[[i]]$outputIds)
      }
    }
    if (!is.null(sccsEraData$metaData$age)) {
      if (sccsEraData$metaData$age$allowRegularization) {
        needRegularization <- TRUE
      } else {
        nonRegularized <- c(nonRegularized, sccsEraData$metaData$age$covariateIds)
        if (sccsEraData$metaData$age$computeConfidenceIntervals) {
          needCi <- c(needCi, sccsEraData$metaData$age$covariateIds)
        }
      }
    }
    if (!is.null(sccsEraData$metaData$seasonality)) {
      if (sccsEraData$metaData$seasonality$allowRegularization) {
        needRegularization <- TRUE
      } else {
        nonRegularized <- c(nonRegularized, sccsEraData$metaData$seasonality$covariateIds)
        if (sccsEraData$metaData$seasonality$computeConfidenceIntervals) {
          needCi <- c(needCi, sccsEraData$metaData$seasonality$covariateIds)
        }
      }
    }

    if (!needRegularization) {
      prior <- createPrior("none")
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
      estimates <- NULL
      priorVariance <- 0
      status <- fit
    } else if (fit$return_flag == "ILLCONDITIONED") {
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
      estimates <- merge(estimates, ff::as.ram(sccsEraData$covariateRef), all.x = TRUE)
      if (length(needCi) == 0) {
        estimates$logLb95 <- NA
        estimates$logUb95 <- NA
        estimates$seLogRr <- NA
      } else {
        ci <- tryCatch({
          result <- confint(fit, parm = needCi, includePenalty = TRUE)
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
      }
      # Remove regularized estimates with logRr = 0:
      estimates <- estimates[estimates$logRr != 0 | !is.na(estimates$seLogRr) | estimates$covariateId <
                               1000, ]
      priorVariance <- fit$variance[1]
    }
  }
  result <- list(estimates = estimates,
                 priorVariance = priorVariance,
                 status = status,
                 metaData = sccsEraData$metaData)
  result$metaData$counts <- summary(sccsEraData)$outcomeCounts
  class(result) <- "sccsModel"
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Fitting the model took", signif(delta, 3), attr(delta, "units")))
  ParallelLogger::logDebug("Model fitting status is: ", status)
  return(result)
}


#' @export
coef.sccsModel <- function(object, ...) {
  return(object$estimates$logRr)
}

#' @export
summary.sccsModel <- function(object, ...) {
  class(object) <- "summary.sccsModel"
  return(object)
}

#' @export
print.summary.sccsModel <- function(x, ...) {
  writeLines("sccsModel object summary")
  writeLines("")
  writeLines(paste("Outcome ID:", paste(x$metaData$outcomeId, collapse = ",")))
  writeLines("")
  writeLines("Outcome count:")
  outcomeCounts <- x$metaData$counts
  rownames(outcomeCounts) <- outcomeCounts$outcomeConceptId
  outcomeCounts$outcomeConceptId <- NULL
  colnames(outcomeCounts) <- c("Event count", "Case count")
  printCoefmat(outcomeCounts)
  writeLines("")
  writeLines("Estimates:")
  d <- x$estimates
  output <- data.frame(d$covariateName,
                       d$covariateId,
                       format(exp(d$logRr), digits = 4, scientific = FALSE),
                       format(exp(d$logLb95), digits = 4, scientific = FALSE),
                       format(exp(d$logUb95), digits = 4, scientific = FALSE),
                       d$logRr,
                       d$seLogRr)

  colnames(output) <- c("Name", "ID", "Estimate", "lower .95", "upper .95", "logRr", "seLogRr")
  if (nrow(output) > 100) {
    print.data.frame(output[1:100,],
                     row.names = F,
                     print.gap = 2,
                     quote = F,
                     right = T,
                     digits = 4)
    writeLines(paste("... (omitting", nrow(output) - 100, "rows)"))
  } else {
    print.data.frame(output, row.names = F, print.gap = 2, quote = F, right = T, digits = 4)
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
  output <- data.frame(d$covariateName,
                       d$covariateId,
                       exp(d$logRr),
                       exp(d$logLb95),
                       exp(d$logUb95),
                       d$logRr,
                       d$seLogRr,
                       d$originalCovariateId,
                       d$originalCovariateName)
  colnames(output) <- c("name",
                        "id",
                        "estimate",
                        "lb95Ci",
                        "ub95Ci",
                        "logRr",
                        "seLogRr",
                        "originalCovariateId",
                        "originalCovariateName")
  return(output)
}
