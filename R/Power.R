# Copyright 2024 Observational Health Data Sciences and Informatics
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

#' Compute the minimum detectable relative risk
#'
#' @details
#' Compute the minimum detectable relative risk (MDRR) for a given study population, using the observed
#' time at risk and total time in days and number of events. Five sample size formulas are implemented:
#' sampling proportion, binomial proportion, 2 signed root likelihood ratio methods, and likelihood extension for
#' age effects. The expressions by Musonda (2006) are used.
#'
#' @param object              An object either of type [SccsIntervalData] as created using the
#'                            [createSccsIntervalData] function, or an object of type `SccsModel` as created
#'                            using the [fitSccsModel()] function.
#' @param exposureCovariateId Covariate Id for the health exposure of interest.
#' @param alpha               Type I error.
#' @param power               1 - beta, where beta is the type II error.
#' @param twoSided            Consider a two-sided test?
#' @param method              The type of sample size formula that will be used. Allowable values are
#'                            "proportion", "binomial", "SRL1", "SRL2", or "ageEffects". Currently "ageEffects"
#'                            is not supported.
#'
#' @references
#' Musonda P, Farrington CP, Whitaker HJ (2006) Samples sizes for self-controlled case series studies,
#' Statistics in Medicine, 15;25(15):2618-31
#'
#' @return
#' A data frame with the MDRR, number of events, time at risk, and total time.
#'
#' @export
computeMdrr <- function(object,
                        exposureCovariateId,
                        alpha = 0.05,
                        power = 0.8,
                        twoSided = TRUE,
                        method = "SRL1") {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertInt(exposureCovariateId, add = errorMessages)
  checkmate::assertNumeric(alpha, lower = 0, upper = 1, len = 1, add = errorMessages)
  checkmate::assertNumeric(power, lower = 0, upper = 1, len = 1, add = errorMessages)
  checkmate::assertLogical(twoSided, len = 1, add = errorMessages)
  checkmate::assertChoice(method, c("proportion", "binomial", "SRL1", "SRL2", "ageEffects"), add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (is(object, "SccsModel")) {
    covariateStatistics <- object$metaData$covariateStatistics
  } else if (is(object, "SccsIntervalData")) {
    covariateStatistics <- attr(object, "metaData")$covariateStatistics
  } else {
    stop(sprintf("The argument 'object' must of type 'SccsModel' or 'SccsIntervalData', but is of type '%s'.", class(object)))
  }

  if (!is.null(covariateStatistics)) {
    covariateStatistics <- covariateStatistics %>%
      filter(.data$covariateId == exposureCovariateId)
  }
  if (is.null(covariateStatistics) || nrow(covariateStatistics) == 0) {
    result <- tibble(
      timeExposed = 0,
      timeTotal = 0,
      propTimeExposed = 0,
      propPopulationExposed = 0,
      events = 0,
      mdrr = Inf
    )
    return(result)
  }

  r <- covariateStatistics$dayCount / covariateStatistics$observedDayCount
  # pr <- exposed$observationPeriods / overall$observationPeriods
  n <- covariateStatistics$observedOutcomeCount

  if (twoSided) {
    alpha <- alpha / 2
  }

  z <- qnorm(1 - alpha)

  if (method == "distribution") {
    # expression 5
    computePower <- function(p, z, r, n, alpha) {
      zbnum <- log(p) * sqrt(n * p * r * (1 - r)) - z * sqrt(p)
      zbden <- p * r + 1 - r
      zb <- zbnum / zbden
      power <- pnorm(zb)
      if (power < alpha | n < 1) {
        power <- alpha
      }
      return(power)
    }
  }

  if (method == "binomial") {
    # expression 6
    computePower <- function(p, z, r, n, alpha) {
      pi <- p * r / (p * r + 1 - r)
      tAlt <- asin(sqrt(pi))
      tNull <- asin(sqrt(r))
      zb <- sqrt(n * 4 * (tAlt - tNull)^2) - z
      power <- pnorm(zb)
      if (power < alpha | n < 1) {
        power <- alpha
      }
      return(power)
    }
  }

  if (method == "SRL1") {
    # expression 7
    computePowerSrl <- function(b, z, r, n, alpha) {
      A <- 2 * ((exp(b) * r / (exp(b) * r + 1 - r)) * b - log(exp(b) * r + 1 - r))
      B <- b^2 / A * exp(b) * r * (1 - r) / (exp(b) * r + 1 - r)^2
      zb <- (sqrt(n * A) - z) / sqrt(B)
      power <- pnorm(zb)
      if (power < alpha | n < 1) {
        power <- alpha
      }
      return(power)
    }
  }

  if (method == "SRL2") {
    stop("SRL2 method not currently supported")
    # expression 8
    # computePowerSrl <- function(b, z, r, n, alpha) {
    #   A <- 2 * pr * (exp(b) * r + 1 - r) / (1 + pr * r * (exp(b) - 1)) * ((exp(b) * r / (exp(b) * r + 1 - r)) * b - log(exp(b) * r + 1 - r))
    #   B <- b^2 / A * pr * (exp(b) * r + 1 - r) / (1 + pr * r * (exp(b) - 1)) * exp(b) * r * (1 - r) / (exp(b) * r + 1 - r)^2
    #   zb <- (sqrt(n * A) - z) / sqrt(B)
    #   power <- pnorm(zb)
    #   if (power < alpha | n < 1) {
    #     power <- alpha
    #   }
    #   return(power)
    # }
  }

  if (method == "ageEffects") {
    stop("Age effects method not currently supported")
  }

  binarySearch <- function(z, r, n, power, alpha, precision = 1e-6) {
    L <- 0
    H <- 10
    while (H >= L) {
      M <- L + (H - L) / 2
      if (method %in% c("SRL1", "SRL2")) {
        powerM <- computePowerSrl(M, z, r, n, alpha)
      } else {
        powerM <- computePower(exp(M), z, r, n, alpha)
      }
      d <- powerM - power
      if (d > precision) {
        H <- M
      } else if (-d > precision) {
        L <- M
      } else {
        return(M)
      }
      if (M == 0 || M == 10) {
        return(M)
      }
    }
  }
  if (r == 1) {
    mdrr <- Inf
  } else {
    mdLogRr <- binarySearch(z, r, n, power, alpha)
    mdrr <- exp(mdLogRr)
  }

  result <- tibble(
    timeExposed = covariateStatistics$dayCount,
    timeTotal = covariateStatistics$observedDayCount,
    propTimeExposed = round(r, 4),
    # propPopulationExposed = round(pr, 4),
    events = covariateStatistics$observedOutcomeCount,
    mdrr = round(mdrr, 4)
  )
  return(result)
}
