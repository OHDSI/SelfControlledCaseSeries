# @file Power.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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
#' Compute the minimum detectable relative risk (MDRR) for a given study population, using the observerd
#' time at risk and total time in days and number of events. Four sample size formulas are implemented:
#' sampling proportion, binomial proportion, signed root likelihood ratio, and likelihood extension for
#' age effects. The expressions by Musonda (2006) are used.
#'
#' @param sccsEraData  A data frame describing the study population as created using the
#'                     \code{\link{createSccsEraData}} function. This should at least have these
#'                     columns: subjectId, treatment, outcomeCount, timeAtRisk.
#' @param heiConceptId ConceptId for the health exposure of interest.
#' @param alpha        Type I error.
#' @param power        1 - beta, where beta is the type II error.
#' @param twoSided     Consider a two-sided test?
#' @param method       The type of sample size formula that will be used. Allowable values are
#'                     "proportion", "binomial", "SRL", or "ageEffects". Currently only "proportion"
#'                     is supported.
#'
#' @references
#' Musonda P, Farrington CP, Whitaker HJ (2006) Samples sizes for self-controlled case series studies,
#' Statistics in Medicine, 15;25(15):2618-31
#'
#' @return
#' A data frame with the MDRR, number of events, time at risk, and total time.
#'
#' @export

computeMdrr <- function(sccsEraData,
                        heiConceptId,
                        alpha = 0.05,
                        power = 0.8,
                        twoSided = TRUE,
                        method = "distribution") # distribution, binomial, SRL, ageEffects
{

  heiCovariateIds <- sccsEraData$covariateRef[ffbase::`%in%`(sccsEraData$covariateRef$originalCovariateId, heiConceptId), ]$covariateId
  mapping <- ffbase::ffmatch(ffbase::unique.ff(sccsEraData$covariates$rowId[ffbase::`%in%`(sccsEraData$covariates$covariateId, heiCovariateIds)]), sccsEraData$outcomes$rowId)
  tExp <- ffbase::sum.ff(sccsEraData$outcomes$time[mapping]) # exposed time
  tTot <- sum(sccsEraData$outcomes$time)                     # total time
  r <- tExp / tTot                                           # exposed time / total time
  if (twoSided) {
    alpha <- alpha / 2                                       # alpha
  }
  z <- qnorm(1-alpha)                                        # z alpha
  n <- ffbase::sum.ff(sccsEraData$outcomes$y)                # number of events

  if (method != "distribution" && method != "binomial" && method != "SRL" && method != "ageEffects")
    stop(paste0("Unknown method '",
                method,
                "', please choose either 'distribution', 'binomial', 'SRL', or 'ageEffects'"))

  if (method == "distribution")
  {
    computePower <- function(p, z, r, n, alpha)
    {
      zbnum <- log(p) * sqrt(n * p * r * (1-r)) - z * sqrt(p)
      zbden <- p * r + 1 - r
      zb <- zbnum / zbden
      power <- pnorm(zb)
      if (power < alpha | n < 1)
        power <- alpha
      return(power)
    }
  }

  if (method == "binomial")
  {
    computePower <- function(p, z, r, n, alpha)
    {
      pi = p*r/(p*r + 1 - r)
      tAlt = asin(sqrt(pi))
      tNull = asin(sqrt(r))
      zb = sqrt(n * 4 * (tAlt - tNull)^2) - z
      power = pnorm(zb)
      if (power < alpha | n < 1)
        power <- alpha
      return(power)
    }
  }

  if (method == "SRL")
  {
    stop("Binomial method not currently supported")
  }

  if (method == "ageEffects")
  {
    stop("Age effects method not currently supported")
  }

  binarySearch <- function(z, r, n, power, alpha, precision = 1e-6)
  {
    L <- 0
    H <- 10
    while (H >= L)
    {
      M <- L + (H - L) / 2
      powerM <- computePower(exp(M), z, r, n, alpha)
      d <- powerM - power
      if (d > precision){
        H <- M
      } else if (-d > precision) {
        L <- M
      } else {
        return(M)
      }
      if (M == 0 || M == 10)
        return(M)
    }
  }
  mdLogRr <- binarySearch(z, r, n, power, alpha)
  mdrr <- exp(mdLogRr)

  result <- data.frame(exposedTime = tExp,
                       totalTime = tTot,
                       exposedProportion = r,
                       events = n,
                       mdrr = mdrr)
  return(result)
}
