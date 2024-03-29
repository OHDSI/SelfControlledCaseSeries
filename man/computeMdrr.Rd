% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Power.R
\name{computeMdrr}
\alias{computeMdrr}
\title{Compute the minimum detectable relative risk}
\usage{
computeMdrr(
  object,
  exposureCovariateId,
  alpha = 0.05,
  power = 0.8,
  twoSided = TRUE,
  method = "SRL1"
)
}
\arguments{
\item{object}{An object either of type \link{SccsIntervalData} as created using the
\link{createSccsIntervalData} function, or an object of type \code{SccsModel} as created
using the \code{\link[=fitSccsModel]{fitSccsModel()}} function.}

\item{exposureCovariateId}{Covariate Id for the health exposure of interest.}

\item{alpha}{Type I error.}

\item{power}{1 - beta, where beta is the type II error.}

\item{twoSided}{Consider a two-sided test?}

\item{method}{The type of sample size formula that will be used. Allowable values are
"proportion", "binomial", "SRL1", "SRL2", or "ageEffects". Currently "ageEffects"
is not supported.}
}
\value{
A data frame with the MDRR, number of events, time at risk, and total time.
}
\description{
Compute the minimum detectable relative risk
}
\details{
Compute the minimum detectable relative risk (MDRR) for a given study population, using the observed
time at risk and total time in days and number of events. Five sample size formulas are implemented:
sampling proportion, binomial proportion, 2 signed root likelihood ratio methods, and likelihood extension for
age effects. The expressions by Musonda (2006) are used.
}
\references{
Musonda P, Farrington CP, Whitaker HJ (2006) Samples sizes for self-controlled case series studies,
Statistics in Medicine, 15;25(15):2618-31
}
