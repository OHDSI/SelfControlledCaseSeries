% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StudyPopulation.R
\name{createStudyPopulation}
\alias{createStudyPopulation}
\title{Create a study population}
\usage{
createStudyPopulation(
  sccsData,
  outcomeId = NULL,
  firstOutcomeOnly = FALSE,
  naivePeriod = 0,
  minAge = NULL,
  maxAge = NULL
)
}
\arguments{
\item{sccsData}{An object of type \link{SccsData} as created using the
\link{getDbSccsData} function.}

\item{outcomeId}{The outcome to create the era data for. If not specified it is
assumed to be the one outcome for which the data was loaded from
the database.}

\item{firstOutcomeOnly}{Whether only the first occurrence of an outcome should be
considered.}

\item{naivePeriod}{The number of days at the start of a patient's observation period
that should not be included in the risk calculations. Note that
the naive period can be used to determine current covariate
status right after the naive period, and whether an outcome is
the first one.}

\item{minAge}{Minimum age at which patient time will be included in the analysis. Note
that information prior to the min age is still used to determine exposure
status after the minimum age (e.g. when a prescription was started just prior
to reaching the minimum age). Also, outcomes occurring before the minimum age
is reached will be considered as prior outcomes when using first outcomes only.
Age should be specified in years, but non-integer values are allowed. If not
specified, no age restriction will be applied.}

\item{maxAge}{Maximum age at which patient time will be included in the analysis. Age should
be specified in years, but non-integer values are allowed. If not
specified, no age restriction will be applied.}
}
\description{
Create a study population
}
