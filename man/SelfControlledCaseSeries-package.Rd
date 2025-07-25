% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SelfControlledCaseSeries.R
\docType{package}
\name{SelfControlledCaseSeries-package}
\alias{SelfControlledCaseSeries}
\alias{SelfControlledCaseSeries-package}
\title{SelfControlledCaseSeries: Self-Controlled Case Series}
\description{
Execute the self-controlled case series (SCCS) design using observational data in the OMOP Common Data Model. Extracts all necessary data from the database and transforms it to the format required for SCCS. Age and season can be modeled using splines assuming constant hazard within calendar months. Event-dependent censoring of the observation period can be corrected for. Many exposures can be included at once (MSCCS), with regularization on all coefficients except for the exposure of interest. Includes diagnostics for all major assumptions of the SCCS.
}
\seealso{
Useful links:
\itemize{
  \item \url{https://ohdsi.github.io/SelfControlledCaseSeries/}
  \item \url{https://github.com/OHDSI/SelfControlledCaseSeries}
  \item Report bugs at \url{https://github.com/OHDSI/SelfControlledCaseSeries/issues}
}

}
\author{
\strong{Maintainer}: Martijn Schuemie \email{schuemie@ohdsi.org}

Authors:
\itemize{
  \item Patrick Ryan
  \item Trevor Shaddox
  \item Marc Suchard
}

}
\keyword{internal}
