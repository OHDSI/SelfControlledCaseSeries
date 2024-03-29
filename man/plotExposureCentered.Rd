% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plots.R
\name{plotExposureCentered}
\alias{plotExposureCentered}
\title{Plot information centered around the start of exposure}
\usage{
plotExposureCentered(
  studyPopulation,
  sccsData,
  exposureEraId = NULL,
  highlightExposedEvents = TRUE,
  title = NULL,
  fileName = NULL
)
}
\arguments{
\item{studyPopulation}{An object created using the \code{\link[=createStudyPopulation]{createStudyPopulation()}} function.}

\item{sccsData}{An object of type \link{SccsData} as created using the
\link{getDbSccsData} function.}

\item{exposureEraId}{The exposure to create the era data for. If not specified it is
assumed to be the one exposure for which the data was loaded from
the database.}

\item{highlightExposedEvents}{Highlight events that occurred during the exposure era using a different color?}

\item{title}{Optional: the main title for the plot}

\item{fileName}{Name of the file where the plot should be saved, for example 'plot.png'.
See the function \code{\link[ggplot2:ggsave]{ggplot2::ggsave()}} for supported file formats.}
}
\value{
A ggplot object. Use the \code{\link[ggplot2:ggsave]{ggplot2::ggsave()}} function to save to file in a different
format.
}
\description{
Plot information centered around the start of exposure
}
\details{
This plot shows the number of events and the number of subjects under observation in week-sized intervals relative to the start
of the first exposure.
}
