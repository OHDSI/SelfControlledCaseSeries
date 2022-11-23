# Copyright 2022 Observational Health Data Sciences and Informatics
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

#' SCCS Interval Data
#'
#' @description
#' SccsIntervalData` is an S4 class that inherits from [Andromeda][Andromeda::Andromeda]. It contains
#' information on the cases and their covariates, divided in non-overlapping time intervals.
#'
#' A `SccsIntervalData` is typically created using [createSccsIntervalData()], can only be saved using
#' [saveSccsIntervalData()], and loaded using [loadSccsIntervalData()].
#'
#' @name SccsIntervalData-class
#' @aliases SccsIntervalData
NULL

#' SccsIntervalData class.
#'
#' @export
#' @import Andromeda
setClass("SccsIntervalData", contains = "Andromeda")

#' Save the cohort method data to file
#'
#' @description
#' Saves an object of type [SccsIntervalData] to a file.
#'
#' @template SccsIntervalData
#' @param file               The name of the file where the data will be written. If the file already
#'                           exists it will be overwritten.
#'
#' @return
#' Returns no output.
#'
#' @export
saveSccsIntervalData <- function(sccsIntervalData, file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsIntervalData, "SccsIntervalData", add = errorMessages)
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  Andromeda::saveAndromeda(sccsIntervalData, file)
}

#' Load the cohort method data from a file
#'
#' @description
#' Loads an object of type [SccsIntervalData] from a file in the file system.
#'
#' @param file       The name of the file containing the data.
#'
#' @return
#' An object of class [SccsIntervalData].
#'
#' @export
loadSccsIntervalData <- function(file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (!file.exists(file)) {
    stop("Cannot find file ", file)
  }
  if (file.info(file)$isdir) {
    stop(file, " is a folder, but should be a file")
  }
  SccsIntervalData <- Andromeda::loadAndromeda(file)
  class(SccsIntervalData) <- "SccsIntervalData"
  attr(class(SccsIntervalData), "package") <- "SelfControlledCaseSeries"
  return(SccsIntervalData)
}

# show()
#' @param object  An object of type `SccsIntervalData`.
#'
#' @export
#' @rdname SccsIntervalData-class
setMethod("show", "SccsIntervalData", function(object) {
  metaData <- attr(object, "metaData")
  cli::cat_line(pillar::style_subtle("# SccsIntervalData object"))
  cli::cat_line("")
  cli::cat_line(paste("Outcome cohort ID:", metaData$outcomeId))
  cli::cat_line("")
  cli::cat_line(pillar::style_subtle("Inherits from Andromeda:"))
  class(object) <- "Andromeda"
  attr(class(object), "package") <- "Andromeda"
  show(object)
})

# summary()
#' @param object  An object of type `SccsIntervalData`.
#'
#' @export
#' @rdname SccsIntervalData-class
setMethod("summary", "SccsIntervalData", function(object) {
  if (!Andromeda::isValidAndromeda(object)) {
    stop("Object is not valid. Probably the Andromeda object was closed.")
  }

  caseCount <- object$outcomes %>%
    summarise(n = n_distinct(.data$stratumId)) %>%
    pull()
  eraCount <- object$outcomes %>%
    count() %>%
    pull()
  outcomeCount <- object$outcomes %>%
    summarise(n = sum(.data$y, na.rm = TRUE)) %>%
    pull()
  covariateCount <- object$covariateRef %>%
    count() %>%
    pull()
  covariateValueCount <- object$covariates %>%
    count() %>%
    pull()

  result <- list(
    metaData = attr(object, "metaData"),
    caseCount = caseCount,
    eraCount = eraCount,
    outcomeCount = outcomeCount,
    covariateCount = covariateCount,
    covariateValueCount = covariateValueCount
  )

  class(result) <- "summary.SccsIntervalData"
  return(result)
})

#' @export
print.summary.SccsIntervalData <- function(x, ...) {
  writeLines("SccsIntervalData object summary")
  writeLines("")
  writeLines(paste("Outcome cohort ID:", x$metaData$outcomeId))
  writeLines("")
  writeLines(paste("Number of cases (observation periods):", x$caseCount))
  writeLines(paste("Number of eras (spans of time):", x$eraCount))
  writeLines(paste("Number of outcomes:", x$outcomeCount))
  writeLines(paste("Number of covariates:", x$covariateCount))
  writeLines(paste("Number of non-zero covariate values:", x$covariateValueCount))
}

#' Check whether an object is a SccsIntervalData object
#'
#' @param x  The object to check.
#'
#' @return
#' A logical value.
#'
#' @export
isSccsIntervalData <- function(x) {
  return(inherits(x, "SccsIntervalData"))
}
