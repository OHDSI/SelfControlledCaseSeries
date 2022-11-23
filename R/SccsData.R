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

#' SCCS Data
#'
#' @description
#' `SccsData` is an S4 class that inherits from [Andromeda][Andromeda::Andromeda]. It contains information on the cases and their covariates.
#'
#' A `SccsData` is typically created using [getDbSccsData()], can only be saved using
#' [saveSccsData()], and loaded using [loadSccsData()].
#'
#' @name SccsData-class
#' @aliases SccsData
NULL

#' SccsData class.
#'
#' @export
#' @import Andromeda
setClass("SccsData", contains = "Andromeda")

#' Save the cohort method data to file
#'
#' @description
#' Saves an object of type [SccsData] to a file.
#'
#' @template SccsData
#' @param file               The name of the file where the data will be written. If the file already
#'                           exists it will be overwritten.
#'
#' @return
#' Returns no output.
#'
#' @export
saveSccsData <- function(sccsData, file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(sccsData, "SccsData", add = errorMessages)
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  Andromeda::saveAndromeda(sccsData, file)
}

#' Load the cohort method data from a file
#'
#' @description
#' Loads an object of type [SccsData] from a file in the file system.
#'
#' @param file       The name of the file containing the data.
#'
#' @return
#' An object of class [SccsData].
#'
#' @export
loadSccsData <- function(file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (!file.exists(file)) {
    stop("Cannot find file ", file)
  }
  if (file.info(file)$isdir) {
    stop(file, " is a folder, but should be a file")
  }
  SccsData <- Andromeda::loadAndromeda(file)
  class(SccsData) <- "SccsData"
  attr(class(SccsData), "package") <- "SelfControlledCaseSeries"
  return(SccsData)
}

# show()
#' @param object  An object of type `SccsData`.
#'
#' @export
#' @rdname SccsData-class
setMethod("show", "SccsData", function(object) {
  metaData <- attr(object, "metaData")
  cli::cat_line(pillar::style_subtle("# SccsData object"))
  cli::cat_line("")
  if (length(metaData$exposureIds) == 0) {
    cli::cat_line("All exposures")
  } else {
    cli::cat_line(paste(
      "Exposure cohort ID(s):",
      paste(metaData$exposureIds, collapse = ",")
    ))
  }
  cli::cat_line(paste(
    "Outcome cohort ID(s):",
    paste(metaData$outcomeIds, collapse = ",")
  ))
  cli::cat_line("")
  cli::cat_line(pillar::style_subtle("Inherits from Andromeda:"))
  class(object) <- "Andromeda"
  attr(class(object), "package") <- "Andromeda"
  show(object)
})

# summary()
#' @param object  An object of type `SccsData`.
#'
#' @export
#' @rdname SccsData-class
setMethod("summary", "SccsData", function(object) {
  if (!Andromeda::isValidAndromeda(object)) {
    stop("Object is not valid. Probably the Andromeda object was closed.")
  }
  caseCount <- object$cases %>%
    count() %>%
    pull()


  outcomeCounts <- object$eras %>%
    filter(.data$eraType == "hoi") %>%
    inner_join(object$cases, by = "caseId") %>%
    group_by(.data$eraId) %>%
    summarise(
      outcomeSubjects = n_distinct(.data$personId),
      outcomeEvents = count(),
      outcomeObsPeriods = n_distinct(.data$caseId)
    ) %>%
    rename(outcomeId = .data$eraId) %>%
    collect()

  result <- list(
    metaData = attr(object, "metaData"),
    caseCount = caseCount,
    outcomeCounts = outcomeCounts,
    eraTypeCount = object$eraRef %>% count() %>% pull(),
    eraCount = object$eras %>% count() %>% pull()
  )
  class(result) <- "summary.SccsData"
  return(result)
})

#' @export
print.summary.SccsData <- function(x, ...) {
  writeLines("SccsData object summary")
  writeLines("")
  metaData <- x$metaData
  if (length(metaData$exposureIds) == 0) {
    writeLines("All exposures")
  } else {
    writeLines(paste(
      "Exposure cohort ID(s):",
      paste(x$metaData$exposureIds, collapse = ",")
    ))
  }
  writeLines(paste(
    "Outcome cohort ID(s):",
    paste(metaData$outcomeIds, collapse = ",")
  ))
  writeLines("")
  writeLines("Outcome counts:")
  outcomeCounts <- as.data.frame(x$outcomeCounts)
  rownames(outcomeCounts) <- outcomeCounts$outcomeId
  outcomeCounts$outcomeId <- NULL
  colnames(outcomeCounts) <- c("Outcome Subjects", "Outcome Events", "Outcome Observation Periods")
  printCoefmat(outcomeCounts)
  writeLines("")
  writeLines("Eras:")
  writeLines(paste("Number of era types:", x$eraTypeCount))
  writeLines(paste("Number of eras:", x$eraCount))
}

#' Check whether an object is a SccsData object
#'
#' @param x  The object to check.
#'
#' @return
#' A logical value.
#'
#' @export
isSccsData <- function(x) {
  return(inherits(x, "SccsData"))
}
