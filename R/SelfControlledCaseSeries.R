# @file SelfControlledCaseSeries.R
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

#' @keywords internal
"_PACKAGE"

#' @importFrom Rcpp evalCpp
#' @importFrom SqlRender loadRenderTranslateSql translateSql
#' @importFrom grDevices rgb
#' @importFrom methods is
#' @importFrom stats aggregate coef confint dgamma nlm pgamma pnorm printCoefmat qnorm rnorm rpois runif splinefun start end
#' @import bit
#' @import Cyclops
#' @import DatabaseConnector
#' @useDynLib SelfControlledCaseSeries
NULL

.onLoad <- function(libname, pkgname) {
  # Copied this from the ff package:
  if (is.null(getOption("ffmaxbytes"))) {
    # memory.limit is windows specific
    if (.Platform$OS.type == "windows") {
      if (getRversion() >= "2.6.0") {
        # memory.limit was silently changed from 2.6.0 to return in MB instead of bytes
        options(ffmaxbytes = 0.5 * utils::memory.limit() * (1024^2))
      } else {
        options(ffmaxbytes = 0.5 * utils::memory.limit())
      }
    } else {
      # some magic constant
      options(ffmaxbytes = 0.5 * 1024^3)
    }
  }

  # Workaround for problem with ff on machines with lots of memory (see
  # https://github.com/edwindj/ffbase/issues/37)
  options(ffbatchbytes = min(getOption("ffbatchbytes"), .Machine$integer.max / 10))
  options(ffmaxbytes = min(getOption("ffmaxbytes"), .Machine$integer.max * 6))

  # Simulate behavior before R 3.6.0. Some explicit ff and ffbase calss so implicit calls work in future:
  ffbase::any.ff(ff::as.ff(c(TRUE, FALSE)))
}

sccs <- function() {
  # todo
}
