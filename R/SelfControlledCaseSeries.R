# @file SelfControlledCaseSeries.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
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
#' @importFrom stats aggregate coef confint dgamma nlm pgamma pnorm printCoefmat qnorm rnorm rpois runif splinefun quantile ppois dpois integrate optim pchisq rbinom
#' @importFrom utils head tail setTxtProgressBar txtProgressBar packageVersion
#' @import dplyr
#' @import Cyclops
#' @import DatabaseConnector
#' @useDynLib SelfControlledCaseSeries
NULL

cache <- new.env()
