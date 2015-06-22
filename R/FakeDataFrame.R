# @file FakeDataFrame.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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

subsetFfdfToFakeDf <- function(ffdf, index) {
  fakeDf <- vector(length = length(ffdf), mode = "list")
  for (i in 1:length(ffdf)) {
    fakeDf[[i]] <- ffdf[index, i]
  }
  names(fakeDf) <- names(ffdf)
  class(fakeDf) <- "fakeDf"
  return(fakeDf)
}

nrow.fakeDf <- function(fakeDf) {
  return(length(fakeDf[[1]]))
}

rbind.fakeDf <- function(fakeDf1, fakeDf2) {
  for (i in 1:length(fakeDf1)) {
    if (is.factor(fakeDf1[[i]])) {
      stop("R cannot concatenate factors")
    }
    fakeDf1[[i]] <- c(fakeDf1[[i]], fakeDf2[[i]])
  }
  return(fakeDf1)
}

as.ffdf.fakeDf <- function(fakeDf) {
  fun <- function(...) {
    ff::ffdf(...)
  }
  return(do.call("fun", sapply(fakeDf, ff::ff)))
}

subsetFakeDf <- function(fakeDf, index) {
  result <- vector(length = length(fakeDf), mode = "list")
  for (i in 1:length(fakeDf)) {
    result[[i]] <- fakeDf[[i]][index]
  }
  names(result) <- names(fakeDf)
  class(result) <- "fakeDf"
  return(result)
}

as.data.frame.fakeDf <- function(fakeDf) {
  class(fakeDf) <- "data.frame"
  row.names(fakeDf) <- seq_len(nrow.fakeDf(fakeDf))
  return(fakeDf)

}
