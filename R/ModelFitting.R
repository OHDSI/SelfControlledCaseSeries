# @file ModelFitting.R
#
# Copyright 2014 Observational Health Data Sciences and Informatics
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

# outcomeConceptId <- 194133
#' @export
fitSccsModel <- function(sccsEraData,
                         exposureConceptId = NULL,
                         outcomeConceptId = NULL,
                         prior = createPrior("laplace", useCrossValidation = TRUE),
                         control = createControl(cvType = "auto",
                                                 startingVariance = 0.1,
                                                 noiseLevel = "quiet")) {
  # TODO filter covariates based on selected outcomes: t <- in.ff(sccsEraData$outcomes$outcomeId,
  # ff::as.ff(outcomeConceptId)) outcomesSubset <- sccsEraData$outcomes[ffbase::ffwhich(t,t == TRUE),]

  outcomesSubset <- sccsEraData$outcomes
  covariatesSubset <- sccsEraData$covariates

  # Add a covariateValue row to covariates with value 1:
  covariatesSubset$covariateValue <- ff::ff(vmode = "double", length = nrow(covariatesSubset))
  for (i in bit::chunk(covariatesSubset$covariateValue)) {
    covariatesSubset$covariateValue[i] <- 1
  }

  # If we're fitting a model specifically for one exposure, exclude that one from regularization:
  if (!is.null(exposureConceptId))
    prior$exclude <- exposureConceptId

  # Re-sort covariates for new interface.  TODO move sorting into SQL
  covariatesSubset <- covariatesSubset[ff::ffdforder(covariatesSubset[c("covariateId", "stratumId","rowId")]),]

  cyclopsData <- Cyclops::convertToCyclopsData(outcomesSubset,
                                               covariatesSubset,
                                               modelType = "cpr",
                                               addIntercept = FALSE,
                                               checkSorting = FALSE,
                                               checkRowIds = FALSE)
  fit <- Cyclops::fitCyclopsModel(cyclopsData, prior = prior, control = control)

  return(fit)
}

#' @export
getModel <- function(fit, sccsEraData) {
  cfs <- coef(fit)
  cfs <- data.frame(coefficient = cfs, id = as.numeric(names(cfs)))
  cfs <- merge(ff::as.ffdf(cfs),
               sccsEraData$covariateRef,
               by.x = "id",
               by.y = "conceptId",
               all.x = TRUE)
  cfs <- ff::as.ram(cfs[, c("coefficient", "id", "conceptName")])
  cfs$conceptName <- as.character(cfs$conceptName)
  cfs <- cfs[order(-abs(cfs$coefficient)), ]
  cfs
}
