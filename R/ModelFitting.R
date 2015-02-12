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
fitModel <- function(sccsEras, 
                     exposureConceptId = NULL, 
                     outcomeConceptId,
                     prior = createPrior("laplace", useCrossValidation = TRUE),
                     control = createControl(cvType = "auto",startingVariance = 0.1, selectorType = "byPid", noiseLevel = "quiet")){
  #t <- in.ff(sccsEras$outcomes$outcomeId, ff::as.ff(outcomeConceptId))
  #outcomesSubset <- sccsEras$outcomes[ffbase::ffwhich(t,t == TRUE),]
  #TODO filter covariates based on selected outcomes
  
  outcomesSubset <- sccsEras$outcomes
  covariatesSubset <- sccsEras$covariates
  if (!is.null(exposureConceptId))
    prior$exclude = exposureConceptId
  cyclopsData <- Cyclops::convertToCyclopsData(outcomesSubset, covariatesSubset, modelType = "cpr", checkSorting = FALSE, checkRowIds = FALSE)
  fit <- Cyclops::fitCyclopsModel(cyclopsData, 
                                  prior = prior,
                                  control = control)  
  
}