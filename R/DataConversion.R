# @file DataConversion.R
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

in.ff <- function(a,b){
  if (length(b) == 0)
    return(ff::as.ff(rep(FALSE, length(a))))
  else
    return(ffbase::ffmatch(x = a, table=b, nomatch = 0L) > 0L)
}

#' @export
createSccsEras <- function(sccsData, 
                           covariateStart = 0,
                           covariatePersistencePeriod = 0, 
                           naivePeriod = 0, 
                           firstOutcomeOnly = FALSE,
                           excludeConceptIds = NULL){
  if (is.null(excludeConceptIds)) {
    erasSubset <- sccsData$eras
  } else {
    t <- in.ff(sccsData$eras$conceptId, ff::as.ff(excludeConceptIds))
    erasSubset <- sccsData$eras[ffbase::ffwhich(t,t == FALSE),]
  }
  metaData <- sccsData$metaData
  metaData$call2 <- match.call() 
  data <- convertToSccs.ffdf(sccsData$cases, erasSubset, covariateStart, covariatePersistencePeriod, naivePeriod, firstOutcomeOnly)
  result <- list(outcomes = data$outcomes,
                 covariates = data$covariates,
                 covariateRef = ff::clone(sccsData$covariateRef),
                 metaData = metaData)
  open(result$outcomes)
  open(result$covariates)
  open(result$covariateRef)
  class(result) <- "sccsEras"
  return(result)
}

#' @export
convertToSccs.data.frame <- function(cases, eras, covariateStart = 0, covariatePersistencePeriod = 0, naivePeriod = 0, firstOutcomeOnly = FALSE) {
  .Call('SelfControlledCaseSeries_convertToSccs', PACKAGE = 'SelfControlledCaseSeries', cases, eras, covariateStart, covariatePersistencePeriod, naivePeriod, firstOutcomeOnly)
}

.appendToData <- function(data, batch){
  n <- nrow(batch$outcomes)
  if (is.null(data$outcomes)){
    if (n == 0){
      data$outcomes <- batch$outcomes #ffdf cannot contain 0 rows, so return data.frame instead
      warning("Data has zero rows, returning an empty data frame")
    } else
      data$outcomes <- ff::as.ffdf(batch$outcomes)    
  } else if (n != 0){
    batch$outcomes$rowId = batch$outcomes$rowId + nrow(data$outcomes) 
    batch$covariates$rowId = batch$covariates$rowId + nrow(data$outcomes)   
    data$outcomes <- ffbase::ffdfappend(data$outcomes,batch$outcomes)
  }
  
  n <- nrow(batch$covariates)
  if (is.null(data$covariates)){
    if (n == 0){
      data$covariates <- batch$covariates #ffdf cannot contain 0 rows, so return data.frame instead
      warning("Data has zero rows, returning an empty data frame")
    } else
      data$covariates <- ff::as.ffdf(batch$covariates)    
  } else if (n != 0){
    data$covariates <- ffbase::ffdfappend(data$covariates,batch$covariates)
  }
  return(data)
}

.lastRowNotHavingThisValue <- function(column, value){
  if (column[1] == value)
    return(0)
  for (i in length(column):1){
    if (column[i] != value)
      return(i)
  }
  return(0)
}

.convertToSccsFromBatchableSources <- function(casesSource,
                                               erasSource,
                                               getCasesBatch,
                                               getErasBatch,
                                               isDone,
                                               covariateStart,
                                               covariatePersistencePeriod, 
                                               naivePeriod, 
                                               firstOutcomeOnly){
  data <- list(outcomes = NULL, covariates = NULL)
  
  #Fetch data in batches:
  batchCases <- getCasesBatch(casesSource)
  
  lastUsedCase <- 0
  spillOverEras <- NULL
  while (!isDone(erasSource)){
    #Get covars:
    batchEras <- getErasBatch(erasSource)
    lastObservationPeriodId <- batchEras$observationPeriodId[nrow(batchEras)]
    endCompleteRow <- .lastRowNotHavingThisValue(batchEras$observationPeriodId,lastObservationPeriodId)
    
    if (endCompleteRow == 0){ #Entire batch is about 1 row
      if (!is.null(spillOverEras)){
        if (spillOverEras$observationPeriodId[1] == batchEras$observationPeriodId[1]){ #SpilloverCovars contains info on same row
          spillOverEras <- rbind(spillOverEras,batchEras)
          erasToConvert <- NULL
        } else { #SplilloverCovars contains covars for a different row
          erasToConvert <- spillOverEras
          spillOverEras <- batchEras
        }
      } else {
        spillOverEras <- batchEras
      }
    } else { #Batch is about different rows (so at least one is complete)
      if (!is.null(spillOverEras)){
        erasToConvert <- rbind(spillOverEras,batchEras[1:endCompleteRow,])      
      } else {
        erasToConvert <- batchEras[1:endCompleteRow,]
      }
      spillOverEras <- batchEras[(endCompleteRow+1):nrow(batchEras),]
    }    
    
    #Get matching cases:
    if (!is.null(erasToConvert)){ # There is a complete row
      completeObservationPeriodId = erasToConvert$observationPeriodId[nrow(erasToConvert)]
      endCompleteRowInCases <- which(batchCases$observationPeriodId == completeObservationPeriodId)
      while (length(endCompleteRowInCases) == 0 & !isDone(casesSource)){
        if (lastUsedCase == nrow(batchCases)){
          batchCases <- getCasesBatch(casesSource)
        } else {      
          newBatchCases <- getCasesBatch(casesSource)
          batchCases <- rbind(batchCases[(lastUsedCase+1):nrow(batchCases),],newBatchCases)          
        }
        lastUsedCase = 0
        endCompleteRowInCases <- which(batchCases$observationPeriodId == completeObservationPeriodId)
      }
      #Convert and append to ffdf:
      batch <- convertToSccs.data.frame(batchCases[(lastUsedCase+1):endCompleteRowInCases,],
                                        erasToConvert,
                                        covariateStart,
                                        covariatePersistencePeriod, 
                                        naivePeriod, 
                                        firstOutcomeOnly)
      data <- .appendToData(data, batch)
      lastUsedCase = endCompleteRowInCases
    }
  }
  #End of covar batches, add spillover to Cyclops:
  erasToConvert <- spillOverEras
  
  completeObservationPeriodId = erasToConvert$observationPeriodId[nrow(erasToConvert)]
  endCompleteRowInCases <- which(batchCases$observationPeriodId == completeObservationPeriodId)
  while (length(endCompleteRowInCases) == 0 & !isDone(casesSource)){
    if (lastUsedCase == nrow(batchCases)){
      batchCases <- getCasesBatch(casesSource)
    } else {      
      batchCases <- getCasesBatch(casesSource)
      batchCases <- rbind(batchCases[(lastUsedCase+1):nrow(batchCases),],newBatchCases)          
    }
    lastUsedCase = 0
    endCompleteRowInCases <- which(batchCases$observationPeriodId == completeObservationPeriodId)
  }
  
  #Convert and append to ffdf:
  batch <- convertToSccs.data.frame(batchCases[(lastUsedCase+1):endCompleteRowInCases,],
                                    erasToConvert,
                                    covariateStart,
                                    covariatePersistencePeriod, 
                                    naivePeriod, 
                                    firstOutcomeOnly)
  data <- .appendToData(data, batch)
  return(data)
}


#' @export
convertToSccs.ffdf <- function(cases, 
                               eras,
                               covariateStart = 0,
                               covariatePersistencePeriod = 0, 
                               naivePeriod = 0, 
                               firstOutcomeOnly = FALSE){
  casesSource <- new.env()
  assign("data",cases,envir=casesSource)
  assign("chunks",ff::chunk.ffdf(cases),envir=casesSource)
  assign("cursor",1,envir=casesSource)
  erasSource <- new.env()
  assign("data",eras,envir=erasSource)
  assign("chunks",ff::chunk.ffdf(eras),envir=erasSource)
  assign("cursor",1,envir=erasSource)
  
  getCasesBatch <- function(casesSource){
    data <- get("data",envir=casesSource)
    chunks <- get("chunks",envir=casesSource)
    cursor <- get("cursor",envir=casesSource)
    batchCases <- data[chunks[[cursor]],]
    assign("cursor",cursor+1,envir=casesSource)
    batchCases
  }
  
  getErasBatch <- function(erasSource){
    data <- get("data",envir=erasSource)
    chunks <- get("chunks",envir=erasSource)
    cursor <- get("cursor",envir=erasSource)
    batchEras <- data[chunks[[cursor]],]
    assign("cursor",cursor+1,envir=erasSource)
    batchEras
  }
  
  isDone <- function(dataSource){
    chunks <- get("chunks",envir=dataSource)
    cursor <- get("cursor",envir=dataSource)
    cursor > length(chunks)
  }
  
  result <- .convertToSccsFromBatchableSources(casesSource,
                                               erasSource,
                                               getCasesBatch,
                                               getErasBatch,
                                               isDone,
                                               covariateStart,
                                               covariatePersistencePeriod, 
                                               naivePeriod, 
                                               firstOutcomeOnly)
  return(result)
}

