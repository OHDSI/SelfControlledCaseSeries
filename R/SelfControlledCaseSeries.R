# @file DatabaseConnector.R
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
#
# @author Observational Health Data Sciences and Informatics
# @author Martijn Schuemie


#' Convert CSV to a CCD input file
#' 
#' @description
#' \code{convertToCcdInputFile} converts era and observation period CSV file to a CCD input file.
#'
#' @param eraFile                 The full path to the era file
#' @param observationPeriodFile   The full path to the observation period file
#' @param targetFile              The full path where the CCD input file will be created
#' @param naivePeriod             Number of days at the start of the observation period that 
#' will not be included in the Poisson regression, but will be used to determine whether an 
#' outcome is a first outcome
#' @param covariateWindowStart    Number of days relative to covariate start when the risk window
#' starts. For example, if 0 then the first day of exposure to a drug will be included in the risk
#' window. 
#' @param covariateWindowEnd      Number of days relative to covariate end when the risk window ends
#' @param firstOutcomeOnly        Should we include only the first occurrence of the outcome, or all?
#' @param minCovariateSubjects    Any covariate that is non-zero for less than this number of 
#' subject will be removed from the dataset.
#' 
#' @export
convertToCcdInputFile <- function(eraFile, 
                                  observationPeriodFile, 
                                  targetFile, 
                                  naivePeriod = 180,
                                  covariateWindowStart = 0,
                                  covariateWindowEnd = 180,
                                  firstOutcomeOnly = TRUE,
                                  minCovariateSubjects = 100,
                                  javaDebugFile){
  pathToJar <- system.file("java", "SCCSConverter.jar", package="SelfControlledCaseSeries")
  .jinit(pathToJar)
  
  .jcall("java/lang/System",,"gc")
  jobject <- .jnew("DesignMatrixBuilder") 
  
  if (!missing(javaDebugFile))
    .jcall(jobject ,"V",method="setDebugFile",as.character(javaDebugFile))
  
  .jcall(jobject ,"V",method="setEra_file",eraFile)
  .jcall(jobject ,"V",method="setObservation_period_file",observationPeriodFile)
  .jcall(jobject ,"V",method="setCcd_in_file",targetFile)
  
  .jcall(jobject ,"V",method="setNaive_period",as.integer(naivePeriod))
  .jcall(jobject ,"V",method="setCovariate_window",as.integer(covariateWindowEnd))
  .jcall(jobject ,"V",method="setCovariate_days_from_start",as.integer(covariateWindowStart))
  .jcall(jobject ,"V",method="setFirst_outcome_only",firstOutcomeOnly)
  .jcall(jobject ,"V",method="setMin_covariate_subjects",as.integer(minCovariateSubjects))
  
  .jcall(jobject ,"V",method="process")
}