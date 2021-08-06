SelfControlledCaseSeries
========================

[![Build Status](https://github.com/OHDSI/SelfControlledCaseSeries/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/SelfControlledCaseSeries/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/OHDSI/SelfControlledCaseSeries/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/SelfControlledCaseSeries?branch=master)

SelfControlledCaseSeries is part of [HADES](https://ohdsi.github.io/Hades).

Introduction
============
SelfControlledCaseSeries is an R package for performing Self-Controlled Case Series (SCCS) analyses in an observational database in the OMOP Common Data Model.

Features
========
- Extracts the necessary data from a database in OMOP Common Data Model format.
- Optionally add seasonality using a spline function.
- Optionally add age using a spline function.
- Optionally correct for event-dependent censoring of the observation period.
- Optionally add many covariates in one analysis (e.g. all drugs).
- Options for constructing different types of covariates and risk windows, including pre-exposure windows (to capture contra-indications).
- Optionally use regularization on all covariates except the outcome of interest.
- Also provides the self-controlled risk interval design as a special case of the SCCS.

Example
=======

```r
sccsData <- getDbSccsData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          outcomeIds = 192671,
                          exposureIds = 1124300)

studyPop <- createStudyPopulation(sccsData = sccsData,
                                  outcomeId = 192671,
                                  firstOutcomeOnly = FALSE,
                                  naivePeriod = 180)

covarDiclofenac = createEraCovariateSettings(label = "Exposure of interest",
                                             includeEraIds = 1124300,
                                             start = 0,
                                             end = 0,
                                             endAnchor = "era end")

sccsIntervalData <- createSccsIntervalData(studyPop,
                                           sccsData,
                                           eraCovariateSettings = covarDiclofenac)
model <- fitSccsModel(sccsIntervalData)
model
# SccsModel object
# 
# Outcome ID: 192671
# 
# Outcome count:
#        outcomeSubjects outcomeEvents outcomeObsPeriods
# 192671          272243        387158            274449
# 
# Estimates:
# # A tibble: 1 x 7
#   Name                                ID Estimate LB95CI UB95CI logRr seLogRr
#   <chr>                            <dbl>    <dbl>  <dbl>  <dbl> <dbl>   <dbl>
# 1 Exposure of interest: Diclofenac  1000     1.18   1.13   1.24 0.167  0.0230
```

Technology
==========
SelfControlledCaseSeries is an R package, with some functions implemented in C++.

System Requirements
===================
Requires R (version 3.2.2 or higher). Installation on Windows requires [RTools](https://cran.r-project.org/bin/windows/Rtools/). Libraries used in SelfControlledCaseSeries require Java.

Installation
============

1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including Java.

2. In R, use the following commands to download and install MethodEvaluation:

  ```r
  install.packages("remotes")
  remotes::install_github("ohdsi/SelfControlledCaseSeries")
  ```

User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/SelfControlledCaseSeries).

PDF versions of the documentation are also available:
* Vignette: [Single studies using the SelfControlledCaseSeries package](https://raw.githubusercontent.com/OHDSI/SelfControlledCaseSeries/master/inst/doc/SingleStudies.pdf)
* Vignette: [Running multiple analyses at once using the SelfControlledCaseSeries package](https://raw.githubusercontent.com/OHDSI/SelfControlledCaseSeries/master/inst/doc/MultipleAnalyses.pdf)
* Package manual: [SelfControlledCaseSeries.pdf](https://raw.githubusercontent.com/OHDSI/SelfControlledCaseSeries/master/extras/SelfControlledCaseSeries.pdf)

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/SelfControlledCaseSeries/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.

License
=======
SelfControlledCaseSeries is licensed under Apache License 2.0

Development
===========
SelfControlledCaseSeries is being developed in R Studio.

### Development status

Beta

# Acknowledgements
- This project is supported in part through the National Science Foundation grant IIS 1251151.
- Part of the code is based on the SCCS package by Yonas Ghebremichael-Weldeselassie, Heather Whitaker, and Paddy Farrington.
