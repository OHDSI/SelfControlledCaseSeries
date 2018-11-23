SelfControlledCaseSeries
========================

[![Build Status](https://travis-ci.org/OHDSI/SelfControlledCaseSeries.svg?branch=master)](https://travis-ci.org/OHDSI/SelfControlledCaseSeries)
[![codecov.io](https://codecov.io/github/OHDSI/SelfControlledCaseSeries/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/SelfControlledCaseSeries?branch=master)

SelfControlledCaseSeries is part of the [OHDSI Methods Library](https://ohdsi.github.io/MethodsLibrary).

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

Example
=======
```r
sccsData <- getDbSccsData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          outcomeIds = 192671,
                          exposureIds = 1124300)
covarDiclofenac = createCovariateSettings(label = "Exposure of interest",
                                          includeCovariateIds = 1124300,
                                          start = 0,
                                          end = 0,
                                          addExposedDaysToEnd = TRUE)
sccsEraData <- createSccsEraData(sccsData,
                                 naivePeriod = 180,
                                 firstOutcomeOnly = FALSE,
                                 covariateSettings = covarDiclofenac)
model <- fitSccsModel(sccsEraData)
summary(model)
# sccsModel object summary
# 
# Outcome ID: 192671
# 
# Outcome count:
#        Event count Case count
# 192671      433433     137888
# 
# Estimates:
#                               Name    ID  Estimate  lower .95  upper .95   logRr  seLogRr
#   Exposure of interest: Diclofenac  1000     1.274      1.213      1.336  0.2421  0.02431
```
Technology
==========
SelfControlledCaseSeries is an R package, with some functions implemented in C++.

System Requirements
===================
Requires R (version 3.2.2 or higher). Installation on Windows requires [RTools](http://cran.r-project.org/bin/windows/Rtools/). Libraries used in SelfControlledCaseSeries require Java.

Getting Started
===============
1. On Windows, make sure [RTools](http://cran.r-project.org/bin/windows/Rtools/) is installed.
2. The DatabaseConnector and SqlRender packages require Java. Java can be downloaded from
<a href="http://www.java.com" target="_blank">http://www.java.com</a>.
3. In R, use the following commands to download and install SelfControlledCaseSeries:

  ```r
  install.packages("drat")
  drat::addRepo("OHDSI")
  install.packages("SelfControlledCaseSeries")
  ```

User Documentation
==================
* Vignette: [Single studies using the SelfControlledCaseSeries package](https://raw.githubusercontent.com/OHDSI/SelfControlledCaseSeries/master/inst/doc/SingleStudies.pdf)
* Vignette: [Running multiple analyses at once using the SelfControlledCaseSeries package](https://raw.githubusercontent.com/OHDSI/SelfControlledCaseSeries/master/inst/doc/MultipleAnalyses.pdf)
* Package manual: [SelfControlledCaseSeries.pdf](https://raw.githubusercontent.com/OHDSI/SelfControlledCaseSeries/master/extras/SelfControlledCaseSeries.pdf)

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/SelfControlledCaseSeries/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

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
