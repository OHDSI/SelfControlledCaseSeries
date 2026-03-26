# Package index

## Getting data and creating a study population

Functions for getting the necessary data from the database in Common
Data Model, and creating a study population.

- [`show(`*`<SccsData>`*`)`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsData-class.md)
  [`summary(`*`<SccsData>`*`)`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsData-class.md)
  : SCCS Data
- [`getDbSccsData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/getDbSccsData.md)
  : Load data for SCCS from the database
- [`saveSccsData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/saveSccsData.md)
  : Save the cohort method data to file
- [`loadSccsData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/loadSccsData.md)
  : Load the cohort method data from a file
- [`isSccsData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/isSccsData.md)
  : Check whether an object is a SccsData object
- [`createStudyPopulation()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createStudyPopulation.md)
  : Create a study population

## Interval data

Functions for creating interval data.

- [`show(`*`<SccsIntervalData>`*`)`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsIntervalData-class.md)
  [`summary(`*`<SccsIntervalData>`*`)`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsIntervalData-class.md)
  : SCCS Interval Data
- [`createEraCovariateSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createEraCovariateSettings.md)
  : Create era covariate settings
- [`createAgeCovariateSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createAgeCovariateSettings.md)
  : Create age covariate settings
- [`createSeasonalityCovariateSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSeasonalityCovariateSettings.md)
  : Create seasonality settings
- [`createCalendarTimeCovariateSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createCalendarTimeCovariateSettings.md)
  : Create calendar time settings
- [`createControlIntervalSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createControlIntervalSettings.md)
  : Create control interval settings
- [`createSccsIntervalData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsIntervalData.md)
  : Create SCCS era data
- [`createScriIntervalData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createScriIntervalData.md)
  : Create Self-Controlled Risk Interval (SCRI) era data
- [`saveSccsIntervalData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/saveSccsIntervalData.md)
  : Save the cohort method data to file
- [`loadSccsIntervalData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/loadSccsIntervalData.md)
  : Load the cohort method data from a file
- [`isSccsIntervalData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/isSccsIntervalData.md)
  : Check whether an object is a SccsIntervalData object

## Model fitting

Functions for creating and viewing models.

- [`fitSccsModel()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/fitSccsModel.md)
  : Fit the SCCS model
- [`getModel()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/getModel.md)
  : Output the full model
- [`plotAgeEffect()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/plotAgeEffect.md)
  : Plot the age effect
- [`plotSeasonality()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/plotSeasonality.md)
  : Plot the seasonality effect
- [`plotCalendarTimeEffect()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/plotCalendarTimeEffect.md)
  : Plot the calendar time effect
- [`hasAgeEffect()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/hasAgeEffect.md)
  : Does the model contain an age effect?
- [`hasSeasonality()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/hasSeasonality.md)
  : Does the model contain a seasonality effect?
- [`hasCalendarTimeEffect()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/hasCalendarTimeEffect.md)
  : Does the model contain a calendar time effect?

## Diagnostics

Functions for producing various study diagnostics.

- [`checkEventExposureIndependenceAssumption()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/checkEventExposureIndependenceAssumption.md)
  : Check diagnostic for event-dependent exposure
- [`checkEventObservationIndependenceAssumption()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/checkEventObservationIndependenceAssumption.md)
  : Check diagnostic for event-dependent observation end
- [`checkRareOutcomeAssumption()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/checkRareOutcomeAssumption.md)
  : Check if rare outcome assumption is violated
- [`checkTimeStabilityAssumption()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/checkTimeStabilityAssumption.md)
  : Check stability of outcome rate over time
- [`computeMdrr()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/computeMdrr.md)
  : Compute the minimum detectable relative risk
- [`computePreExposureGainP()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/computePreExposureGainP.md)
  : Compute P for pre-exposure risk gain
- [`computeTimeStability()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/computeTimeStability.md)
  : Check stability of outcome rate over time
- [`getAttritionTable()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/getAttritionTable.md)
  : Get the attrition table for a population
- [`plotAgeSpans()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/plotAgeSpans.md)
  : Plot the age ranges spanned by each observation period.
- [`plotCalendarTimeSpans()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/plotCalendarTimeSpans.md)
  : Plot the calendar time ranges spanned by each observation period.
- [`plotEventObservationDependence()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/plotEventObservationDependence.md)
  : Plot time from event to observation end for censored and uncensored
  time.
- [`plotEventToCalendarTime()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/plotEventToCalendarTime.md)
  : Plot the ratio of observed to expected events over calendar time.
- [`plotExposureCentered()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/plotExposureCentered.md)
  : Plot information centered around the start of exposure

## Running multiple analyses

Functions for running multiple analyses in an efficient way.

- [`createCreateSccsIntervalDataArgs()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createCreateSccsIntervalDataArgs.md)
  :

  Create a parameter object for the
  [`createSccsIntervalData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsIntervalData.md)
  function

- [`createCreateScriIntervalDataArgs()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createCreateScriIntervalDataArgs.md)
  :

  Create a parameter object for the
  [`createScriIntervalData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createScriIntervalData.md)
  function

- [`createCreateStudyPopulationArgs()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createCreateStudyPopulationArgs.md)
  :

  Create a parameter object for the
  [`createStudyPopulation()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createStudyPopulation.md)
  function

- [`createFitSccsModelArgs()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createFitSccsModelArgs.md)
  : Create a parameter object for the function fitSccsModel

- [`createGetDbSccsDataArgs()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createGetDbSccsDataArgs.md)
  : Create a parameter object for the function getDbSccsData

- [`createExposure()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createExposure.md)
  : Create exposure definition

- [`createExposuresOutcome()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createExposuresOutcome.md)
  : Create a exposures-outcome combination.

- [`createSccsAnalysesSpecifications()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsAnalysesSpecifications.md)
  : Create full SCCS analysis specifications

- [`saveExposuresOutcomeList()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/saveExposuresOutcomeList.md)
  :

  Save a list of `ExposuresOutcome` to file

- [`loadExposuresOutcomeList()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/loadExposuresOutcomeList.md)
  :

  Load a list of `ExposuresOutcome` from file

- [`createSccsAnalysis()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsAnalysis.md)
  : Create a SelfControlledCaseSeries analysis specification

- [`saveSccsAnalysisList()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/saveSccsAnalysisList.md)
  : Save a list of SccsAnalysis to file

- [`loadSccsAnalysisList()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/loadSccsAnalysisList.md)
  : Load a list of sccsAnalysis from file

- [`convertJsonToSccsAnalysesSpecifications()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/convertJsonToSccsAnalysesSpecifications.md)
  : Convert JSON to SccsAnalysesSpecifications

- [`convertSccsAnalysesSpecificationsToJson()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/convertSccsAnalysesSpecificationsToJson.md)
  : Convert SccsAnalysesSpecifications to JSON

- [`convertUntypedListToSccsAnalysesSpecifications()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/convertUntypedListToSccsAnalysesSpecifications.md)
  : Convert untyped list to SccsAnalysesSpecifications

- [`createSccsMultiThreadingSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsMultiThreadingSettings.md)
  : Create SelfControlledCaseSeries multi-threading settings

- [`createDefaultSccsMultiThreadingSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createDefaultSccsMultiThreadingSettings.md)
  : Create default SelfControlledCaseSeries multi-threading settings

- [`runSccsAnalyses()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/runSccsAnalyses.md)
  : Run a list of analyses

- [`getFileReference()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/getFileReference.md)
  : Get file reference

- [`getResultsSummary()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/getResultsSummary.md)
  : Get a summary report of the analyses results

- [`getDiagnosticsSummary()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/getDiagnosticsSummary.md)
  : Get a summary report of the analyses diagnostics

- [`createSccsDiagnosticThresholds()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsDiagnosticThresholds.md)
  : Create SCCS diagnostics thresholds

- [`exportToCsv()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/exportToCsv.md)
  : Export SCCSresults to CSV files

## Result upload

Uploading results to a database.

- [`createResultsDataModel()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createResultsDataModel.md)
  : Create the results data model tables on a database server.
- [`getDataMigrator()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/getDataMigrator.md)
  : Get database migrations instance
- [`getResultsDataModelSpecifications()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/getResultsDataModelSpecifications.md)
  : Get specifications for SelfControlledCaseSeries results data model
- [`migrateDataModel()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/migrateDataModel.md)
  : Migrate Data model
- [`uploadResults()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/uploadResults.md)
  : Upload results to the database server.

## Simulation

Functions for simulating cohort method data objects.

- [`createSimulationRiskWindow()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSimulationRiskWindow.md)
  : Create a risk window definition for simulation
- [`createSccsSimulationSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsSimulationSettings.md)
  : Create SCCS simulation settings
- [`simulateSccsData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/simulateSccsData.md)
  : Simulate SCCS data

## Helper functions

Various helper functions

- [`cyclicSplineDesign()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/cyclicSplineDesign.md)
  : Create a design matrix for a cyclic spline
