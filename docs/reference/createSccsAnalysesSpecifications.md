# Create full SCCS analysis specifications

Create full SCCS analysis specifications

## Usage

``` r
createSccsAnalysesSpecifications(
  sccsAnalysisList,
  exposuresOutcomeList,
  analysesToExclude = NULL,
  combineDataFetchAcrossOutcomes = FALSE,
  sccsDiagnosticThresholds = SelfControlledCaseSeries::createSccsDiagnosticThresholds(),
  controlType = "outcome"
)
```

## Arguments

- sccsAnalysisList:

  A list of objects of type `SccsAnalysis` as created using the
  [`createSccsAnalysis()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsAnalysis.md)
  function.

- exposuresOutcomeList:

  A list of objects of type `ExposuresOutcome` as created using the
  [`createExposuresOutcome()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createExposuresOutcome.md)
  function.

- analysesToExclude:

  Analyses to exclude. See the Analyses to Exclude section for details.

- combineDataFetchAcrossOutcomes:

  Should fetching data from the database be done one outcome at a time,
  or for all outcomes in one fetch? Combining fetches will be more
  efficient if there is large overlap in the subjects that have the
  different outcomes.

- sccsDiagnosticThresholds:

  An object of type `SccsDiagnosticThresholds` as created using
  createSccsDiagnosticThresholds().

- controlType:

  Type of negative (and positive) controls. Can be "outcome" or
  "exposure". When set to "outcome", controls with the same exposure
  (and nesting cohort) are grouped together for calibration. When set to
  "exposure", controls with the same outcome are grouped together.

## Value

An object of type `SccsAnalysesSpecifications`.

## Details

### Analyses to Exclude

Normally, `runSccsAnalyses` will run all combinations of
exposures-outcome-analyses settings. However, sometimes we may not need
all those combinations. Using the `analysesToExclude` argument, we can
remove certain items from the full matrix. This argument should be a
data frame with at least one of the following columns:

- exposureId

- outcomeId

- nestingCohortId

- analysisId

This data frame will be joined to the outcome model reference table
before executing, and matching rows will be removed. For example, if one
specifies only one exposure ID and analysis ID, then any analyses with
that exposure and that analysis ID will be skipped.
