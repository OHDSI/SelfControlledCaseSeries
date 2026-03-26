# Create a parameter object for the function getDbSccsData

Create a parameter object for the function getDbSccsData

## Usage

``` r
createGetDbSccsDataArgs(
  nestingCohortId = NULL,
  deleteCovariatesSmallCount = 0,
  studyStartDates = c(),
  studyEndDates = c(),
  maxCasesPerOutcome = 0,
  exposureIds = "exposureId",
  customCovariateIds = NULL
)
```

## Arguments

- nestingCohortId:

  A cohort definition ID identifying the records in the
  nestingCohortTable to use as nesting cohort.

- deleteCovariatesSmallCount:

  The minimum count for a covariate to appear in the data to be kept.

- studyStartDates:

  A character object specifying the minimum dates where data is used.
  Date format is 'yyyymmdd'. Use "" to indicate all time prior. See
  section for more information.

- studyEndDates:

  A character object specifying the maximum dates where data is used.
  Date format is 'yyyymmdd'. Use "" to indicate to the end of
  observation. See section for more information.

- maxCasesPerOutcome:

  If there are more than this number of cases for a single outcome cases
  will be sampled to this size. maxCasesPerOutcome = 0 indicates no
  maximum size.

- exposureIds:

  A list of identifiers to extract from the exposure table. If
  exposureTable = DRUG_ERA, exposureIds should be CONCEPT_ID. If
  exposureTable = "drug_era", exposureIds is used to select the
  drug_concept_id. If no exposure IDs are provided, all drugs or cohorts
  in the exposureTable are included as exposures.

- customCovariateIds:

  A list of cohort definition IDs identifying the records in the
  customCovariateTable to use for building custom covariates.

## Value

An object of type `GetDbSccsDataArgs`.

## Details

Create an object defining the parameter values.
