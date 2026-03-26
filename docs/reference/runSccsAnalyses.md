# Run a list of analyses

Run a list of analyses

## Usage

``` r
runSccsAnalyses(
  connectionDetails,
  cdmDatabaseSchema,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
  exposureDatabaseSchema = cdmDatabaseSchema,
  exposureTable = "drug_era",
  outcomeDatabaseSchema = cdmDatabaseSchema,
  outcomeTable = "cohort",
  customCovariateDatabaseSchema = cdmDatabaseSchema,
  customCovariateTable = "cohort",
  nestingCohortDatabaseSchema = cdmDatabaseSchema,
  nestingCohortTable = "cohort",
  outputFolder,
  sccsMultiThreadingSettings = createSccsMultiThreadingSettings(),
  sccsAnalysesSpecifications
)
```

## Arguments

- connectionDetails:

  An R object of type `ConnectionDetails` created using the function
  [`DatabaseConnector::createConnectionDetails()`](https://ohdsi.github.io/DatabaseConnector/reference/createConnectionDetails.html).

- cdmDatabaseSchema:

  The name of the database schema that contains the OMOP CDM instance.
  Requires read permissions to this database. On SQL Server, this should
  specify both the database and the schema, so for example
  'cdm_instance.dbo'.

- tempEmulationSchema:

  Some database platforms like Oracle and Impala do not truly support
  temp tables. To emulate temp tables, provide a schema with write
  privileges where temp tables can be created.

- exposureDatabaseSchema:

  The name of the database schema that is the location where the
  exposure data used to define the exposure cohorts is available. If
  `exposureTable = "DRUG_ERA"`, `exposureDatabaseSchema` is not used but
  assumed to be `cdmDatabaseSchema`. Requires read permissions to this
  database.

- exposureTable:

  The table name that contains the exposure cohorts. If
  `exposureTable <> "DRUG_ERA"`, then expectation is `exposureTable` has
  format of COHORT table: cohort_concept_id, SUBJECT_ID,
  COHORT_START_DATE, COHORT_END_DATE.

- outcomeDatabaseSchema:

  The name of the database schema that is the location where the data
  used to define the outcome cohorts is available. Requires read
  permissions to this database.

- outcomeTable:

  The table name that contains the outcome cohorts.

- customCovariateDatabaseSchema:

  The name of the database schema that is the location where the custom
  covariate data is available.

- customCovariateTable:

  Name of the table holding the custom covariates. This table should
  have the same structure as the cohort table.

- nestingCohortDatabaseSchema:

  The name of the database schema that is the location where the nesting
  cohort is defined.

- nestingCohortTable:

  Name of the table holding the nesting cohort. This table should have
  the same structure as the cohort table.

- outputFolder:

  Name of the folder where all the outputs will written to.

- sccsMultiThreadingSettings:

  An object of type `SccsMultiThreadingSettings` as created using the
  [`createSccsMultiThreadingSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsMultiThreadingSettings.md)
  or
  [`createDefaultSccsMultiThreadingSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createDefaultSccsMultiThreadingSettings.md)
  functions.

- sccsAnalysesSpecifications:

  An object of type `SccsAnalysesSpecifications` as created using the
  [`createSccsAnalysesSpecifications()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsAnalysesSpecifications.md)
  function

## Value

A tibble describing for each exposure-outcome-analysisId combination
where the intermediary and outcome model files can be found, relative to
the `outputFolder`.

## Details

Run a list of analyses for the exposures-outcomes of interest. This
function will run all specified analyses against all hypotheses of
interest, meaning that the total number of outcome models is
`length(sccsAnalysisList) * length(exposuresOutcomeList)` When you
provide several analyses it will determine whether any of the analyses
have anything in common, and will take advantage of this fact.

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
