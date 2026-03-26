# Load data for SCCS from the database

Load all data needed to perform an SCCS analysis from the database.

## Usage

``` r
getDbSccsData(
  connectionDetails,
  cdmDatabaseSchema,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
  outcomeDatabaseSchema = cdmDatabaseSchema,
  outcomeTable = "condition_era",
  outcomeIds,
  exposureDatabaseSchema = cdmDatabaseSchema,
  exposureTable = "drug_era",
  customCovariateDatabaseSchema = cdmDatabaseSchema,
  customCovariateTable = "cohort",
  nestingCohortDatabaseSchema = cdmDatabaseSchema,
  nestingCohortTable = "cohort",
  getDbSccsDataArgs
)
```

## Arguments

- connectionDetails:

  An R object of type `ConnectionDetails` created using the function
  [`DatabaseConnector::createConnectionDetails()`](https://ohdsi.github.io/DatabaseConnector/reference/createConnectionDetails.html)
  function.

- cdmDatabaseSchema:

  The name of the database schema that contains the OMOP CDM instance.
  Requires read permissions to this database. On SQL Server, this should
  specify both the database and the schema, so for example
  'cdm_instance.dbo'.

- tempEmulationSchema:

  Some database platforms like Oracle and Impala do not truly support
  temp tables. To emulate temp tables, provide a schema with write
  privileges where temp tables can be created.

- outcomeDatabaseSchema:

  The name of the database schema that is the location where the data
  used to define the outcome cohorts is available. If
  `outcomeTable = "condition_era"`, `outcomeDatabaseSchema` is not used.
  Requires read permissions to this database.

- outcomeTable:

  The table name that contains the outcome cohorts. If `outcomeTable` is
  not `"condition_era"`, then expectation is `outcomeTable` has format
  of cohort table (see details).

- outcomeIds:

  A list of IDs used to define outcomes. If `outcomeTable` is not
  `"condition_era"` the list contains records found in the
  `cohort_definition_id` field.

- exposureDatabaseSchema:

  The name of the database schema that is the location where the
  exposure data used to define the exposure eras is available. If
  `exposureTable = "drug_era"`, `exposureDatabaseSchema` is not used but
  assumed to be equal to `cdmDatabaseSchema`. Requires read permissions
  to this database.

- exposureTable:

  The tablename that contains the exposure cohorts. If `exposureTable`
  is not "drug_era", then expectation is `exposureTable` has format of a
  cohort table (see details).

- customCovariateDatabaseSchema:

  The name of the database schema that is the location where the custom
  covariate data is available.

- customCovariateTable:

  Name of the table holding the custom covariates. This table should
  have the same structure as the cohort table (see details).

- nestingCohortDatabaseSchema:

  The name of the database schema that is the location where the nesting
  cohort is defined.

- nestingCohortTable:

  Name of the table holding the nesting cohort. This table should have
  the same structure as the cohort table (see details).

- getDbSccsDataArgs:

  An object of type `GetDbSccsDataArgs` as created by the
  [`createGetDbSccsDataArgs()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createGetDbSccsDataArgs.md)
  function.

## Value

An
[SccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsData-class.md)
object.

## Details

This function downloads several types of information:

- Information on the occurrences of the outcome(s) of interest. Note
  that information for multiple outcomes can be fetched in one go, and
  later the specific outcome can be specified for which we want to build
  a model.

- Information on the observation time and age for the people with the
  outcomes.

- Information on exposures of interest which we want to include in the
  model.

Five different database schemas can be specified, for five different
types of information: The

- **cdmDatabaseSchema** is used to extract patient age and observation
  period. The

- **outcomeDatabaseSchema** is used to extract information about the
  outcomes, the

- **exposureDatabaseSchema** is used to retrieve information on
  exposures, and the

- **customCovariateDatabaseSchema** is optionally used to find
  additional, user-defined covariates. All four locations could point to
  the same database schema.

- **nestingCohortDatabaseSchema** is optionally used to define a cohort
  in which the analysis is nested, for example a cohort of diabetics
  patients.

All five locations could point to the same database schema.

Cohort tables are assumed to have the following fields:
`cohort_definition_id`, `subject_id`, `cohort_start_date`, and
`cohort_end_date.`

## Study period start and end dates

A study start and end date define a period when patient data will be
included in the analysis. Multiple non-overlapping periods can be
defined, which for example will allow for excluding the time of the
COVID pandemic, when most outcome rates were unstable.
