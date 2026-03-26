# Create a study population

Create a study population

## Usage

``` r
createStudyPopulation(sccsData, outcomeId = NULL, createStudyPopulationArgs)
```

## Arguments

- sccsData:

  An object of type
  [SccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsData-class.md)
  as created using the
  [getDbSccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/getDbSccsData.md)
  function.

- outcomeId:

  The outcome to create the era data for. If not specified it is assumed
  to be the one outcome for which the data was loaded from the database.

- createStudyPopulationArgs:

  An object of type `CreateStudyPopulationArgs` as created using the
  [`createCreateStudyPopulationArgs()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createCreateStudyPopulationArgs.md)
  function.

## Value

A `list` specifying the study population, with the following items:

- `cases`: A `tibble` with one row per observation period of a person
  with the outcome.

- `outcomes`: A `tibble` listing the days when a case has the outcome.

- `metaData`: A `list` with meta data about the study population,
  including the attrition.

## Details

Create a study population for a specific outcome, applying several
restrictions.
