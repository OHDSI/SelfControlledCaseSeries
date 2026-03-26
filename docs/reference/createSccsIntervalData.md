# Create SCCS era data

Create SCCS era data

## Usage

``` r
createSccsIntervalData(studyPopulation, sccsData, createSccsIntervalDataArgs)
```

## Arguments

- studyPopulation:

  An object created using the
  [`createStudyPopulation()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createStudyPopulation.md)
  function.

- sccsData:

  An object of type
  [SccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsData-class.md)
  as created using the
  [getDbSccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/getDbSccsData.md)
  function.

- createSccsIntervalDataArgs:

  An object of type `CreateSccsIntervalDataArgs` as created by the
  `createCreateSccsIntervalDataArgs` function.

## Value

An object of type `SccsIntervalData`.

## Details

This function creates covariates based on the data in the `sccsData`
argument, according to the provided settings. It chops patient time into
periods during which all covariates remain constant. The output details
these periods, their durations, and a sparse representation of the
covariate values.
