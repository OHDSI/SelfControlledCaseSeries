# Compute P for pre-exposure risk gain

This function is deprecated. Use `computePreExposureGain()` instead.

## Usage

``` r
computePreExposureGainP(sccsData, studyPopulation, exposureEraId = NULL)
```

## Arguments

- sccsData:

  An object of type
  [SccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsData-class.md)
  as created using the
  [getDbSccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/getDbSccsData.md)
  function.

- studyPopulation:

  An object created using the
  [`createStudyPopulation()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createStudyPopulation.md)
  function.

- exposureEraId:

  The exposure to create the era data for. If not specified it is
  assumed to be the one exposure for which the data was loaded from the
  database.

## Value

A one-sided p-value for whether the rate before exposure is higher than
after, against the null of no change.#'
