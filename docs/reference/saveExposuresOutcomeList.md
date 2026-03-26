# Save a list of `ExposuresOutcome` to file

Write a list of objects of type `ExposuresOutcome` to file. The file is
in JSON format.

## Usage

``` r
saveExposuresOutcomeList(exposuresOutcomeList, file)
```

## Arguments

- exposuresOutcomeList:

  A list of objects of type `ExposuresOutcome` as created using the
  [`createExposuresOutcome()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createExposuresOutcome.md)
  function.

- file:

  The name of the file where the results will be written
