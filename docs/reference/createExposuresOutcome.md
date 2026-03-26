# Create a exposures-outcome combination.

Create a exposures-outcome combination.

## Usage

``` r
createExposuresOutcome(outcomeId, exposures, nestingCohortId = NULL)
```

## Arguments

- outcomeId:

  An integer used to identify the outcome in the outcome cohort table.

- exposures:

  A list of object of type `Exposure` as created by
  [`createExposure()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createExposure.md).

- nestingCohortId:

  (Optional) the nesting cohort ID.

## Value

An object of type `ExposuresOutcome`.

## Details

Create a set of hypotheses of interest, to be used with the
[`runSccsAnalyses()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/runSccsAnalyses.md)
function.
