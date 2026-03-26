# Create exposure definition

Create exposure definition

## Usage

``` r
createExposure(exposureId, exposureIdRef = "exposureId", trueEffectSize = NA)
```

## Arguments

- exposureId:

  An integer used to identify the exposure in the exposure cohort table.

- exposureIdRef:

  A string used to refer to the exposure when defining covariates using
  the
  [`createEraCovariateSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createEraCovariateSettings.md)
  function.

- trueEffectSize:

  For negative and positive controls: the known true effect size. To be
  used for empirical calibration. Negative controls have
  `trueEffectSize = 1`. If the true effect size is unknown, use
  `trueEffectSize = NA`.

## Value

An object of type `Exposure`.

## Details

Create an exposure definition, to be used with the
[createExposuresOutcome](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createExposuresOutcome.md)
function.
