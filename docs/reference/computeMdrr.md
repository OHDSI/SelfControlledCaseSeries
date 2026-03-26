# Compute the minimum detectable relative risk

Compute the minimum detectable relative risk

## Usage

``` r
computeMdrr(
  object,
  exposureCovariateId,
  alpha = 0.05,
  power = 0.8,
  twoSided = TRUE,
  method = "SRL1"
)
```

## Arguments

- object:

  An object either of type
  [SccsIntervalData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsIntervalData-class.md)
  as created using the
  [createSccsIntervalData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsIntervalData.md)
  function, or an object of type `SccsModel` as created using the
  [`fitSccsModel()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/fitSccsModel.md)
  function.

- exposureCovariateId:

  Covariate Id for the health exposure of interest.

- alpha:

  Type I error.

- power:

  1 - beta, where beta is the type II error.

- twoSided:

  Consider a two-sided test?

- method:

  The type of sample size formula that will be used. Allowable values
  are "proportion", "binomial", "SRL1", "SRL2", or "ageEffects".
  Currently "ageEffects" is not supported.

## Value

A data frame with the MDRR, number of events, time at risk, and total
time.

## Details

Compute the minimum detectable relative risk (MDRR) for a given study
population, using the observed time at risk and total time in days and
number of events. Five sample size formulas are implemented: sampling
proportion, binomial proportion, 2 signed root likelihood ratio methods,
and likelihood extension for age effects. The expressions by Musonda
(2006) are used.

## References

Musonda P, Farrington CP, Whitaker HJ (2006) Samples sizes for
self-controlled case series studies, Statistics in Medicine,
15;25(15):2618-31
