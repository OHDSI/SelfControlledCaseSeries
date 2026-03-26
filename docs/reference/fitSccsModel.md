# Fit the SCCS model

Fit the SCCS model

## Usage

``` r
fitSccsModel(sccsIntervalData, fitSccsModelArgs)
```

## Arguments

- sccsIntervalData:

  An object of type
  [SccsIntervalData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsIntervalData-class.md)
  as created using the
  [createSccsIntervalData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsIntervalData.md)
  function.

- fitSccsModelArgs:

  An object of type `FitSccsModelArgs` as created by the
  [`createFitSccsModelArgs()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createFitSccsModelArgs.md)
  function.

## Value

An object of type `SccsModel`. Generic functions `print`, `coef`, and
`confint` are available.

## Details

Fits the SCCS model as a conditional Poisson regression. When allowed,
coefficients for some or all covariates can be regularized.

## References

Suchard, M.A., Simpson, S.E., Zorych, I., Ryan, P., and Madigan, D.
(2013). Massive parallelization of serial inference algorithms for
complex generalized linear models. ACM Transactions on Modeling and
Computer Simulation 23, 10
