# Output the full model

Output the full model

## Usage

``` r
getModel(sccsModel)
```

## Arguments

- sccsModel:

  An object of type `SccsModel` as created using the
  [`fitSccsModel()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/fitSccsModel.md)
  function.

## Value

A `tibble` with the coefficients and confidence intervals (when
not-regularized) for all covariates in the model.
