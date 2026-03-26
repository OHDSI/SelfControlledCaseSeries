# Does the model contain a seasonality effect?

Does the model contain a seasonality effect?

## Usage

``` r
hasSeasonality(sccsModel)
```

## Arguments

- sccsModel:

  An object of type `SccsModel` as created using the
  [`fitSccsModel()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/fitSccsModel.md)
  function.

## Value

TRUE if the model contains an age effect, otherwise FALSE.
