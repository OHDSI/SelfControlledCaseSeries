# Check diagnostic for event-dependent observation end

This diagnostic tests whether there is a dependency between the event
and the end of observation. It does so by adding a probe window at the
end of observation, and checking whether the rate of the outcome is
elevated (or decreased) during this window.

The end of observation probe window will automatically be added to the
model by the
[`createSccsIntervalData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsIntervalData.md)
function, unless the `endOfObservationEraLength` argument is set to 0.
This function extracts the estimate for that window from the model, and
compares it to the `nullBounds`.#'

## Usage

``` r
checkEventObservationIndependenceAssumption(sccsModel, nullBounds = c(0.5, 2))
```

## Arguments

- sccsModel:

  A fitted SCCS model as created using
  [`fitSccsModel()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/fitSccsModel.md).

- nullBounds:

  The bounds for the null hypothesis on the incidence rate ratio scale.

## Value

A tibble with one row and four columns: `ratio` indicates the estimates
incidence rate ratio for the probe at the end of observation. `lb` and
`ub` represent the upper and lower bounds of the 95 percent confidence
interval, and `pass` is `TRUE` if the confidence interval intersects the
null bounds.
