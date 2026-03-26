# Check diagnostic for event-dependent exposure

This diagnostic tests whether there is a dependency between the event
and subsequent exposures. This requires you have indicated one of the
era covariates to be a pre-exposure window. This function simply checks
whether the confidence interval for the effect estimate of that pre-
exposure window overlaps with the `nullBounds`.

To designate an era covariate to be the pre-exposure window, set
`preExposure = TRUE` when calling
[`createEraCovariateSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createEraCovariateSettings.md).
Note that, by default, `preExposure` will be `TRUE` if `start` is
smaller than 0.

## Usage

``` r
checkEventExposureIndependenceAssumption(sccsModel, nullBounds = c(0.8, 1.25))
```

## Arguments

- sccsModel:

  A fitted SCCS model as created using
  [`fitSccsModel()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/fitSccsModel.md).

- nullBounds:

  The bounds for the null hypothesis on the incidence rate ratio scale.

## Value

A tibble with one row per pre-exposure window and four columns: `ratio`
indicates the estimates incidence rate ratio for the pre-exposure
window. `lb` and `ub` represent the upper and lower bounds of the 95
percent confidence interval, and `pass` is `TRUE` if the confidence
interval intersects the null bounds.
