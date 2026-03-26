# Check stability of outcome rate over time

Check stability of outcome rate over time

## Usage

``` r
computeTimeStability(
  studyPopulation,
  sccsModel = NULL,
  maxRatio = 1.1,
  alpha = 0.05
)
```

## Arguments

- studyPopulation:

  An object created using the
  [`createStudyPopulation()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createStudyPopulation.md)
  function.

- sccsModel:

  Optional: A fitted SCCS model as created using
  [`fitSccsModel()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/fitSccsModel.md).
  If the model contains splines for seasonality and or calendar time
  these will be adjusted for before computing stability.

- maxRatio:

  The maximum global ratio between the observed and expected count.

- alpha:

  The alpha (type 1 error) used to test for stability.

## Value

A tibble with one row and three columns: `ratio` indicates the estimated
mean ratio between observed and expected. `p` is the p-value against the
null-hypothesis that the ratio is smaller than `maxRatio`, and `pass` is
`TRUE` if `p` is greater than `alpha`.

## Details

DEPRECATED. Use
[`checkTimeStabilityAssumption()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/checkTimeStabilityAssumption.md)
instead.
