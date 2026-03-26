# Plot the ratio of observed to expected events over calendar time.

Plot the ratio of observed to expected events over calendar time.

## Usage

``` r
plotEventToCalendarTime(
  studyPopulation,
  sccsModel = NULL,
  title = NULL,
  fileName = NULL
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
  If the model contains splines for seasonality and or calendar time a
  panel will be added with outcome counts adjusted for these splines.

- title:

  Optional: the main title for the plot

- fileName:

  Name of the file where the plot should be saved, for example
  'plot.png'. See the function
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  for supported file formats.

## Value

A ggplot object. Use the
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
function to save to file in a different format.

## Details

Plot the ratio of observed to expected events over calendar time. The
expected count expected rate considers which persons were observed
during that month, and if specified in the model, the adjustment for
season and calendar time.
