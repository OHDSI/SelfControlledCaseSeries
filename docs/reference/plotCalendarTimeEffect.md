# Plot the calendar time effect

Plot the calendar time effect

## Usage

``` r
plotCalendarTimeEffect(
  sccsModel,
  rrLim = c(0.1, 10),
  title = NULL,
  fileName = NULL
)
```

## Arguments

- sccsModel:

  An object of type `sccsModel` as created using the
  [`fitSccsModel`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/fitSccsModel.md)
  function.

- rrLim:

  The limits on the incidence rate ratio scale in the plot.

- title:

  Optional: the main title for the plot

- fileName:

  Name of the file where the plot should be saved, for example
  'plot.png'. See the function `ggsave` in the ggplot2 package for
  supported file formats.

## Value

A Ggplot object. Use the ggsave function to save to file.

## Details

Plot the spline curve of the calendar time effect.
