# Plot the calendar time ranges spanned by each observation period.

Plot the calendar time ranges spanned by each observation period.

## Usage

``` r
plotCalendarTimeSpans(
  studyPopulation,
  maxPersons = 10000,
  title = NULL,
  fileName = NULL
)
```

## Arguments

- studyPopulation:

  An object created using the
  [`createStudyPopulation()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createStudyPopulation.md)
  function.

- maxPersons:

  The maximum number of persons to plot. If there are more than this
  number of persons a random sample will be taken to avoid visual
  clutter.

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

Plots a line per patient from their observation start to their
observation end.
