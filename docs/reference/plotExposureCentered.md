# Plot information centered around the start of exposure

Plot information centered around the start of exposure

## Usage

``` r
plotExposureCentered(
  studyPopulation,
  sccsData,
  exposureEraId = NULL,
  highlightExposedEvents = TRUE,
  title = NULL,
  fileName = NULL
)
```

## Arguments

- studyPopulation:

  An object created using the
  [`createStudyPopulation()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createStudyPopulation.md)
  function.

- sccsData:

  An object of type
  [SccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsData-class.md)
  as created using the
  [getDbSccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/getDbSccsData.md)
  function.

- exposureEraId:

  The exposure to create the era data for. If not specified it is
  assumed to be the one exposure for which the data was loaded from the
  database.

- highlightExposedEvents:

  Highlight events that occurred during the exposure era using a
  different color?

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

This plot shows the number of events and the number of subjects under
observation in week-sized intervals relative to the start of the first
exposure.
