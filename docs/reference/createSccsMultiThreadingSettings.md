# Create SelfControlledCaseSeries multi-threading settings

Create SelfControlledCaseSeries multi-threading settings

## Usage

``` r
createSccsMultiThreadingSettings(
  getDbSccsDataThreads = 1,
  createStudyPopulationThreads = 1,
  createIntervalDataThreads = 1,
  fitSccsModelThreads = 1,
  cvThreads = 1,
  calibrationThreads = 1
)
```

## Arguments

- getDbSccsDataThreads:

  The number of parallel threads to use for building the `SccsData`
  objects.

- createStudyPopulationThreads:

  The number of parallel threads to use for building the
  `studyPopulation` objects.

- createIntervalDataThreads:

  The number of parallel threads to use for building the
  `SccsIntervalData` objects.

- fitSccsModelThreads:

  The number of parallel threads to use for fitting the models.

- cvThreads:

  The number of parallel threads to use for the cross- validation when
  estimating the hyperparameter for the outcome model. Note that the
  total number of CV threads at one time could be
  `fitSccsModelThreads * cvThreads`.

- calibrationThreads:

  The number of parallel threads to use for empirical calibration.

## Value

An object of type `SccsMultiThreadingSettings`.

## See also

[`createDefaultSccsMultiThreadingSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createDefaultSccsMultiThreadingSettings.md)
