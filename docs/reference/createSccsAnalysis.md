# Create a SelfControlledCaseSeries analysis specification

Create a SelfControlledCaseSeries analysis specification

## Usage

``` r
createSccsAnalysis(
  analysisId = 1,
  description = "",
  getDbSccsDataArgs,
  createStudyPopulationArgs,
  createIntervalDataArgs,
  fitSccsModelArgs
)
```

## Arguments

- analysisId:

  An integer that will be used later to refer to this specific set of
  analysis choices.

- description:

  A short description of the analysis.

- getDbSccsDataArgs:

  An object representing the arguments to be used when calling the
  [getDbSccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/getDbSccsData.md)
  function.

- createStudyPopulationArgs:

  An object representing the arguments to be used when calling the
  [getDbSccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/getDbSccsData.md)
  function.

- createIntervalDataArgs:

  An object representing the arguments to be used when calling the
  [createSccsIntervalData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsIntervalData.md)
  or
  [createScriIntervalData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createScriIntervalData.md)
  function.

- fitSccsModelArgs:

  An object representing the arguments to be used when calling the
  [fitSccsModel](https://ohdsi.github.io/SelfControlledCaseSeries/reference/fitSccsModel.md)
  function.

## Value

An object of type `SccsAnalysis`, to be used with the
[runSccsAnalyses](https://ohdsi.github.io/SelfControlledCaseSeries/reference/runSccsAnalyses.md)
function.
