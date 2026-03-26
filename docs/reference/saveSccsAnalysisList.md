# Save a list of SccsAnalysis to file

Write a list of objects of type `SccsAnalysis` to file. The file is in
JSON format.

## Usage

``` r
saveSccsAnalysisList(sccsAnalysisList, file)
```

## Arguments

- sccsAnalysisList:

  A list of objects of type `SccsAnalysis` as created using the
  [`createSccsAnalysis()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsAnalysis.md)
  function.

- file:

  The name of the file where the results will be written
