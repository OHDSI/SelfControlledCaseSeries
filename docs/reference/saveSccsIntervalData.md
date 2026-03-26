# Save the cohort method data to file

Saves an object of type
[SccsIntervalData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsIntervalData-class.md)
to a file.

## Usage

``` r
saveSccsIntervalData(sccsIntervalData, file)
```

## Arguments

- sccsIntervalData:

  An object of type
  [SccsIntervalData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsIntervalData-class.md)
  as created using the
  [createSccsIntervalData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsIntervalData.md)
  function.

- file:

  The name of the file where the data will be written. If the file
  already exists it will be overwritten.

## Value

Returns no output.
