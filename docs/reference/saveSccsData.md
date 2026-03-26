# Save the cohort method data to file

Saves an object of type
[SccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsData-class.md)
to a file.

## Usage

``` r
saveSccsData(sccsData, file)
```

## Arguments

- sccsData:

  An object of type
  [SccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsData-class.md)
  as created using the
  [getDbSccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/getDbSccsData.md)
  function.

- file:

  The name of the file where the data will be written. If the file
  already exists it will be overwritten.

## Value

Returns no output.
