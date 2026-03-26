# Create default SelfControlledCaseSeries multi-threading settings

Create SelfControlledCaseSeries multi-threading settings based on the
maximum number of cores to be used.

## Usage

``` r
createDefaultSccsMultiThreadingSettings(maxCores)
```

## Arguments

- maxCores:

  Maximum number of CPU cores to use.

## Value

An object of type `SccsMultiThreadingSettings`.

## See also

[`createSccsMultiThreadingSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsMultiThreadingSettings.md)

## Examples

``` r
settings <- createDefaultSccsMultiThreadingSettings(10)
```
