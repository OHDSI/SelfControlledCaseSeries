# Create a parameter object for the [`createScriIntervalData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createScriIntervalData.md) function

Create a parameter object for the
[`createScriIntervalData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createScriIntervalData.md)
function

## Usage

``` r
createCreateScriIntervalDataArgs(eraCovariateSettings, controlIntervalSettings)
```

## Arguments

- eraCovariateSettings:

  Either an object of type `EraCovariateSettings` as created using the
  [`createEraCovariateSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createEraCovariateSettings.md)
  function, or a list of such objects.

- controlIntervalSettings:

  An object of type `ControlIntervalSettings` as created using the
  [`createControlIntervalSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createControlIntervalSettings.md)
  function.

## Value

An object of type `CreateScriIntervalDataArgs`.

## Details

Create an object defining the parameter values.
