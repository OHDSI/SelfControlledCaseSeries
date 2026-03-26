# SCCS Data

`SccsData` is an S4 class that inherits from
[Andromeda](https://rdrr.io/pkg/Andromeda/man/Andromeda-class.html). It
contains information on the cases and their covariates.

A `SccsData` is typically created using
[`getDbSccsData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/getDbSccsData.md),
can only be saved using
[`saveSccsData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/saveSccsData.md),
and loaded using
[`loadSccsData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/loadSccsData.md).

## Usage

``` r
# S4 method for class 'SccsData'
show(object)

# S4 method for class 'SccsData'
summary(object)
```

## Arguments

- object:

  An object of type `SccsData`.
