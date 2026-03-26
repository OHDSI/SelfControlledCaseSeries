# SCCS Interval Data

SccsIntervalData\` is an S4 class that inherits from
[Andromeda](https://rdrr.io/pkg/Andromeda/man/Andromeda-class.html). It
contains information on the cases and their covariates, divided in
non-overlapping time intervals.

A `SccsIntervalData` is typically created using
[`createSccsIntervalData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsIntervalData.md),
can only be saved using
[`saveSccsIntervalData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/saveSccsIntervalData.md),
and loaded using
[`loadSccsIntervalData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/loadSccsIntervalData.md).

## Usage

``` r
# S4 method for class 'SccsIntervalData'
show(object)

# S4 method for class 'SccsIntervalData'
summary(object)
```

## Arguments

- object:

  An object of type `SccsIntervalData`.
