# Create a design matrix for a cyclic spline

Create a design matrix for a cyclic spline

## Usage

``` r
cyclicSplineDesign(x, knots, ord = 3)
```

## Arguments

- x:

  Vector of coordinates of the points to be interpolated.

- knots:

  Location of the knots.

- ord:

  Order of the spline function. `ord = 3` implies quadratic.

## Details

This function is used by other functions in this package.
