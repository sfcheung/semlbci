# Dataset (Simple Mediation Model)

Generated from a simple mediation model, n = 200

## Usage

``` r
simple_med
```

## Format

A data frame with 200 rows and three variables:

- x:

  x, the independent variable

- m:

  m, the mediator

- y:

  y, the dependent variable

## Details

This model is used for examples like this one:

    library(lavaan)
    mod <- "m ~ x
            y ~ m"
    fit <- cfa(mod, simple_med)
    summary(fit)

## Examples

``` r

print(head(simple_med), digits = 3)
#>        x      m     y
#> 1 -0.345   7.28 -5.64
#> 2 -0.366  -5.45 -4.53
#> 3 -0.829  -7.02 -7.82
#> 4 -0.339   4.37  1.56
#> 5 -0.963  -4.02 -7.29
#> 6 -1.075 -11.54 -4.15
nrow(simple_med)
#> [1] 200

```
