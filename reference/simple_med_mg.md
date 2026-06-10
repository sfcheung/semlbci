# Dataset (Simple Mediation Model, Two Groups)

Generated from a simple mediation model, n = 200, two groups, n = 100
each.

## Usage

``` r
simple_med_mg
```

## Format

A data frame with 500 rows and four variables:

- gp:

  gp, the grouping variable

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
    fit <- sem(mod, simple_med_mg, gp = "group")
    summary(fit)

## Examples

``` r

print(head(simple_med_mg), digits = 3)
#>    gp      x      m     y
#> 1 gp1 -0.345   7.28 -5.64
#> 2 gp1 -0.366  -5.45 -4.53
#> 3 gp1 -0.829  -7.02 -7.82
#> 4 gp1 -0.339   4.37  1.56
#> 5 gp1 -0.963  -4.02 -7.29
#> 6 gp1 -1.075 -11.54 -4.15
nrow(simple_med_mg)
#> [1] 200
table(simple_med_mg$gp)
#> 
#> gp1 gp2 
#> 100 100 
```
