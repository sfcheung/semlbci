# Dataset (CFA, Two Factors, Six Variables)

Generated from a two-factor model with six variables, n = 500

## Usage

``` r
cfa_two_factors
```

## Format

A data frame with 500 rows and six variables, `x1` to `x6`.

## Details

This model is used for examples like this one:

    library(lavaan)
    mod <- "f1 =~ x1 + x2 + x3
            f2 =~ x4 + x5 + x6"
    fit <- cfa(mod, cfa_two_factors)
    summary(fit)

## Examples

``` r

print(head(cfa_two_factors), digits = 3)
#>       x1     x2     x3    x4     x5     x6
#> 1 -0.445  1.606  4.199 -0.61  2.437  0.393
#> 2 -2.342 -3.404 -3.066 -0.81  0.245 -2.139
#> 3 -2.033 -0.355  1.142  1.71 -0.722  1.491
#> 4  1.204 -0.234  0.858 -2.05 -2.322 -1.038
#> 5  2.161  0.890  3.437  4.74  0.765  4.522
#> 6  3.482  2.014  0.870 -2.51  0.487 -0.553
nrow(cfa_two_factors)
#> [1] 500
```
