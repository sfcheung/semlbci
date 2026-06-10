# Dataset (CFA, Two Factors, One Standardized Error Variance Close to Zero)

Generated from a two-factor model, with one standardized error variance
close to zero.

## Usage

``` r
cfa_evar_near_zero
```

## Format

A data frame with 120 rows and six variables, `x1` to `x6`

## Details

This model is used for examples like this one:

    # If fitted by the following model, the standardized
    # error variance of `x3` is close to zero.
    # Consequently, the R-square of `x3` is close to one:

    library(lavaan)
    mod <- "f1 =~ x1 + x2 + x3
            f2 =~ x4 + x5 + x6"
    fit <- cfa(mod, cfa_evar_near_zero)
    summary(fit, standardized = TRUE, rsquare = TRUE)

## Examples

``` r

print(head(cfa_evar_near_zero), digits = 3)
#>       x1      x2      x3     x4    x5     x6
#> 1  3.750  1.3970  6.3719 -0.602  2.04 -0.363
#> 2 -1.789 -1.3930 -1.2493 -2.550 -1.52 -3.492
#> 3  1.257 -3.3515 -2.1210 -3.071 -0.94 -1.437
#> 4 -0.403 -0.1972  0.0288  0.899  2.03 -2.030
#> 5 -0.643 -0.9646 -2.3602  0.377 -2.09 -0.186
#> 6 -1.771  0.0562 -0.5200  0.311  2.13 -3.931
nrow(cfa_evar_near_zero)
#> [1] 120


```
