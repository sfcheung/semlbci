# Dataset (CFA, Two Factors, Six Variables, Two Groups)

Generated from a two-factor model with six variables, n = 500, two
groups, n = 250 each.

## Usage

``` r
cfa_two_factors_mg
```

## Format

A data frame with 500 rows, one grouping variable, `gp`, six variables,
`x1` to `x6`.

## Details

This model is used for examples like this one:

    library(lavaan)
    mod <- "f1 =~ x1 + x2 + x3
            f2 =~ x4 + x5 + x6"
    fit <- cfa(mod, cfa_two_factors_mg, group = "gp")
    summary(fit)

## Examples

``` r
print(head(cfa_two_factors_mg), digits = 3)
#>    gp     x1     x2     x3    x4     x5     x6
#> 1 gp1 -0.445  1.606  4.199 -0.61  2.437  0.393
#> 2 gp1 -2.342 -3.404 -3.066 -0.81  0.245 -2.139
#> 3 gp1 -2.033 -0.355  1.142  1.71 -0.722  1.491
#> 4 gp1  1.204 -0.234  0.858 -2.05 -2.322 -1.038
#> 5 gp1  2.161  0.890  3.437  4.74  0.765  4.522
#> 6 gp1  3.482  2.014  0.870 -2.51  0.487 -0.553
nrow(cfa_two_factors_mg)
#> [1] 500
table(cfa_two_factors_mg$gp)
#> 
#> gp1 gp2 
#> 250 250 
```
