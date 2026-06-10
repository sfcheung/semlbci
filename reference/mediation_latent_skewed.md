# Dataset (SEM, Three Factors, Nine Variables, Mediation, Skewed)

Generated from a three-factor model with nine variables, n = 150, with
some observed variables positively skewed.

## Usage

``` r
mediation_latent_skewed
```

## Format

A data frame with 150 rows and nine variables:

- x1:

  x1

- x2:

  x2

- x3:

  x3

- x4:

  x4

- x5:

  x5

- x6:

  x6

- x7:

  x7

- x8:

  x8

- x9:

  x9

## Details

This model is used for examples like this one:

    mod <-
    "
    fx =~ x1 + x2 + x3
    fm =~ x4 + x5 + x6
    fy =~ x7 + x8 + x9
    fm ~ a*fx
    fy ~ b*fm + cp*fx
    ab := a*b
    "
    fit <- lavaan::sem(mod, mediation_latent)

## Examples

``` r

print(head(mediation_latent_skewed), digits = 3)
#>       x1      x2     x3     x4     x5      x6      x7     x8      x9
#> 1 -0.531 -0.8247 -0.135  2.153  1.682  2.6862  0.0378 -0.575 -0.3487
#> 2 -0.366 -0.4707 -0.378  0.502  0.839  0.5743  2.1701  1.427  0.5184
#> 3 -0.587 -0.9881  2.105 -1.242 -0.993 -0.0189 -0.2263 -1.436 -1.0298
#> 4 -0.204 -0.9530 -0.899  0.137 -0.400 -0.0742 -0.6008 -0.107 -0.8484
#> 5  0.552 -0.0556 -0.367  2.555 -0.354  0.5379 -0.4397 -0.678 -0.7983
#> 6 -0.300 -0.4719 -0.501  0.803  1.904 -0.1835  1.2731 -0.317 -0.0114
nrow(mediation_latent_skewed)
#> [1] 150


```
