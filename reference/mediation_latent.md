# Dataset (SEM, Three Factors, Nine Variables, Mediation)

Generated from a three-factor model with nine variables, n = 150

## Usage

``` r
mediation_latent
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
print(head(mediation_latent), digits = 3)
#>       x1     x2      x3     x4     x5     x6      x7     x8     x9
#> 1 -1.393  0.141 -0.8749  0.166 -0.652 -0.469 -0.0309 -1.111 -1.378
#> 2 -0.565  0.729 -0.1052  0.744  1.815  1.371 -0.2996 -0.626  0.147
#> 3 -2.057 -2.128 -0.9271 -0.258 -1.160 -2.156 -1.8819 -1.273 -1.148
#> 4  0.698 -0.455  0.3913  0.671  0.695 -1.938 -0.0666  0.306  0.181
#> 5 -0.922 -1.848 -1.2790  1.215 -0.445 -0.173 -2.7843 -2.077 -1.086
#> 6  1.151 -0.056  0.0593  0.112  0.717 -0.744 -0.4511 -0.848 -0.494
nrow(mediation_latent)
#> [1] 150

```
