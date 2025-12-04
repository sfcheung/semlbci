# Dataset (Six Variables, One Correlation Close to One)

Generated from a regression model six variables, x4\~~x5 correlation
close to one.

## Usage

``` r
reg_cor_near_one
```

## Format

A data frame with 100 rows and six variables:

- x1:

  x1

- x2:

  x2

- x3:

  x3

- x4:

  x4, with correlation with x5 nearly equal to 1

- x5:

  x5, with correlation with x4 nearly equal to 1

- y:

  y, the dependent variable

## Details

This model is used for examples like this one:

    out <- lm(y ~ x1 + x2 + x3 + x4 + x5, reg_cor_near_one)
    summary(out)
    cor(reg_cor_near_one[, c("x4", "x5")])

## Examples

``` r
print(head(reg_cor_near_one), digits = 3)
#>       x1     x2      x3      x4      x5      y
#> 1 -0.687 -0.266  1.1400  0.2151  0.2368  0.305
#> 2  1.651 -0.447  0.0955 -0.8698 -0.9360 -0.481
#> 3  0.909  1.106 -1.1913  0.0804  0.0891  0.799
#> 4  0.598  1.710 -0.6715 -1.4201 -1.4841 -0.236
#> 5  0.122 -0.689 -0.2420 -0.7906 -0.8070 -0.390
#> 6  0.923  1.041 -0.4671 -2.1551 -2.1470 -0.788
nrow(reg_cor_near_one)
#> [1] 100

```
