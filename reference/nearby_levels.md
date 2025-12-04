# LBCI Bounds of Nearby Levels of Confidence

Find LBCIs with levels of confidence different from those stored in a
`semlbci`- class object.

## Usage

``` r
nearby_levels(x, ciperc_levels = c(-0.025, 0.025), ciperc_range = c(0.6, 0.99))
```

## Arguments

- x:

  The output of
  [`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md).

- ciperc_levels:

  A numeric vector of deviations from the original level of confidence.
  The default is `c(-.025, .025)`. Therefore, if the original level is
  .95, the levels to be used is `c(-.025, .025) + .95` or
  `c(.925, .975)`.

- ciperc_range:

  A numeric vector of two numbers, which are the minimum and maximum
  levels of confidence to be used, respectively. Default is
  `c(.60, .99)`.

## Value

A `semlbci_list`-class object, which is simply a named list of
`semlbci`-class object, names being the levels of confidence.

## Details

It receives a `semlbci`-class object, gets the original level of
confidence, generates one or more levels of confidence different from
this level by certain amounts, and repeats the original call to
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
with these levels of confidence. The results are returned as a list of
class `semlbci_list`, with the original`semlbci`-class included.

## See also

[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md),
[`ci_order()`](https://sfcheung.github.io/semlbci/reference/ci_order.md)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
library(lavaan)
mod <-
"
m ~ x
y ~ m
"
fit_med <- sem(mod, simple_med, fixed.x = FALSE)
lbci_fit <- semlbci(fit_med)
lbci_fit_nb <- nearby_levels(lbci_fit,
                             ciperc_levels = c(-.050, .050))
names(lbci_fit_nb)
#> [1] "0.9"  "0.95" "0.99"
# Check the order of the confidence bounds.
# A confidence interval with a higher level of confidence
# should enclose a confidence interval with
# a lower level of confidence.
ci_order(lbci_fit_nb)
#>     lb_0.99 lb_0.95  lb_0.9  ub_0.9 ub_0.95 ub_0.99 Order
#> m~x   0.557 < 0.828 ! 0.965 < 2.387 ! 2.525 < 2.795    OK
#> y~m   0.345 < 0.391 ! 0.414 < 0.656 ! 0.679 < 0.725    OK
```
