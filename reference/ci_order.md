# Check The Order of Bounds in a List of `semlbci` Objects

Check whether the LBCIs in a list of `semlbci`-class of objects are
consistent with their levels of confidence.

## Usage

``` r
ci_order(semlbci_list)

# S3 method for class 'ci_order'
print(x, digits = 3, ...)
```

## Arguments

- semlbci_list:

  An object of class `semlbci_list`, such as the output of
  [`nearby_levels()`](https://sfcheung.github.io/semlbci/reference/nearby_levels.md).

- x:

  The output of `ci_order()`.

- digits:

  The number of decimal places in the printout.

- ...:

  Additional arguments. Not used.

## Value

A `ci_order`-class object with a `print` method `print.ci_order()`. The
number of rows is equal to the number of parameters in `semlbci_list`,
and the columns stores the confidence limits from the list, ordered
according to the level of confidence.

`x` is returned invisibly. Called for its side effect.

## Methods (by generic)

- `print(ci_order)`: The print method of the output of `ci_order()`.

## See also

[`nearby_levels()`](https://sfcheung.github.io/semlbci/reference/nearby_levels.md),
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)

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

# Check the order of the confidence bounds.
# A confidence interval with a higher level of confidence
# should enclose a confidence interval with
# a lower level of confidence.
ci_order(lbci_fit_nb)
#>     lb_0.99 lb_0.95  lb_0.9  ub_0.9 ub_0.95 ub_0.99 Order
#> m~x   0.557 < 0.828 ! 0.965 < 2.387 ! 2.525 < 2.795    OK
#> y~m   0.345 < 0.391 ! 0.414 < 0.656 ! 0.679 < 0.725    OK
```
