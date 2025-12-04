# Pre-analysis Check For 'semlbci'

Check the output passed to
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)

## Usage

``` r
check_sem_out(
  sem_out,
  robust = c("none", "satorra.2000"),
  multigroup_ok = TRUE
)
```

## Arguments

- sem_out:

  The output from an SEM analysis. Currently only supports a
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object.

- robust:

  Whether the LBCI based on robust likelihood ratio test is to be found.
  Only "satorra.2000" in
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html)
  is supported for now. If `"none"`, the default, then likelihood ratio
  test based on maximum likelihood estimation will be used.

- multigroup_ok:

  If `TRUE`, will not check whether the model is a multiple-group model.
  Default is `TRUE`.

## Value

A numeric vector of one element. If 0, the model and estimation method
are officially supported. If larger than zero, then the model and method
are not officially supported but users can still try to use
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
on it at their own risks. If less than zero, then the model and/or the
method are officially not supported.

The attributes `info` contains the reason for a value other than zero.

## Details

It checks whether the model and the estimation method in the `sem_out`
object passed to
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
are supported by the current version of
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md).
This function is to be used by
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
but is exported such that the compatibility of an SEM output can be
checked directly.

Estimation methods (`estimator` in
[`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html))
currently supported:

- Maximum likelihood (`ML`) and its variants (e.g., `MLM`, `MLR`). For
  methods with robust test statistics (e.g., `MLR`), only robust LBCIs
  (`robust = "satorra.2000"` in calling
  [`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md))
  can be requested.

Estimation methods not yet supported:

- Generalized least squares (`GLS`).

- Weighted least squares (a.k.a. asymptotically distribution free)
  (`WLS`) and its variants (e.g., `WLSMV`).

- Unweighted least squares (`ULS`).

- Diagonally weighted least squares (`DWLS`).

- Other methods not listed.

Models supported:

- Single-group models with continuous variables.

- Multiple-group models with continuous variables.

Models not tested:

- Models with categorical variables.

Models not yet supported:

- Models with formative factors.

- Multilevel models.

## See also

[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md),
[`ci_i_one()`](https://sfcheung.github.io/semlbci/reference/ci_i_one.md)

## Examples

``` r
library(lavaan)
#> This is lavaan 0.6-20
#> lavaan is FREE software! Please report any bugs.
data(cfa_two_factors)
mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
"

fit <- sem(mod, cfa_two_factors)

# Should be 0
check_sem_out(fit)
#> [1] 0

fit2 <- sem(mod, cfa_two_factors, estimator = "DWLS")
#> Warning: lavaan->lav_options_est_dwls():  
#>    estimator “DWLS” is not recommended for continuous data. Did you forget to 
#>    set the ordered= argument?

# Should be negative because DWLS is officially not supported
check_sem_out(fit2)
#> [1] -2
#> attr(,"info")
#> [1] "Estimator DWLS is not yet supported."                       
#> [2] "Only support models fitted with likelihood set to 'normal'."

fit3 <- sem(mod, cfa_two_factors, estimator = "MLR")

# Should be negative because MLR is supported only if
# robust is set to "satorra.2000"
check_sem_out(fit3)
#> [1] -1
#> attr(,"info")
#> [1] "Test method(s) yuan.bentler.mplus is/are not yet supported when 'robust' is 'none'."

# Should be zero because robust is set to "satorra.2000"
check_sem_out(fit3, robust = "satorra.2000")
#> [1] 0
```
