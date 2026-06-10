# Confidence Intervals for a 'smelbci' Object

Return the confidence intervals of the parameters in the output of
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md).

## Usage

``` r
# S3 method for class 'semlbci'
confint(object, parm, level = 0.95, ...)
```

## Arguments

- object:

  The output of
  [`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md).

- parm:

  The parameters for which the confidence intervals are returned. Not
  used because parameters are defined by three or more columns (`lhs`,
  `op`, `rhs`, and `group` for multisample models).

- level:

  Ignored. The level of confidence is determined when calling
  [`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
  and cannot be changed.

- ...:

  Optional arguments. Ignored.

## Value

A two-column matrix of the confidence intervals.

## Details

It returns the likelihood-based confidence intervals in the output of
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md).

## See also

[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r


library(lavaan)
mod <-
"
m ~ a*x
y ~ b*m
ab := a * b
"
fit_med <- sem(mod, simple_med, fixed.x = FALSE)
p_table <- parameterTable(fit_med)
p_table
#>   id lhs op rhs user block group free ustart exo label plabel  start    est
#> 1  1   m  ~   x    1     1     1    1     NA   0     a   .p1.  1.676  1.676
#> 2  2   y  ~   m    1     1     1    2     NA   0     b   .p2.  0.535  0.535
#> 3  3   m ~~   m    0     1     1    3     NA   0         .p3. 34.710 34.710
#> 4  4   y ~~   y    0     1     1    4     NA   0         .p4. 40.119 40.119
#> 5  5   x ~~   x    0     1     1    5     NA   0         .p5.  0.935  0.935
#> 6  6  ab := a*b    1     0     0    0     NA   0    ab         0.000  0.897
#>      se
#> 1 0.431
#> 2 0.073
#> 3 3.471
#> 4 4.012
#> 5 0.094
#> 6 0.261
lbci_med <- semlbci(fit_med,
                    pars = "ab :=")
lbci_med
#> 
#> Results:
#>   id lhs op rhs label   est lbci_lb lbci_ub    lb    ub cl_lb cl_ub
#> 6  6  ab := a*b    ab 0.897   0.427   1.464 0.385 1.409 0.950 0.950
#> 
#> Annotation:
#> * lbci_lb, lbci_ub: The lower and upper likelihood-based bounds.
#> * est: The point estimates from the original lavaan output.
#> * lb, ub: The original lower and upper bounds, extracted from the
#>     original lavaan output. Usually Wald CIs for free parameters and
#>     delta method CIs for user-defined parameters
#> * cl_lb, cl_ub: One minus the p-values of chi-square difference tests
#>     at the bounds. Should be close to the requested level of
#>     confidence, e.g., .95 for 95% confidence intervals.
#> 
#> Call:
#> semlbci(sem_out = fit_med, pars = "ab :=")

confint(lbci_med)
#>        2.5 %   97.5 %
#> ab 0.4265275 1.464037
```
