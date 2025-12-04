# Print Method of a 'semlbci' Object

Prints the results of a `semlbci` object, the output of
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md).

## Usage

``` r
# S3 method for class 'semlbci'
print(
  x,
  digits = 3,
  annotation = TRUE,
  time = FALSE,
  verbose = FALSE,
  verbose_if_needed = TRUE,
  drop_no_lbci = TRUE,
  output = c("table", "text", "lavaan"),
  sem_out = NULL,
  lbci_only = drop_no_lbci,
  ratio_digits = 1,
  se = TRUE,
  zstat = TRUE,
  pvalue = TRUE,
  boot.ci.type = "perc",
  ...
)
```

## Arguments

- x:

  The output of
  [`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md).

- digits:

  The number of digits after the decimal point. To be passed to
  [`formatC()`](https://rdrr.io/r/base/formatc.html). Default is 3.

- annotation:

  If `TRUE`, print table notes. Default is `TRUE`.

- time:

  If `TRUE`, print the time spent on each bound. Default is `FALSE`.

- verbose:

  If `TRUE`, additional diagnostic information will always be printed.
  This argument overrides `verbose_if_needed`. Default is `FALSE`.

- verbose_if_needed:

  If `TRUE`, additional diagnostic information will be printed only if
  necessary. If `FALSE`, additional diagnostic information will always
  be printed. Default is `TRUE`.

- drop_no_lbci:

  If `TRUE`, parameters without LBCIs will be removed. Default is
  `TRUE`.

- output:

  The type of printout. If `"table"`, the default, the results will be
  printed in a table. If `"text"` or `"lavaan"`, then the results will
  be printed in the `lavaan` style, as in the
  [`summary()`](https://rdrr.io/r/base/summary.html) method for the
  output of `lavaan`.

- sem_out:

  If `output` is `"text"` or `"lavaan"`, the original output of `lavaan`
  used in calling
  [`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
  needs to be supplied to this argument.

- lbci_only:

  Used only if `output` is `"text"` or `"lavaan"`. If `TRUE`, only the
  likelihood-based confidence intervals (LBCIs) will be printed. If
  `FALSE`, and LBCIs will be printed alongside the confidence intervals
  by `lavaan`. Its default value depend on the argument `drop_no_lbci`.
  If `drop_no_lbci` is `TRUE`, then `lbci_only` is `TRUE` by default. If
  `drop_no_lbci` is `FALSE`, then `lbci_only` is `FALSE` by default.

- ratio_digits:

  The number of digits after the decimal points for the ratios of
  distance from the confidence limits to the point estimates. Default is
  1.

- se:

  Logical. To be passed to
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html).
  Whether standard error (S.E.) will be printed. Only applicable if
  `output` is `"text"` or `"lavaan"`.

- zstat:

  Logical. To be passed to
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html).
  Whether z-values will be printed. Only applicable if `output` is
  `"text"` or `"lavaan"`.

- pvalue:

  Logical. To be passed to
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html).
  Whether p-values will be printed. Only applicable if `output` is
  `"text"` or `"lavaan"`.

- boot.ci.type:

  Logical. To be passed to
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html).
  The type of bootstrap confidence intervals to be printed if
  bootstrapping confidence intervals available. Possible values are
  `"norm"`, `"basic"`, `"perc"`, or `"bca.simple"`. The default value is
  `"perc"`. Refer to the help of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  for further information. Only applicable if `output` is `"text"` or
  `"lavaan"`.

- ...:

  Other arguments. They will be ignored.

## Value

`x` is returned invisibly. Called for its side effect.

## Details

Prints the results of
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
as a table.

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
                    pars = c("ab :="))
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
#> semlbci(sem_out = fit_med, pars = c("ab :="))

print(lbci_med, verbose_if_needed = FALSE)
#> 
#> Results:
#>   id lhs op rhs label   est lbci_lb lbci_ub ok_l ok_u    lb    ub ratio_l
#> 6  6  ab := a*b    ab 0.897   0.427   1.464    0    0 0.385 1.409   0.919
#>   ratio_u check_l check_u cl_lb cl_ub
#> 6   1.108    TRUE    TRUE 0.950 0.950
#> 
#> Annotation:
#> * lbci_lb, lbci_ub: The lower and upper likelihood-based bounds.
#> * est: The point estimates from the original lavaan output.
#> * ok_l, ok_u: Whether the search encountered any problem. If no problem
#>     encountered, it is equal to 0. Any value other than 0 indicates
#>     something was wrong in the search. Try running 'semlbci()' again,
#>     setting 'semlbci_out' to this output, and set 'try_k_more_times' to
#>     a positive number greater than 2 (the default), e.g., 3 to 5.
#> * lb, ub: The original lower and upper bounds, extracted from the
#>     original lavaan output. Usually Wald CIs for free parameters and
#>     delta method CIs for user-defined parameters
#> * cl_lb, cl_ub: One minus the p-values of chi-square difference tests
#>     at the bounds. Should be close to the requested level of
#>     confidence, e.g., .95 for 95% confidence intervals.
#> * ratio_l, ratio_u: Ratio of a to b, a = Distance from the point
#>     estimate to the likelihood-based bound, b = Distance from the point
#>     estimate to the original bound. A bound should be interpreted with
#>     caution if the ratio is too large or too small, indicating a large
#>     difference between the original interval and the likelihood-based
#>     interval.
#> * check_l, check_u: Whether the final solution of a bound passed the
#>     post optimization check of lavaan by lavaan::lavInspect(fit,
#>     'post.check'), where fit is the final solution.
#> 
#> Call:
#> semlbci(sem_out = fit_med, pars = c("ab :="))

print(lbci_med, verbose = TRUE)
#> 
#> Results:
#>   id lhs op rhs label   est lbci_lb lbci_ub ok_l ok_u    lb    ub ratio_l
#> 6  6  ab := a*b    ab 0.897   0.427   1.464    0    0 0.385 1.409   0.919
#>   ratio_u check_l check_u cl_lb cl_ub
#> 6   1.108    TRUE    TRUE 0.950 0.950
#> 
#> Annotation:
#> * lbci_lb, lbci_ub: The lower and upper likelihood-based bounds.
#> * est: The point estimates from the original lavaan output.
#> * ok_l, ok_u: Whether the search encountered any problem. If no problem
#>     encountered, it is equal to 0. Any value other than 0 indicates
#>     something was wrong in the search. Try running 'semlbci()' again,
#>     setting 'semlbci_out' to this output, and set 'try_k_more_times' to
#>     a positive number greater than 2 (the default), e.g., 3 to 5.
#> * lb, ub: The original lower and upper bounds, extracted from the
#>     original lavaan output. Usually Wald CIs for free parameters and
#>     delta method CIs for user-defined parameters
#> * cl_lb, cl_ub: One minus the p-values of chi-square difference tests
#>     at the bounds. Should be close to the requested level of
#>     confidence, e.g., .95 for 95% confidence intervals.
#> * ratio_l, ratio_u: Ratio of a to b, a = Distance from the point
#>     estimate to the likelihood-based bound, b = Distance from the point
#>     estimate to the original bound. A bound should be interpreted with
#>     caution if the ratio is too large or too small, indicating a large
#>     difference between the original interval and the likelihood-based
#>     interval.
#> * check_l, check_u: Whether the final solution of a bound passed the
#>     post optimization check of lavaan by lavaan::lavInspect(fit,
#>     'post.check'), where fit is the final solution.
#> 
#> Call:
#> semlbci(sem_out = fit_med, pars = c("ab :="))

print(lbci_med, time = TRUE)
#> 
#> Results:
#>   id lhs op rhs label   est lbci_lb lbci_ub    lb    ub sec_l sec_u cl_lb cl_ub
#> 6  6  ab := a*b    ab 0.897   0.427   1.464 0.385 1.409 0.186 0.163 0.950 0.950
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
#> * sec_l, sec_u: The time (in seconds) used to search a bound.
#> 
#> Call:
#> semlbci(sem_out = fit_med, pars = c("ab :="))

print(lbci_med, annotation = FALSE)
#> 
#> Results:
#>   id lhs op rhs label   est lbci_lb lbci_ub    lb    ub cl_lb cl_ub
#> 6  6  ab := a*b    ab 0.897   0.427   1.464 0.385 1.409 0.950 0.950

print(lbci_med, digits = 4)
#> 
#> Results:
#>   id lhs op rhs label    est lbci_lb lbci_ub     lb     ub  cl_lb  cl_ub
#> 6  6  ab := a*b    ab 0.8969  0.4265  1.4640 0.3849 1.4088 0.9500 0.9500
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
#> semlbci(sem_out = fit_med, pars = c("ab :="))

# Text output

print(lbci_med, output = "lavaan", sem_out = fit_med)
#> Likelihood-Based CI Notes:
#> 
#> - lb.lower, lb.upper: The lower and upper likelihood-based confidence
#>                       bounds.
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Defined Parameters:
#>                    Estimate  Std.Err  z-value  P(>|z|) lb.lower lb.upper
#>     ab                0.897    0.261    3.434    0.001    0.427    1.464
#> 

print(lbci_med, output = "lavaan", sem_out = fit_med, lbci_only = FALSE)
#> Likelihood-Based CI Notes:
#> 
#> - lb.lower, lb.upper: The lower and upper likelihood-based confidence
#>                       bounds.
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Defined Parameters:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>     ab                0.897    0.261    3.434    0.001    0.385    1.409
#>  lb.lower lb.upper
#>     0.427    1.464
#> 

print(lbci_med, output = "lavaan", sem_out = fit_med, lbci_only = FALSE,
      se = FALSE, zstat = FALSE, pvalue = FALSE)
#> Likelihood-Based CI Notes:
#> 
#> - lb.lower, lb.upper: The lower and upper likelihood-based confidence
#>                       bounds.
#> 
#> Parameter Estimates:
#> 
#> 
#> Defined Parameters:
#>                    Estimate lb.lower lb.upper
#>     ab                0.897    0.427    1.464
#> 
```
