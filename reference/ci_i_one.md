# Likelihood-Based Confidence Bound for One Parameter

Find the likelihood-based confidence bound for one parameter.

## Usage

``` r
ci_i_one(
  i,
  which = NULL,
  sem_out,
  method = c("wn", "ur"),
  standardized = FALSE,
  robust = "none",
  sf_full = NA,
  sf_args = list(),
  sem_out_name = NULL,
  try_k_more_times = 0,
  ...
)
```

## Arguments

- i:

  The position (row number) of the target parameters as appeared in the
  parameter table of the
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object.

- which:

  Whether the lower bound or the upper bound is to be found. Must be
  `"lbound"` or `"ubound"`.

- sem_out:

  The SEM output. Currently supports
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  outputs only.

- method:

  The approach to be used. Default is `"wn"` (Wu-Neale-2012 Method).
  Another method is "ur", root finding by
  [`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html).

- standardized:

  Logical. Whether the bound of the LBCI of the standardized solution is
  to be searched. Default is `FALSE`.

- robust:

  Whether the LBCI based on robust likelihood ratio test is to be found.
  Only `"satorra.2000"` in
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html)
  is supported for now. If `"none"`, the default, then likelihood ratio
  test based on maximum likelihood estimation will be used. For "ur",
  `"satorra.2000"` is automatically used if a scaled test statistic is
  requested in `sem_out`.

- sf_full:

  A list with the scaling and shift factors. Ignored if `robust` is
  `"none"`. If `robust` is `"satorra.2000"` and `sf_full` is supplied,
  then its value will be used. If `robust` is `"satorra.2000"` but
  `sf_full` is `NA`, then scaling factors will be computed internally.

- sf_args:

  The list of arguments to be used for computing scaling factors if
  `robust` is `"satorra.2000"`. Used only by
  [`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md).
  Ignored if `robust` is not `"satorra.2000"`.

- sem_out_name:

  The name of the object supplied to `sem_out`. `NULL` by default.
  Originally used by some internal functions. No longer used in the
  current version but kept for backward compatibility.

- try_k_more_times:

  How many more times to try if the status code is not zero. Default is
  0.

- ...:

  Arguments to be passed to the function corresponds to the requested
  method
  ([`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
  for "wn").

## Value

A list of the following elements.

- `bound`: The bound located. `NA` if the search failed.

- `diags`: Diagnostic information.

- `method`: Method used. Currently only `"wn"` is the only possible
  value.

- `times`: Total time used in the search.

- `sf_full`: The scaling and shift factors used.

- `ci_bound_i_out`: The original output from
  [`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md).

- `attempt_lb_var`: How many attempts used to reduce the lower bounds of
  free variances.

- `attempt_more_times`: How many additional attempts used to search for
  the bounds. Controlled by `try_k_more_times`.

## Details

### Important Notice

This function is not supposed to be used directly by users in typical
scenarios. Its interface is user-*unfriendly* because it should be used
through
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md).
It is exported such that interested users can examine how a confidence
bound is found, or use it for experiments or simulations.

### Usage

`ci_i_one()` is the link between
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
and the lowest level function (currently
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)).
When called by
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md)
to find the bound of a parameter, `ci_i_one()` calls a function
([`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
by default) one or more times to find the bound (limit) for a
likelihood-based confidence interval.

## See also

[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md),
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)

## Examples

``` r

data(simple_med)

library(lavaan)
mod <-
"
m ~ x
y ~ m
"
fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)

parameterTable(fit_med)
#>   id lhs op rhs user block group free ustart exo label plabel  start    est
#> 1  1   m  ~   x    1     1     1    1     NA   0         .p1.  1.676  1.676
#> 2  2   y  ~   m    1     1     1    2     NA   0         .p2.  0.535  0.535
#> 3  3   m ~~   m    0     1     1    3     NA   0         .p3. 34.710 34.710
#> 4  4   y ~~   y    0     1     1    4     NA   0         .p4. 40.119 40.119
#> 5  5   x ~~   x    0     1     1    5     NA   0         .p5.  0.935  0.935
#>      se
#> 1 0.431
#> 2 0.073
#> 3 3.471
#> 4 4.012
#> 5 0.094

# Find the LBCI for the first parameter
# The method "wn" needs the constraint function.
# Use set_constraint() to generate this function:
fn_constr0 <- set_constraint(fit_med)

# Call ci_i to find the bound, the lower bound in this example.
# The constraint function, assigned to f_constr, is passed
# to ci_bound_wn_i().
# npar is an argument for ci_bound_wn_i().
out <- ci_i_one(i = 1,
                which = "lbound",
                sem_out = fit_med,
                npar = 5,
                f_constr = fn_constr0)
out$bounds
#>   lbound 
#> 0.827702 
```
