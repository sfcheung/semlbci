# A 'cibound' Output From a 'semlbci' Object

Get the `cibound` output of a bound from a `semlbci` object, the output
of
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md).

## Usage

``` r
get_cibound(x, row_id, which = c("lbound", "ubound"))

get_cibound_status_not_0(x)
```

## Arguments

- x:

  The output of
  [`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md).

- row_id:

  The row number in `x`. Should be the number on the left, not the
  actual row number, because some rows may be omitted in the printout of
  `x`.

- which:

  The bound for which the
  [`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
  is to be extracted. Either `"lbound"` or `"ubound"`.

## Value

`get_cibound()` returns a `cibound`-class object. See
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
for details. `get_cibound_status_not_0()` returns a list of
`cibound`-class objects with `status` not equal to zero. If all bounds
have `status` equal to zero, it returns an empty list.

## Details

The function `get_cibound()` returns the original output of
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md)
for a bound. Usually for diagnosis.

The function `get_cibound_status_not_0()` checks the status code of each
bound, and returns the `cibound` outputs of bounds with status code not
equal to zero (i.e., something wrong in the search). Printing it can
print the diagnostic information for all bounds that failed in the
search.

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

# Get the output of ci_bound_wn_i() of the lower
# bound of the LBCI for the indirect effect:
get_cibound(lbci_med, row_id = 6, which = "lbound")
#> Target Parameter:       ab := a*b (group = 0, block = 0)
#> Position:               6
#> Which Bound:            Lower Bound
#> Method:                 Wu-Neale-2012
#> Confidence Level:       0.95
#> Achieved Level:         0.95000000158433
#> Standardized:           No
#> Likelihood-Based Bound: 0.42653
#> Wald Bound:             0.38491
#> Point Estimate:         0.89687
#> Ratio to Wald Bound:    0.9187
#> 
#> -- Check --
#> Level achieved?         Yes (Difference: 1.5843e-09; Tolerance: 5e-04)
#> Solution admissible?    Yes
#> Direction valid?        Yes
#> 
#> -- Optimization Information --
#> Solver Status:          4
#> Convergence Message:    NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
#> Iterations:             11
#> Termination Conditions:
#>  xtol_rel: 1e-05
#>  ftol_rel: 1e-05
#>  maxeval:  500
#>  maxtime: 300
#> 
#> -- Parameter Estimates --
#>              a        b     m~~m     y~~y    x~~x
#> Start  0.77753  0.49504 35.46538 40.17887 0.93513
#> Final  0.86224  0.49467 35.32975 40.17929 0.93513
#> Change 0.08471 -0.00036 -0.13563  0.00042 0.00000
#> 
#> Bound before check:     0.42653
#> Status Code:            0
#> Call: ci_bound_wn_i(i = i, npar = 5L, sem_out = sem_out, f_constr = "<not printed>", 
#>     which = which, standardized = standardized, wald_ci_start = wald_ci_start, 
#>     ciperc = 0.95, sf = sf, sf2 = sf2, std_method = std_method_i, 
#>     debug = FALSE)
#> 

# Get the output of ci_bound_wn_i() of the upper
# bound of the LBCI for the indirect effect:
get_cibound(lbci_med, row_id = 6, which = "ubound")
#> Target Parameter:       ab := a*b (group = 0, block = 0)
#> Position:               6
#> Which Bound:            Upper Bound
#> Method:                 Wu-Neale-2012
#> Confidence Level:       0.95
#> Achieved Level:         0.950000073477175
#> Standardized:           No
#> Likelihood-Based Bound: 1.46404
#> Wald Bound:             1.40883
#> Point Estimate:         0.89687
#> Ratio to Wald Bound:    1.10784
#> 
#> -- Check --
#> Level achieved?         Yes (Difference: 7.3477e-08; Tolerance: 5e-04)
#> Solution admissible?    Yes
#> Direction valid?        Yes
#> 
#> -- Optimization Information --
#> Solver Status:          3
#> Convergence Message:    NLOPT_FTOL_REACHED: Optimization stopped because ftol_rel or ftol_abs (above) was reached.
#> Iterations:             6
#> Termination Conditions:
#>  xtol_rel: 1e-05
#>  ftol_rel: 1e-05
#>  maxeval:  500
#>  maxtime: 300
#> 
#> -- Parameter Estimates --
#>              a       b     m~~m     y~~y    x~~x
#> Start  2.32329 0.60639 35.10196 40.30883 0.93513
#> Final  2.38407 0.61409 35.10928 40.31234 0.93513
#> Change 0.06077 0.00770  0.00732  0.00352 0.00000
#> 
#> Bound before check:     1.46404
#> Status Code:            0
#> Call: ci_bound_wn_i(i = i, npar = 5L, sem_out = sem_out, f_constr = "<not printed>", 
#>     which = which, standardized = standardized, wald_ci_start = wald_ci_start, 
#>     ciperc = 0.95, sf = sf, sf2 = sf2, std_method = std_method_i, 
#>     debug = FALSE)
#> 
```
