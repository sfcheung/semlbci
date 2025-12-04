# Print Method of a 'cibound'-class Object

Print the diagnostic information of a `cibound`-class object.

## Usage

``` r
# S3 method for class 'cibound'
print(x, digits = 5, ...)
```

## Arguments

- x:

  The output of a `ci_bound_xx_i` function. Currently the only such
  function is
  [`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md).

- digits:

  The number of digits after the decimal point. To be passed to
  [`round()`](https://rdrr.io/r/base/Round.html). Default is 5.

- ...:

  Other arguments. They will be ignored.

## Value

`x` is returned invisibly. Called for its side effect.

## Details

This is the print method for the output of
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md),
a `cibound`-class object. It prints the diagnostic information on the
bound being found and the search process.

## Examples

``` r
data(simple_med)
dat <- simple_med

mod <-
"
m ~ x
y ~ m
"

fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)

fn_constr0 <- set_constraint(fit_med)

out1l <- ci_bound_wn_i(i = 1,
                       npar = 5,
                       sem_out = fit_med,
                       f_constr = fn_constr0,
                       which = "lbound")

# Print the output
out1l
#> Target Parameter:       m ~ x (group = 1, block = 1)
#> Position:               1
#> Which Bound:            Lower Bound
#> Method:                 Wu-Neale-2012
#> Confidence Level:       0.95
#> Achieved Level:         0.950000000054991
#> Standardized:           No
#> Likelihood-Based Bound: 0.8277
#> Wald Bound:             0.83177
#> Point Estimate:         1.67613
#> Ratio to Wald Bound:    1.00482
#> 
#> -- Check --
#> Level achieved?         Yes (Difference: 5.4991e-11; Tolerance: 5e-04)
#> Solution admissible?    Yes
#> Direction valid?        Yes
#> 
#> -- Optimization Information --
#> Solver Status:          4
#> Convergence Message:    NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
#> Iterations:             3
#> Termination Conditions:
#>  xtol_rel: 1e-05
#>  ftol_rel: 1e-05
#>  maxeval:  500
#>  maxtime: 300
#> 
#> -- Parameter Estimates --
#>             m~x     y~m     m~~m     y~~y    x~~x
#> Start   0.83177 0.53508 35.37699 40.11896 0.93513
#> Final   0.82770 0.53508 35.37711 40.11896 0.93513
#> Change -0.00407 0.00000  0.00011  0.00000 0.00000
#> 
#> Bound before check:     0.8277
#> Status Code:            0
#> Call: ci_bound_wn_i(i = 1, npar = 5, sem_out = fit_med, f_constr = fn_constr0, 
#>     which = "lbound")
#> 
```
