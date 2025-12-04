# Equality Constraint for Finding the LBCI by Wu-Neale-2012

Create the equality constraint for finding the likelihood-based
confidence interval (LBCI) by the Wu-Neale-2012 method.

## Usage

``` r
set_constraint(sem_out, ciperc = 0.95)
```

## Arguments

- sem_out:

  The SEM output. Currently supports
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  outputs only.

- ciperc:

  The intendeted coverage probability of the confidence interval.
  Default is .95.

## Value

An equality constraint function to be used by
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md).

## Details

### Important Notice

This function is not supposed to be used directly by users in typical
scenarios. Its interface is user-*unfriendly* because it should be used
through
[`semlbci()`](https://sfcheung.github.io/semlbci/reference/semlbci.md).
It is exported such that interested users can examine how a confidence
bound is found, or use it for experiments or simulations.

### Usage

The Wu-Neale-2012 method uses a simple objective function that is
optimized with an equality constraint. `set_constraint()` generates the
equality constraint function to be used by
[`ci_bound_wn_i()`](https://sfcheung.github.io/semlbci/reference/ci_bound_wn_i.md).

It currently supports
[lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
outputs only.

## Examples

``` r
library(lavaan)
data(simple_med)
dat <- simple_med
mod <-
"
m ~ x
y ~ m
"
fit_med <- sem(mod, simple_med, fixed.x = FALSE)

fn_constr0 <- set_constraint(fit_med)
out <- fn_constr0(coef(fit_med), sem_out = fit_med)
out
#> $objective
#> [1] 0.02637152
#> 
#> $gradient
#>              [,1]        [,2]          [,3]          [,4]          [,5]
#> [1,] 6.351961e-18 6.56375e-18 -5.605735e-20 -2.245085e-18 -1.144598e-18
#> 
#> $constraints
#> [1] -0.009603647
#> 
#> $jacobian
#>              [,1]        [,2]          [,3]          [,4]          [,5]
#> [1,] 6.351961e-18 6.56375e-18 -5.605735e-20 -2.245085e-18 -1.144598e-18
#> 
#> $parameterTable
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
#> 
lavTech(fit_med, "optim")$fx
#> [1] 0.02637152
```
