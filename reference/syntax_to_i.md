# Parameter Positions From lavaan Syntax

Converts lavaan syntax to positions in the model parameter table.

## Usage

``` r
syntax_to_i(syntax, sem_out)
```

## Arguments

- syntax:

  A vector of parameters, defined as in lavaan.

- sem_out:

  The SEM output. Currently `lavaan` output only.

## Value

A numeric vector of positions (row numbers) in the parameter table.

## Details

`syntax_to_i()` converts a vector of strings, in lavaan syntax, to the
positions in the parameter table of a
[lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html) fit
object.

Each element in the vector should have left hand side (`lhs`), operator
(`op`), and/or right hand side (`rhs`). For example:all.x

- `"m ~ x"` denotes the coefficient of the path from `x` to `m`.

- `"y ~~ x"` denotes the covariance between `y` and `x`.

For user-defined parameters, only `lhs` and `op` will be interpreted.
For example:

- To specify the user parameter `ab`, both `"ab := ..."` and `"ab :="`
  will do, `...` the definition of `ab` in the model. The right-hand
  side will be ignored.

To denote a labelled parameters, such as `"y ~ a*x"`, treat it as a
user-defined parameters and use `:=`, e.g., `"a :="` in this example.

For multiple-group models, if a parameter is specified as in a
single-group models, then this parameter in all groups will be selected.
For example:all.x

- If a model has three groups, `"y ~ x"` denotes this path parameter in
  all three groups, and it will be converted to three row numbers.

To select the parameter in a specific group, "multiply" the
right-hand-side variable by the group number. For example:

- `"y ~ 2*x"` denotes the path coefficient from `x` to `y` in Group 2.

To denote the parameters in more than one group, multiply the right-hand
side variable by a vector of number. For example:all.x

- `"f1 =~ c(2,3)*x2"` denotes the factor loading of `x2` on `f1` in
  Group 2 and Group 3.

Elements that cannot be converted to a parameter in the parameter table
will be ignored.

Currently supports
[lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
outputs only.

## Examples

``` r

library(lavaan)
data(simple_med)
mod <-
"
m ~ a*x
y ~ b*m
ab:= a*b
asq:= a^2
"
fit_med <- sem(mod, simple_med, fixed.x = FALSE)
p_table <- parameterTable(fit_med)

pars <- c("m ~ x",
          "y ~ m",
          "asq := 1",
          "ab  := 2")
out <- syntax_to_i(pars, fit_med)
out
#> [1] 1 2 6 7
p_table[out, ]
#>   id lhs op rhs user block group free ustart exo label plabel start   est    se
#> 1  1   m  ~   x    1     1     1    1     NA   0     a   .p1. 1.676 1.676 0.431
#> 2  2   y  ~   m    1     1     1    2     NA   0     b   .p2. 0.535 0.535 0.073
#> 6  6  ab := a*b    1     0     0    0     NA   0    ab        0.000 0.897 0.261
#> 7  7 asq := a^2    1     0     0    0     NA   0   asq        0.000 2.809 1.444
```
