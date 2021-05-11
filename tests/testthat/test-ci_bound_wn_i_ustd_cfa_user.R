library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors)
dat <- cfa_two_factors
mod <- 
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + e*x6
cd := c*d
"
fit <- lavaan::cfa(mod, cfa_two_factors)

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7,
              tol_constraints_eq = 1e-10
              )
time1l <- system.time(out1l <- ci_bound_wn_i(16, 13, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time1u <- system.time(out1u <- ci_bound_wn_i(16, 13, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))

timexx <- rbind(time1l, time1u)
timexx

# Check the results

modc0 <- 
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + e*x6
cd := c*d
"

test_out1l <- test_constr(fit = fit, dat = cfa_two_factors, ciperc = ciperc, parc = "cd == ", modc0 = modc0, ci_out = out1l, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE)
test_out1u <- test_constr(fit = fit, dat = cfa_two_factors, ciperc = ciperc, parc = "cd == ", modc0 = modc0, ci_out = out1u, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE)

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_out1l)
    expect_true(test_out1u)
  })
