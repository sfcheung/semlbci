skip("WIP")

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <- 
"
f1 =~ x1 + c(b1, b2)*x2 + c(c1, c1)*x3
f2 =~ x4 + c(d1, d2)*x5 + c(e1, e1)*x6
f1 ~~ c(fr1, fr2)*f2
ce := c1*e1
b1 == c1
"
fit <- lavaan::cfa(mod, cfa_two_factors_mg, group = "gp")

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7
              )
time1l <- system.time(out1l <- ci_bound_wn_i(47, 38, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time1u <- system.time(out1u <- ci_bound_wn_i(47, 38, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))

timexx <- rbind(time1l, time1u)
timexx

# Check the results

modc0 <- 
"
f1 =~ x1 + c(b1, b2)*x2 + c(c1, c1)*x3
f2 =~ x4 + c(d1, d2)*x5 + c(e1, e1)*x6
f1 ~~ c(fr1, fr2)*f2
ce := c1*e1
b1 == c1
"

test_out1l <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "ce == ", modc0 = modc0, ci_out = out1l, semfct = lavaan::cfa, tol = 1e-4, group = "gp")
test_out1u <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "ce == ", modc0 = modc0, ci_out = out1u, semfct = lavaan::cfa, tol = 1e-4, group = "gp")

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_out1l)
    expect_true(test_out1u)
  })

