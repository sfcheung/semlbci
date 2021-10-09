library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <- 
"
f1 =~ x1 + c(a1, a2)*x2 + x3
f2 =~ x4 + x5 + x6
f1 ~ c(fr1, fr2)*f2
"
fit <- lavaan::sem(mod, cfa_two_factors_mg, group = "gp")

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
time1l <- system.time(out1l <- ci_bound_wn_i(2, 38, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time1u <- system.time(out1u <- ci_bound_wn_i(2, 38, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time2l <- system.time(out2l <- ci_bound_wn_i(30, 38, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time2u <- system.time(out2u <- ci_bound_wn_i(30, 38, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))

timexx <- rbind(time1l, time1u, time2l, time2u)
timexx

# Check the results

modc0 <- 
"
f1 =~ x1 + c(a1, a2)*x2 + x3
f2 =~ x4 + x5 + x6
f1 ~ c(fr1, fr2)*f2
"

test_out1l <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "a1 == ", modc0 = modc0, ci_out = out1l, semfct = lavaan::sem, tol = 1e-4, group = "gp")
test_out1u <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "a1 == ", modc0 = modc0, ci_out = out1u, semfct = lavaan::sem, tol = 1e-4, group = "gp")
test_out2l <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "fr2 == ", modc0 = modc0, ci_out = out2l, semfct = lavaan::sem, tol = 1e-4, group = "gp")
test_out2u <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "fr2 == ", modc0 = modc0, ci_out = out2u, semfct = lavaan::sem, tol = 1e-4, group = "gp")

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_out1l)
    expect_true(test_out1u)
    expect_true(test_out2l)
    expect_true(test_out2u)
  })

