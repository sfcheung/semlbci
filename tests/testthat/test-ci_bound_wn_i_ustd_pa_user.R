skip_on_cran()
skip_if_not(Sys.getenv("SEMLBCI_TEST_COMPREHENSIVE") == "TRUE")
library(testthat)
library(semlbci)

# Fit the model

library(lavaan)
data(simple_med)
dat <- simple_med
mod <- 
"
m ~ a*x
y ~ b*m
ab:= a*b
asq:= a^2
"
fit <- lavaan::sem(mod, simple_med, fixed.x = FALSE)
lavaan::parameterTable(fit)

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

time1l <- system.time(out1l <- ci_bound_wn_i(6, 5, sem_out = fit, f_constr = fn_constr0, which = "lbound", verbose = TRUE, ciperc = ciperc))
time1u <- system.time(out1u <- ci_bound_wn_i(6, 5, sem_out = fit, f_constr = fn_constr0, which = "ubound", verbose = TRUE, ciperc = ciperc))
time2l <- system.time(out2l <- ci_bound_wn_i(7, 5, sem_out = fit, f_constr = fn_constr0, which = "lbound", verbose = TRUE, ciperc = ciperc))
time2u <- system.time(out2u <- ci_bound_wn_i(7, 5, sem_out = fit, f_constr = fn_constr0, which = "ubound", verbose = TRUE, ciperc = ciperc))

timexx <- rbind(time1l, time1u, time2l, time2u)
timexx

# Check the results

modc0 <- 
"
m ~ a*x
y ~ b*m
ab:= a*b
asq:= a^2
"

test_out1l <- test_constr(fit = fit, dat = simple_med, ciperc = ciperc, parc = "ab == ", modc0 = modc0, ci_out = out1l, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE)
test_out1u <- test_constr(fit = fit, dat = simple_med, ciperc = ciperc, parc = "ab == ", modc0 = modc0, ci_out = out1u, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE)
test_out2l <- test_constr(fit = fit, dat = simple_med, ciperc = ciperc, parc = "asq == ", modc0 = modc0, ci_out = out2l, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE)
test_out2u <- test_constr(fit = fit, dat = simple_med, ciperc = ciperc, parc = "asq == ", modc0 = modc0, ci_out = out2u, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE)

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_out1l)
    expect_true(test_out1u)
    expect_true(test_out2l)
    expect_true(test_out2u)
  })

