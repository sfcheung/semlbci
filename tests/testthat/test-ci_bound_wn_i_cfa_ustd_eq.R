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
b == d
"
fit <- lavaan::sem(mod, cfa_two_factors)

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
time1l <- system.time(out1l <- ci_bound_wn_i(3, 13, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time1u <- system.time(out1u <- ci_bound_wn_i(3, 13, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time2l <- system.time(out2l <- ci_bound_wn_i(5, 13, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time2u <- system.time(out2u <- ci_bound_wn_i(5, 13, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time3l <- system.time(out3l <- ci_bound_wn_i(6, 13, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time3u <- system.time(out3u <- ci_bound_wn_i(6, 13, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time4l <- system.time(out4l <- ci_bound_wn_i(15, 13, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time4u <- system.time(out4u <- ci_bound_wn_i(15, 13, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))

timexx <- rbind(time1l, time1u,
                time2l, time2u,
                time3l, time3u,
                time4l, time4u)
timexx


# system.time(out03l <- ci_bound_wn_i(3, 13, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0))
# system.time(out03u <- ci_bound_wn_i(3, 13, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0))
# system.time(out05l <- ci_bound_wn_i(5, 13, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0))
# system.time(out05u <- ci_bound_wn_i(5, 13, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0))
# system.time(out06l <- ci_bound_wn_i(6, 13, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0))
# system.time(out06u <- ci_bound_wn_i(6, 13, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0))
# system.time(out15l <- ci_bound_wn_i(15, 13, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0))
# system.time(out15u <- ci_bound_wn_i(15, 13, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0))

# Check the results

modc0 <- 
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + e*x6
b == d
"

test_out1l <- test_constr(fit = fit, dat = dat, ciperc = ciperc, parc = "c == ", modc0 = modc0, ci_out = out1l, semfct = lavaan::cfa, tol = 1e-4, fixed.x = FALSE)
test_out1u <- test_constr(fit = fit, dat = dat, ciperc = ciperc, parc = "c == ", modc0 = modc0, ci_out = out1u, semfct = lavaan::cfa, tol = 1e-4, fixed.x = FALSE)
test_out2l <- test_constr(fit = fit, dat = dat, ciperc = ciperc, parc = "d == ", modc0 = modc0, ci_out = out2l, semfct = lavaan::cfa, tol = 1e-4, fixed.x = FALSE)
test_out2u <- test_constr(fit = fit, dat = dat, ciperc = ciperc, parc = "d == ", modc0 = modc0, ci_out = out2u, semfct = lavaan::cfa, tol = 1e-4, fixed.x = FALSE)
test_out3l <- test_constr(fit = fit, dat = dat, ciperc = ciperc, parc = "e == ", modc0 = modc0, ci_out = out3l, semfct = lavaan::cfa, tol = 1e-4, fixed.x = FALSE)
test_out3u <- test_constr(fit = fit, dat = dat, ciperc = ciperc, parc = "e == ", modc0 = modc0, ci_out = out3u, semfct = lavaan::cfa, tol = 1e-4, fixed.x = FALSE)

modc0 <- 
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + e*x6
f1 ~~ fr*f2
b == d
"

test_out4l <- test_constr(fit = fit, dat = dat, ciperc = ciperc, parc = "fr == ", modc0 = modc0, ci_out = out4l, semfct = lavaan::cfa, tol = 1e-4, fixed.x = FALSE)
test_out4u <- test_constr(fit = fit, dat = dat, ciperc = ciperc, parc = "fr == ", modc0 = modc0, ci_out = out4u, semfct = lavaan::cfa, tol = 1e-4, fixed.x = FALSE)

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_out1l)
    expect_true(test_out1u)
    expect_true(test_out2l)
    expect_true(test_out2u)
    expect_true(test_out3l)
    expect_true(test_out3u)
    expect_true(test_out4l)
    expect_true(test_out4u)
  })

