skip("WIP")

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)
data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <- 
"
f1 =~ x1 + c(b1, b2)*x2 + c(c1, c2)*x3
f2 =~ x4 + c(d1, d2)*x5 + c(e1, e2)*x6
f1 ~~ c(r1, r2)*f2
b1 == b2
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
              xtol_rel = 1e-7
              )
time1l <- system.time(out1l <- ci_bound_wn_i(3, 38, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time1u <- system.time(out1u <- ci_bound_wn_i(3, 38, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time2l <- system.time(out2l <- ci_bound_wn_i(29, 38, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time2u <- system.time(out2u <- ci_bound_wn_i(29, 38, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time3l <- system.time(out3l <- ci_bound_wn_i(30, 38, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time3u <- system.time(out3u <- ci_bound_wn_i(30, 38, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time4l <- system.time(out4l <- ci_bound_wn_i(26, 38, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time4u <- system.time(out4u <- ci_bound_wn_i(26, 38, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))

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
f1 =~ x1 + c(b1, b2)*x2 + c(c1, c2)*x3
f2 =~ x4 + c(d1, d2)*x5 + c(e1, e2)*x6
f1 ~~ c(r1, r2)*f2
b1 == b2
"

test_out1l <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "c1 == ", modc0 = modc0, ci_out = out1l, semfct = lavaan::cfa, tol = 1e-4, fixed.x = FALSE, group = "gp")
test_out1u <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "c1 == ", modc0 = modc0, ci_out = out1u, semfct = lavaan::cfa, tol = 1e-4, fixed.x = FALSE, group = "gp")
test_out2l <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "e2 == ", modc0 = modc0, ci_out = out2l, semfct = lavaan::cfa, tol = 1e-4, fixed.x = FALSE, group = "gp")
test_out2u <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "e2 == ", modc0 = modc0, ci_out = out2u, semfct = lavaan::cfa, tol = 1e-4, fixed.x = FALSE, group = "gp")
test_out3l <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "r2 == ", modc0 = modc0, ci_out = out3l, semfct = lavaan::cfa, tol = 1e-4, fixed.x = FALSE, group = "gp")
test_out3u <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "r2 == ", modc0 = modc0, ci_out = out3u, semfct = lavaan::cfa, tol = 1e-4, fixed.x = FALSE, group = "gp")

modc0 <- 
"
f1 =~ x1 + c(b1, b2)*x2 + c(c1, c2)*x3
f2 =~ x4 + c(d1, d2)*x5 + c(e1, e2)*x6
f1 ~~ c(r1, r2)*f2
b1 == b2
"

test_out4l <- suppressWarnings(test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "c2 == ", modc0 = modc0, ci_out = out4l, semfct = lavaan::cfa, tol = 1e-4, fixed.x = FALSE, group = "gp"))
test_out4u <- suppressWarnings(test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "c2 == ", modc0 = modc0, ci_out = out4u, semfct = lavaan::cfa, tol = 1e-4, fixed.x = FALSE, group = "gp"))

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

