skip("WIP: Tests not passed or tests not ready. To fix")

library(testthat)
library(semlbci)

# Do CFA later. Too slow for lavaan

data(cfa_two_factors)
dat <- cfa_two_factors
mod <- 
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
"
fit_cfa <- lavaan::sem(mod, dat)
parameterTable(fit_cfa)

fn_constr0 <- set_constraint(fit_cfa)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7
              )
system.time(out2l <- ci_bound_wn_i(2, 13, sem_out = fit_cfa, which = "lbound", opts = opts0, f_constr = fn_constr0))
system.time(out2u <- ci_bound_wn_i(2, 13, sem_out = fit_cfa, which = "ubound", opts = opts0, f_constr = fn_constr0))
system.time(out3l <- ci_bound_wn_i(3, 13, sem_out = fit_cfa, which = "lbound", opts = opts0, f_constr = fn_constr0))
system.time(out3u <- ci_bound_wn_i(3, 13, sem_out = fit_cfa, which = "ubound", opts = opts0, f_constr = fn_constr0))
system.time(out5l <- ci_bound_wn_i(5, 13, sem_out = fit_cfa, which = "lbound", opts = opts0, f_constr = fn_constr0))
system.time(out5u <- ci_bound_wn_i(5, 13, sem_out = fit_cfa, which = "ubound", opts = opts0, f_constr = fn_constr0))
system.time(out6l <- ci_bound_wn_i(6, 13, sem_out = fit_cfa, which = "lbound", opts = opts0, f_constr = fn_constr0))
system.time(out6u <- ci_bound_wn_i(6, 13, sem_out = fit_cfa, which = "ubound", opts = opts0, f_constr = fn_constr0))


modc0 <- 
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + e*x6
"

modc <- paste(modc0, "\nb == ", out02l)
fit_cfac <- lavaan::cfa(modc, dat)
anova(fit_cfac, fit_cfa)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_cfac, fit_cfa)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-6,
        ignore_attr = TRUE
      )
  })

modc <- paste(modc0, "\ne == ", out06u)
fit_cfac <- lavaan::cfa(modc, dat)
anova(fit_cfac, fit_cfa)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_cfac, fit_cfa)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-6,
        ignore_attr = TRUE
      )
  })



