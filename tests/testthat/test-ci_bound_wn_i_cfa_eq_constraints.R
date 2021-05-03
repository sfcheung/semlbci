skip("WIP: Tests not passed or tests not ready. To fix")

library(testthat)
library(semlbci)

library(lavaan)
data(cfa_two_factors)
dat <- cfa_two_factors
mod <- 
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + e*x6
b == d
"
fit_cfa <- lavaan::sem(mod, dat)
parameterEstimates(fit_cfa)
parameterTable(fit_cfa)

fn_constr0 <- set_constraint(fit_cfa)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7
              )
system.time(out03l <- ci_bound_wn_i(3, 13, sem_out = fit_cfa, which = "lbound", opts = opts0, f_constr = fn_constr0))
system.time(out03u <- ci_bound_wn_i(3, 13, sem_out = fit_cfa, which = "ubound", opts = opts0, f_constr = fn_constr0))
system.time(out05l <- ci_bound_wn_i(5, 13, sem_out = fit_cfa, which = "lbound", opts = opts0, f_constr = fn_constr0))
system.time(out05u <- ci_bound_wn_i(5, 13, sem_out = fit_cfa, which = "ubound", opts = opts0, f_constr = fn_constr0))
system.time(out06l <- ci_bound_wn_i(6, 13, sem_out = fit_cfa, which = "lbound", opts = opts0, f_constr = fn_constr0))
system.time(out06u <- ci_bound_wn_i(6, 13, sem_out = fit_cfa, which = "ubound", opts = opts0, f_constr = fn_constr0))
system.time(out15l <- ci_bound_wn_i(15, 13, sem_out = fit_cfa, which = "lbound", opts = opts0, f_constr = fn_constr0))
system.time(out15u <- ci_bound_wn_i(15, 13, sem_out = fit_cfa, which = "ubound", opts = opts0, f_constr = fn_constr0))


modc0 <- 
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + e*x6
b == d
"

modc <- paste(modc0, "\nc == ", out03l)
cat(modc)
fit_cfac <- lavaan::cfa(modc, dat)
anova(fit_cfac, fit_cfa)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_cfac, fit_cfa)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

modc <- paste(modc0, "\nc == ", out03u)
cat(modc)
fit_cfac <- lavaan::cfa(modc, dat)
anova(fit_cfac, fit_cfa)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_cfac, fit_cfa)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

modc <- paste(modc0, "\nd == ", out05l)
cat(modc)
fit_cfac <- lavaan::cfa(modc, dat)
anova(fit_cfac, fit_cfa)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_cfac, fit_cfa)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

modc <- paste(modc0, "\nd == ", out05u)
cat(modc)
fit_cfac <- lavaan::cfa(modc, dat)
anova(fit_cfac, fit_cfa)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_cfac, fit_cfa)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

modc <- paste(modc0, "\ne == ", out06l)
cat(modc)
fit_cfac <- lavaan::cfa(modc, dat)
anova(fit_cfac, fit_cfa)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_cfac, fit_cfa)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-3,
        ignore_attr = TRUE
      )
  })


modc <- paste(modc0, "\ne == ", out06u)
cat(modc)
fit_cfac <- lavaan::cfa(modc, dat)
anova(fit_cfac, fit_cfa)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_cfac, fit_cfa)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-3,
        ignore_attr = TRUE
      )
  })



modc0 <- 
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + e*x6
f1 ~~ fr*f2
b == d
"


modc <- paste(modc0, "\nfr == ", out15l)
cat(modc)
fit_cfac <- lavaan::cfa(modc, dat)
anova(fit_cfac, fit_cfa)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_cfac, fit_cfa)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-3,
        ignore_attr = TRUE
      )
  })


modc <- paste(modc0, "\nfr == ", out15u)
cat(modc)
fit_cfac <- lavaan::cfa(modc, dat)
anova(fit_cfac, fit_cfa)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_cfac, fit_cfa)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-3,
        ignore_attr = TRUE
      )
  })

