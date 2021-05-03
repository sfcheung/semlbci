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
f1 ~~ fr*f2
b == d
ce := c*e
"
fit_cfa <- lavaan::cfa(mod, dat)
ptable <- parameterTable(fit_cfa)
ptable

fn_constr0 <- set_constraint(fit_cfa)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7
              )
# system.time(out03l <- ci_bound_wn_i(3, 13, sem_out = fit_cfa, which = "lbound", opts = opts0, f_constr = fn_constr0))
# system.time(out03u <- ci_bound_wn_i(3, 13, sem_out = fit_cfa, which = "ubound", opts = opts0, f_constr = fn_constr0))
# system.time(out06l <- ci_bound_wn_i(6, 13, sem_out = fit_cfa, which = "lbound", opts = opts0, f_constr = fn_constr0))
# system.time(out06u <- ci_bound_wn_i(6, 13, sem_out = fit_cfa, which = "ubound", opts = opts0, f_constr = fn_constr0))
# system.time(out07l <- ci_bound_wn_i(7, 13, sem_out = fit_cfa, which = "lbound", opts = opts0, f_constr = fn_constr0))
# system.time(out07u <- ci_bound_wn_i(7, 13, sem_out = fit_cfa, which = "ubound", opts = opts0, f_constr = fn_constr0))

system.time(out17l <- ci_bound_wn_i(17, 13, sem_out = fit_cfa, which = "lbound", opts = opts0, f_constr = fn_constr0))
system.time(out17u <- ci_bound_wn_i(17, 13, sem_out = fit_cfa, which = "ubound", opts = opts0, f_constr = fn_constr0))

modc0 <- 
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + e*x6
cd := c*d
"

modc <- paste(modc0, "\ncd == ", out16l)
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

modc <- paste(modc0, "\ncd == ", out16u)
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


