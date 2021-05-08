skip("WIP: ci_bound_nm_i on hold")

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
cd := c*d
"
fit_cfa <- lavaan::cfa(mod, dat)
ptable <- parameterTable(fit_cfa)
ptable

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7,
              tol_constraints_eq = 1e-10
              )
system.time(out16l <- ci_bound_nm_i(16, 13, sem_out = fit_cfa, which = "lbound", opts = opts0))
system.time(out16u <- ci_bound_nm_i(16, 13, sem_out = fit_cfa, which = "ubound", opts = opts0))

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


