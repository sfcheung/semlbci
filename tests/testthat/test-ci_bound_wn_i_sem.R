
library(testthat)
library(semlbci)

library(lavaan)

data(cfa_two_factors)
dat <- cfa_two_factors
mod <- 
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f1 ~ f2
"
fit_sem <- lavaan::sem(mod, dat)
ptable <- parameterTable(fit_sem)
ptable

fn_constr0 <- set_constraint(fit_sem)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7,
              tol_constraints_eq = 1e-10
              )
system.time(out02l <- ci_bound_wn_i(2, 13, sem_out = fit_sem, which = "lbound", opts = opts0, f_constr = fn_constr0))
system.time(out07u <- ci_bound_wn_i(7, 13, sem_out = fit_sem, which = "ubound", opts = opts0, f_constr = fn_constr0))

modc0 <- 
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + e*x6
f1 ~ g*f2
"

modc <- paste(modc0, "\nb == ", out02l)
fit_semc <- lavaan::cfa(modc, dat)
anova(fit_semc, fit_sem)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_semc, fit_sem)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-6,
        ignore_attr = TRUE
      )
  })

modc <- paste(modc0, "\ng == ", out07u)
fit_semc <- lavaan::cfa(modc, dat)
anova(fit_semc, fit_sem)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_semc, fit_sem)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-6,
        ignore_attr = TRUE
      )
  })


