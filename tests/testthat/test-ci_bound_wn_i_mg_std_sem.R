skip_on_cran()

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f1 ~ f2
"
fit <- lavaan::sem(mod, cfa_two_factors_mg, group = "gp")

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(#ftol_abs = 1e-7,
              ftol_rel = 1e-4
              # xtol_abs = 1e-7,
              # xtol_rel = 1e-7,
              # tol_constraints_eq = 1e-10
              )
time1l <- system.time(out1l <- ci_bound_wn_i(25, 38, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))
time2u <- system.time(out2u <- ci_bound_wn_i(30, 38, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))

test_that("Check against precomputed answers", {
    expect_equal(out1l$bound, 0.655515, tolerance = 1e-5)
    expect_equal(out2u$bound, 0.6976961, tolerance = 1e-5)
  })

skip("Run only if data changed")

# Check the results

modc0 <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f1 ~ f2
tstd := geteststd()
"

geteststd <- get_std_genfct(fit = fit, i = 25)

test_limit <- out1l
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, dat, do.fit = FALSE, group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE,
                   optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02))
                   )
fitc_out1l <- fitc

geteststd <- get_std_genfct(fit = fit, i = 30)

test_limit <- out2u
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, dat, do.fit = FALSE, group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE,
                   optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02))
                   )
fitc_out2u <- fitc

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-3))
    expect_true(test_p(fitc_out2u, fit, ciperc = ciperc, tol = 1e-3))
  })

