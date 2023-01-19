skip_on_cran()

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <-
"
f1 =~ x1 + c(a1, a2)*x2 + c(b1, b2)*x3
f2 =~ x4 + c(c1, c1)*x5 + c(d1, d2)*x6
ad := a1 * c1
b1 == b2
"
fit <- lavaan::cfa(mod, cfa_two_factors_mg, group = "gp")
ptable <- parameterTable(fit)
ptable
stable <- standardizedSolution(fit)
stable

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
time1l <- system.time(out1l <- ci_bound_wn_i(47, 38, sem_out = fit, f_constr = fn_constr0, which = "lbound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))
time3u <- system.time(out3u <- ci_bound_wn_i(26, 38, sem_out = fit, f_constr = fn_constr0, which = "ubound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))

test_that("Check against precomputed answers", {
    expect_equal(out1l$bound, 0.4089111, tolerance = 1e-5)
    expect_equal(out3u$bound, 0.6011265, tolerance = 1e-5)
  })

skip("Run only if data changed")

# Check the results

modc0 <-
"
f1 =~ x1 + c(a1, a2)*x2 + c(b1, b2)*x3
f2 =~ x4 + c(c1, c1)*x5 + c(d1, d2)*x6
ad := a1 * c1
b1 == b2
tstd := geteststd()
"

geteststd <- get_std_genfct(fit = fit, i = 47)

test_limit <- out1l
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, cfa_two_factors_mg, do.fit = FALSE, group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE,
                   optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02))
                   )
fitc_out1l <- fitc

geteststd <- get_std_genfct(fit = fit, i = 26)

test_limit <- out3u
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, cfa_two_factors_mg, do.fit = FALSE, group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE,
                   optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02))
                   )
fitc_out3u <- fitc

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out3u, fit, ciperc = ciperc, tol = 1e-4))
  })

