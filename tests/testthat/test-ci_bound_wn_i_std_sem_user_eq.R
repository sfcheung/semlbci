skip_on_cran()

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors)
dat <- cfa_two_factors
mod <-
"
f1 =~ x1 + a*x2 + c*x3
f2 =~ x4 + b*x5 + d*x6
f1 ~ f2
ab := a * b
c == d
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
time1l <- system.time(out1l <- ci_bound_wn_i(16, 13, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))
time2u <- system.time(out2u <- ci_bound_wn_i( 6, 13, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))

test_that("Check against precomputed answers", {
    expect_equal(out1l$bound, 0.4558762, tolerance = 1e-5)
    expect_equal(out2u$bound, 0.8478685, tolerance = 1e-5)
  })

skip("Run only if data changed")

# Check the results

modc0 <-
"
f1 =~ x1 + a*x2 + c*x3
f2 =~ x4 + b*x5 + d*x6
f1 ~ f2
ab := a * b
c == d
tstd := geteststd()
"

geteststd <- get_std_genfct(fit = fit, i = 16)

test_limit <- out1l
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, dat, do.fit = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE,
                   optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02))
                   )
fitc_out1l <- fitc

geteststd <- get_std_genfct(fit = fit, i = 6)

test_limit <- out2u
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, dat, do.fit = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE,
                   optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02))
                   )
fitc_out2u <- fitc

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out2u, fit, ciperc = ciperc, tol = 1e-4))
  })

