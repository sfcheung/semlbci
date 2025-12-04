skip_on_cran()

library(testthat)
library(semlbci)

# Fit the model

suppressMessages(library(lavaan))

data(cfa_two_factors)
dat <- cfa_two_factors
mod <-
"
f1 =~ x1 + a*x2 + b*x3
f2 =~ x4 + a*x5 + d*x6
b == d
"
fit <- lavaan::cfa(mod, cfa_two_factors)

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
time1l <- system.time(out1l <- ci_bound_wn_i(5, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))
time3u <- system.time(out3u <- ci_bound_wn_i(15, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))

test_that("Check against precomputed answers", {
    # 2025-12-04:
    # Increase the tolerance due to changes in lavaan optimization
    # lavaan 0.6-21:     0.6905356
    # lavaan pre 0.6-21: 0.6905471
    # lavaan 0.6-21:     0.6153947
    # lavaan pre 0.6-21: 0.6153876
    expect_equal(out1l$diag$ciperc_final, .96, tolerance = 1e-3)
    expect_equal(out3u$diag$ciperc_final, .96, tolerance = 1e-3)
    expect_equal(out1l$bound, 0.6905471, tolerance = 1e-3)
    expect_equal(out3u$bound, 0.6153876, tolerance = 1e-3)
  })

skip("Run only if data changed")

# Check the results

modc0 <-
"
f1 =~ x1 + a*x2 + b*x3
f2 =~ x4 + a*x5 + d*x6
b == d
tstd := geteststd()
"

geteststd <- get_std_genfct(fit = fit, i = 5)

test_limit <- out1l
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, cfa_two_factors, do.fit = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE,
                   optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out1l <- fitc

geteststd <- get_std_genfct(fit = fit, i = 15)

test_limit <- out3u
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, cfa_two_factors, do.fit = FALSE)
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

