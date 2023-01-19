skip_on_cran()

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors)
dat <- cfa_two_factors
mod <-
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + e*x6
0 == (b - d)^2
"
fit <- lavaan::cfa(mod, cfa_two_factors, test = "satorra.bentler")

# Find the scaling factors

sf1 <- scaling_factor3(fit, 3)
sf2 <- scaling_factor3(fit, 5)
sf3 <- scaling_factor3(fit, 6)
sf4 <- scaling_factor3(fit, 15)

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
time1l <- system.time(out1l <- ci_bound_wn_i( 3, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = list(ftol_abs = 1e-7), verbose = TRUE, ciperc = ciperc, sf = sf1$c_r, sf2 = sf1$c_rb))
time3u <- system.time(out3u <- ci_bound_wn_i( 6, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = list(ftol_abs = 1e-7), verbose = TRUE, ciperc = ciperc, sf = sf3$c_r, sf2 = sf1$c_rb))

test_that("Check against precomputed answers", {
    expect_equal(out1l$bound, 0.5235391, tolerance = 1e-5)
    expect_equal(out3u$bound, 1.54298, tolerance = 1e-5)
  })

skip("Run only if data changed")

# Check the results

modc0 <-
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + e*x6
f1 ~~ g*f2
0 == (b - d)^2
"

test_limit <- out1l
modc <- paste(modc0, "\nc == ", test_limit$bound)
fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- suppressWarnings(update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE
                  #  optim.force.converged = TRUE,
                  #  optim.dx.tol = .01,
                  #  warn = FALSE,
                  #  control = list(
                  #     eval.max = 2,
                  #     iterations = 1,
                  #     control.outer = list(tol = 1e-02,
                  #                          itmax = 1)
                  # )
                ))
fitc_out1l <- fitc

test_limit <- out3u
modc <- paste(modc0, "\ne == ", test_limit$bound)
fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- suppressWarnings(update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE
                  #  optim.force.converged = TRUE,
                  #  optim.dx.tol = .01,
                  #  warn = FALSE,
                  #  control = list(
                  #     eval.max = 2,
                  #     iterations = 1,
                  #     control.outer = list(tol = 1e-02,
                  #                          itmax = 1)
                  # )
                ))
fitc_out3u <- fitc

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-3))
    expect_true(test_p(fitc_out3u, fit, ciperc = ciperc, tol = 1e-3))
  })

