skip_on_cran()

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <-
"
f1 =~ x1 + c(b1, b2)*x2 + c(c1, c2)*x3
f2 =~ x4 + c(d1, d2)*x5 + c(e1, e2)*x6
f1 ~~ c(fr1, fr2)*f2
ce := c1*e1
"
fit <- lavaan::cfa(mod, cfa_two_factors_mg, test = "satorra.bentler", group = "gp")


# Find the scaling factors

sf1 <- scaling_factor3(fit, 47)

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(#ftol_abs = 1e-7,
              ftol_rel = 1e-4
              # xtol_abs = 1e-7,
              # xtol_rel = 1e-7
              )
time1l <- system.time(out1l <- ci_bound_wn_i(47, 38, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc, sf = sf1$c_r, sf2 = sf1$c_rb))
time1u <- system.time(out1u <- ci_bound_wn_i(47, 38, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc, sf = sf1$c_r, sf2 = sf1$c_rb))

test_that("Check against precomputed answers", {
    expect_equal(out1l$bound, 0.4942793, tolerance = 1e-5)
    expect_equal(out1u$bound, 1.408892, tolerance = 1e-5)
  })

skip("Run only if data changed")

# Check the results

modc0 <-
"
f1 =~ x1 + c(b1, b2)*x2 + c(c1, c2)*x3
f2 =~ x4 + c(d1, d2)*x5 + c(e1, e2)*x6
f1 ~~ c(fr1, fr2)*f2
ce := c1*e1
"

test_limit <- out1l
modc <- paste(modc0, "\nce == ", test_limit$bound)
fitc <- lavaan::sem(modc, cfa_two_factors_mg, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler", group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE,
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
                )
fitc_out1l <- fitc

test_limit <- out1u
modc <- paste(modc0, "\nce == ", test_limit$bound)
fitc <- lavaan::sem(modc, cfa_two_factors_mg, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler", group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE,
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
                )
fitc_out1u <- fitc

get_scaling_factor(lavTestLRT(fitc_out1l, fit, method = "satorra.2000", A.method = "exact"))
sf1

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out1u, fit, ciperc = ciperc, tol = 1e-3))
  })

