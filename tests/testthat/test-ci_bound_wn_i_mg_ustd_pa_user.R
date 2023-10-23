skip_on_cran()

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(simple_med_mg)
dat <- simple_med_mg
mod <-
"
m ~ c(a1, a2)*x
y ~ c(b1, b2)*m
ab:= a1*b1
asq:= a2^2
"
fit <- lavaan::sem(mod, simple_med_mg, fixed.x = FALSE, group = "gp")
lavaan::parameterTable(fit)

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

time1l <- system.time(out1l <- ci_bound_wn_i(17,16, sem_out = fit, f_constr = fn_constr0, which = "lbound", verbose = TRUE, ciperc = ciperc))
time2u <- system.time(out2u <- ci_bound_wn_i(18,16, sem_out = fit, f_constr = fn_constr0, which = "ubound", verbose = TRUE, ciperc = ciperc))

# 2023-10-23: Updated precomputed answers manually checked for validity.
test_that("Check against precomputed answers", {
    expect_equal(out1l$bound, 0.04712413, tolerance = 1e-4)
    expect_equal(out2u$bound, 13.24741, tolerance = 1e-4)
  })

skip("Run only if data changed")

# Check the results

modc0 <-
"
m ~ c(a1, a2)*x
y ~ c(b1, b2)*m
ab:= a1*b1
asq:= a2^2
"

test_limit <- out1l
modc <- paste(modc0, "\nab == ", test_limit$bound)
fitc <- lavaan::sem(modc, simple_med_mg, fixed.x = FALSE, do.fit = FALSE, group = "gp")
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

test_limit <- out2u
modc <- paste(modc0, "\nasq == ", test_limit$bound)
fitc <- lavaan::sem(modc, simple_med_mg, fixed.x = FALSE, do.fit = FALSE, group = "gp")
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
fitc_out2u <- fitc

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out2u, fit, ciperc = ciperc, tol = 1e-4))
  })
