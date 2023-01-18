skip_on_cran()

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)
data(simple_med_mg)
dat <- simple_med_mg
mod <-
"
m ~ c(a1, a1)*x
y ~ c(a2, a2)*m
asq := a2^2
"
fit <- lavaan::sem(mod, simple_med_mg, fixed.x = FALSE, group = "gp")

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              # ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              # xtol_rel = 1e-7,
              tol_constraints_eq = c(1e-10, 1e-10, 1e-10),
              print_level = 0
              )
time1l <- system.time(out1l <- ci_bound_wn_i( 1,16, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE, ciperc = ciperc))
# time1u <- system.time(out1u <- ci_bound_wn_i( 1,16, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE, ciperc = ciperc))
# time2l <- system.time(out2l <- ci_bound_wn_i( 2,16, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE, ciperc = ciperc))
# time2u <- system.time(out2u <- ci_bound_wn_i( 2,16, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE, ciperc = ciperc))
# time3l <- system.time(out3l <- ci_bound_wn_i(17,16, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE, ciperc = ciperc))
time3u <- system.time(out3u <- ci_bound_wn_i(17,16, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE, ciperc = ciperc))

# timexx <- rbind(time1l, time1u, time2l, time2u, time3l, time3u)
# timexx

test_that("Check against precomputed answers", {
    expect_equal(out1l$bound, 0.8130598, tolerance = 1e-5)
    expect_equal(out2u$bound, 0.7953846, tolerance = 1e-5)
  })

skip("Run only if data changed")

# Check the results

test_p <- function(fit0, fit1, ciperc, tol) {
    out <- lavTestLRT(fit0, fit1)
    abs(out[2, "Pr(>Chisq)"] - (1 - ciperc)) < tol
  }

modc0 <-
"
m ~ c(a1, a1)*x
y ~ c(a2, a2)*m
asq := a2^2
"

test_limit <- out1l
modc <- paste(modc0, "\na1 == ", test_limit$bound)
fitc <- lavaan::sem(modc, simple_med_mg, fixed.x = FALSE, do.fit = FALSE, group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE
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

test_limit <- out3u
modc <- paste(modc0, "\nasq == ", test_limit$bound)
fitc <- lavaan::sem(modc, simple_med_mg, fixed.x = FALSE, do.fit = FALSE, group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE
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



# test_out1l <- test_constr(fit = fit, dat = simple_med_mg, ciperc = ciperc, parc = "a1 == ", modc0 = modc0, ci_out = out1l, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE, group = "gp")
# test_out1u <- test_constr(fit = fit, dat = simple_med_mg, ciperc = ciperc, parc = "a1 == ", modc0 = modc0, ci_out = out1u, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE, group = "gp")
# test_out2l <- test_constr(fit = fit, dat = simple_med_mg, ciperc = ciperc, parc = "a2 == ", modc0 = modc0, ci_out = out2l, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE, group = "gp")
# test_out2u <- test_constr(fit = fit, dat = simple_med_mg, ciperc = ciperc, parc = "a2 == ", modc0 = modc0, ci_out = out2u, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE, group = "gp")
# test_out3l <- test_constr(fit = fit, dat = simple_med_mg, ciperc = ciperc, parc = "asq == ", modc0 = modc0, ci_out = out3l, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE, group = "gp")
# test_out3u <- test_constr(fit = fit, dat = simple_med_mg, ciperc = ciperc, parc = "asq == ", modc0 = modc0, ci_out = out3u, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE, group = "gp")

# test_that("Check p-value for the chi-square difference test", {
#     expect_true(test_out1l)
#     expect_true(test_out1u)
#     expect_true(test_out2l)
#     expect_true(test_out2u)
#     expect_true(test_out3l)
#     expect_true(test_out3u)
#   })

