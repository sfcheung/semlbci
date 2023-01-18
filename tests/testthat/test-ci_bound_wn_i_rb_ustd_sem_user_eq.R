skip_on_cran()
library(testthat)
library(semlbci)

# NOTE
# Difficult to solve for ci_bound_wn_i(16, 13) ubound.

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
0 == (c - d)^2
"
fit <- lavaan::sem(mod, cfa_two_factors, test = "satorra.bentler")

# Find the scaling factors

sf1 <- scaling_factor3(fit, 16)
sf2 <- scaling_factor3(fit, 5)

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
time1l <- system.time(out1l <- ci_bound_wn_i(16, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = list(ftol_abs = 1e-7), verbose = TRUE, ciperc = ciperc, sf = sf1$c_r, sf2 = sf1$c_rb))
# time1u <- system.time(out1u <- ci_bound_wn_i(16, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = list(ftol_abs = 1e-7), verbose = TRUE, ciperc = ciperc, sf = sf1$c_r, sf2 = sf1$c_rb, wald_ci_start = TRUE))
# time2l <- system.time(out2l <- ci_bound_wn_i( 5, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = list(ftol_abs = 1e-7), verbose = TRUE, ciperc = ciperc, sf = sf2$c_r, sf2 = sf1$c_rb))
time2u <- system.time(out2u <- ci_bound_wn_i( 5, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = list(ftol_abs = 1e-7), verbose = TRUE, ciperc = ciperc, sf = sf2$c_r, sf2 = sf1$c_rb))

# timexx <- rbind(time1l, time1u, time2l, time2u)
# timexx

test_that("Check against precomputed answers", {
    expect_equal(out1l$bound, 0.9409085, tolerance = 1e-5)
    expect_equal(out2u$bound, 0.9463263, tolerance = 1e-5)
  })

skip("Run only if data changed")

# Check the results

test_p <- function(fit0, fit1, ciperc, tol) {
    out <- lavTestLRT(fit0, fit1, method = "satorra.2000", A.method = "exact")
    abs(out[2, "Pr(>Chisq)"] - (1 - ciperc)) < tol
  }


modc0 <-
"
f1 =~ x1 + a*x2 + c*x3
f2 =~ x4 + b*x5 + d*x6
f1 ~ g*f2
ab := a * b
0 == (c - d)^2
"

test_limit <- out1l
modc <- paste(modc0, "\nab == ", test_limit$bound)
fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
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
# print(lavTestLRT(fitc, fit, method = "satorra.2000", A.method = "exact"))

# test_limit <- out1u
# modc <- paste(modc0, "\nab == ", test_limit$bound)
# fitc <- lavaan::sem(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
# ptable <- parameterTable(fitc)
# ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
# fitc <- update(fitc, start = ptable, do.fit = TRUE,
#                    baseline = FALSE, h1 = FALSE, se = "none",
#                    verbose = FALSE
#                   #  optim.force.converged = TRUE,
#                   #  optim.dx.tol = .01,
#                   #  warn = FALSE,
#                   #  control = list(
#                   #     eval.max = 2,
#                   #     iterations = 1,
#                   #     control.outer = list(tol = 1e-02,
#                   #                          itmax = 1)
#                   # )
#                 )
# fitc_out1u <- fitc
# print(lavTestLRT(fitc, fit, method = "satorra.2000", A.method = "exact"))

# test_limit <- out2l
# modc <- paste(modc0, "\nb == ", test_limit$bound)
# fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
# ptable <- parameterTable(fitc)
# ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
# fitc <- update(fitc, start = ptable, do.fit = TRUE,
#                    baseline = FALSE, h1 = FALSE, se = "none",
#                    verbose = FALSE
#                   #  optim.force.converged = TRUE,
#                   #  optim.dx.tol = .01,
#                   #  warn = FALSE,
#                   #  control = list(
#                   #     eval.max = 2,
#                   #     iterations = 1,
#                   #     control.outer = list(tol = 1e-02,
#                   #                          itmax = 1)
#                   # )
#                 )
# fitc_out2l <- fitc
# print(lavTestLRT(fitc, fit, method = "satorra.2000", A.method = "exact"))

test_limit <- out2u
modc <- paste(modc0, "\nb == ", test_limit$bound)
fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
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
# print(lavTestLRT(fitc, fit, method = "satorra.2000", A.method = "exact"))


get_scaling_factor <- function(lrt_out) {
    data.frame(c_p = 1 / attr(lrt_out, "scale")[2],
              c_pb = attr(lrt_out, "shift")[2],
              c_r = 1 / attr(lrt_out, "scale")[2],
              c_rb = attr(lrt_out, "shift")[2])
  }

(lr_out_1l <- lavTestLRT(fitc_out1l, fit, method = "satorra.2000", A.method = "exact"))
get_scaling_factor(lr_out_1l)
sf1
(lr_out_2u <- lavTestLRT(fitc_out2u, fit, method = "satorra.2000", A.method = "exact"))
get_scaling_factor(lr_out_2u)
sf2

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-3))
    # expect_true(test_p(fitc_out1u, fit, ciperc = ciperc, tol = 1e-3))
    # expect_true(test_p(fitc_out2l, fit, ciperc = ciperc, tol = 1e-3))
    expect_true(test_p(fitc_out2u, fit, ciperc = ciperc, tol = 1e-3))
  })

