skip_on_cran()

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors)
dat <- cfa_two_factors
mod <-
"
f1 =~ x1 + a*x2 + b*x3
f2 =~ x4 + b*x5 + d*x6
ad := a * d
a == d
"
fit <- lavaan::cfa(mod, cfa_two_factors)
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
              ftol_rel = 1e-5
              #xtol_abs = 1e-7,
              #xtol_rel = 1e-7
              )
time1l <- system.time(out1l <- ci_bound_wn_i(16, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))
# time1u <- system.time(out1u <- ci_bound_wn_i(16, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))
# time2l <- system.time(out2l <- ci_bound_wn_i(2, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))
time2u <- system.time(out2u <- ci_bound_wn_i(2, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))
time3l <- system.time(out3l <- ci_bound_wn_i(15, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))
# time3u <- system.time(out3u <- ci_bound_wn_i(15, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))

# timexx <- rbind(time1l, time2u, time3l)
# timexx
# colSums(timexx)

test_that("Check against precomputed answers", {
    expect_equal(out1l$bound, 0.5252068, tolerance = 1e-5)
    # expect_equal(out1u$bound, 0.6973046, tolerance = 1e-5)
    # expect_equal(out2l$bound, 0.6588698, tolerance = 1e-5)
    expect_equal(out2u$bound, 0.802191, tolerance = 1e-5)
    expect_equal(out3l$bound, 0.4075831, tolerance = 1e-4)
    # expect_equal(out3u$bound, 0.617597, tolerance = 1e-5)
  })


skip("Run only if data changed")

# Not yet have a way to find how to make test_constr work in standardized solution

test_p <- function(fit0, fit1, ciperc, tol) {
    abs(anova(fit0, fit1)[2, "Pr(>Chisq)"] - (1 - ciperc)) < tol
  }

# gen_test_data <- FALSE
# if (gen_test_data) {

modc0 <-
"
f1 =~ x1 + a*x2 + b*x3
f2 =~ x4 + b*x5 + d*x6
ad := a * d
a == d
tstd := geteststd()
"

geteststd <- get_std_genfct(fit = fit, i = 16)

test_limit <- out1l
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, cfa_two_factors, do.fit = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out1l <- fitc

test_limit <- out1u
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, cfa_two_factors, do.fit = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out1u <- fitc


geteststd <- get_std_genfct(fit = fit, i = 2)

test_limit <- out2l
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, cfa_two_factors, do.fit = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out2l <- fitc

test_limit <- out2u
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, cfa_two_factors, do.fit = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out2u <- fitc


geteststd <- get_std_genfct(fit = fit, i = 15)

test_limit <- out3l
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, cfa_two_factors, do.fit = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out3l <- fitc

test_limit <- out3u
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, cfa_two_factors, do.fit = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out3u <- fitc

# save(fitc_out1l, fitc_out1u,
#      fitc_out2l, fitc_out2u,
#      fitc_out3l, fitc_out3u,
#      file = "inst/testdata/test-ci_bound_wn_i_std_cfa_user_eq.RData",
#      compress = "xz",
#      compression_level = 9)
# }

# load(system.file("testdata", "test-ci_bound_wn_i_std_cfa_user_eq.RData",
#                   package = "semlbci"))

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out1u, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out2l, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out2u, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out3l, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out3u, fit, ciperc = ciperc, tol = 1e-4))
  })

