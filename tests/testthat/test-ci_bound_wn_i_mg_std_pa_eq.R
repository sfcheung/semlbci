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
y ~ c(b1, b1)*m
"
fit <- lavaan::sem(mod, simple_med_mg, fixed.x = FALSE, group = "gp")
lavaan::parameterTable(fit)
lavaan::standardizedSolution(fit)

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
time1l <- system.time(out1l <- ci_bound_wn_i(1, 16, sem_out = fit, f_constr = fn_constr0, which = "lbound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, opts = opts0, wald_ci_start = FALSE, std_method = "internal"))
time2u <- system.time(out2u <- ci_bound_wn_i(10, 16, sem_out = fit, f_constr = fn_constr0, which = "ubound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, opts = opts0, std_method = "internal"))

test_that("Check against precomputed answers", {
    expect_equal(out1l$bound, 0.1385524, tolerance = 1e-5)
    expect_equal(out2u$bound, 0.5580358, tolerance = 1e-5)
  })

skip("Run only if data changed")

# Check the results

geteststd <- get_std_genfct(fit = fit, i = 1)

modc0 <-
"
m ~ c(a1, a1)*x
y ~ c(b1, b1)*m
astd := geteststd()
"

test_limit <- out1l
modc <- paste(modc0, "\nastd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, simple_med_mg, do.fit = FALSE, fixed.x = FALSE, group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE,
                   optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02))
                   )
fitc_out1l <- fitc

geteststd <- get_std_genfct(fit = fit, i = 10)

modc0 <-
"
m ~ c(a1, a1)*x
y ~ c(b1, b1)*m
bstd := geteststd()
"

test_limit <- out2u
modc <- paste(modc0, "\nbstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, simple_med_mg, do.fit = FALSE, fixed.x = FALSE, group = "gp")
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

