library(testthat)
library(semlbci)

# context("Check ci_bound_i: Check diagnostic info")

data(simple_med)
dat <- simple_med
mod <- 
"
m ~ x
y ~ m
"
fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)

fn_constr0 <- set_constraint(fit_med)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7,
              tol_constraints_eq = 1e-7
              )
system.time(out1l <- ci_bound_i(1, 5, sem_out = fit_med, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE))
system.time(out1u <- ci_bound_i(1, 5, sem_out = fit_med, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE))
# system.time(out1ls <- ci_bound_i(2, 5, sem_out = fit_med, f_constr = fn_constr0, which = "lbound", opts = opts0, standardized = TRUE, verbose = TRUE))
# system.time(out1us <- ci_bound_i(2, 5, sem_out = fit_med, f_constr = fn_constr0, which = "ubound", opts = opts0, standardized = TRUE, verbose = TRUE))

out1l_diag <- attr(out1l, "diag")
# out1ls_diag <- attr(out1ls, "diag")

test_that("Check diagnostic object: status", {
    expect_equal(
        out1l_diag$status, 0,
        ignore_attr = TRUE
      )
  })

test_that("Check diagnostic object: est_org", {
    expect_true(
        is.numeric(out1l_diag$est_org) & length(out1l_diag$est_org) == 1
      )
  })

test_that("Check diagnostic object: ci_org_limit", {
    expect_true(
        is.numeric(out1l_diag$ci_org_limit) & length(out1l_diag$ci_org_limit) == 1
      )
  })

test_that("Check diagnostic object: ci_limit_ratio", {
    expect_equal(
        out1l_diag$ci_limit_ratio,
        abs((out1l - out1l_diag$est_org) / (out1l_diag$ci_org_limit - out1l_diag$est_org)),
        ignore_attr = TRUE
      )
  })

test_that("Check diagnostic object: fit_post_check", {
    expect_true(
        isTRUE(out1l_diag$fit_post_check) | isFALSE(out1l_diag$fit_post_check)
      )
  })