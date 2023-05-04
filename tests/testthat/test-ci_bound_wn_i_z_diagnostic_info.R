library(testthat)
library(semlbci)

# context("Check ci_bound_wn_i: Check diagnostic info")

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
system.time(out1l <- ci_bound_wn_i(1, 5, sem_out = fit_med, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE))
system.time(out1u <- ci_bound_wn_i(1, 5, sem_out = fit_med, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE))
# system.time(out1ls <- ci_bound_wn_i(2, 5, sem_out = fit_med, f_constr = fn_constr0, which = "lbound", opts = opts0, standardized = TRUE, verbose = TRUE))
# system.time(out1us <- ci_bound_wn_i(2, 5, sem_out = fit_med, f_constr = fn_constr0, which = "ubound", opts = opts0, standardized = TRUE, verbose = TRUE))

out1l_diag <- out1l$diag
out1u_diag <- out1u$diag
# out1l_diag <- attr(out1l, "diag")
# out1ls_diag <- attr(out1ls, "diag")

# Manual check for status

status_check <- function(x) {
    chk <- 0
    if (x$diag$history$status < 0) {
        chk <- 1
      }
    if (!x$diag$fit_post_check) {
        chk <- 1
      }
    if (abs(x$diag$ciperc_final - x$diag$ciperc) > 5e-4) {
        chk <- 1
      }
    chk
  }

status_checkl <- status_check(out1l)
status_checku <- status_check(out1u)

test_that("Check diagnostic object: status", {
    expect_equal(
        out1l_diag$status,
        status_checkl,
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
    if (status_checkl == 0) {
        expect_equal(
            out1l_diag$ci_limit_ratio,
            abs((out1l$bound - out1l_diag$est_org) / (out1l_diag$ci_org_limit - out1l_diag$est_org)),
            ignore_attr = TRUE
          )
      }
    if (status_checku == 0) {
        expect_equal(
            out1u_diag$ci_limit_ratio,
            abs((out1u$bound - out1u_diag$est_org) / (out1u_diag$ci_org_limit - out1u_diag$est_org)),
            ignore_attr = TRUE
          )
      }
  })

test_that("Check diagnostic object: fit_post_check", {
    expect_true(
        isTRUE(out1l_diag$fit_post_check) | isFALSE(out1l_diag$fit_post_check)
      )
  })


