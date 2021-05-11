library(testthat)
library(semlbci)

# context("Check ci_i: Check diagnostic info")

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
system.time(out1 <- ci_i(1, npar = 5, sem_out = fit_med, method = "wn", f_constr = fn_constr0, opts = opts0, verbose = TRUE))
# system.time(out1_nm <- ci_i(1, 5, sem_out = fit_med, method = "nm", opts = opts0, verbose = TRUE))
# system.time(out1ls <- ci_bound_wn_i(2, 5, sem_out = fit_med, f_constr = fn_constr0, which = "lbound", opts = opts0, standardized = TRUE, verbose = TRUE))
# system.time(out1us <- ci_bound_wn_i(2, 5, sem_out = fit_med, f_constr = fn_constr0, which = "ubound", opts = opts0, standardized = TRUE, verbose = TRUE))

out1_lb_diag <- attr(out1, "lb_diag")
out1_ub_diag <- attr(out1, "ub_diag")
# out1_nm_lb_diag <- attr(out1_nm, "lb_diag")
# out1_nm_ub_diag <- attr(out1_nm, "ub_diag")

test_that("Check method", {
    expect_true(
        attr(out1, "method") == "wn"
      )
  })

# test_that("Check method", {
#     expect_true(
#         attr(out1_nm, "method") == "nm"
#       )
#   })

test_that("Check existence of diagnostic objects", {
    expect_true(
        !is.null(out1_lb_diag) &
        !is.null(out1_ub_diag)
        # !is.null(out1_nm_lb_diag) &
        # !is.null(out1_nm_ub_diag) 
      )
  })



