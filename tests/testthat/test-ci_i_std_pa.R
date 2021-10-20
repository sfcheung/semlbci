skip_on_cran()
skip("ci_i is no longer used")
library(testthat)
library(semlbci)

# Fit the model

library(lavaan)
data(simple_med)
dat <- simple_med
mod <- 
"
m ~ x
y ~ m
"
fit <- lavaan::sem(mod, simple_med, fixed.x = FALSE)

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

# opts0 <- list(print_level = 3)
# opts0 <- list()
# opts0 <- list(ftol_abs = 1e-7,
#               ftol_rel = 1e-7
#               # xtol_abs = 1e-3,
#               # xtol_rel = 1e-3,
#               # tol_constraints_eq = 1e-3
#               )
# time1l <- system.time(out1l <- ci_bound_wn_i(1, 5, sem_out = fit, f_constr = fn_constr0, which = "lbound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))
# time1u <- system.time(out1u <- ci_bound_wn_i(1, 5, sem_out = fit, f_constr = fn_constr0, which = "ubound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))
# time2l <- system.time(out2l <- ci_bound_wn_i(2, 5, sem_out = fit, f_constr = fn_constr0, which = "lbound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))
# time2u <- system.time(out2u <- ci_bound_wn_i(2, 5, sem_out = fit, f_constr = fn_constr0, which = "ubound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))

out1 <- ci_i(1, npar = 5, sem_out = fit, method = "wn",
             f_constr = fn_constr0,
             ciperc = ciperc,
             standardized = TRUE,
             wald_ci_start = FALSE)
out2 <- ci_i(2, npar = 5, sem_out = fit, method = "wn",
             f_constr = fn_constr0,
             ciperc = ciperc,
             standardized = TRUE,
             wald_ci_start = FALSE)

# Check with known results

test_that("Check with know results", {
    expect_equal(unname(out1$bounds), c(0.1256137, 0.3945989), tolerance = 1e-4)
    expect_equal(unname(out2$bounds), c(0.3365330, 0.5656700), tolerance = 1e-4)
  })

