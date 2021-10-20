skip_on_cran()
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

out1l <- ci_i_one(1, npar = 5, which = "lbound", sem_out = fit, method = "wn",
             f_constr = fn_constr0,
             ciperc = ciperc,
             standardized = TRUE,
             wald_ci_start = FALSE,
             opts = list(ftol_rel = 1e-5))
out2u <- ci_i_one(2, npar = 5, which = "ubound", sem_out = fit, method = "wn",
             f_constr = fn_constr0,
             ciperc = ciperc,
             standardized = TRUE,
             wald_ci_start = FALSE,
             opts = list(ftol_rel = 1e-5))

# Check with known results

test_that("Check with know results", {
    expect_equal(unname(out1l$bounds["lbound"]), c(0.1256137), tolerance = 1e-4)
    expect_equal(unname(out2u$bounds["ubound"]), c(0.5656700), tolerance = 1e-4)
  })

