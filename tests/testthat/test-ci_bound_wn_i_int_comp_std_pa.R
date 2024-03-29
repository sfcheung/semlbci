skip("Run only if there is a major change in std_method")
# To be tested in interactive sessions only due to scoping or speed issues

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
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7
              # xtol_abs = 1e-3,
              # xtol_rel = 1e-3,
              # tol_constraints_eq = 1e-3
              )
time1l_lav <- system.time(out1l_lav <- ci_bound_wn_i(1, 5, sem_out = fit, f_constr = fn_constr0, which = "lbound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, std_method = "lavaan"))
time1l_int <- system.time(out1l_int <- ci_bound_wn_i(1, 5, sem_out = fit, f_constr = fn_constr0, which = "lbound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, std_method = "internal"))
time2u_lav <- system.time(out2u_lav <- ci_bound_wn_i(2, 5, sem_out = fit, f_constr = fn_constr0, which = "ubound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, std_method = "lavaan"))
time2u_int <- system.time(out2u_int <- ci_bound_wn_i(2, 5, sem_out = fit, f_constr = fn_constr0, which = "ubound", verbose = TRUE, ciperc = ciperc, standardized = TRUE, std_method = "internal"))

timexx <- rbind(time1l_lav, time1l_int, time2u_lav, time2u_int)
timexx

# Check the results

test_that("Compare the two methods", {
    expect_equal(out1l_lav$bound, out1l_int$bound, tolerance = 1e-6)
    expect_equal(out2u_lav$bound, out2u_int$bound, tolerance = 1e-6)
  })

