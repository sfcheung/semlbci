skip("Skip due to speed or other issues")
# To be tested in interactive sessions only due to scoping or speed issues

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(simple_med_mg)
dat <- simple_med_mg
mod <- 
"
m ~ c(a1, a2)*x
y ~ c(b1, b2)*m
ab := a2*b2
a1 == a2
"
fit <- lavaan::sem(mod, simple_med_mg, fixed.x = FALSE, group = "gp")

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7
              # tol_constraints_eq = 1e-7
              )
time1l_lav <- system.time(out1l_lav <- ci_bound_wn_i(17, 16, sem_out = fit, f_constr = fn_constr0, which = "lbound", standardized = TRUE, ciperc = ciperc, opts = opts0, verbose = TRUE, std_method = "lavaan"))
time1l_int <- system.time(out1l_int <- ci_bound_wn_i(17, 16, sem_out = fit, f_constr = fn_constr0, which = "lbound", standardized = TRUE, ciperc = ciperc, opts = opts0, verbose = TRUE, std_method = "internal"))
time2u_lav <- system.time(out2u_lav <- ci_bound_wn_i(1, 16, sem_out = fit, f_constr = fn_constr0, which = "ubound", standardized = TRUE, ciperc = ciperc, opts = opts0, verbose = TRUE, std_method = "lavaan"))
time2u_int <- system.time(out2u_int <- ci_bound_wn_i(1, 16, sem_out = fit, f_constr = fn_constr0, which = "ubound", standardized = TRUE, ciperc = ciperc, opts = opts0, verbose = TRUE, std_method = "internal"))

timexx <- rbind(time1l_lav, time1l_int, time2u_lav, time2u_int)
timexx

# Check the results

test_that("Compare the two methods", {
    expect_equal(out1l_lav$bound, out1l_int$bound, tolerance = 1e-6)
    expect_equal(out2u_lav$bound, out2u_int$bound, tolerance = 1e-6)
  })

