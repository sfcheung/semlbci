skip("Skip due to speed or other issues")
# To be tested in interactive sessions only due to scoping or speed issues

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors)
dat <- cfa_two_factors
mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
"
fit <- lavaan::cfa(mod, cfa_two_factors, test = "satorra.bentler")

# Find the scaling factors

sf1 <- scaling_factor2(fit, i = 2, standardized = TRUE)
sf2 <- scaling_factor2(fit, i = 6, standardized = TRUE)

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
time1l_lav <- system.time(out1l_lav <- ci_bound_wn_i(2, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf1$c_r, sf2 = sf1$c_rb, std_method = "lavaan"))
time1l_int <- system.time(out1l_int <- ci_bound_wn_i(2, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf1$c_r, sf2 = sf1$c_rb, std_method = "internal"))
time2u_lav <- system.time(out2u_lav <- ci_bound_wn_i(6, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf2$c_r, sf2 = sf1$c_rb, std_method = "lavaan"))
time2u_int <- system.time(out2u_int <- ci_bound_wn_i(6, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf2$c_r, sf2 = sf1$c_rb, std_method = "internal"))

timexx <- rbind(time1l_lav, time1l_int, time2u_lav, time2u_int)
timexx

# Check the results

test_that("Compare the two methods", {
    expect_equal(out1l_lav$bound, out1l_int$bound, tolerance = 1e-6)
    expect_equal(out2u_lav$bound, out2u_int$bound, tolerance = 1e-6)
  })

