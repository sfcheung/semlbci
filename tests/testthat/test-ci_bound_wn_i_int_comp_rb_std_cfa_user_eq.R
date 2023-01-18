skip("Run only if there is a major change in std_method")
# To be tested in interactive sessions only due to scoping or speed issues

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors)
dat <- cfa_two_factors
mod <-
"
f1 =~ x1 + a*x2 + b*x3
f2 =~ x4 + c*x5 + d*x6
ad := a * d
0 == (a - d)^2
"
fit <- lavaan::cfa(mod, cfa_two_factors, test = "satorra.bentler")
ptable <- parameterTable(fit)

# Find the scaling factors
update_args <- list(
                    optim.dx.tol = .01,
                    warn = TRUE,
                    control = list(eval.max = 10,
                                  iterations = 4,
                                  control.outer = list(tol = 1e-02,
                                  itmax = 10)
                                  )
                              )

sf1 <- scaling_factor3(fit, i = 16, standardized = TRUE, update_args = update_args)
sf2 <- scaling_factor3(fit, i =  2, standardized = TRUE, update_args = update_args)
sf3 <- scaling_factor3(fit, i = 15, standardized = TRUE, update_args = update_args)

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7
              )
time1l_lav <- system.time(out1l_lav <- ci_bound_wn_i(16, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf1$c_r, sf2 = sf1$c_rb, wald_ci_start = FALSE, std_method = "lavaan"))
time1l_int <- system.time(out1l_int <- ci_bound_wn_i(16, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf1$c_r, sf2 = sf1$c_rb, wald_ci_start = FALSE, std_method = "internal"))
time2u_lav <- system.time(out2u_lav <- ci_bound_wn_i( 2, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf2$c_r, sf2 = sf1$c_rb, wald_ci_start = FALSE, std_method = "lavaan"))
time2u_int <- system.time(out2u_int <- ci_bound_wn_i( 2, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf2$c_r, sf2 = sf1$c_rb, wald_ci_start = FALSE, std_method = "internal"))
time3u_lav <- system.time(out3u_lav <- ci_bound_wn_i(15, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf3$c_r, sf2 = sf1$c_rb, wald_ci_start = FALSE, std_method = "lavaan"))
time3u_int <- system.time(out3u_int <- ci_bound_wn_i(15, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf3$c_r, sf2 = sf1$c_rb, wald_ci_start = FALSE, std_method = "internal"))

timexx <- rbind(time1l_lav, time1l_int,
                time2u_lav, time2u_int,
                time3u_lav, time3u_int)
timexx

# Check the results

test_that("Compare the two methods", {
    expect_equal(out1l_lav$bound, out1l_int$bound, tolerance = 1e-6)
    expect_equal(out2u_lav$bound, out2u_int$bound, tolerance = 1e-6)
    expect_equal(out3u_lav$bound, out3u_int$bound, tolerance = 1e-6)
  })

