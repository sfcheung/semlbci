skip_if(Sys.getenv("SEMLBCI_TEST_SLOW") == "",
        "Skip due to speed or other issues")
# To be tested in interactive sessions only due to scoping or speed issues

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)
data(simple_med)
dat <- simple_med
mod <-
"
m ~ a*x
y ~ b*m
ab := a*b
0 == (a - b)^2
"
fit <- lavaan::sem(mod, simple_med, fixed.x = FALSE, test = "satorra.bentler")

# Find the scaling factors

sf1 <- scaling_factor2(fit, i = 6, standardized = TRUE)
sf2 <- scaling_factor2(fit, i = 1, standardized = TRUE)

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-10
              )
time1l_lav <- system.time(out1l_lav <- ci_bound_wn_i(6, 5, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf1$c_r, wald_ci_start = FALSE, std_method = "lavaan"))
time1l_int <- system.time(out1l_int <- ci_bound_wn_i(6, 5, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf1$c_r, wald_ci_start = FALSE, std_method = "internal"))


timexx <- rbind(time1l_lav, time1l_int)
timexx

# Check the results

test_that("Compare the two methods", {
    expect_equal(out1l_lav$bound, out1l_int$bound, tolerance = 1e-6)
  })

