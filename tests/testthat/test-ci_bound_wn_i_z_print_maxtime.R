skip_on_cran()

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

opts0 <- list()
opts0 <- list(ftol_rel = 1e-7,
              maxtime = .01
              )
time1l <- system.time(out1l <- ci_bound_wn_i(17, 16, sem_out = fit, f_constr = fn_constr0, which = "lbound", standardized = TRUE, ciperc = ciperc, opts = opts0, verbose = TRUE, wald_ci_start = FALSE, std_method = "internal"))

expect_output(print(out1l), "timeout", fixed = TRUE)
