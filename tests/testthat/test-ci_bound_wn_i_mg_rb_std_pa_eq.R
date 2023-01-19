skip_on_cran()

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(simple_med_mg)
dat <- simple_med_mg
mod <-
"
m ~ c(a, a)*x
y ~ c(b1, b2)*m
ab:= a*b1
"
fit <- lavaan::sem(mod, simple_med_mg, fixed.x = FALSE,
                   group = "gp",
                   test = "satorra.bentler")
lavaan::parameterTable(fit)

# Find the scaling factors

sf1 <- scaling_factor3(fit, 10, standardized = TRUE)

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(#ftol_abs = 1e-7,
              ftol_rel = 1e-5
              # xtol_abs = 1e-7,
              # xtol_rel = 1e-7,
              # print_level = 0
              )
time1l <- system.time(out1l <- ci_bound_wn_i(10,16, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE, ciperc = ciperc, sf = sf1$c_r, sf2 = sf1$c_rb, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))
time1u <- system.time(out1u <- ci_bound_wn_i(10,16, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = list(ftol_rel = 1e-8), verbose = TRUE, ciperc = ciperc, sf = sf1$c_r, sf2 = sf1$c_rb, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))

test_that("Check against precomputed answers", {
    expect_equal(out1l$bound, 0.304952, tolerance = 1e-5)
    expect_equal(out1u$bound, 0.5995398, tolerance = 1e-5)
  })

skip("Run only if data changed")

# Check the results

geteststd1 <- get_std_genfct(fit = fit, i = 10)

modc0 <-
"
m ~ c(a, a)*x
y ~ c(b1, b2)*m
ab:= a*b1
astd := geteststd1()
"

test_limit <- out1l
modc <- paste(modc0, "\nastd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, simple_med_mg, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler", group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE,
                   optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02))
                   )
fitc_out1l <- fitc

test_limit <- out1u
modc <- paste(modc0, "\nastd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, simple_med_mg, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler", group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE,
                   optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02))
                   )
fitc_out1u <- fitc

get_scaling_factor(lavTestLRT(fitc_out1l, fit, method = "satorra.2000", A.method = "exact"))
sf1

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-5))
    expect_true(test_p(fitc_out1u, fit, ciperc = ciperc, tol = 1e-5))
  })

