skip_on_cran()

# NOTE:
# The nonlinear constraints may lead to difficulties in the search.
# Need to use ftol_abs only and set wald_ci_start to FALSE.

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
0 == (a - b)^2
"
fit <- lavaan::sem(mod, simple_med, fixed.x = FALSE, test = "satorra.bentler")

# Find the scaling factors

sf1 <- scaling_factor3(fit, i = 1, standardized = TRUE)
sf2 <- scaling_factor3(fit, i = 2, standardized = TRUE)

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7
              )
time1l <- system.time(out1l <- ci_bound_wn_i(1, 5, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf1$c_r, sf2 = sf1$c_rb, wald_ci_start = FALSE, std_method = "internal"))
time2u <- system.time(out2u <- ci_bound_wn_i(2, 5, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf2$c_r, sf2 = sf1$c_rb, wald_ci_start = FALSE, std_method = "internal"))

test_that("Check against precomputed answers", {
    expect_equal(out1l$bound, 0.06561438, tolerance = 1e-5)
    expect_equal(out2u$bound, 0.5737812, tolerance = 1e-5)
  })

skip("Run only if data changed")

# Check the results

geteststd1 <- get_std_genfct(fit = fit, i = 1)

modc0 <-
"
m ~ a*x
y ~ b*m
0 == (a - b)^2
astd := geteststd1()
"

test_limit <- out1l
modc <- paste(modc0, "\nastd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, simple_med, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE,
                   optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02))
                   )
fitc_out1l <- fitc

geteststd2 <- get_std_genfct(fit = fit, i = 2)

modc0 <-
"
m ~ a*x
y ~ b*m
0 == (a - b)^2
bstd := geteststd2()
"

test_limit <- out2u
modc <- paste(modc0, "\nbstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, simple_med, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE,
                   optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02))
                   )
fitc_out2u <- fitc

(lr_out_1l <- lavTestLRT(fitc_out1l, fit, method = "satorra.2000", A.method = "exact"))
get_scaling_factor(lr_out_1l)
sf1
(lr_out_2u <- lavTestLRT(fitc_out2u, fit, method = "satorra.2000", A.method = "exact"))
get_scaling_factor(lr_out_2u)
sf2

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-5))
    expect_true(test_p(fitc_out2u, fit, ciperc = ciperc, tol = 1e-5))
  })

