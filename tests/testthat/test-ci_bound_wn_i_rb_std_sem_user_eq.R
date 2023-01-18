skip_on_cran()

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors)
dat <- cfa_two_factors
mod <-
"
f1 =~ x1 + a*x2 + c*x3
f2 =~ x4 + b*x5 + d*x6
f1 ~ f2
ab := a * b
0 == (c - d)^2
"
fit <- lavaan::sem(mod, cfa_two_factors, test = "satorra.bentler")
ptable <- parameterTable(fit)
ptable

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
sf2 <- scaling_factor3(fit, i =  6, standardized = TRUE, update_args = update_args)

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7
              )
time1l <- system.time(out1l <- ci_bound_wn_i(16, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf1$c_r, sf2 = sf1$c_rb, wald_ci_start = FALSE, std_method = "internal"))
# time1u <- system.time(out1u <- ci_bound_wn_i(16, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf1$c_r, sf2 = sf1$c_rb, wald_ci_start = FALSE, std_method = "internal"))
# time2l <- system.time(out2l <- ci_bound_wn_i( 6, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf2$c_r, sf2 = sf1$c_rb, wald_ci_start = FALSE, std_method = "internal"))
time2u <- system.time(out2u <- ci_bound_wn_i( 6, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf2$c_r, sf2 = sf1$c_rb, wald_ci_start = FALSE, std_method = "internal"))

# timexx <- rbind(time1l, time1u, time2l, time2u)
# timexx
# colSums(timexx)

test_that("Check against precomputed answers", {
    expect_equal(out1l$bound, 0.4571021, tolerance = 1e-5)
    expect_equal(out2u$bound, 0.8457521, tolerance = 1e-5)
  })

skip("Run only if data changed")


# Check the results

test_p <- function(fit0, fit1, ciperc, tol) {
    out <- lavTestLRT(fit0, fit1, method = "satorra.2000", A.method = "exact")
    abs(out[2, "Pr(>Chisq)"] - (1 - ciperc)) < tol
  }


# gen_test_data <- FALSE
# if (gen_test_data) {

geteststd1 <- get_std_genfct(fit = fit, i = 16)

modc0 <-
"
f1 =~ x1 + a*x2 + c*x3
f2 =~ x4 + b*x5 + d*x6
f1 ~ f2
ab := a * b
0 == (c - d)^2
tstd := geteststd1()
"


test_limit <- out1l
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out1l <- fitc
# sf1
# (tmp <- lavTestLRT(fitc, fit, method = "satorra.2000", A.method = "exact"))
# get_scaling_factor(tmp)

# test_limit <- out1u
# modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
# fitc <- lavaan::sem(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
# ptable <- parameterTable(fitc)
# ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
# fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
#                    verbose = FALSE, optim.force.converged = TRUE,
#                    control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
# fitc_out1u <- fitc
# sf1
# (tmp <- lavTestLRT(fitc, fit, method = "satorra.2000", A.method = "exact"))
# get_scaling_factor(tmp)


geteststd2 <- get_std_genfct(fit = fit, i = 6)

modc0 <-
"
f1 =~ x1 + a*x2 + c*x3
f2 =~ x4 + b*x5 + d*x6
f1 ~ f2
ab := a * b
0 == (c - d)^2
tstd := geteststd2()
"

# test_limit <- out2l
# modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
# fitc <- lavaan::sem(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
# ptable <- parameterTable(fitc)
# ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
# fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
#                    verbose = FALSE, optim.force.converged = TRUE,
#                    control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
# fitc_out2l <- fitc
# sf2
# (tmp <- lavTestLRT(fitc, fit, method = "satorra.2000", A.method = "exact"))
# get_scaling_factor(tmp)

test_limit <- out2u
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out2u <- fitc
# sf2
# (tmp <- lavTestLRT(fitc, fit, method = "satorra.2000", A.method = "exact"))
# get_scaling_factor(tmp)

# save(fitc_out1l, fitc_out1u,
#      fitc_out2l, fitc_out2u,
#      geteststd1,
#      geteststd2,
#      file = "inst/testdata/test-ci_bound_wn_i_rb_std_sem_user_eq.RData",
#      compress = "xz",
#      compression_level = 9)
# }

# load(system.file("testdata", "test-ci_bound_wn_i_rb_std_sem_user_eq.RData",
#                   package = "semlbci"))


get_scaling_factor <- function(lrt_out) {
    data.frame(c_p = 1 / attr(lrt_out, "scale")[2],
               c_pb = attr(lrt_out, "shift")[2],
               c_r = 1 / attr(lrt_out, "scale")[2],
               c_rb = attr(lrt_out, "shift")[2])
  }

(lr_out_1l <- lavTestLRT(fitc_out1l, fit, method = "satorra.2000", A.method = "exact"))
get_scaling_factor(lr_out_1l)
sf1
(lr_out_2u <- lavTestLRT(fitc_out2u, fit, method = "satorra.2000", A.method = "exact"))
get_scaling_factor(lr_out_2u)
sf2

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-5))
    # expect_true(test_p(fitc_out1u, fit, ciperc = ciperc, tol = 1e-5))
    # expect_true(test_p(fitc_out2l, fit, ciperc = ciperc, tol = 1e-5))
    expect_true(test_p(fitc_out2u, fit, ciperc = ciperc, tol = 1e-4))
  })

