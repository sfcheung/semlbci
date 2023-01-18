skip_on_cran()

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

sf1 <- scaling_factor3(fit, i = 2, standardized = TRUE)
sf2 <- scaling_factor3(fit, i = 6, standardized = TRUE)
sf3 <- scaling_factor3(fit, i = 15, standardized = TRUE)

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
time1l <- system.time(out1l <- ci_bound_wn_i(2, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf1$c_r, sf2 = sf1$c_rb, std_method = "internal"))
# time1u <- system.time(out1u <- ci_bound_wn_i(2, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf1$c_r, sf2 = sf1$c_rb, std_method = "internal"))
# time2l <- system.time(out2l <- ci_bound_wn_i(6, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf2$c_r, sf2 = sf1$c_rb, std_method = "internal"))
# time2u <- system.time(out2u <- ci_bound_wn_i(6, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf2$c_r, sf2 = sf1$c_rb, std_method = "internal"))
# time3l <- system.time(out3l <- ci_bound_wn_i(15, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf3$c_r, sf2 = sf1$c_rb, std_method = "internal"))
time3u <- system.time(out3u <- ci_bound_wn_i(15, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, sf = sf3$c_r, sf2 = sf1$c_rb, std_method = "internal"))

# timexx <- rbind(time1l, time1u, time2l, time2u, time3l, time3u)
# timexx
# colSums(timexx)

test_that("Check against precomputed answers", {
    expect_equal(out1l$bound, 0.6789599, tolerance = 1e-5)
    expect_equal(out3u$bound, 0.6150597, tolerance = 1e-5)
  })


skip("Run only if data changed")

# Check the results

test_p <- function(fit0, fit1, ciperc, tol) {
    out <- lavTestLRT(fit0, fit1, method = "satorra.2000", A.method = "exact")
    abs(out[2, "Pr(>Chisq)"] - (1 - ciperc)) < tol
  }

get_scaling_factor <- function(lrt_out) {
    data.frame(c_p = 1 / attr(lrt_out, "scale")[2],
               c_pb = attr(lrt_out, "shift")[2],
               c_r = 1 / attr(lrt_out, "scale")[2],
               c_rb = attr(lrt_out, "shift")[2])
  }


# gen_test_data <- FALSE
# if (gen_test_data) {

geteststd1 <- get_std_genfct(fit = fit, i = 2)

modc0 <-
"
f1 =~ x1 + a*x2 + b*x3
f2 =~ x4 + c*x5 + d*x6
astd := geteststd1()
"


test_limit <- out1l
modc <- paste(modc0, "\nastd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out1l <- fitc

# test_limit <- out1u
# modc <- paste(modc0, "\nastd == ", test_limit$bound, "\n0 < 1")
# fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
# ptable <- parameterTable(fitc)
# ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
# fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
#                    verbose = FALSE, optim.force.converged = TRUE,
#                    control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
# fitc_out1u <- fitc


# geteststd2 <- get_std_genfct(fit = fit, i = 6)

# modc0 <-
# "
# f1 =~ x1 + a*x2 + b*x3
# f2 =~ x4 + c*x5 + d*x6
# dstd := geteststd2()
# "

# test_limit <- out2l
# modc <- paste(modc0, "\ndstd == ", test_limit$bound, "\n0 < 1")
# fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
# ptable <- parameterTable(fitc)
# ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
# fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
#                    verbose = FALSE, optim.force.converged = TRUE,
#                    control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
# fitc_out2l <- fitc

# test_limit <- out2u
# modc <- paste(modc0, "\ndstd == ", test_limit$bound, "\n0 < 1")
# fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
# ptable <- parameterTable(fitc)
# ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
# fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
#                    verbose = FALSE, optim.force.converged = TRUE,
#                    control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
# fitc_out2u <- fitc


geteststd3 <- get_std_genfct(fit = fit, i = 15)

modc0 <-
"
f1 =~ x1 + a*x2 + b*x3
f2 =~ x4 + c*x5 + d*x6
dstd := geteststd3()
"

# test_limit <- out3l
# modc <- paste(modc0, "\ndstd == ", test_limit$bound, "\n0 < 1")
# fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
# ptable <- parameterTable(fitc)
# ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
# fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
#                    verbose = FALSE, optim.force.converged = TRUE,
#                    control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
# fitc_out3l <- fitc

test_limit <- out3u
modc <- paste(modc0, "\ndstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out3u <- fitc

# save(fitc_out1l, fitc_out1u,
#      fitc_out2l, fitc_out2u,
#      fitc_out3l, fitc_out3u,
#      geteststd1,
#      geteststd2,
#      geteststd3,
#      file = "inst/testdata/test-ci_bound_wn_i_rb_std_cfa.RData",
#      compress = "xz",
#      compression_level = 9)
# }

# load(system.file("testdata", "test-ci_bound_wn_i_rb_std_cfa.RData",
#                   package = "semlbci"))

(lr_out_1l <- lavTestLRT(fitc_out1l, fit, method = "satorra.2000", A.method = "exact"))
get_scaling_factor(lr_out_1l)
sf1
(lr_out_3u <- lavTestLRT(fitc_out3u, fit, method = "satorra.2000", A.method = "exact"))
get_scaling_factor(lr_out_3u)
sf3

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-5))
    # expect_true(test_p(fitc_out1u, fit, ciperc = ciperc, tol = 1e-5))
    # expect_true(test_p(fitc_out2l, fit, ciperc = ciperc, tol = 1e-5))
    # expect_true(test_p(fitc_out2u, fit, ciperc = ciperc, tol = 1e-5))
    # expect_true(test_p(fitc_out3l, fit, ciperc = ciperc, tol = 1e-5))
    expect_true(test_p(fitc_out3u, fit, ciperc = ciperc, tol = 1e-5))
  })

