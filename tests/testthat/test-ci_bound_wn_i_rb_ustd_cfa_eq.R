skip_on_cran()
skip_if_not(Sys.getenv("SEMLBCI_TEST_COMPREHENSIVE") == "TRUE")
library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors)
dat <- cfa_two_factors
mod <- 
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + e*x6
0 == (b - d)^2
"
fit <- lavaan::cfa(mod, cfa_two_factors, test = "satorra.bentler")

# Find the scaling factors

sf1 <- scaling_factor3(fit, 3)
sf2 <- scaling_factor3(fit, 5)
sf3 <- scaling_factor3(fit, 6)
sf4 <- scaling_factor3(fit, 15)

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
time1l <- system.time(out1l <- ci_bound_wn_i( 3, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = list(ftol_abs = 1e-7), verbose = TRUE, ciperc = ciperc, sf = sf1$c_r, sf2 = sf1$c_rb))
time1u <- system.time(out1u <- ci_bound_wn_i( 3, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = list(ftol_abs = 1e-7), verbose = TRUE, ciperc = ciperc, sf = sf1$c_r, sf2 = sf1$c_rb))
time2l <- system.time(out2l <- ci_bound_wn_i( 5, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = list(ftol_abs = 1e-7), verbose = TRUE, ciperc = ciperc, sf = sf2$c_r, sf2 = sf1$c_rb))
time2u <- system.time(out2u <- ci_bound_wn_i( 5, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = list(ftol_abs = 1e-7), verbose = TRUE, ciperc = ciperc, sf = sf2$c_r, sf2 = sf1$c_rb, wald_ci_start = FALSE))
time3l <- system.time(out3l <- ci_bound_wn_i( 6, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = list(ftol_abs = 1e-7), verbose = TRUE, ciperc = ciperc, sf = sf3$c_r, sf2 = sf1$c_rb))
time3u <- system.time(out3u <- ci_bound_wn_i( 6, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = list(ftol_abs = 1e-7), verbose = TRUE, ciperc = ciperc, sf = sf3$c_r, sf2 = sf1$c_rb))
time4l <- system.time(out4l <- ci_bound_wn_i(15, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = list(ftol_abs = 1e-7), verbose = TRUE, ciperc = ciperc, sf = sf4$c_r, sf2 = sf1$c_rb))
time4u <- system.time(out4u <- ci_bound_wn_i(15, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = list(ftol_abs = 1e-7), verbose = TRUE, ciperc = ciperc, sf = sf4$c_r, sf2 = sf1$c_rb))

timexx <- rbind(time1l, time1u,
                time2l, time2u,
                time3l, time3u,
                time4l, time4u)
timexx
colSums(timexx)

# Check the results

test_p <- function(fit0, fit1, ciperc, tol) {
    out <- lavTestLRT(fit0, fit1, method = "satorra.2000", A.method = "exact")
    abs(out[2, "Pr(>Chisq)"] - (1 - ciperc)) < tol
  }

get_scaling_factor <- function(lrt_out) {
    diff_from_p <- qchisq(lrt_out[2, "Pr(>Chisq)"], 1, lower.tail = FALSE)
    chisq_1 <- lrt_out[2, "Chisq"]
    chisq_0 <- lrt_out[1, "Chisq"]
    chisq_diff_c <- chisq_1 - chisq_0
    chisq_diff_p <- qchisq(lrt_out[2, "Pr(>Chisq)"], 1, lower.tail = FALSE)
    chisq_diff_r <- lrt_out[2, "Chisq diff"]
    out <- 
      data.frame(chisq_1 = chisq_1,
        chisq_0 = chisq_0,
        chisq_diff_c = chisq_diff_c,
        chisq_diff_r = chisq_diff_r,
        chisq_diff_p = chisq_diff_p,
        c_p = chisq_diff_c / chisq_diff_p,
        c_r = chisq_diff_c / chisq_diff_r)
    out
  }

gen_test_data <- FALSE
if (gen_test_data) {

modc0 <- 
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + e*x6
f1 ~~ g*f2
0 == (b - d)^2
"

test_limit <- out1l
modc <- paste(modc0, "\nc == ", test_limit$bound)
fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- suppressWarnings(update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE
                  #  optim.force.converged = TRUE,
                  #  optim.dx.tol = .01,
                  #  warn = FALSE,
                  #  control = list(
                  #     eval.max = 2,
                  #     iterations = 1,
                  #     control.outer = list(tol = 1e-02,
                  #                          itmax = 1)
                  # )
                ))
fitc_out1l <- fitc
lavTestLRT(fitc, fit, method = "satorra.2000", A.method = "exact")

test_limit <- out1u
modc <- paste(modc0, "\nc == ", test_limit$bound)
fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- suppressWarnings(update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE
                  #  optim.force.converged = TRUE,
                  #  optim.dx.tol = .01,
                  #  warn = FALSE,
                  #  control = list(
                  #     eval.max = 2,
                  #     iterations = 1,
                  #     control.outer = list(tol = 1e-02,
                  #                          itmax = 1)
                  # )
                ))
fitc_out1u <- fitc
lavTestLRT(fitc, fit, method = "satorra.2000", A.method = "exact")


test_limit <- out2l
modc <- paste(modc0, "\nd == ", test_limit$bound)
fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- suppressWarnings(update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE
                  #  optim.force.converged = TRUE,
                  #  optim.dx.tol = .01,
                  #  warn = FALSE,
                  #  control = list(
                  #     eval.max = 2,
                  #     iterations = 1,
                  #     control.outer = list(tol = 1e-02,
                  #                          itmax = 1)
                  # )
                ))
fitc_out2l <- fitc
lavTestLRT(fitc, fit, method = "satorra.2000", A.method = "exact")

test_limit <- out2u
modc <- paste(modc0, "\nd == ", test_limit$bound)
fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- suppressWarnings(update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE
                  #  optim.force.converged = TRUE,
                  #  optim.dx.tol = .01,
                  #  warn = FALSE,
                  #  control = list(
                  #     eval.max = 2,
                  #     iterations = 1,
                  #     control.outer = list(tol = 1e-02,
                  #                          itmax = 1)
                  # )
                ))
fitc_out2u <- fitc
lavTestLRT(fitc, fit, method = "satorra.2000", A.method = "exact")



test_limit <- out3l
modc <- paste(modc0, "\ne == ", test_limit$bound)
fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- suppressWarnings(update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE
                  #  optim.force.converged = TRUE,
                  #  optim.dx.tol = .01,
                  #  warn = FALSE,
                  #  control = list(
                  #     eval.max = 2,
                  #     iterations = 1,
                  #     control.outer = list(tol = 1e-02,
                  #                          itmax = 1)
                  # )
                ))
fitc_out3l <- fitc
lavTestLRT(fitc, fit, method = "satorra.2000", A.method = "exact")


test_limit <- out3u
modc <- paste(modc0, "\ne == ", test_limit$bound)
fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- suppressWarnings(update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE
                  #  optim.force.converged = TRUE,
                  #  optim.dx.tol = .01,
                  #  warn = FALSE,
                  #  control = list(
                  #     eval.max = 2,
                  #     iterations = 1,
                  #     control.outer = list(tol = 1e-02,
                  #                          itmax = 1)
                  # )
                ))
fitc_out3u <- fitc
lavTestLRT(fitc, fit, method = "satorra.2000", A.method = "exact")


test_limit <- out4l
modc <- paste(modc0, "\ng == ", test_limit$bound)
fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- suppressWarnings(update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE
                  #  optim.force.converged = TRUE,
                  #  optim.dx.tol = .01,
                  #  warn = FALSE,
                  #  control = list(
                  #     eval.max = 2,
                  #     iterations = 1,
                  #     control.outer = list(tol = 1e-02,
                  #                          itmax = 1)
                  # )
                ))
fitc_out4l <- fitc
lavTestLRT(fitc, fit, method = "satorra.2000", A.method = "exact")

test_limit <- out4u
modc <- paste(modc0, "\ng == ", test_limit$bound)
fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- suppressWarnings(update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE
                  #  optim.force.converged = TRUE,
                  #  optim.dx.tol = .01,
                  #  warn = FALSE,
                  #  control = list(
                  #     eval.max = 2,
                  #     iterations = 1,
                  #     control.outer = list(tol = 1e-02,
                  #                          itmax = 1)
                  # )
                ))
fitc_out4u <- fitc
lavTestLRT(fitc, fit, method = "satorra.2000", A.method = "exact")

save(fitc_out1l, fitc_out1u,
     fitc_out2l, fitc_out2u,
     fitc_out3l, fitc_out3u,
     fitc_out4l, fitc_out4u,
     file = "inst/testdata/test-ci_bound_wn_i_rb_ustd_cfa_eq.RData",
     compress = "xz",
     compression_level = 9)
}

load(system.file("testdata", "test-ci_bound_wn_i_rb_ustd_cfa_eq.RData",
                  package = "semlbci"))



test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-3))
    expect_true(test_p(fitc_out1u, fit, ciperc = ciperc, tol = 1e-3))
    expect_true(test_p(fitc_out2l, fit, ciperc = ciperc, tol = 1e-3))
    expect_true(test_p(fitc_out2u, fit, ciperc = ciperc, tol = 1e-3))
    expect_true(test_p(fitc_out3l, fit, ciperc = ciperc, tol = 1e-3))
    expect_true(test_p(fitc_out3u, fit, ciperc = ciperc, tol = 1e-3))
    expect_true(test_p(fitc_out4l, fit, ciperc = ciperc, tol = 1e-3))
    expect_true(test_p(fitc_out4u, fit, ciperc = ciperc, tol = 1e-3))
  })

