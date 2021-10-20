skip_on_cran()
library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <- 
"
f1 =~ x1 + c(b1, b2)*x2 + c(c1, c1)*x3
f2 =~ x4 + c(d1, d2)*x5 + c(e1, e1)*x6
f1 ~~ c(fr1, fr2)*f2
ce := c1*e1
b1 == c1
"
fit <- lavaan::cfa(mod, cfa_two_factors_mg, test = "satorra.bentler", group = "gp")


# Find the scaling factors

sf1 <- scaling_factor3(fit, 47)
#sf2 <- scaling_factor3(fit, 30)

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(#ftol_abs = 1e-7,
              ftol_rel = 1e-4
              # xtol_abs = 1e-7,
              # xtol_rel = 1e-7
              )
time1l <- system.time(out1l <- ci_bound_wn_i(47, 38, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc, sf = sf1$c_r, sf2 = sf1$c_rb))
time1u <- system.time(out1u <- ci_bound_wn_i(47, 38, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc, sf = sf1$c_r, sf2 = sf1$c_rb))

timexx <- rbind(time1l, time1u)
timexx
colSums(timexx)


# Check the results

test_p <- function(fit0, fit1, ciperc, tol) {
    out <- lavTestLRT(fit0, fit1, method = "satorra.2000", A.method = "exact")
    abs(out[2, "Pr(>Chisq)"] - (1 - ciperc)) < tol
  }

# gen_test_data <- FALSE
# if (gen_test_data) {

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


get_scaling_factor_ab <- function(lrt_out1, lrt_out2) {
    chisq_1 <- lrt_out1[2, "Chisq"]
    chisq_2 <- lrt_out2[2, "Chisq"]
    chisq_0 <- lrt_out1[1, "Chisq"]
    chisq_diff_c_1 <- chisq_1 - chisq_0
    chisq_diff_c_2 <- chisq_2 - chisq_0
    chisq_diff_p_1 <- qchisq(lrt_out1[2, "Pr(>Chisq)"], 1, lower.tail = FALSE)
    chisq_diff_p_2 <- qchisq(lrt_out2[2, "Pr(>Chisq)"], 1, lower.tail = FALSE)
    chisq_diff_r_1 <- lrt_out1[2, "Chisq diff"]
    chisq_diff_r_2 <- lrt_out2[2, "Chisq diff"]
    c_p  <- (chisq_2 - chisq_1) / (chisq_diff_p_2 - chisq_diff_p_1)
    c_pb <- chisq_diff_p_1 - (chisq_1 - chisq_0) / c_p
    c_r  <- (chisq_2 - chisq_1) / (chisq_diff_r_2 - chisq_diff_r_1)
    c_rb <- chisq_diff_r_1 - (chisq_1 - chisq_0) / c_r
    out <- 
      data.frame(
        chisq_2 = chisq_2,
        chisq_1 = chisq_1,
        chisq_0 = chisq_0,
        chisq_diff_c_1 = chisq_diff_c_1,
        chisq_diff_c_2 = chisq_diff_c_2,
        chisq_diff_r_1 = chisq_diff_r_1,
        chisq_diff_r_2 = chisq_diff_r_2,
        chisq_diff_p_1 = chisq_diff_p_1,
        chisq_diff_p_2 = chisq_diff_p_2,
        c_p = c_p,
        c_pb = c_pb,
        c_r = c_r,
        c_rb = c_rb)
    out
  }


modc0 <-
"
f1 =~ x1 + c(b1, b2)*x2 + c(c1, c1)*x3
f2 =~ x4 + c(d1, d2)*x5 + c(e1, e1)*x6
f1 ~~ c(fr1, fr2)*f2
ce := c1*e1
b1 == c1
"

test_limit <- out1l
modc <- paste(modc0, "\nce == ", test_limit$bound)
fitc <- lavaan::sem(modc, cfa_two_factors_mg, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler", group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE,
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
                )
fitc_out1l <- fitc


test_limit <- out1l
modc <- paste(modc0, "\nce == ", test_limit$bound * .8)
fitc <- lavaan::sem(modc, cfa_two_factors_mg, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler", group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE,
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
                )
fitc_out1lb <- fitc

(tmp_lrt <- lavTestLRT(fitc_out1l, fit, method = "satorra.2000", A.method = "exact"))
(tmp_lrtb <- lavTestLRT(fitc_out1lb, fit, method = "satorra.2000", A.method = "exact"))
get_scaling_factor_ab(tmp_lrt, tmp_lrtb)
sf1

test_limit <- out1u
modc <- paste(modc0, "\nce == ", test_limit$bound)
fitc <- lavaan::sem(modc, cfa_two_factors_mg, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler", group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE,
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
                )
fitc_out1u <- fitc

lavTestLRT(fitc_out1u, fit, method = "satorra.2000", A.method = "exact")

# save(fitc_out1l, fitc_out1u,
#      file = "inst/testdata/test-ci_bound_wn_i_mg_rb_ustd_cfa_user_eq.RData",
#      compress = "xz",
#      compression_level = 9)
# }

# load(system.file("testdata", "test-ci_bound_wn_i_mg_rb_ustd_cfa_user_eq.RData",
#                   package = "semlbci"))


test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out1u, fit, ciperc = ciperc, tol = 1e-4))
  })

