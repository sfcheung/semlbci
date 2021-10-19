skip_on_cran()
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
cd := c*d
"
fit <- lavaan::cfa(mod, cfa_two_factors, test = "satorra.bentler")

# Find the scaling factors

sf1 <- scaling_factor3(fit, 16)

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
time1l <- system.time(out1l <- ci_bound_wn_i(16, 13, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE, ciperc = ciperc, sf = sf1$c_r, sf2 = sf1$c_rb))
time1u <- system.time(out1u <- ci_bound_wn_i(16, 13, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE, ciperc = ciperc, sf = sf1$c_r, sf2 = sf1$c_rb))

timexx <- rbind(time1l, time1u)
timexx

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


modc0 <- 
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + e*x6
cd := c*d
"

test_limit <- out1l
modc <- paste(modc0, "\ncd == ", test_limit$bound)
fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
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

test_limit <- out1u
modc <- paste(modc0, "\ncd == ", test_limit$bound)
fitc <- lavaan::cfa(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
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


test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out1u, fit, ciperc = ciperc, tol = 1e-4))
  })

