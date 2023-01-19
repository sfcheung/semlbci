skip_on_cran()

# Updated for lavaan 0.6-13

library(testthat)
library(semlbci)

# NOTE
# Difficult to solve for ci_bound_wn_i(16, 13) ubound.

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

# Find the scaling factors

sf1 <- scaling_factor3(fit, 16)
sf2 <- scaling_factor3(fit, 5)

sf1_ans <- structure(list(c_p = 0.898863303310259, c_pb = -8.88178419700125e-16,
    c_r = 0.898863303310259, c_rb = -8.88178419700125e-16), class = "data.frame", row.names = c(NA,
-1L))

sf2_ans <- structure(list(c_p = 0.941637663849086, c_pb = -2.22044604925031e-16,
    c_r = 0.941637663849086, c_rb = -2.22044604925031e-16), class = "data.frame", row.names = c(NA,
-1L))

test_that("Check scaling factor", {
    expect_equal(sf1$c_r, sf1_ans$c_r)
    expect_equal(sf2$c_r, sf2_ans$c_r)
  })

skip("Run only if data changed")

# Find chisq diff

modc0 <-
"
f1 =~ x1 + a*x2 + c*x3
f2 =~ x4 + b*x5 + d*x6
f1 ~ f2
ab := a * b
0 == (c - d)^2
"

i <- 16
est_i <- parameterTable(fit)[i, "est"]
modc <- paste(modc0, "\n", parameterTable(fit)[i, "label"], " == ", est_i * .98)
fitc <- lavaan::sem(modc, cfa_two_factors_mg, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- coef(fit)
fitc <- update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE
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
lrt_out_a <- lavTestLRT(fit, fitc, method = "satorra.2000", A.method = "exact")
(sf1_ans <- get_scaling_factor(lrt_out_a))
writeClipboard(capture.output(dput(sf1_ans)))


i <- 5
est_i <- parameterTable(fit)[i, "est"]
modc <- paste(modc0, "\n", parameterTable(fit)[i, "label"], " == ", est_i * .98)
fitc <- lavaan::sem(modc, cfa_two_factors_mg, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- coef(fit)
fitc <- update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE
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
lrt_out_a <- lavTestLRT(fit, fitc, method = "satorra.2000", A.method = "exact")
(sf2_ans <- get_scaling_factor(lrt_out_a))
writeClipboard(capture.output(dput(sf2_ans)))

