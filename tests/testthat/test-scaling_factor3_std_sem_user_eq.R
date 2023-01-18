skip_on_cran()
# skip_if_not(Sys.getenv("SEMLBCI_TEST_COMPREHENSIVE") == "TRUE")

# Updated for lavaan 0.6-13

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

# Find the scaling factors

sf1 <- scaling_factor3(fit, i = 16, standardized = TRUE)
sf2 <- scaling_factor3(fit, i =  6, standardized = TRUE)

sf1_ans <- structure(list(c_p = 0.97076381179568, c_pb = -1.11022302462516e-15,
    c_r = 0.97076381179568, c_rb = -1.11022302462516e-15), class = "data.frame", row.names = c(NA,
-1L))

sf2_ans <- structure(list(c_p = 0.928130035137944, c_pb = -2.22044604925031e-16,
    c_r = 0.928130035137944, c_rb = -2.22044604925031e-16), class = "data.frame", row.names = c(NA,
-1L))


test_that("Check scaling factor", {
    expect_equal(sf1$c_r, sf1_ans$c_r)
    expect_equal(sf2$c_r, sf2_ans$c_r)
  })


# Generate expected results

# Run once and than comment out the code

# Find chisq diff

# get_scaling_factor <- function(lrt_out) {
#     data.frame(c_p = 1 / attr(lrt_out, "scale")[2],
#                c_pb = attr(lrt_out, "shift")[2],
#                c_r = 1 / attr(lrt_out, "scale")[2],
#                c_rb = attr(lrt_out, "shift")[2])
#   }

# modc0 <-
# "
# f1 =~ x1 + a*x2 + c*x3
# f2 =~ x4 + b*x5 + d*x6
# f1 ~ f2
# ab := a * b
# 0 == (c - d)^2
# tstd := geteststd()
# "

# geteststd <- semlbci:::get_std_genfct(fit = fit, i = 16)

# i <- 16
# est_i <- standardizedSolution(fit)[i, "est.std"]
# modc <- paste(modc0, "\ntstd == ", est_i * .98, "\n0 < 1")
# fitc <- lavaan::sem(modc, cfa_two_factors_mg, do.fit = FALSE, test = "satorra.bentler")
# ptable <- parameterTable(fitc)
# ptable[ptable$free > 0, "est"] <- coef(fit)
# fitc <- update(fitc, start = ptable, do.fit = TRUE,
#                    baseline = FALSE, h1 = FALSE, se = "none",
#                    verbose = FALSE
#                   #  optim.force.converged = TRUE,
#                   #  optim.dx.tol = .01,
#                   #  warn = FALSE,
#                   #  control = list(
#                   #     eval.max = 2,
#                   #     iterations = 1,
#                   #     control.outer = list(tol = 1e-02,
#                   #                          itmax = 1)
#                   # )
#                 )
# lrt_out_a <- lavTestLRT(fit, fitc, method = "satorra.2000", A.method = "exact")
# (sf1_ans <- get_scaling_factor(lrt_out_a))
# writeClipboard(capture.output(dput(sf1_ans)))


# geteststd <- semlbci:::get_std_genfct(fit = fit, i = 6)

# i <- 6
# est_i <- standardizedSolution(fit)[i, "est.std"]
# modc <- paste(modc0, "\ntstd == ", est_i * .98, "\n0 < 1")
# fitc <- lavaan::sem(modc, cfa_two_factors_mg, do.fit = FALSE, test = "satorra.bentler")
# ptable <- parameterTable(fitc)
# ptable[ptable$free > 0, "est"] <- coef(fit)
# fitc <- update(fitc, start = ptable, do.fit = TRUE,
#                    baseline = FALSE, h1 = FALSE, se = "none",
#                    verbose = FALSE
#                   #  optim.force.converged = TRUE,
#                   #  optim.dx.tol = .01,
#                   #  warn = FALSE,
#                   #  control = list(
#                   #     eval.max = 2,
#                   #     iterations = 1,
#                   #     control.outer = list(tol = 1e-02,
#                   #                          itmax = 1)
#                   # )
#                 )
# lrt_out_a <- lavTestLRT(fit, fitc, method = "satorra.2000", A.method = "exact")
# (sf2_ans <- get_scaling_factor(lrt_out_a))
# writeClipboard(capture.output(dput(sf2_ans)))
