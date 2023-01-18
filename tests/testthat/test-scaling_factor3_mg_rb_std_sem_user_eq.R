skip_on_cran()
# skip_if_not(Sys.getenv("SEMLBCI_TEST_COMPREHENSIVE") == "TRUE")

# Updated for lavaan 0.6-13

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <-
"
f1 =~ x1 + c(a1, a2)*x2 + c(c1, c2)*x3
f2 =~ x4 + c(b1, b2)*x5 + c(d1, d2)*x6
f1 ~ c(fr1, fr2)*f2
ab := a1 * b2
c1 == c2
"
fit <- lavaan::sem(mod, cfa_two_factors_mg,
                   test = "satorra.bentler",
                   group = "gp")

# Find the scaling factors

sf1 <- scaling_factor3(fit, 47, standardized = TRUE)
sf2 <- scaling_factor3(fit, 26, standardized = TRUE)

sf1_ans <- structure(list(c_p = 0.962196632741836, c_pb = 1.4432899320127e-15,
    c_r = 0.962196632741836, c_rb = 1.4432899320127e-15), class = "data.frame", row.names = c(NA,
-1L))


test_that("Check scaling factor (MV)", {
    expect_equal(sf1$c_r, sf1_ans$c_r, tolerance = 1e-6)
    expect_equal(sf1$c_rb, sf1_ans$c_rb, tolerance = 1e-6)
  })

# Generate expected results

# Run once and than comment out the code

# Find chisq diff

# get_scaling_factor <- function(lrt_out) {
#     data.frame(c_p = 1 / attr(lrt_out_a, "scale")[2],
#                c_pb = attr(lrt_out_a, "shift")[2],
#                c_r = 1 / attr(lrt_out_a, "scale")[2],
#                c_rb = attr(lrt_out_a, "shift")[2])
#   }

# modc0 <-
# "
# f1 =~ x1 + c(a1, a2)*x2 + c(c1, c2)*x3
# f2 =~ x4 + c(b1, b2)*x5 + c(d1, d2)*x6
# f1 ~ c(fr1, fr2)*f2
# ab := a1 * b2
# c1 == c2
# tstd := geteststd()
# "

# geteststd <- semlbci:::get_std_genfct(fit = fit, i = 47)

# i <- 47
# est_i <- standardizedSolution(fit)[i, "est.std"]
# modc <- paste(modc0, "\ntstd == ", est_i * .98, "\n0 < 1")
# fitc <- lavaan::sem(modc, cfa_two_factors_mg, do.fit = FALSE, test = "satorra.bentler", group = "gp")
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
