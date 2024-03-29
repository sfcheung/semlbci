skip_on_cran()

# Updated for lavaan 0.6-13

library(semlbci)
library(lavaan)
options(width = 132)

# Fit model

library(lavaan)

data(cfa_two_factors)
dat <- cfa_two_factors
mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f1 ~ f2
"
fit <- lavaan::sem(mod, cfa_two_factors, test = "satorra.bentler")

summary(fit)

# Find the scaling factor

sc1 <- scaling_factor3(fit, i = 2)

# Pre-computed answer

sc1_ans <- structure(list(c_p = 0.974234283692787, c_pb = -6.66133814775094e-16,
    c_r = 0.974234283692787, c_rb = -6.66133814775094e-16), class = "data.frame", row.names = c(NA,
-1L))


test_that("Check scaling factor", {
    expect_equal(sc1$c_r, sc1_ans$c_r)
  })

skip("Run only if data changed")

# # Find chisq diff

modc0 <-
"
f1 =~ x1 + a * x2 + x3
f2 =~ x4 + x5 + x6
f1 ~ f2
"

i <- 2
est_i <- parameterTable(fit)[i, "est"]
modc <- paste(modc0, "\n", "a == ", est_i * .98)
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

