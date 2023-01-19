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

# summary(fit)

# Find the scaling factor

sc1 <- scaling_factor3(fit, i = 2, standardized = TRUE, std_method = "lavaan")

# Pre-computed answer

sc1_ans <- structure(list(c_p = 0.913240582855474, c_pb = -4.44089209850063e-16,
    c_r = 0.913240582855474, c_rb = -4.44089209850063e-16), class = "data.frame", row.names = c(NA,
-1L))

test_that("Check scaling factor", {
    expect_equal(sc1$c_r, sc1_ans$c_r)
  })

skip("Run only if data changed")

# Generate expected results

# Run once and than comment out the code

# Find chisq diff

modc0 <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f1 ~ f2
tstd := geteststd()
"

geteststd <- semlbci:::get_std_genfct(fit = fit, i = 2)

i <- 2
est_i <- standardizedSolution(fit)[i, "est.std"]
modc <- paste(modc0, "\ntstd == ", est_i * .98, "\n0 < 1")
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


geteststd <- semlbci:::get_std_genfct(fit = fit, i = 6)

i <- 6
est_i <- standardizedSolution(fit)[i, "est.std"]
modc <- paste(modc0, "\ntstd == ", est_i * .98, "\n0 < 1")
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
