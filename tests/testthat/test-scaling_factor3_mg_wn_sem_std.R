skip_on_cran()

# Updated for lavaan 0.6-13

library(semlbci)
library(lavaan)
options(width = 132)

# Fit model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <-
"
f1 =~ 1*x1 + c(a1, a2)*x2 + x3
f2 =~ x4 + x5 + x6
f1 ~ f2
"
fit <- lavaan::cfa(mod, cfa_two_factors_mg, test = "satorra.bentler", group = "gp", meanstructure = TRUE)

# summary(fit)

# Find the scaling factor

sc1 <- scaling_factor3(fit, i = 2, standardized = TRUE)

# Pre-computed answer:

sc1_ans <- structure(list(c_p = 0.951803780870742, c_pb = -6.66133814775094e-16,
    c_r = 0.951803780870742, c_rb = -6.66133814775094e-16), class = "data.frame", row.names = c(NA,
-1L))

test_that("Check scaling factor (MV)", {
    expect_equal(sc1$c_r, sc1_ans$c_r)
    expect_equal(sc1$c_rb, sc1_ans$c_rb)
  })

skip("Run only if data changed")

# Find chisq diff

modc0 <-
"
f1 =~ 1*x1 + c(a1, a2)*x2 + x3
f2 =~ x4 + x5 + x6
f1 ~ f2
tstd := geteststd()
"

geteststd <- semlbci:::get_std_genfct(fit = fit, i = 2)

i <- 2
est_i <- standardizedSolution(fit)[i, "est.std"]
modc <- paste(modc0, "\ntstd == ", est_i * .98, "\n0 < 1")
fitc <- lavaan::cfa(modc, cfa_two_factors_mg, do.fit = FALSE, test = "satorra.bentler", group = "gp")
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
fitc_a <- fitc

lrt_out_a <- lavTestLRT(fit, fitc, method = "satorra.2000", A.method = "exact")
(sf1_ans <- get_scaling_factor(lrt_out_a))
writeClipboard(capture.output(dput(sf1_ans)))
