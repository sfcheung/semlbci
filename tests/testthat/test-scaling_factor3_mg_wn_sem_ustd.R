skip_on_cran()
# skip_if_not(Sys.getenv("SEMLBCI_TEST_COMPREHENSIVE") == "TRUE")

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
fit <- lavaan::sem(mod, cfa_two_factors_mg, test = "satorra.bentler", group = "gp", meanstructure = TRUE)

#summary(fit)

# Find the scaling factor

sc1 <- scaling_factor3(fit, i = 2)

# Pre-computed answer

sc1_ans <- structure(list(c_p = 0.981422583046108, c_pb = 7.7715611723761e-16,
    c_r = 0.981422583046108, c_rb = 7.7715611723761e-16), class = "data.frame", row.names = c(NA,
-1L))


test_that("Check scaling factor (MV)", {
    expect_equal(sc1$c_r, sc1_ans$c_r)
    expect_equal(sc1$c_rb, sc1_ans$c_rb)
  })

# Find chisq diff

# get_scaling_factor <- function(lrt_out) {
#     data.frame(c_p = 1 / attr(lrt_out, "scale")[2],
#                c_pb = attr(lrt_out, "shift")[2],
#                c_r = 1 / attr(lrt_out, "scale")[2],
#                c_rb = attr(lrt_out, "shift")[2])
#   }


# modc0 <-
# "
# f1 =~ 1*x1 + c(a1, a2)*x2 + x3
# f2 =~ x4 + x5 + x6
# f1 ~ f2
# "

# i <- 2
# est_i <- parameterTable(fit)[2, "est"]
# modc <- paste(modc0, "\na1 == ", est_i * .98)
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

