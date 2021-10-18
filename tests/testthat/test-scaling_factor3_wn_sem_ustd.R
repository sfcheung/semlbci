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

sc1_ans <- structure(list(chisq_1 = 1269.53149643256, chisq_0 = 12.4816797169425, 
    chisq_diff_c = 1257.04981671561, chisq_diff_r = 1290.29519670651,
    chisq_diff_p = 1290.29519670651, c_p = 0.974234283692787,
    c_r = 0.974234283692787), class = "data.frame", row.names = c(NA,
    -1L))

test_that("Check scaling factor", {
    expect_equal(sc1$c_r, sc1_ans$c_r)
  })


# # Find chisq diff

# get_scaling_factor <- function(lrt_out) {
#     diff_from_p <- qchisq(lrt_out[2, "Pr(>Chisq)"], 1, lower.tail = FALSE)
#     chisq_1 <- lrt_out[2, "Chisq"]
#     chisq_0 <- lrt_out[1, "Chisq"]
#     chisq_diff_c <- chisq_1 - chisq_0
#     chisq_diff_p <- qchisq(lrt_out[2, "Pr(>Chisq)"], 1, lower.tail = FALSE)
#     chisq_diff_r <- lrt_out[2, "Chisq diff"]
#     out <-
#       data.frame(chisq_1 = chisq_1,
#         chisq_0 = chisq_0,
#         chisq_diff_c = chisq_diff_c,
#         chisq_diff_r = chisq_diff_r,
#         chisq_diff_p = chisq_diff_p,
#         c_p = chisq_diff_c / chisq_diff_p,
#         c_r = chisq_diff_c / chisq_diff_r)
#     out
#   }

# modc0 <-
# "
# f1 =~ x1 + a*x2 + b*x3
# f2 =~ x4 + c*x5 + d*x6
# f1 ~ g*f2
# "

# i <- 2
# est_i <- parameterTable(fit)[2, "est"]
# modc <- paste(modc0, "\na == ", est_i * .98)
# fitc <- lavaan::sem(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
# ptable <- parameterTable(fitc)
# ptable[ptable$free > 0, "est"] <- coef(fit)
# fitc <- update(fitc, start = ptable, do.fit = TRUE,
#                    baseline = FALSE, h1 = FALSE, se = "none",
#                    verbose = FALSE,
#                    optim.force.converged = TRUE,
#                    optim.dx.tol = .01,
#                    warn = FALSE,
#                    control = list(
#                       eval.max = 2,
#                       iterations = 1,
#                       control.outer = list(tol = 1e-02,
#                                            itmax = 1)
#                   )
#                 )

# lrt_out <- lavTestLRT(fit, fitc, method = "satorra.2000", A.method = "exact")
# lrt_out
# sc1l <- get_scaling_factor(lrt_out)
# sc1l

# est_i <- parameterTable(fit)[2, "est"]
# modc <- paste(modc0, "\na == ", est_i * 1.02)
# fitc <- lavaan::sem(modc, cfa_two_factors, do.fit = FALSE, test = "satorra.bentler")
# ptable <- parameterTable(fitc)
# ptable[ptable$free > 0, "est"] <- coef(fit)
# fitc <- update(fitc, start = ptable, do.fit = TRUE,
#                    baseline = FALSE, h1 = FALSE, se = "none",
#                    verbose = FALSE,
#                    optim.force.converged = TRUE,
#                    optim.dx.tol = .01,
#                    warn = FALSE,
#                    control = list(
#                       eval.max = 2,
#                       iterations = 1,
#                       control.outer = list(tol = 1e-02,
#                                            itmax = 1)
#                   )
#                 )

# lrt_out <- lavTestLRT(fit, fitc, method = "satorra.2000", A.method = "exact")
# lrt_out
# sc1u <- get_scaling_factor(lrt_out)
# sc1u

# test_that("Check scaling factor", {
#     expect_equal(sc1$c_r, sc1l$c_r)
#     expect_equal(sc1$c_r, sc1u$c_r)
#   })
