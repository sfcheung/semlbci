skip_if(Sys.getenv("SEMLBCI_TEST_SLOW") == "",
        "Skip due to speed or other issues")
# To be tested in interactive sessions only due to scoping or speed issues

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

sc1 <- scaling_factor2(fit, i = 2, standardized = TRUE, std_method = "lavaan")

# Pre-computed answer

sc1_ans <- structure(list(chisq_1 = 2857.47722096584, chisq_0 = 12.4816797169425, 
    chisq_diff_c = 2844.9955412489, chisq_diff_r = 3115.2749830207,
    chisq_diff_p = Inf, c_p = 0, c_r = 0.913240582855474), class = "data.frame", row.names = c(NA,
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
# f1 =~ x1 + x2 + x3
# f2 =~ x4 + x5 + x6
# f1 ~ f2
# tstd := geteststd()
# "

# geteststd <- semlbci:::get_std_genfct(fit = fit, i = 2)

# i <- 2
# est_i <- standardizedSolution(fit)[i, "est.std"]
# modc <- paste(modc0, "\ntstd == ", est_i * .98, "\n0 < 1")
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

# i <- 2
# est_i <- standardizedSolution(fit)[i, "est.std"]
# modc <- paste(modc0, "\ntstd == ", est_i * 1.02, "\n0 < 1")
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
