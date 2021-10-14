skip_if(Sys.getenv("SEMLBCI_TEST_SLOW") == "",
        "Skip due to speed or other issues")
# To be tested in interactive sessions only due to scoping or speed issues

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

sc1 <- scaling_factor2(fit, i = 2, standardized = TRUE, debug = TRUE)

# Pre-computed answer
sc1_ans <- structure(list(chisq_2 = 24.4342136814431, chisq_1 = 23.2151878626046, 
    chisq_0 = 23.1807714515506, chisq_diff_c_1 = 0.0344164110539502,
    chisq_diff_c_2 = 1.25344222989243, chisq_diff_r_1 = 0.318461595541362,
    chisq_diff_r_2 = 1.22409092825278, chisq_diff_p_1 = 0.318461595541362,
    chisq_diff_p_2 = 1.22409092825278, c_p = 1.34605381562539,
    c_pb = 0.292893218813452, c_r = 1.34605381562539, c_rb = 0.292893218813452),
    class = "data.frame", row.names = c(NA, -1L))

test_that("Check scaling factor (MV)", {
    expect_equal(sc1$c_r, sc1_ans$c_r)
    expect_equal(sc1$c_rb, sc1_ans$c_rb)
  })

# Find chisq diff

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


# get_scaling_factor_ab <- function(lrt_out1, lrt_out2) {
#     chisq_1 <- lrt_out1[2, "Chisq"]
#     chisq_2 <- lrt_out2[2, "Chisq"]
#     chisq_0 <- lrt_out1[1, "Chisq"]
#     chisq_diff_c_1 <- chisq_1 - chisq_0
#     chisq_diff_c_2 <- chisq_2 - chisq_0
#     chisq_diff_p_1 <- qchisq(lrt_out1[2, "Pr(>Chisq)"], 1, lower.tail = FALSE)
#     chisq_diff_p_2 <- qchisq(lrt_out2[2, "Pr(>Chisq)"], 1, lower.tail = FALSE)
#     chisq_diff_r_1 <- lrt_out1[2, "Chisq diff"]
#     chisq_diff_r_2 <- lrt_out2[2, "Chisq diff"]
#     c_p  <- (chisq_2 - chisq_1) / (chisq_diff_p_2 - chisq_diff_p_1)
#     c_pb <- chisq_diff_p_1 - (chisq_1 - chisq_0) / c_p
#     c_r  <- (chisq_2 - chisq_1) / (chisq_diff_r_2 - chisq_diff_r_1)
#     c_rb <- chisq_diff_r_1 - (chisq_1 - chisq_0) / c_r
#     out <- 
#       data.frame(
#         chisq_2 = chisq_2,
#         chisq_1 = chisq_1,
#         chisq_0 = chisq_0,
#         chisq_diff_c_1 = chisq_diff_c_1,
#         chisq_diff_c_2 = chisq_diff_c_2,
#         chisq_diff_r_1 = chisq_diff_r_1,
#         chisq_diff_r_2 = chisq_diff_r_2,
#         chisq_diff_p_1 = chisq_diff_p_1,
#         chisq_diff_p_2 = chisq_diff_p_2,
#         c_p = c_p,
#         c_pb = c_pb,
#         c_r = c_r,
#         c_rb = c_rb)
#     out
#   }


# modc0 <-
# "
# f1 =~ 1*x1 + c(a1, a2)*x2 + x3
# f2 =~ x4 + x5 + x6
# f1 ~ f2
# tstd := geteststd()
# "

# geteststd <- semlbci:::get_std_genfct(fit = fit, i = 2)

# i <- 2
# est_i <- standardizedSolution(fit)[i, "est.std"]
# modc <- paste(modc0, "\ntstd == ", est_i * .98, "\n0 < 1")
# fitc <- lavaan::cfa(modc, cfa_two_factors_mg, do.fit = FALSE, test = "satorra.bentler", group = "gp")
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
# fitc_a <- fitc

# i <- 2
# est_i <- standardizedSolution(fit)[i, "est.std"]
# modc <- paste(modc0, "\ntstd == ", est_i * .88, "\n0 < 1")
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
# fitc_b <- fitc

# i <- 2
# est_i <- standardizedSolution(fit)[i, "est.std"]
# modc <- paste(modc0, "\ntstd == ", est_i * .78, "\n0 < 1")
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
# fitc_c <- fitc

# lrt_out_a <- lavTestLRT(fit, fitc_a, method = "satorra.2000", A.method = "exact")
# lrt_out_a
# lrt_out_b <- lavTestLRT(fit, fitc_b, method = "satorra.2000", A.method = "exact")
# lrt_out_b
# lrt_out_c <- lavTestLRT(fit, fitc_c, method = "satorra.2000", A.method = "exact")
# lrt_out_c
# get_scaling_factor(lrt_out_a)
# get_scaling_factor(lrt_out_b)
# get_scaling_factor(lrt_out_c)
# sc1_b <- get_scaling_factor_ab(lrt_out_a, lrt_out_b)
# sc1_c <- get_scaling_factor_ab(lrt_out_a, lrt_out_c)
# get_scaling_factor_ab(lrt_out_b, lrt_out_c)

# test_that("Check scaling factor (MV)", {
#     expect_equal(sc1$c_r, sc1_b$c_r)
#     expect_equal(sc1$c_rb, sc1_b$c_rb)
#     expect_equal(sc1$c_r, sc1_c$c_r)
#     expect_equal(sc1$c_rb, sc1_c$c_rb)
#   })