skip("WIP")

options_old <- options(width = 132)
library(testthat)
library(semlbci)
library(ggplot2)

# Fit the model

library(lavaan)

data(cfa_two_factors)
dat <- cfa_two_factors
mod <-
"
f1 =~ x1 + a*x2 + b*x3
f2 =~ x4 + c*x5 + d*x6
f1 ~~ cov12 * f2
f1 ~~ v11 * f1
f2 ~~ v22 * f2
#corf1f2 := cov12 / sqrt(v11 * v22)
"
fit <- lavaan::cfa(mod, cfa_two_factors)
ptable <- parameterTable(fit)
ptable
parameterEstimates(fit)
standardizedSolution(fit)

# fit_lbci <- semlbci(fit, c("f1 ~~ f2"))
# fit_lbci

# Factor Covariance

# i <- "f1 ~~ f2"
# # # # Plot the quadratic approximation of the loglikelihood
# # # a_loglik_w <- loglike_quad_range(fit, par_i = i)
# # # # Get the LBCI
# # # theta_int <- unlist(unname(confint(fit_lbci)[1, ]))
# # # # Plot the true loglikelihood over the LBCI
# # # a_loglik <- loglike_range(fit, par_i = i,
# # #                             interval = theta_int)
# # # # Get the ranges for the plot
# # # theta_range <- range(c(a_loglik_w$theta, a_loglik$theta))
# # # loglik_range <- range(c(a_loglik_w$loglike, a_loglik$loglik))
# # # # Plot quadratic approximation
# # # plot(a_loglik_w$theta, a_loglik_w$loglike, type = "l", col = "blue",
# # #      xlim = theta_range, ylim = loglik_range)
# # # # Plot true loglikelihood
# # # points(a_loglik$theta, a_loglik$loglike, type = "l", col = "red")

# # fit_logl <- lavaan::fitMeasures(fit, "logl")

# # a_loglik$pvalue_check <- pchisq(2 * (fit_logl - a_loglik$loglike),
# #                                 df = 1,
# #                                 lower.tail = FALSE)
# # print(a_loglik, digits = 3)

# loglike_point(a_loglik_w[1, "theta"], fit, par_i = i)$lrt[2, "Pr(>Chisq)"]
# loglike_point(a_loglik_w[nrow(a_loglik_w), "theta"], fit, par_i = i)$lrt[2, "Pr(>Chisq)"]
# loglike_point(a_loglik[1, "theta"], fit, par_i = i)$lrt[2, "Pr(>Chisq)"]
# loglike_point(a_loglik[nrow(a_loglik_w), "theta"], fit, par_i = i)$lrt[2, "Pr(>Chisq)"]

i <- "f1 ~~ f2"
out <- loglike_compare(fit, par_i = i, n_points = 41)
out
plot(out, type = "default")
plot(out, type = "ggplot2")
p <- plot(out, type = "ggplot2", add_pvalues = TRUE)
p

# User Parameters

data(simple_med)
dat <- simple_med
mod <-
"
m ~ a*x
y ~ b*m
ab:= a*b
"
fit <- lavaan::sem(mod, simple_med, fixed.x = FALSE)
lavaan::parameterTable(fit)

# fit_lbci <- semlbci(fit, c("ab := "))
# fit_lbci

# i <- "ab :="
# # Plot the quadratic approximation of the loglikelihood
# a_loglik_w <- loglike_quad_range(fit, par_i = i)
# # Get the LBCI
# theta_int <- unlist(unname(confint(fit_lbci)[1, ]))
# # Plot the true loglikelihood over the LBCI
# a_loglik <- loglike_range(fit, par_i = i,
#                             interval = theta_int)
# # Get the ranges for the plot
# theta_range <- range(c(a_loglik_w$theta, a_loglik$theta))
# loglik_range <- range(c(a_loglik_w$loglike, a_loglik$loglik))
# # Plot quadratic approximation
# plot(a_loglik_w$theta, a_loglik_w$loglike, type = "l", col = "blue",
#      xlim = theta_range, ylim = loglik_range)
# # Plot true loglikelihood
# points(a_loglik$theta, a_loglik$loglike, type = "l", col = "red")

# fit_logl <- lavaan::fitMeasures(fit, "logl")

# a_loglik$pvalue_check <- pchisq(2 * (fit_logl - a_loglik$loglike),
#                                 df = 1,
#                                 lower.tail = FALSE)
# print(a_loglik, digits = 3)

# loglike_point(a_loglik_w[1, "theta"], fit, par_i = i)$lrt[2, "Pr(>Chisq)"]
# loglike_point(a_loglik_w[nrow(a_loglik_w), "theta"], fit, par_i = i)$lrt[2, "Pr(>Chisq)"]
# loglike_point(a_loglik[1, "theta"], fit, par_i = i)$lrt[2, "Pr(>Chisq)"]
# loglike_point(a_loglik[nrow(a_loglik_w), "theta"], fit, par_i = i)$lrt[2, "Pr(>Chisq)"]

i <- "ab :="
out <- loglike_compare(fit, i, n_points = 20)
out
plot(out)
plot(out, type = "ggplot2")
plot(out, type = "ggplot2", add_pvalues = TRUE)

options(options_old)
