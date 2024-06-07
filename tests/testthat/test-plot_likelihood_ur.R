skip("Plots: Test in interactive sections")

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
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f1 ~~ cov12 * f2
f1 ~~ v11 * f1
f2 ~~ v22 * f2
corf1f2 := cov12 / sqrt(v11 * v22)
"
fit <- lavaan::cfa(mod, dat)
ptable <- parameterTable(fit)
ptable
parameterEstimates(fit)
standardizedSolution(fit)

loglike_point_ur(2, fit, par_i = "f1 ~~ f2")
loglike_point_ur(.514, fit, par_i = "f1 ~~ f2", standardized = TRUE)
loglike_point_ur(coef(fit)["cov12"], fit, par_i = "f1 ~~ f2")
loglike_point_ur(coef(fit, type = "user")["corf1f2"], fit, par_i = "f1 ~~ f2", standardized = TRUE)
loglike_point_ur(.515, fit, par_i = 16)

i <- "f1 ~~ f2"
out <- loglike_compare_ur(fit, par_i = i, n_points = 13)
out2 <- loglike_compare_ur(fit, par_i = i, n_points = 13, parallel = TRUE)
out
out2
plot(out, type = "default")
plot(out, type = "ggplot2")
p <- plot(out, type = "ggplot2", add_pvalues = TRUE)
p
plot(out2, type = "ggplot2")

out <- loglike_compare_ur(fit, par_i = i, n_points = 13, standardized = TRUE)
out2 <- loglike_compare_ur(fit, par_i = i, n_points = 13, standardized = TRUE, parallel = TRUE)
out
out2
plot(out, type = "default")
plot(out, type = "ggplot2")
p <- plot(out, type = "ggplot2", add_pvalues = TRUE)
p
plot(out2, type = "ggplot2")


i <- "f1 =~ x2"
out <- loglike_compare_ur(fit, par_i = i, n_points = 13)
out2 <- loglike_compare_ur(fit, par_i = i, n_points = 13, parallel = TRUE)
out
out2
plot(out, type = "default")
plot(out, type = "ggplot2")
p <- plot(out, type = "ggplot2", add_pvalues = TRUE, size_label = 10, size_point = 15)
p
plot(out2, type = "ggplot2")

out <- loglike_compare_ur(fit, par_i = i, standardized = TRUE, n_points = 13)
out2 <- loglike_compare_ur(fit, par_i = i, standardized = TRUE, n_points = 13, parallel = TRUE)
out
out2
plot(out, type = "default")
plot(out, type = "ggplot2")
p <- plot(out, type = "ggplot2", add_pvalues = TRUE, size_label = 10, size_point = 15)
p
plot(out2, type = "ggplot2")

# User Parameters

data(simple_med)
dat <- simple_med
mod <-
"
m ~ a*x
y ~ b*m
ab:= a*b
"
fit <- lavaan::sem(mod, dat, fixed.x = FALSE)
lavaan::parameterTable(fit)

i <- "ab :="
out <- loglike_compare_ur(fit, par_i = i, n_points = 13)
out2 <- loglike_compare_ur(fit, par_i = i, n_points = 13, parallel = TRUE)
out
out2
plot(out)
plot(out, type = "ggplot2")
plot(out, type = "ggplot2", add_pvalues = TRUE)
plot(out2, type = "ggplot2")

out <- loglike_compare_ur(fit, par_i = i, standardized = TRUE, n_points = 13)
out2 <- loglike_compare_ur(fit, par_i = i, standardized = TRUE, n_points = 13, parallel = TRUE)
out
out2
plot(out)
plot(out, type = "ggplot2")
plot(out, type = "ggplot2", add_pvalues = TRUE)
plot(out2, type = "ggplot2", add_pvalues = TRUE)

options(options_old)
