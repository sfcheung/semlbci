library(testthat)
library(semlbci)

library(lavaan)

# Two groups

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <-
"
f1 =~ x1 + c(a1, a2)*x2 + c(b1, b2)*x3
f2 =~ x4 + c(c1, c1)*x5 + c(d1, d2)*x6
f1 ~ f2
ad := a1 * c1
b1 == b2
"
fit <- lavaan::sem(mod, cfa_two_factors_mg, group = "gp")

est_std <- parameterEstimates(fit,
                              standardized = TRUE,
                              se = FALSE,
                              zstat = FALSE,
                              pvalue = FALSE,
                              ci = FALSE,
                              cov.std = FALSE,
                              remove.system.eq = FALSE,
                              remove.eq = FALSE,
                              remove.ineq = FALSE,
                              remove.def = FALSE,
                              remove.nonfree = FALSE)
est_std <- est_std[, c("lhs", "op", "rhs", "group", "label",
                       "std.all")]
std <- std_lav(coef(fit), fit)
est_std_test <- est_std
est_std_test$std.all <- std

est_std <- est_std[!(est_std$op %in% c("~1", "==")), ]
est_std_test <- est_std_test[!(est_std_test$op %in% c("~1", "==")), ]

test_that("Check against standardized solution", {
    expect_equal(est_std_test$std.all,
                 est_std$std.all)
  })

# One group

data(cfa_two_factors)
dat <- cfa_two_factors
mod <-
"
f1 =~ x1 + a*x2 + b*x3
f2 =~ x4 + d*x5 + e*x6
f1 ~ f*f2
ad := a * d
b == e
"
fit <- lavaan::sem(mod, cfa_two_factors)

est_std <- parameterEstimates(fit,
                              standardized = TRUE,
                              se = FALSE,
                              zstat = FALSE,
                              pvalue = FALSE,
                              ci = FALSE,
                              cov.std = FALSE,
                              remove.system.eq = FALSE,
                              remove.eq = FALSE,
                              remove.ineq = FALSE,
                              remove.def = FALSE,
                              remove.nonfree = FALSE)
est_std <- est_std[, c("lhs", "op", "rhs", "label",
                       "std.all")]
std <- std_lav(coef(fit), fit)
est_std_test <- est_std
est_std_test$std.all <- std

est_std <- est_std[!(est_std$op %in% c("~1", "==")), ]
est_std_test <- est_std_test[!(est_std_test$op %in% c("~1", "==")), ]

test_that("Check against standardized solution", {
    expect_equal(est_std_test$std.all,
                 est_std$std.all)
  })
