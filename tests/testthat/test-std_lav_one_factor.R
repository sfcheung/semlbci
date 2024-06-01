skip_on_cran()

library(testthat)
library(semlbci)
library(lavaan)

dat_cov <- matrix(c(1, .30, .40,
                    .30, 1, .20,
                    .40, .20, 1), 3, 3)
colnames(dat_cov) <- rownames(dat_cov) <- c("x1", "x2", "x3")
dat_mean <- c(1, 2, 3)
names(dat_mean) <- colnames(dat_cov)

mod <-
"
f1 =~ x1 + x2 + x3
"
fit <- lavaan::sem(mod,
                   sample.cov = dat_cov,
                   sample.nobs = 50)

expect_no_error(std_lav(lavaan::coef(fit), fit))

mod <-
"
x1 ~ x2 + x3
"
fit <- lavaan::sem(mod,
                   sample.cov = dat_cov,
                   sample.nobs = 50)
expect_no_error(std_lav(lavaan::coef(fit), fit))

mod <-
"
x1 ~ x2 + x3
"
fit <- lavaan::sem(mod,
                   sample.cov = dat_cov,
                   sample.nobs = 50)
expect_no_error(std_lav(lavaan::coef(fit), fit))

mod <-
"
x1 ~ x2 + x3
"
fit <- lavaan::sem(mod,
                   sample.cov = dat_cov,
                   sample.mean = dat_mean,
                   sample.nobs = 50,
                   meanstructure = TRUE)
expect_no_error(std_lav(lavaan::coef(fit), fit))
