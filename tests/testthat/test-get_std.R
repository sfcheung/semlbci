skip("Skip due to speed or other issues")
# To be tested in interactive sessions only due to scoping or speed issues

library(testthat)
library(semlbci)
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
fit <- lavaan::sem(mod, cfa_two_factors)

# std_method: lavaan

fit00 <- lavaan::update(fit,
                        model = parameterTable(fit),
                        add = paste0("tmp", " := ",
                                    "semlbci::get_std('",
                                    "fit", "',", 7,
                                    ", std_method = ",
                                    sQuote("lavaan"),
                                    ")"),
                        do.fit = TRUE,
                        baseline = FALSE,
                        h1 = FALSE,
                        se = "none")

parameterEstimates(fit)
parameterEstimates(fit00)

test_that("Check user computed standardized solution", {
    expect_equal(parameterEstimates(fit00)[16, "est"],
                 standardizedSolution(fit, se = FALSE)[7, "est.std"],
                 tolerance = 1e-5)
  })


# std_method: internal

fit00 <- lavaan::update(fit,
                        model = parameterTable(fit),
                        add = paste0("tmp", " := ",
                                    "semlbci::get_std('",
                                    "fit", "',", 7,
                                    ", std_method = ",
                                    sQuote("internal"),
                                    ")"),
                        do.fit = TRUE,
                        baseline = FALSE,
                        h1 = FALSE,
                        se = "none")

parameterEstimates(fit)
parameterEstimates(fit00)

test_that("Check user computed standardized solution", {
    expect_equal(parameterEstimates(fit00)[16, "est"],
                 standardizedSolution(fit, se = FALSE)[7, "est.std"],
                 tolerance = 1e-5)
  })

