skip("Test parallel processing: Test in interactive sections")

library(testthat)
library(semlbci)

# lavaan example: sem()

library(lavaan)
model <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a*y2 + b*y3 + c*y4
     dem65 =~ y5 + a*y6 + b*y7 + c*y8

  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'

fit <- sem(model, data = PoliticalDemocracy, test = "satorra.bentler")
# summary(fit, fit.measures = TRUE)

p_table <- parameterTable(fit)
i_free <- which(p_table$free > 0)

fit_lbci_parallel <- semlbci(fit, pars = i_free[9:10], parallel = TRUE, ncpus = 3, method = "ur")
fit_lbci_no_parallel <- semlbci(fit, pars = i_free[9:10], parallel = FALSE, ncpus = 3, method = "ur")

test_that("Compare parallel and non-parallel results", {
  expect_true(all.equal(data.frame(fit_lbci_parallel[12:14, -c(18, 19)]),
                        data.frame(fit_lbci_no_parallel[12:14, -c(18, 19)]),
              check_attributes = FALSE))
})
