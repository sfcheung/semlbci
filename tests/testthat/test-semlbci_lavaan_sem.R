skip_on_cran()
skip("WIP")
skip_if_not(Sys.getenv("SEMLBCI_TEST_COMPREHENSIVE") == "TRUE")
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

fit <- sem(model, data = PoliticalDemocracy)
# summary(fit, fit.measures = TRUE)

p_table <- parameterTable(fit)
i_free <- which(p_table$free > 0)
fit_lbci <- semlbci(fit, pars = i_free[c(2, 3, 8, 9)])
fit_lbci <- semlbci(fit, pars = i_free[c(2, 3, 8, 9)], parallel = TRUE, ncpus = 4)

# Error for this:
# Error in if (i_op == ":=") { : missing value where TRUE/FALSE needed
# fit_lbci <- semlbci(fit, pars = i_free[32:34])

