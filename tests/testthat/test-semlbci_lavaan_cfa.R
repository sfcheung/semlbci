skip_on_cran()
skip("WIP")
skip_if_not(Sys.getenv("SEMLBCI_TEST_COMPREHENSIVE") == "TRUE")
library(testthat)
library(semlbci)

# lavaan example: cfa()

library(lavaan)
## The famous Holzinger and Swineford (1939) example
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data = HolzingerSwineford1939)
#summary(fit, fit.measures = TRUE)

p_table <- parameterTable(fit)
i_free <- which(p_table$free > 0)
length(i_free)
fit_lbci <- semlbci(fit, pars = i_free[c(2, 3, 8, 9)])
fit_lbci <- semlbci(fit, pars = i_free[17:21])

# Warnings for this:
# In mapply(.pb_env$FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY,  :
#  longer argument not a multiple of length of shorter
# fit_lbci <- semlbci(fit, pars = i_free, parallel = TRUE, ncpus = 4)
# fit_lbci <- semlbci(fit, pars = i_free[1:21], parallel = TRUE, ncpus = 8)
# fit_lbci <- semlbci(fit, pars = i_free[1:21], parallel = TRUE, ncpus = 4)
# fit_lbci <- semlbci(fit, pars = i_free[21:1], parallel = TRUE, ncpus = 4)

# No warning?
# fit_lbci <- semlbci(fit, pars = i_free[1:21], parallel = TRUE, ncpus = 7)
# fit_lbci <- semlbci(fit, pars = i_free[1:21], parallel = TRUE, ncpus = 7)

# No warnings:
# fit_lbci <- semlbci(fit, pars = i_free[1:20], parallel = TRUE, ncpus = 4)
# fit_lbci <- semlbci(fit, pars = i_free[2:21], parallel = TRUE, ncpus = 4)
# fit_lbci <- semlbci(fit, pars = i_free[18:21], parallel = TRUE, ncpus = 4)
# fit_lbci <- semlbci(fit, pars = i_free[1:3], parallel = TRUE, ncpus = 2)
# fit_lbci <- semlbci(fit, pars = i_free[1:2], parallel = TRUE, ncpus = 2)
# fit_lbci <- semlbci(fit, pars = i_free[1:2], parallel = TRUE, ncpus = 2)
# fit_lbci <- semlbci(fit, pars = i_free[20:21], parallel = TRUE, ncpus = 2)


# Error for this:
# Error in if (is.na(f0$objective)) { : argument is of length zero
# fit_lbci <- semlbci(fit)
