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
fit_lbci <- semlbci(fit, pars = i_free[c(2, 3, 8, 9)])
fit_lbci <- semlbci(fit, pars = i_free[17:21])

# Warnings for this:
# In mapply(.pb_env$FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY,  :
#  longer argument not a multiple of length of shorter
# fit_lbci <- semlbci(fit, pars = i_free, parallel = TRUE, ncpus = 4)

# Error for this:
# Error in if (is.na(f0$objective)) { : argument is of length zero
# fit_lbci <- semlbci(fit)
