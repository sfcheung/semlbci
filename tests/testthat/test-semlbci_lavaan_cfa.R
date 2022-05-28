skip("Test parallel processing: Test in interactive sections")

opt_old <- options(width = 132)
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

fit_lbci <- semlbci(fit, pars = i_free, parallel = TRUE, ncpus = 4)

options(opt_old)
