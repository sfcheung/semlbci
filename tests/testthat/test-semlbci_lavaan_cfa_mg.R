skip_on_cran()
skip("WIP")
skip_if_not(Sys.getenv("SEMLBCI_TEST_COMPREHENSIVE") == "TRUE")
library(testthat)
library(semlbci)

# https://lavaan.ugent.be/tutorial/groups.html

library(lavaan)
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, 
           data = HolzingerSwineford1939, 
           group = "school")
# summary(fit)

p_table <- parameterTable(fit)
i_free <- which(p_table$free > 0)

# One bound fails
fit_lbci <- semlbci(fit, pars = i_free[c(2, 3, 8, 9)])


fit_lbci <- semlbci(fit, pars = i_free[c(31, 35, 17, 50)], parallel = TRUE, ncpus = 4)

