skip("Test parallel processing: Test in interactive sections")

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
# Model issue. Not a bug.
fit_lbci <- semlbci(fit, pars = i_free[c(2, 3, 8, 9)], parallel = TRUE, ncpus = 4)

fit_lbci

# opts0 <- list(ftol_abs = 1e-5,
#               ftol_rel = 1e-5,
#               xtol_abs = 1e-5,
#               xtol_rel = 1e-5
#               )
opts0 <- list(ftol_rel = 1e-5,
              )
fit_lbci <- semlbci(fit, pars = i_free[c(2)], options = opts0)
