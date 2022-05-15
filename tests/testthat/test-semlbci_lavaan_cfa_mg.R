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
# Model issue. Not a bug.
# fit_lbci <- semlbci(fit, pars = i_free[c(2, 3, 8, 9)])

# opts0 <- list(ftol_abs = 1e-5,
#               ftol_rel = 1e-5,
#               xtol_abs = 1e-5,
#               xtol_rel = 1e-5
#               )
# fit_lbci <- semlbci(fit, pars = i_free[c(2)], options = opts0, wald_ci_start = TRUE, perturbation_factor = .8, p_tol = 1e-3)
# print(fit_lbci, verbose_if_needed = FALSE)
# tmp <- attr(fit_lbci, "lb_out")
# tmp[[1]]

# HS.model2 <- ' visual  =~ x1 + x2 + c(a1, a2) * x3
#               textual =~ x4 + x5 + x6
#               speed   =~ x7 + x8 + x9 '

# fit2 <- cfa(HS.model2, 
#            data = HolzingerSwineford1939, 
#            group = "school")

# fit2b <- update(fit2, add = "a1 == -0.004")
# lavTestLRT(fit2, fit2b)

# OK
# fit_lbci <- semlbci(fit, pars = i_free[c(31, 35, 17, 50)], parallel = TRUE, ncpus = 4)

