skip_on_cran()
skip("WIP")
skip_if_not(Sys.getenv("SEMLBCI_TEST_COMPREHENSIVE") == "TRUE")
library(testthat)
library(semlbci)

# https://lavaan.ugent.be/tutorial/mediation.html

library(lavaan)
set.seed(1234)
X <- rnorm(100)
M <- 0.5*X + rnorm(100)
Y <- 0.7*M + rnorm(100)
Data <- data.frame(X = X, Y = Y, M = M)
model <- ' # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
fit <- sem(model, data = Data)
# summary(fit)

p_table <- parameterTable(fit)
i_free <- which(p_table$free > 0)
i_user <- which(p_table$op == ":=")
i_par <- unique(c(i_free, i_user))



# OK
# fit_lbci <- semlbci(fit)
# fit_lbci <- semlbci(fit, pars = i_par[c(3, 1, 2, 7, 6)])
# fit_lbci <- semlbci(fit, pars = i_par[c(7, 3)])
