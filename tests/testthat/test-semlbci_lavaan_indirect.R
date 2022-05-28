skip("Test parallel processing: Test in interactive sections")

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

fit_lbci_no_parallel <- semlbci(fit)
fit_lbci_parallel <- semlbci(fit, parallel = 4)

test_that("Compare parallel and non-parallel results", {
  expect_true(all.equal(data.frame(fit_lbci_no_parallel[, -c(18, 19)]),
          data.frame(fit_lbci_parallel[, -c(18, 19)]),
          check_attributes = FALSE))
})
