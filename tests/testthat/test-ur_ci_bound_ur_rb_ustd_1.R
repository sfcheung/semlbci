skip_on_cran()

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(simple_med)
dat <- simple_med
mod <-
"
m ~ a*x
y ~ b*m
ab := a*b
"
fit <- lavaan::sem(mod, simple_med, fixed.x = FALSE, test = "satorra.bentler")

## One group

test_that("ci_bound_ur: One group, unstandardized, satorra.2000", {

# Test one bound is enough

est_i <- gen_est_i(i = 2, sem_out = fit)
system.time(
ci_lb <- ci_bound_ur(sem_out = fit,
                     func = est_i,
                     which = "lbound",
                     progress = FALSE,
                     user_callr = FALSE)
)
# system.time(
# ci_ub <- ci_bound_ur(sem_out = fit,
#                      func = est_i,
#                      which = "ubound",
#                      progress = FALSE,
#                      user_callr = FALSE)
# )

expect_true(grepl("satorra.2000",
                  attr(ci_lb$lrt, "head"),
                  fixed = TRUE))
expect_equal(round(ci_lb$lrt[2, "Pr(>Chisq)"], 2),
             .05,
             tolerance = 1e-3)
# expect_equal(round(ci_ub$lrt[2, "Pr(>Chisq)"], 2),
#              .05,
#              tolerance = 1e-3)

})