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

system.time(
ci_lb <- ci_bound_ur_i(i = 2,
                       sem_out = fit,
                       which = "lbound",
                       verbose = TRUE)
)
# system.time(
# ci_ub <- ci_bound_ur_i(i = 9,
#                        sem_out = fit,
#                        which = "ubound",
#                        verbose = TRUE)
# )
expect_true(grepl("satorra.2000",
                  attr(ci_lb$diag$history$lrt, "head"),
                  fixed = TRUE))
expect_equal(round(1 - ci_lb$diag$ciperc_final, 2),
             .05,
             tolerance = 1e-3)
# expect_equal(round(1 - ci_ub$diag$ciperc_final, 2),
#              .05,
#              tolerance = 1e-3)

})