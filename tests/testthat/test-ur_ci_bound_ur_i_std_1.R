skip_on_cran()

# Ready

library(testthat)
library(lavaan)

# Standardized

HS.model <- ' visual  =~ x1 + x2 + d1*x3
              textual =~ x4 + x5 + d2*x6
              visual ~~ textual
              d12 := d1^2'
fit <- sem(HS.model, data = HolzingerSwineford1939)

## One group

test_that("ci_bound_ur: One group, standardized", {

# Take more than one minute to run
# Test one bound is enough

system.time(
ci_lb <- ci_bound_ur_i(i = 7,
                       sem_out = fit,
                       which = "ubound",
                       standardized = TRUE)
)
# system.time(
# ci_ub <- ci_bound_ur_i(i = 7,
#                        sem_out = fit,
#                        which = "ubound",
#                        standardized = TRUE)
# )

expect_equal(round(1 - ci_lb$diag$ciperc_final, 2),
             .05,
             tolerance = 1e-3)
# expect_equal(round(1 - ci_ub$diag$ciperc_final, 2),
#              .05,
#              tolerance = 1e-3)

})
