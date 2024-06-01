skip_on_cran()
skip("To be run in an interactive session")

# Ready

library(testthat)
library(lavaan)

HS.model_gp <- ' visual  =~ x1 + x2 + x3
                 textual =~ x4 + x5 + x6
                 visual ~~ c(a1, a2)*textual
                 adiff := a1^2'
fit_gp <- sem(HS.model_gp, data = HolzingerSwineford1939,
              group = "school",
              group.equal = "loadings")

## Two groups

test_that("ci_bound_ur: Two groups, standardized", {

# Take more than one minute to run
# Test one bound is enough

system.time(
ci_lb <- ci_bound_ur_i(i = 29,
                       sem_out = fit_gp,
                       which = "lbound",
                       standardized = TRUE)
)
# system.time(
# ci_ub <- ci_bound_ur_i(i = 29,
#                        sem_out = fit_gp,
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
