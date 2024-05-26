skip_on_cran()

# Ready

library(testthat)
library(lavaan)

HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
              visual ~~ a*textual
              visual ~~ b*speed
              textual ~~ d*speed
              ab := a*b'
fit_gp <- sem(HS.model, data = HolzingerSwineford1939,
              group = "school",
              group.equal = "loadings")

## Two groups

test_that("ci_bound_ur: Two groups, unstandardized", {

# Test one bound is enough

est_i <- gen_est_i(i = 46, sem_out = fit_gp)
system.time(
ci_lb <- ci_bound_ur_i(i = 46,
                       sem_out = fit_gp,
                       which = "ubound")
)
# system.time(
# ci_ub <- ci_bound_ur_i(i = 46,
#                        sem_out = fit_gp,
#                        which = "ubound")
# )

expect_equal(round(1 - ci_lb$diag$ciperc_final, 2),
             .05,
             tolerance = 1e-3)
# expect_equal(round(1 - ci_ub$diag$ciperc_final, 2),
#              .05,
#              tolerance = 1e-3)

})
