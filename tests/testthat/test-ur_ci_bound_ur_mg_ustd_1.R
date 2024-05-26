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
# system.time(
# ci_lb <- ci_bound_ur(sem_out = fit_gp,
#                       func = est_i,
#                       which = "lbound",
#                       progress = FALSE,
#                       use_callr = FALSE)
# )
system.time(
ci_ub <- ci_bound_ur(sem_out = fit_gp,
                      func = est_i,
                      which = "ubound",
                      progress = FALSE,
                      use_callr = TRUE)
)

# expect_equal(round(ci_lb$lrt[2, "Pr(>Chisq)"], 2),
#              .05,
#              tolerance = 1e-3)
expect_equal(round(ci_ub$lrt[2, "Pr(>Chisq)"], 2),
             .05,
             tolerance = 1e-3)

})
