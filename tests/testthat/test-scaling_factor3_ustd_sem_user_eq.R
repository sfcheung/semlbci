library(testthat)
library(semlbci)

# NOTE
# Difficult to solve for ci_bound_wn_i(16, 13) ubound.

# Fit the model

library(lavaan)

data(cfa_two_factors)
dat <- cfa_two_factors
mod <- 
"
f1 =~ x1 + a*x2 + c*x3
f2 =~ x4 + b*x5 + d*x6
f1 ~ f2
ab := a * b
0 == (c - d)^2
"
fit <- lavaan::sem(mod, cfa_two_factors, test = "satorra.bentler")

# Find the scaling factors

sf1 <- scaling_factor3(fit, 16)
sf2 <- scaling_factor3(fit, 5)

sf1_ans <- structure(list(chisq_1 = 21.5003381119452, chisq_0 = 17.6652846883121, 
    chisq_diff_c = 3.83505342363311, chisq_diff_r = 4.26655911917828,
    chisq_diff_p = 4.26655911917828, c_p = 0.898863303310261,
    c_r = 0.89886330331026), class = "data.frame", row.names = c(NA,
    -1L))
sf2_ans <- structure(list(chisq_1 = 21.7422253282056, chisq_0 = 17.6652846883121, 
    chisq_diff_c = 4.0769406398935, chisq_diff_r = 4.32962783500862,
    chisq_diff_p = 4.32962783500862, c_p = 0.941637663849088,
    c_r = 0.941637663849087), class = "data.frame", row.names = c(NA,
    -1L))

test_that("Check scaling factor", {
    expect_equal(sf1$c_r, sf1_ans$c_r)
    expect_equal(sf2$c_r, sf2_ans$c_r)
  })

