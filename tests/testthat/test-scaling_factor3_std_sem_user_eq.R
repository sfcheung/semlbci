library(testthat)
library(semlbci)

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

sf1 <- scaling_factor3(fit, i = 16, standardized = TRUE)
sf2 <- scaling_factor3(fit, i =  6, standardized = TRUE)

sf1_ans <- structure(list(chisq_2 = 441.632117207802, chisq_1 = 176.652846883121, 
    chisq_0 = 17.6652846883121, chisq_diff_c_1 = 158.987562194809,
    chisq_diff_c_2 = 423.96683251949, chisq_diff_r_1 = 163.775740569397,
    chisq_diff_r_2 = 436.73530818506, chisq_diff_p_1 = 163.775740569397, 
    chisq_diff_p_2 = 436.73530818506, c_p = 0.970763811795681,
    c_pb = 5.6843418860808e-14, c_r = 0.97076381179568, c_rb = 0), class = "data.frame", row.names = c(NA, 
    -1L))
sf2_ans <- structure(list(chisq_2 = 441.632117207802, chisq_1 = 176.652846883121, 
    chisq_0 = 17.6652846883121, chisq_diff_c_1 = 158.987562194809,
    chisq_diff_c_2 = 423.96683251949, chisq_diff_r_1 = 171.298801003869,
    chisq_diff_r_2 = 456.796802676984, chisq_diff_p_1 = 171.298801003869,
    chisq_diff_p_2 = 456.796802676984, c_p = 0.928130035137944, 
    c_pb = 5.6843418860808e-14, c_r = 0.928130035137944, c_rb = -2.8421709430404e-14), class = "data.frame", row.names = c(NA,
    -1L))

test_that("Check scaling factor", {
    expect_equal(sf1$c_r, sf1_ans$c_r)
    expect_equal(sf2$c_r, sf2_ans$c_r)
  })

