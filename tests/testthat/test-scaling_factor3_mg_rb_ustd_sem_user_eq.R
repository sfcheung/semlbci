library(testthat)
library(semlbci)


# Fit the model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <- 
"
f1 =~ x1 + c(a1, a2)*x2 + c(c1, c2)*x3
f2 =~ x4 + c(b1, b2)*x5 + c(d1, d2)*x6
f1 ~ c(fr1, fr2)*f2
ab := a1 * b2
c1 == c2
"
fit <- lavaan::sem(mod, cfa_two_factors_mg,
                   test = "satorra.bentler",
                   group = "gp")

# Find the scaling factors

sf1 <- scaling_factor3(fit, 47)
sf2 <- scaling_factor3(fit, 26)

sf1_ans <- structure(list(chisq_2 = 579.525628380961, chisq_1 = 231.810251352385, 
    chisq_0 = 23.1810251352385, chisq_diff_c_1 = 208.629226217146,
    chisq_diff_c_2 = 556.344603245723, chisq_diff_r_1 = 206.789807666807,
    chisq_diff_r_2 = 551.32073532825, chisq_diff_p_1 = 206.789807666807,
    chisq_diff_p_2 = 551.32073532825, c_p = 1.00924285488316,
    c_pb = 0.0712510699413826, c_r = 1.00924285488316, c_rb = 0.0712510699413542), class = "data.frame", row.names = c(NA,
    -1L))
sf2_and <- structure(list(chisq_2 = 579.525628380961, chisq_1 = 231.810251352385, 
    chisq_0 = 23.1810251352385, chisq_diff_c_1 = 208.629226217146,
    chisq_diff_c_2 = 556.344603245723, chisq_diff_r_1 = 177.189635851032,
    chisq_diff_r_2 = 472.336697857089, chisq_diff_p_1 = 177.189635851032,
    chisq_diff_p_2 = 472.336697857089, c_p = 1.17810888804118, 
    c_pb = 0.101398647398611, c_r = 1.17810888804118, c_rb = 0.101398647398554), class = "data.frame", row.names = c(NA,
    -1L))

test_that("Check scaling factor (MV)", {
    expect_equal(sf1$c_r, sf1_ans$c_r, tolerance = 1e-6)
    expect_equal(sf1$c_rb, sf1_ans$c_rb, tolerance = 1e-6)
  })
