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

sf1 <- scaling_factor3(fit, 47, standardized = TRUE)
sf2 <- scaling_factor3(fit, 26, standardized = TRUE)

sf1_ans <- structure(list(chisq_2 = 579.525628380961, chisq_1 = 231.810251352385, 
    chisq_0 = 23.1810251352385, chisq_diff_c_1 = 208.629226217146,
    chisq_diff_c_2 = 556.344603245723, chisq_diff_r_1 = 208.6762634152,
    chisq_diff_r_2 = 556.407101467913, chisq_diff_p_1 = 208.6762634152,
    chisq_diff_p_2 = 556.407101467913, c_p = 0.999955537379938,
    c_pb = 0.0377605835724069, c_r = 0.999955537379938, c_rb = 0.0377605835725205), class = "data.frame", row.names = c(NA,
    -1L))
sf2_ans <- structure(list(chisq_2 = 579.525628380961, chisq_1 = 231.810251352385, 
    chisq_0 = 23.1810251352385, chisq_diff_c_1 = 208.629226217146,
    chisq_diff_c_2 = 556.344603245723, chisq_diff_r_1 = 175.090097048184,
    chisq_diff_r_2 = 466.575696846021, chisq_diff_p_1 = 175.090097048184, 
    chisq_diff_p_2 = 466.575696846021, c_p = 1.19290756479819,
    c_pb = 0.198737169481547, c_r = 1.19290756479819, c_rb = 0.198737169481547), class = "data.frame", row.names = c(NA,
    -1L))

test_that("Check scaling factor (MV)", {
    expect_equal(sf1$c_r, sf1_ans$c_r, tolerance = 1e-6)
    expect_equal(sf1$c_rb, sf1_ans$c_rb, tolerance = 1e-6)
  })
