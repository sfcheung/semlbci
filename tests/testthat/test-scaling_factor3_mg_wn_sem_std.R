library(semlbci)
library(lavaan)
options(width = 132)

# Fit model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <-
"
f1 =~ 1*x1 + c(a1, a2)*x2 + x3
f2 =~ x4 + x5 + x6
f1 ~ f2
"
fit <- lavaan::cfa(mod, cfa_two_factors_mg, test = "satorra.bentler", group = "gp", meanstructure = TRUE)

# summary(fit)

# Find the scaling factor

sc1 <- scaling_factor3(fit, i = 2, standardized = TRUE)

# Pre-computed answer
sc1_ans <- structure(list(chisq_2 = 24.4342136814431, chisq_1 = 23.2151878626046, 
    chisq_0 = 23.1807714515506, chisq_diff_c_1 = 0.0344164110539502,
    chisq_diff_c_2 = 1.25344222989243, chisq_diff_r_1 = 0.318461595541362,
    chisq_diff_r_2 = 1.22409092825278, chisq_diff_p_1 = 0.318461595541362,
    chisq_diff_p_2 = 1.22409092825278, c_p = 1.34605381562539,
    c_pb = 0.292893218813452, c_r = 1.34605381562539, c_rb = 0.292893218813452),
    class = "data.frame", row.names = c(NA, -1L))

test_that("Check scaling factor (MV)", {
    expect_equal(sc1$c_r, sc1_ans$c_r)
    expect_equal(sc1$c_rb, sc1_ans$c_rb)
  })
