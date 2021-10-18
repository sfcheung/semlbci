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
fit <- lavaan::sem(mod, cfa_two_factors_mg, test = "satorra.bentler", group = "gp", meanstructure = TRUE)

#summary(fit)

# Find the scaling factor

#sc1b <- scaling_factor2(fit, i = 2)
sc1 <- scaling_factor3(fit, i = 2)

# Pre-computed answer

sc1_ans <- structure(list(chisq_2 = 24.4237002561198, chisq_1 = 23.1884425117417, 
    chisq_0 = 23.1807714515506, chisq_diff_c_1 = 0.00767106019106123,
    chisq_diff_c_2 = 1.24292880456922, chisq_diff_r_1 = 0.298420153666688,
    chisq_diff_r_2 = 1.18841305035751, chisq_diff_p_1 = 0.298420153666688,
    chisq_diff_p_2 = 1.18841305035751, c_p = 1.38794112736304,
    c_pb = 0.292893218813453, c_r = 1.38794112736304, c_rb = 0.292893218813453), class = "data.frame", row.names = c(NA, 
    -1L))

test_that("Check scaling factor (MV)", {
    expect_equal(sc1$c_r, sc1_ans$c_r)
    expect_equal(sc1$c_rb, sc1_ans$c_rb)
  })

