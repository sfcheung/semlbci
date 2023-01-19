library(semlbci)
library(lavaan)
options(width = 132)

# Fit model

library(lavaan)

data(cfa_two_factors)
dat <- cfa_two_factors
mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f1 ~ f2
"
fit <- lavaan::sem(mod, cfa_two_factors, test = "satorra.bentler")

# summary(fit)

# Find the scaling factor

sc1 <- scaling_factor3(fit, i = 2, standardized = TRUE, std_method = "lavaan")

# Pre-computed answer

sc1_ans <- structure(list(chisq_1 = 2857.47722096584, chisq_0 = 12.4816797169425, 
    chisq_diff_c = 2844.9955412489, chisq_diff_r = 3115.2749830207,
    chisq_diff_p = Inf, c_p = 0, c_r = 0.913240582855474), class = "data.frame", row.names = c(NA,
    -1L))

test_that("Check scaling factor", {
    expect_equal(sc1$c_r, sc1_ans$c_r)
  })
