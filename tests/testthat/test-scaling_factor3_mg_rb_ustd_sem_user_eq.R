skip_on_cran()

# Updated for lavaan 0.6-13

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

sf1_ans <- structure(list(c_r = 0.937333221642136, c_rb = 0), class = "data.frame", row.names = c(NA,
-1L))

sf2_ans <- structure(list(c_r = 1.05865024030559, c_rb = 0), class = "data.frame", row.names = c(NA,
-1L))

test_that("Check scaling factor (MV)", {
    expect_equal(sf1$c_r, sf1_ans$c_r, tolerance = 1e-6)
    expect_equal(sf1$c_rb, sf1_ans$c_rb, tolerance = 1e-6)
    expect_equal(sf2$c_r, sf2_ans$c_r, tolerance = 1e-6)
    expect_equal(sf2$c_rb, sf2_ans$c_rb, tolerance = 1e-6)
  })

skip("Run only if data changed")

# Generate expected results

# Run once and than comment out the code

mod2 <-
"
f1 =~ x1 + c(a1, a2)*x2 + c(c1, c2)*x3
f2 =~ x4 + c(b1, b2)*x5 + c(d1, d2)*x6
f1 ~ c(fr1, fr2)*f2
ab := a1 * b2
c1 == c2
ab == 1
"
fit2 <- lavaan::sem(mod2, cfa_two_factors_mg,
                    test = "satorra.bentler",
                    group = "gp",
                    start = fit,
                    check.start = FALSE)
lrt47 <- lavaan::lavTestLRT(fit2, fit, method = "satorra.2000",
                            A.method = "exact")
sf1_ans <- data.frame(c_r = 1 / attr(lrt47, "scale")[2],
                      c_rb = attr(lrt47, "shift")[2])

mod3 <-
"
f1 =~ x1 + c(a1, a2)*x2 + c(1.1, 1.1)*x3
f2 =~ x4 + c(b1, b2)*x5 + c(d1, d2)*x6
f1 ~ c(fr1, fr2)*f2
ab := a1 * b2
"
fit3 <- lavaan::sem(mod3, cfa_two_factors_mg,
                    test = "satorra.bentler",
                    group = "gp",
                    start = fit,
                    check.start = FALSE)
lrt26 <- lavaan::lavTestLRT(fit3, fit, method = "satorra.2000",
                            A.method = "exact")

sf2_ans <- data.frame(c_r = 1 / attr(lrt26, "scale")[2],
                      c_rb = attr(lrt26, "shift")[2])
