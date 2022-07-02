library(testthat)
library(semlbci)


library(lavaan)
dat <- cfa_two_factors

mod <-
"
f1 =~ x1 + x2 + a*x3
f2 =~ x4 + a*x5 + equal('f1=~x2')*x6
f1 ~~ 0*f2
asq := a^2
"

fit <- lavaan::sem(mod, dat, meanstructure = TRUE)
ptable <- lavaan::parameterTable(fit)

id_free <- ptable[ptable$free > 0, "id"]
p_int <- ptable$op == "~1"

HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit2 <- cfa(HS.model,
           data = HolzingerSwineford1939,
           group = "school")
ptable2 <- lavaan::parameterTable(fit2)
id_free2 <- ptable2[ptable2$free > 0, "id"]
p_int2 <- ptable2$op == "~1"
id_int2 <- seq_len(nrow(ptable2))[p_int2]
id_test2 <- id_free2[!(id_free2 %in% id_int2)]

test_that("Correctly removed variances and error variances", {
    expect_equal(remove_intercepts(id_free, fit), c(2:3, 5:6, 8:15))
    expect_equal(remove_intercepts(id_free2, fit2), id_test2)
  })
