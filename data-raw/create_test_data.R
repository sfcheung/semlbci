# Generate data

# Data based on a two factor model

library(MASS)

set.seed(954745)
n <- 500
lambda <- matrix(c(  1,   0,
                   1.2,   0,
                   0.8,   0,
                     0,   1,
                     0, 0.8,
                     0, 1.2), 6, 2, byrow = TRUE)
f_var <- c(2, 3)
f_cor <- .60
phi <- matrix(c(1, f_cor,
                f_cor, 1), 2, 2, byrow = TRUE)
phi <- diag(sqrt(f_var)) %*% phi %*% diag(sqrt(f_var))
psi <- diag(c(3, 2, 4, 4, 3, 2))
dat_f <- mvrnorm(n, c(0, 0), phi)
dat_e <- mvrnorm(n, rep(0, 6), psi)
dat   <- lambda %*% t(dat_f) + t(dat_e)
dat   <- t(dat)
dat   <- as.data.frame(dat)
colnames(dat) <- paste0("x", 1:6)
head(dat)

cfa_two_factors <- dat

usethis::use_data(cfa_two_factors, overwrite = TRUE)
