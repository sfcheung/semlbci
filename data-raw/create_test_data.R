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

# Data based on a simple mediation model

library(MASS)

set.seed(9654534)
n <- 500
gamma <- matrix(c(2,
                  1), 2, 1, byrow = TRUE)
beta  <- matrix(c(0, 0,
                  3, 0), 2, 2, byrow = TRUE)
theta <- matrix(2, 1, 1)
psi   <- diag(c(3, 4))
xi    <- mvrnorm(n, 0, theta)
eta   <- solve(diag(2) - beta) %*% (gamma %*% t(xi) + t(mvrnorm(n, c(0, 0), psi)))
eta   <- t(eta)
dat   <- cbind(xi, eta)
dat   <- as.data.frame(dat)
colnames(dat) <- c("x", "m", "y")
head(dat)

simple_med <- dat

usethis::use_data(simple_med, overwrite = TRUE)