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
n <- 200
gamma <- matrix(c(2,
                  1), 2, 1, byrow = TRUE)
beta  <- matrix(c(0, 0,
                  .5, 0), 2, 2, byrow = TRUE)
theta <- matrix(1, 1, 1)
psi   <- diag(c(6, 6)^2)
xi    <- mvrnorm(n, 0, theta)
eta   <- solve(diag(2) - beta) %*% (gamma %*% t(xi) + t(mvrnorm(n, c(0, 0), psi)))
eta   <- t(eta)
dat   <- cbind(xi, eta)
dat   <- as.data.frame(dat)
colnames(dat) <- c("x", "m", "y")
head(dat)

simple_med <- dat

usethis::use_data(simple_med, overwrite = TRUE)

# # Data with a parameter estimate near its attainable boundary

# library(MASS)

# set.seed(3842093)
# n <- 50
# gamma <- matrix(c(.9995,
#                   .5), 2, 1, byrow = TRUE)
# beta  <- matrix(c(0, 0,
#                   .5, 0), 2, 2, byrow = TRUE)
# theta <- matrix(1, 1, 1)
# # psi   <- diag(c(6, 6)^2)
# xi    <- mvrnorm(n, 0, theta)
# sigmay <-  solve(diag(2) - beta) %*% gamma %*% theta %*% t(gamma) %*% t(solve(diag(2) - beta))
# sigmay
# psi <- diag(1 - diag(sigmay))
# eta   <- solve(diag(2) - beta) %*% (gamma %*% t(xi) + t(mvrnorm(n, c(0, 0), psi)))
# eta   <- t(eta)
# dat   <- cbind(xi, eta)
# dat   <- as.data.frame(dat)
# colnames(dat) <- c("x", "m", "y")
# head(dat)
# coef(lm.beta::lm.beta(lm(m ~ x, dat)))
# coef(lm.beta::lm.beta(lm(y ~ m + x, dat)))
# library(lavaan)
# mod <- 
# "
# m ~ x
# y ~ m + x
# "
# fit <- sem(mod, dat, fixed.x = FALSE)
# # coef(fit)
# dplyr::filter(as.data.frame(parameterEstimates(fit)), 
#               lhs == "y", rhs == "y")
# dplyr::filter(as.data.frame(standardizedSolution(fit)), 
#               lhs == "m", rhs == "x")

# Data based on a regression model with one correlation cloe to one.

set.seed(8134704)
n <- 100
k <- 4
xs <- mvrnorm(n, rep(0, k), diag(k))
rxx <- .999
xs2 <- rxx*xs[, k] + rnorm(n, 0, sqrt(1 - rxx))
xs <- cbind(xs, xs2)
colnames(xs) <- paste0("x", seq_len(k + 1))
cor(xs)
v_y <- .10
bx <- sqrt((1 - v_y) / ((k + 1) + 2 * rxx))
y <- xs %*% t(t(rep(bx, k + 1))) + rnorm(n, 0, sqrt(v_y))
lm_out <- lm(y ~ xs)
coef(lm.beta::lm.beta(lm_out))
mod <- paste0("y ~ ", paste(colnames(xs), collapse = " + "))
dat <- data.frame(xs, y)
head(dat)
library(lavaan)
fit <- sem(mod, dat, fixed.x = FALSE)
# coef(fit)
dplyr::filter(as.data.frame(parameterEstimates(fit)), 
              lhs == "y", rhs == "y")
dplyr::filter(as.data.frame(standardizedSolution(fit)), 
              lhs == paste0("x", k),
              rhs == paste0("x", k + 1))
mod2 <- 
paste(mod, "\n",
"
x4 ~~ r45*x5
r45 < 1
")
fit2 <- sem(mod2, dat, fixed.x = FALSE)
dplyr::filter(as.data.frame(parameterEstimates(fit2)), 
              lhs == "y", rhs == "y")
dplyr::filter(as.data.frame(standardizedSolution(fit2)), 
              lhs == paste0("x", k),
              rhs == paste0("x", k + 1))

reg_cor_near_one <- dat

usethis::use_data(reg_cor_near_one, overwrite = TRUE)


# Data based on a two factor model, with one error variance close to zero

library(MASS)

set.seed(5325235)
n <- 120
lambda <- matrix(c(  1,   0,
                   1.2,   0,
                   2.0,   0,
                     0,   1,
                     0, 0.8,
                     0, 1.2), 6, 2, byrow = TRUE)
f_var <- c(2, 3)
f_cor <- .60
phi <- matrix(c(1, f_cor,
                f_cor, 1), 2, 2, byrow = TRUE)
phi <- diag(sqrt(f_var)) %*% phi %*% diag(sqrt(f_var))
psi <- diag(c(3, 2, .10, 4, 3, 2))
dat_f <- mvrnorm(n, c(0, 0), phi)
dat_e <- mvrnorm(n, rep(0, 6), psi)
dat   <- lambda %*% t(dat_f) + t(dat_e)
dat   <- t(dat)
dat   <- as.data.frame(dat)
colnames(dat) <- paste0("x", 1:6)
head(dat)
mod <- 
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
"
fit <- cfa(mod, dat)
dplyr::filter(as.data.frame(parameterEstimates(fit)), 
              lhs == "x3", rhs == "x3")
dplyr::filter(parameterEstimates(fit), 
              lhs == rhs)

mod2 <- 
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
x3 ~~ e3*x3
e3 > 0
"
fit2 <- cfa(mod2, dat)
dplyr::filter(as.data.frame(parameterEstimates(fit2)), 
              lhs == "x3", rhs == "x3")
dplyr::filter(parameterEstimates(fit2), 
              lhs == rhs)


cfa_evar_near_zero <- dat

usethis::use_data(cfa_evar_near_zero, overwrite = TRUE)
