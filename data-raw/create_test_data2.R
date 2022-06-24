# Generate data

# Data based on a three factor model with latent level mediation

set.seed(836193)
n <- 150
fx <- rnorm(n)
a0 <- .27
b0 <- .35
c0 <- .45
fm <- a0 * fx + rnorm(n, 0, sqrt(1 - a0^2))
evy <- 1 - (b0^2 + c0^2 + 2 * b0 * c0 * a0)
fy <- b0 * fm + c0 * fx + rnorm(n, 0, sqrt(evy))
ff <- cbind(fx, fm, fy)
cff <- matrix(0, 3, 3)
cff[c(4, 7, 8)] <- cff[c(2, 3, 6)] <- c(a0, a0 * b0 + c0, b0 + a0 * c0)
diag(cff) <- 1
lambda <- matrix(0, 9, 3)
lambda[c(1:3, 1:3 + 12, 1:3 + 12 * 2)] <- .7
exx <- diag(1 - diag(lambda %*% cff %*% t(lambda)))
dat <- ff %*% t(lambda) + MASS::mvrnorm(n, rep(0, 9), exx)
dat <- data.frame(dat)
colnames(dat) <- paste0("x", 1:9)
print(head(dat), digits = 3)

mediation_latent <- dat

usethis::use_data(mediation_latent, overwrite = TRUE)
