#' @title Standardized Solution from RAM Matrices
#'
#' @description Computes the standardized solution using RAM
#'   matrices
#'
#' Can not yet find the standardized solution for the means
#' 
#' @return A list of RAM matrices with the standardized
#'   solution.
#'
#' @param ram_est A list of RAM matrices for one group, with
#'   unstandardized estimates.
#'
#'
#' @noRd

std_ram <- function(ram_est) {
    mA <- ram_est$A
    mS <- ram_est$S
    iA <- diag(nrow(mA))
    Binv <- solve(iA - mA)
    sdinv <- diag((diag(Binv %*% mS %*% t(Binv)))^(-.5))
    mAz <- sdinv %*% mA %*% solve(sdinv)
    mSz <- sdinv %*% mS %*% sdinv
    colnames(mAz) <- rownames(mAz) <- colnames(mA)
    colnames(mSz) <- rownames(mSz) <- colnames(mS)
    mMz <- ram_est$M
    mMz[] <- 0
    list(A = mAz,
         S = mSz,
         F = ram_est$F,
         M = mMz)
  }
