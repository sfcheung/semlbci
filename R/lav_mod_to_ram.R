#' @title lavaan matrices to RAM matrices
#'
#' @description Converts a list of lavaan matrices to a list of
#'   RAM matrices.
#'
#' @return A list of A, S, F, and M RAM matrices.
#'
#' @param lav_mod A list of lavaan model matrices for one group.
#'
#'
#' @noRd

lav_mod_to_ram <- function(lav_mod) {
    ov_names <- rownames(lav_mod$theta)
    lv_names <- rownames(lav_mod$psi)
    lv_names <- lv_names[!(lv_names %in% ov_names)]
    all_names <- c(ov_names, lv_names)
    p <- length(ov_names)
    q <- length(lv_names)
    k <- length(all_names)

    # Initialize the matrices
    mA <- matrix(0, k, k)
    colnames(mA) <- rownames(mA) <- all_names
    mS <- matrix(0, k, k)
    colnames(mS) <- rownames(mS) <- all_names
    mF <- cbind(diag(p), matrix(0, p, q))
    colnames(mF) <- all_names
    rownames(mF) <- ov_names
    mM <- matrix(0, 1, p + q)
    colnames(mM) <- all_names

    # Theta to S
    theta_names <- rownames(lav_mod$theta)
    mS[theta_names, theta_names] <- lav_mod$theta

    # Psi to S
    psi_names <- rownames(lav_mod$psi)
    mS[psi_names, psi_names] <- lav_mod$psi

    # Lambda to A
    lambda_rnames <- rownames(lav_mod$lambda)
    lambda_cnames <- colnames(lav_mod$lambda)
    mA[lambda_rnames, lambda_cnames] <- lav_mod$lambda

    # Beta to A
    beta_names <- rownames(lav_mod$beta)
    mA[beta_names, beta_names] <- lav_mod$beta

    # Nu to M
    mM[, rownames(lav_mod$nu)] <- lav_mod$nu
    mM[, rownames(lav_mod$alpha)] <- lav_mod$alpha

    # Output
    list(A = mA,
         S = mS,
         F = mF,
         M = mM)
  }