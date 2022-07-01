#' @title Scaling Factor in Satorra-2000 Test (Adjusted for Mean and Variance)
#'
#' @description Finds the scaling factor used in the satorra-2000 test.
#'
#' @return The scaling factor
#'
#' @param sem_out The source fit object.
#'
#' @param i The position of the parameter as appeared in the parameter
#'  table.
#'
#' @param standardized If `TRUE`, the limit to be found is for the
#'  standardized solution. Default is `FALSE`.
#'
#' @param update_args  `NULL`. Not used. Included to maintain compatibility
#'  with older versions.
#'
#' @param force_converged  `NULL`. Not used. Included to maintain compatibility
#'  with older versions.
#'
#' @param std_method Not used. Included to maintain compatibility
#'  with older versions.
#'
#' @param sem_out_name  `NULL`. Not used. Included to maintain compatibility
#'  with older versions.
#'
#' @param debug Print debug information, if any. Default is `FALSE`.
#'
#' @noRd

scaling_factor3 <- function(sem_out,
                           i,
                           standardized = FALSE,
                           update_args = NULL,
                           force_converged = NULL,
                           std_method = "lavaan",
                           debug = FALSE,
                           sem_out_name = NULL
                           ) {
    # This function will NOT check whether the SEM was done with robust model
    # test. This check should be done before calling this function.
    p_table <- lavaan::parameterTable(sem_out)
    npar <- sum(p_table$free > 0)
    i_op <- p_table[i, "op"]
    i_lor <- get_lhs_op_rhs(i, sem_out, more = TRUE)
    i_labelled <- nchar(p_table[i, "label"]) > 0
    i_in_free <- p_table[i, "free"]

    # Adapted from lavaan::lav_test_diff_Satorra2000()

    gamma <- lavaan::lavTech(sem_out, "Gamma")
    v <- lavaan::lavTech(sem_out, "WLS.V")
    py <- lavaan::lavTech(sem_out, "delta")
    p <- lavaan::lavTech(sem_out, "information")
    # .Machine$double.eps^(3 / 4)) to reproduce the results of lavaan 0.6-9
    pinv <- MASS::ginv(lavaan::lavInspect(sem_out,
              "augmented.information"),
              tol = .Machine$double.eps^(3 / 4))[seq_len(npar), seq_len(npar)]
    if (standardized) {
        p_std <- lavaan::parameterEstimates(
                              sem_out,
                              standardized = TRUE,
                              se = FALSE,
                              zstat = FALSE,
                              pvalue = FALSE,
                              ci = FALSE,
                              cov.std = FALSE,
                              remove.system.eq = FALSE,
                              remove.eq = FALSE,
                              remove.ineq = FALSE,
                              remove.def = FALSE,
                              remove.nonfree = FALSE)
        p_std$id <- seq_len(nrow(p_std))
        if (lavaan::lavTech(sem_out, "ngroups") > 1) {
            i_lor <- get_lhs_op_rhs(i, sem_out, more = TRUE)
            i_std <- merge(p_std, i_lor, by = c("lhs", "op", "rhs", "group"))$id
          } else {
            i_lor <- get_lhs_op_rhs(i, sem_out)
            i_std <- merge(p_std, i_lor, by = c("lhs", "op", "rhs"))$id
          }
        gfct <- function(param) {
            std0 <- std_lav(param, sem_out)
            std0[i_std]
          }
      } else {
          if (i_op == ":=") {
              i_name <- p_table[i, "label"]
              gfct <- function(param) {
                  sem_out@Model@def.function(param)[i_name]
                }
            } else {
              gfct <- function(param) {
                  param[i_in_free]
                }
        }
      }

    gd <- lavaan::lav_func_jacobian_complex(gfct, lavaan::coef(sem_out))

    # Satorra-2000, p. 240
    x <- pinv %*% t(gd) %*% MASS::ginv(gd %*% pinv %*% t(gd)) %*% gd %*% pinv
    ng <- lavaan::lavTech(sem_out, "ngroups")
    fg <- lavaan::lavTech(sem_out, "nobs") / lavaan::lavTech(sem_out, "ntotal")
    tmpfct <- function(v_i, gamma_i, py_i) {
        # Satorra-2000, Eq. 23
        tmp <- v_i %*% gamma_i %*% v_i %*% (py_i %*% x %*% t(py_i))
        c(tr_ug = sum(diag(tmp)),
          tr_ug2 = sum(diag(tmp %*% tmp)))
      }
    ugs <- as.vector(mapply(tmpfct, v, gamma, py) %*% matrix(fg, ng, 1))
    tr_ug <- ugs[1]
    tr_ug2 <- ugs[2]

    # Satorra-2000
    # Asparouhov, T., & Muthén, B. O. (2010). Simple second order chi-square
    #     correction. Obtained from
    #     https://www.statmodel.com/download/WLSMV_new_chi21.pdf
    a <- sqrt(1 / tr_ug2)
    b <- 1 - tr_ug / sqrt(tr_ug2)
    out <-
      data.frame(
        c_p = 1 / a,
        c_pb = b,
        c_r = 1 / a,
        c_rb = b)
    return(out)
  }

