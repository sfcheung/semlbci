#' @title Starting Values for Optimization
#'
#' @description Sets the starting values for optimization
#'
#' @details It currently supports `lavaan` output only.
#'
#' @return A `lavaan` parameter table in which parameters estimated with
#'  target fixed to its lower or upper Wald confidence limit.
#'
#' @param i The position of the target parameters as in the parameter
#'  table of lavaan.
#'
#' @param sem_out The SEM output. Currently supports a `lavaan` output only.
#'
#' @param which Whether the lower bound or the upper bound is to be
#'  found. Must be "lbound" or "ubound".
#'
#' @param standardized If TRUE, the LBCI is for the standardized
#'  estimate.
#'
#' @param ciperc The level of confidence for the confidence interval.
#'
#' @noRd

set_start_wn <- function(i = NULL,
                      sem_out = NULL,
                      which = NULL,
                      standardized = FALSE,
                      ciperc = .95) {
    ptable <- as.data.frame(sem_out@ParTable, stringsAsFactors = FALSE)
    est <- lavaan::parameterEstimates(sem_out)
    p_free <- ptable$free > 0
    i_free <- which(p_free)
    id_free <- ptable$id[i_free]
    npar <- sum(p_free)
    qcrit <- stats::qnorm(1 - (1 - ciperc) / 2, 0, 1)
    get_fix2 <- function(ptable, i, qcrit, ratio = 1) {
        switch(which,
              lbound = ptable[i, "est"] - qcrit * ratio * ptable[i, "se"],
              ubound = ptable[i, "est"] + qcrit * ratio * ptable[i, "se"])
      }
    if (standardized) {
        # Standardized solution
        # Will exist in this block
        p_std <- lavaan::standardizedSolution(sem_out,
                                              type = "std.all",
                                              se = FALSE,
                                              zstat = FALSE,
                                              pvalue = FALSE,
                                              ci = FALSE,
                                              remove.eq = FALSE,
                                              remove.ineq = FALSE,
                                              remove.def = FALSE,
                                              output = "data.frame")
        p_std$id <- seq_len(nrow(p_std))
        if (lavaan::lavTech(sem_out, "ngroups") > 1) {
            i_lor <- get_lhs_op_rhs(i, sem_out, more = TRUE)
            i_std <- merge(p_std, i_lor, by = c("lhs", "op", "rhs", "group"))$id
          } else {
            i_lor <- get_lhs_op_rhs(i, sem_out)
            i_std <- merge(p_std, i_lor, by = c("lhs", "op", "rhs"))$id
          }
        start0 <- lavaan::parameterTable(sem_out)
        # The function to be minimized.
        k <- switch(which,
                    lbound = 1,
                    ubound = -1)
        tmp_fct <- function(param, sem_out) {
            start1 <- start0
            start1[start1$free > 0, "est"] <- param
            sem_out2 <- sem_out
            sem_out2@ParTable <- as.list(start1)
            sem_model <- sem_out2@Model
            sem_model <- lavaan::lav_model_set_parameters(sem_model,
                                      start1[start1$free > 0, "est"])
            sem_out2@Model <- sem_model
            std0 <- lavaan::standardizedSolution(sem_out2,
                                            type = "std.all",
                                            se = FALSE,
                                            zstat = FALSE,
                                            pvalue = FALSE,
                                            ci = FALSE,
                                            remove.eq = FALSE,
                                            remove.ineq = FALSE,
                                            remove.def = FALSE,
                                            output = "data.frame")
            std0[i_std, "est.std"]
          }
        # NOTE: g_i0 is of the same length as the parameter vector
        g_i0 <- lavaan::lav_func_gradient_simple(tmp_fct,
                                                 lavaan::coef(sem_out),
                                                 sem_out = sem_out)
        start_free <- rep(NA, npar)
        for (j in seq_len(npar)) {
            start_free[j] <- get_fix2(ptable,
                                      id_free[j],
                                      qcrit,
                                      ratio = g_i0[j])
          }
        tmp_fct(start_free, sem_out = sem_out)
        ptable2_final <- ptable
        ptable2_final[id_free, "est"] <- start_free
        return(ptable2_final)
      }
    if (ptable[i, "op"] %in% c("~", "~~", ":=", "=~")) {
        # Unstandardized solution
        # Will exist in this block
        # If the target is not a user-defined parameter,
        # can handle equality constraints by label
        i_label <- ptable[i, "label"]
        if (i_label == "") {
            i_label <- gen_unique_name(get_names_from_ptable(ptable))
            ptable[i, "label"] <- i_label
          }
        for (rs in c(1, .5, .25, .1)) {
            i_est_fix2 <- get_fix2(ptable, i, qcrit, ratio = rs)
            sem_out2 <- tryCatch(lavaan::update(sem_out, model = ptable,
                                      add = paste0(i_label, " == ", i_est_fix2),
                                      do.fit = TRUE,
                                      baseline = FALSE,
                                      h1 = FALSE,
                                      se = "none"),
                                      error = function(e) e,
                                      warning = function(e) e)
            if (!inherits(sem_out2, "warning") &&
                !inherits(sem_out2, "error")) {
                break
              }
          }
        if (inherits(sem_out2, "warning") | inherits(sem_out2, "error")) {
            return(ptable)
          }
        ptable2_final <- lavaan::parameterTable(sem_out2)
        i_con <- get_i_from_lor(ptable2_final,
                                lhs = i_label,
                                op = "==",
                                rhs = i_est_fix2)
        ptable2_final <- ptable2_final[-i_con, ]
        return(ptable2_final)
      }
    ptable
  }