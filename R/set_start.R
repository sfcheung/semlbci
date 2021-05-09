#' @title Set the starting values for optimization
#'
#' @description Set the starting values for optimization
#'
#' @details
#'
#' Currently supports \code{lavaan} output only.
#'
#' @return
#' A lavaan parameter table, with parameters estimated with target fixed to its
#'  lower or upper Wald confidence limit.
#'
#' @param i The position of the target parameters as in the parameter table of
#'          lavaan.
#' @param sem_out The SEM output. Currently \code{lavaan} output only.
#' @param which Whether the lower bound or the upper bound is to be found.
#'              Must be "lbound" or "ubound".
#' @param standardized If TRUE, the LBCI is for the standardized estimate.
#'
#' @keywords internal

set_start <- function(i = NULL,
                      sem_out = NULL,
                      which = NULL,
                      standardized = FALSE) {
    ptable <- as.data.frame(sem_out@ParTable, stringsAsFactors = FALSE)
    est    <- lavaan::parameterEstimates(sem_out)
    p_free <- ptable$free > 0
    i_free <- which(p_free)
    id_free <- ptable$id[i_free]
    npar <- sum(p_free)
    if (sem_out@Model@eq.constraints) {
        return(ptable)
      }
    if (standardized) {
        # If standardized solution is requested
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
        i_lor <- get_lhs_op_rhs(i, sem_out)
        i_std <- merge(p_std, i_lor, by = c("lhs", "op", "rhs"))$id
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
            k * std0[i_std, "est.std"]
          }
        g_i0 <- numDeriv::grad(tmp_fct, 
                              x = lavaan::coef(sem_out), 
                              sem_out = sem_out)
        g_i0 <- round(g_i0, 5)
        g_i  <- (g_i0 != 0)
        id_g <- id_free[g_i]
        id_g_pos <- id_free[g_i0 > 0]
        id_g_neg <- id_free[g_i0 < 0]
        i_est_fix2 <- lavaan::parameterTable(sem_out)$est
        if (length(id_g_neg) > 0) {
            i_est_fix2[id_g_neg] <- est[id_g_neg, "ci.upper"]
          }
        if (length(id_g_pos) > 0) {
            i_est_fix2[id_g_pos] <- est[id_g_pos, "ci.lower"]
          }
        ptable2 <- ptable
        ptable2[id_g, "ustart"] <- i_est_fix2[id_g]
        ptable2[id_g, "start"] <- i_est_fix2[id_g]
        ptable2[id_g, "free"]  <- 0
        ptable2_def <- ptable2[ptable2$op == ":=", ]
        ptable2 <- ptable2[ptable2$op != ":=", ]
        sem_out2 <- lavaan::update(sem_out, model = ptable2, start = ptable2)
        ptable2_out <- as.data.frame(sem_out2@ParTable,
                                     stringsAsFactors = FALSE)
        ptable2_final <- rbind(ptable2_out, ptable2_def)
        ptable2_final[id_g, "ustart"] <- NA
        ptable2_final[id_g, "free"]  <- max(ptable2_final$free) +
                                            seq_len(length(id_g))
        return(ptable2_final)
      }
    if (ptable[i, "op"] != ":=") {
        # If the target is not a user-defined parameter
        i_est_fix2 <- switch(which,
                            lbound = est[i, "ci.lower"],
                            ubound = est[i, "ci.upper"])
        ptable2 <- ptable
        ptable2[i, "ustart"] <- i_est_fix2
        ptable2[i, "start"] <- i_est_fix2
        ptable2[i, "free"]  <- 0
        ptable2_def <- ptable2[ptable2$op == ":=", ]
        ptable2 <- ptable2[ptable2$op != ":=", ]
        sem_out2 <- lavaan::update(sem_out, model = ptable2, start = ptable2)
        ptable2_out <- as.data.frame(sem_out2@ParTable,
                                     stringsAsFactors = FALSE)
        ptable2_final <- rbind(ptable2_out, ptable2_def)
        ptable2_final[i, "ustart"] <- NA
        ptable2_final[i, "free"]  <- max(ptable2_final$free) + 1
        return(ptable2_final)
      }
    if (ptable[i, "op"] == ":=") {
        # If the target is a user-defined parameter
        i_name <- ptable[i, "label"]
        k <- switch(which,
                    lbound = 1,
                    ubound = -1)
        tmp_fct <- function(param, sem_out) {
            k * sem_out@Model@def.function(param)[i_name]
          }
        g_i0 <- numDeriv::grad(tmp_fct,
                              x = lavaan::coef(sem_out),
                              sem_out = sem_out)
        g_i0 <- round(g_i0, 5)
        g_i  <- (g_i0 != 0)
        id_g <- id_free[g_i]
        id_g_pos <- id_free[g_i0 > 0]
        id_g_neg <- id_free[g_i0 < 0]
        i_est_fix2 <- lavaan::parameterTable(sem_out)$est
        if (length(id_g_neg) > 0) {
            i_est_fix2[id_g_neg] <- est[id_g_neg, "ci.upper"]
          }
        if (length(id_g_pos) > 0) {
            i_est_fix2[id_g_pos] <- est[id_g_pos, "ci.lower"]
          }
        ptable2 <- ptable
        ptable2[id_g, "ustart"] <- i_est_fix2[id_g]
        ptable2[id_g, "start"] <- i_est_fix2[id_g]
        ptable2[id_g, "free"]  <- 0
        ptable2_def <- ptable2[ptable2$op == ":=", ]
        ptable2 <- ptable2[ptable2$op != ":=", ]
        sem_out2 <- lavaan::update(sem_out, model = ptable2, start = ptable2)
        ptable2_out <- as.data.frame(sem_out2@ParTable,
                                     stringsAsFactors = FALSE)
        ptable2_final <- rbind(ptable2_out, ptable2_def)
        ptable2_final[id_g, "ustart"] <- NA
        ptable2_final[id_g, "free"]  <- max(ptable2_final$free) +
                                            seq_len(length(id_g))
        return(ptable2_final)
      }
    ptable
  }