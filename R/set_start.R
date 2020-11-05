#'@title Set the starting values for optimization
#'
#'@description Set the starting values for optimization
#'
#'@details 
#' 
#' Currently supports \code{lavaan} output only.
#'
#'@return
#' A lavaan parameter table, with parameters estimated with target fixed to its 
#'  lower or upper Wald confidence limit.
#' 
#' @param i The position of the target parameters as in the parameter table of lavaan.
#' @param sem_out The SEM output. Currently \code{lavaan} output only.
#' @param which Whether the lower bound or the upper bound is to be found. Must be "lbound" or "ubound".
#'
#'@examples
#' library(lavaan)
#' data(cfa_two_factors)
#' mod <- 
#' "
#' f1 =~ x1 + x2 + a*x3
#' f2 =~ x4 + a*x5 + equal('f1=~x2')*x6
#' f1 ~~ 0*f2
#' asq := a^2
#' "
#' fit <- sem(mod, cfa_two_factors)
#'@export

set_start <- function(i = NULL,
                      sem_out = NULL,
                      which = NULL) {
    ptable <- as.data.frame(sem_out@ParTable, stringsAsFactors = FALSE)
    est    <- lavaan::parameterEstimates(sem_out)
    p_free <- ptable$free > 0
    i_free <- which(p_free)
    id_free <- ptable$id[i_free]
    npar <- sum(p_free)
    if (sem_out@Model@eq.constraints) {
        return(ptable)
      }
    # If the target is not a user-defined parameter
    if (ptable[i, "op"] != ":=") {
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
        ptable2_out <- as.data.frame(sem_out2@ParTable, stringsAsFactors = FALSE)
        ptable2_final <- rbind(ptable2_out, ptable2_def)
        ptable2_final[i, "ustart"] <- NA
        ptable2_final[i, "free"]  <- max(ptable2_final$free) + 1
      } else {
        i_name <- ptable[i, "label"]
        k <- switch(which,
                    lbound = 1,
                    ubound = -1)
        tmp_fct <- function(param, sem_out) {
            k * sem_out@Model@def.function(param)[i_name]
          }
        g_i <- numDeriv::grad(tmp_fct, 
                              x = lavaan::coef(sem_out), 
                              sem_out = sem_out)
        g_i <- (round(g_i, 5) != 0)
        id_g <- id_free[g_i]
        i_est_fix2 <- switch(which,
                            lbound = est[id_g, "ci.lower"],
                            ubound = est[id_g, "ci.upper"])
        ptable2 <- ptable
        ptable2[id_g, "ustart"] <- i_est_fix2
        ptable2[id_g, "start"] <- i_est_fix2
        ptable2[id_g, "free"]  <- 0
        ptable2_def <- ptable2[ptable2$op == ":=", ]
        ptable2 <- ptable2[ptable2$op != ":=", ]
        sem_out2 <- lavaan::update(sem_out, model = ptable2, start = ptable2)
        ptable2_out <- as.data.frame(sem_out2@ParTable, stringsAsFactors = FALSE)
        ptable2_final <- rbind(ptable2_out, ptable2_def)
        ptable2_final[id_g, "ustart"] <- NA
        ptable2_final[id_g, "free"]  <- max(ptable2_final$free) + 
                                            seq_len(length(id_g))
      }
    ptable2_final
  }  