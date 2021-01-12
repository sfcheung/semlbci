#' @title Find the free parameters on which a parameter depends on
#'
#' @description Find the free parameters on which a parameter depends on
#'
#' @details
#'
#' Ideally, this should be done analytically. However, it is not easy to
#' implement this for a wide variety of models. Therefore, whether a parameter
#' depends on another parameter is determined by the graident of the function
#' with the target parameter as the output other parameters as the control
#' variables of this function.
#' 
#' This function is particularly important when the target parameter is one
#' in the standardized solution. In this case, the dependency among parameters
#' can be very complicated.
#' 
#' This function is currently used by [ci_bound_nm_i()].
#'
#' Currently supports a [lavaan::lavaan-class] output only.
#'
#' @seealso
#' [ci_bound_nmi_i()]
#'
#' @return
#' A numeric vector of the positions of the free parameters in the
#' [lavaan::lavaan-class] parameter table.
#' 
#' @param i The position of the target parameter as in the parameter table of
#'          lavaan.
#' @param sem_out The SEM output. Currently support [lavaan::lavaan-class]
#'                outputs only.
#' @param standardized If `TRUE`, the LBCI is for the standardized estimate.
#' @param signed If `TRUE`, return a vector of 1 or -1 to indicate the
#'                direction of the dependence. Default is [`FALSE`].
#'
#' @examples
#' \dontrun{
#' data(simple_med)
#' dat <- simple_med
#' mod <-
#' "
#' m ~ a*x
#' y ~ b*m
#' ab:= a*b
#' ainvb := a/b
#' "
#' fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)
#' ptable <- lavaan::parameterTable(fit_med)
#' ptable
#'
#' out <- lapply(1:7, find_dependent,
#'                    sem_out = fit_med,
#'                    standardized = FALSE)
#' #' Correctly identified that ab depends on a and b
#' ptable[out[[6]], ]
#'
#' out_std <- lapply(c(1,2, 3, 4, 6, 7), find_dependent,
#'                    sem_out = fit_med,
#'                    standardized = TRUE)
#' #' Correclty identified that the standardized ab depends on
#' #' all free parameters
#' ptable[out_std[[6]], ]
#' }
#'@export

find_dependent <- function(i = NULL,
                      sem_out = NULL,
                      standardized = FALSE,
                      signed = FALSE) {
    ptable <- as.data.frame(sem_out@ParTable, stringsAsFactors = FALSE)
    est    <- lavaan::parameterEstimates(sem_out)
    p_free <- ptable$free > 0
    i_free <- which(p_free)
    id_free <- ptable$id[i_free]
    npar <- sum(p_free)
    if (standardized) {
        # If standardized solution is requested
        # If standardized, both free and user defined parameters are handled in the same way
        p_std_check <- lavaan::standardizedSolution(sem_out,
                                              type = "std.all",
                                              se = TRUE,
                                              zstat = TRUE,
                                              pvalue = TRUE,
                                              ci = FALSE,
                                              remove.eq = FALSE,
                                              remove.ineq = FALSE,
                                              remove.def = FALSE,
                                              output = "data.frame")
        if (is.na(p_std_check[i, "z"])) {
            stop("The requested parameter is fixed in the standardized solution.")
          }
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
        tmp_fct <- function(param, sem_out) {
            start1 <- start0
            start1[start1$free > 0, "est"] <- param
            sem_out2 <- sem_out
            sem_out2@ParTable <- as.list(start1)
            sem_model <- sem_out2@Model
            sem_model <- update_model(sem_model, 
                                      start1[start1$free > 0, "est"] )
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
        g_i0 <- lavaan::lav_func_gradient_simple(tmp_fct, 
                              x = lavaan::coef(sem_out), 
                              sem_out = sem_out)
        g_i0 <- round(g_i0, 5)
        g_i  <- (g_i0 != 0)
        if (signed) {
            tmp1 <- ifelse(g_i0 > 0,  1, 0)
            tmp2 <- ifelse(g_i0 < 0, -1, 0)
            tmp <- tmp1 + tmp2
            return(tmp[which(g_i)])
          } else {
            return(which(g_i))
          }
      } else if (ptable[i, "op"] == ":=") {
        # If the target is a user-defined parameter
        i_name <- ptable[i, "label"]
        tmp_fct <- function(param, sem_out) {
            sem_out@Model@def.function(param)[i_name]
          }
        g_i0 <- lavaan::lav_func_gradient_simple(tmp_fct, 
                              x = lavaan::coef(sem_out), 
                              sem_out = sem_out)
        g_i0 <- round(g_i0, 5)
        g_i  <- (g_i0 != 0)
        if (signed) {
            tmp1 <- ifelse(g_i0 > 0,  1, 0)
            tmp2 <- ifelse(g_i0 < 0, -1, 0)
            tmp <- tmp1 + tmp2
            return(tmp[which(g_i)])
          } else {
            return(which(g_i))
          }
      } else {
        # If the target is none of the above
        if (signed) {
            return(1)
          } else {
            return(i)
          }
      }
    npar
  }  