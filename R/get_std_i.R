#' @title Get standardized solution within lavaan
#'
#' @description Get standardized solution within lavaan
#'
#' @return
#' The estimatate in the standardized solution.
#'
#' @param i The positon of the parameter as in the parameter table.
#' @param ... Optional arguments. Not used.
#' 
#' @examples
#' # TODO
#'
#' @export

get_std_i <- function(i, ...) {
    .x. <- get(".x.", envir = parent.frame())
    i_main <- 1
    while (!exists("lavoptions", parent.frame(i_main)) &
           !exists("PARTABLE", parent.frame(i_main))) {
        i_main <- i_main + 1
        if (i_main > 10) browser()
      } 
    if (!exists("get_std_i_env", parent.frame(i_main))) {
        assign("get_std_i_env", new.env(), pos = parent.frame(i_main))
        i_tmp <- 1
        while (!exists("lavmodel", parent.frame(i_tmp))) {
            i_tmp <- i_tmp + 1
            if (i_tmp > 10) browser()
          }
        assign("lavmodel_tmp", get("lavmodel", parent.frame(i_tmp)),
               pos = parent.frame(i_main)$get_std_i_env)
        i_tmp <- 1
        while (!exists("lavpartable", parent.frame(i_tmp))) {
            i_tmp <- i_tmp + 1
            if (i_tmp > 10) browser()
          }
        assign("pt_tmp", get("lavpartable", parent.frame(i_tmp)),
               pos = parent.frame(i_main)$get_std_i_env)
      }
    lavmodel_tmp <- get("lavmodel_tmp", parent.frame(i_main)$get_std_i_env)
    pt_tmp <- get("pt_tmp", parent.frame(i_main)$get_std_i_env)
    lor <- c(pt_tmp$lhs[i], pt_tmp$op[i], pt_tmp$rhs[i],
             pt_tmp$block[i], pt_tmp$group[i])
    pt_tmp <- as.data.frame(pt_tmp)
    pt_tmp[grep("get_std_i", pt_tmp$rhs), "rhs"] <- 1
    pt_est <- pt_tmp$free
    nfree <- sum(pt_tmp$free > 0)
    pt_est[pt_est > 0] <- .x.[seq_len(nfree)]
    pt_tmp$est <- pt_est
    fit_tmp <- lavaan::lavaan(pt_tmp, do.fit = FALSE)
    fit_tmp@Model <- lavaan::lav_model_set_parameters(
                         fit_tmp@Model, .x.
                       )
    std <- lavaan::standardizedSolution(
                      fit_tmp,
                      se = FALSE,
                      zstat = FALSE,
                      pvalue = FALSE,
                      ci = FALSE,
                      cov.std = FALSE,
                      remove.eq = FALSE,
                      remove.ineq = FALSE,
                      remove.def = FALSE,
                      )
    out <- std[i, "est.std"]
    return(out)
  }
