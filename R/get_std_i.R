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
    for (j in 1:6) {
        lavmodel_tmp <- tryCatch(get("lavmodel", envir = parent.frame(j)),
                        error = function(e) e)
        if (inherits(lavmodel_tmp, "lavModel")) break # lavModel found
      }
    if (!inherits(lavmodel_tmp, "lavModel")) {return(NA)}
    for (j in 1:6) {
        pt_tmp <- tryCatch(get("lavpartable", envir = parent.frame(j)),
                        error = function(e) e)
        if (inherits(pt_tmp, "list")) break # lavModel found
      }
    if (!inherits(pt_tmp, "list")) {return(NA)}
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
