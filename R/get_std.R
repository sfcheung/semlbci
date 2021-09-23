#' @title Get standardized estimate within lavaan
#'
#' @description This function is to be used to define user-defined
#'  parameters in `lavaan`. Exported to be used by [scaling_factor()].
#'  Not to be used by normal users.
#'
#' @return
#' The standardized estimate of the *i* parameter.
#'
#' @param fit_str A one-element character vector. The name of the fit object.
#' @param i The position of the parameter in the parameter table.
#' 
#' @examples
#' # TODO
#'
#' @export

get_std <- function(fit_str, i) {
    fit <- eval(get(fit_str), parent.frame())
    fit_pt <- lavaan::parameterTable(fit)
    .x. <- get(".x.", envir = parent.frame())
    fit@Model <- lavaan::lav_model_set_parameters(
                      fit@Model, .x.
                    )
    fit_pt2 <- fit_pt
    nfree <- sum(fit_pt$free > 0)
    fit_pt2[fit_pt$free > 0, "est"] <- .x.[seq_len(nfree)]
    fit@ParTable <- as.list(fit_pt2)
    std <- lavaan::standardizedSolution(
                      fit,
                      se = FALSE,
                      zstat = FALSE,
                      pvalue = FALSE,
                      ci = FALSE,
                      cov.std = FALSE,
                      remove.eq = FALSE,
                      remove.ineq = FALSE,
                      remove.def = FALSE,
                      )
    std[i, "est.std"]
  }
