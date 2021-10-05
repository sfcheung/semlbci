#' @title Standardized Estimate of a Parameter
#'
#' @description Generates a function to get the standardized estiamte
#'  of a parameter.
#'
#' @details This function is no longer used by other functions. Kept
#'  here in case future functions need it.
#'
#' @return A function to be used in a lavaan model.
#'
#' @param fit The source fit object.
#'
#' @param i The position of the standardized parameter in the parameter table.
#'
#' @examples
#' # To Do
#'
#' @noRd

get_std_genfct <- function(fit, i) {
    fit_pt <- lavaan::parameterTable(fit)
    force(i)
    tmpfct <- function(...) {
        .x. <- get(".x.", envir = parent.frame())
        fit@Model <- lavaan::lav_model_set_parameters(
                          fit@Model, .x.
                        )
        fit_pt2 <- fit_pt
        nfree <- sum(fit_pt$free > 0)
        fit_pt2[fit_pt$free > 0, "est"]  <- 
                                      .x.[seq_len(nfree)]
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
    return(tmpfct)
  }
