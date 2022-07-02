#' @title Free Parameters in an SEM Output
#'
#' @description Find the free parameters in an SEM output.
#'
#' @details Currently supports [lavaan::lavaan-class] outputs only.
#'
#' @return A boolean vector of the same length as the number of rows
#' of the \code{lavaan}. parameter tables. A position is `TRUE` if the
#' corresponding parameter is free.
#'
#' @param sem_out The SEM output. Currently supports
#'  [lavaan::lavaan-class] outputs only.
#'
#' @examples
#' \dontrun{
#'
#' data(cfa_two_factors)
#' library(lavaan)
#'
#' mod <-
#' "
#' f1 =~ x1 + x2 + a*x3
#' f2 =~ x4 + a*x5 + equal('f1=~x2')*x6
#' f1 ~~ 0*f2
#' asq := a^2
#' "
#'
#' fit <- sem(mod, cfa_two_factors)
#' parameterTable(fit)
#' find_free(fit)
#'
#' }
#' @noRd

find_free <- function(sem_out) {
    if (!inherits(sem_out, "lavaan")) {
        stop("sem_out is not a supported object.")
      }
    ptable <- lavaan::parameterTable(sem_out)
    i <- ptable$free > 0
    i
  }