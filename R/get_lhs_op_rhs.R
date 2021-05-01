#' @title Get lhs, op, rhs, and other info of a parameter from an SEM output
#'
#' @description Get lhs, op, rhs and other information necessary for 
#'              uniquely identifying a parameter from an SEM output
#'
#' @details 
#' 
#' Currently supports \code{lavaan} output only.
#'
#' @return
#' A one row data frame of lhs, op, rhs, block, and group of the parameter in 
#' the parameter table.
#' 
#' By default, only return lhs, op, and rhs.
#' 
#' @param i The position of the target parameters as in the parameter table of lavaan.
#' @param sem_out The SEM output. Currently \code{lavaan} output only.
#' @param more If `TRUE`, block and group are also returned. Default is `FALSE`.
#'
#' @examples
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
#' @keywords internal

get_lhs_op_rhs <- function(i, sem_out, more = FALSE) {
    if (!inherits(sem_out, "lavaan")) {
        stop("sem_out is not a supported object.")
      }
    ptable <- lavaan::parameterTable(sem_out)
    # more is added to maintain compatibility with existing fucntions.
    # They may assume that the output has only three columns.
    if (more) {
        return(as.data.frame(
                as.list(ptable[i, c("lhs", "op", "rhs", "block", "group")])
              ))
      } else {
        return(as.data.frame(as.list(ptable[i, c("lhs", "op", "rhs")])))
      }
  }