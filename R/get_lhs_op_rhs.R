#'@title Get lhs, op, and rhs of a parameter from an SEM output
#'
#'@description Get lhs, op, and rhs of a parameter from an SEM output
#'
#'@details 
#' 
#' Currently supports \code{lavaan} output only.
#'
#'@return
#' A one row data frame of lhs, op, and rhs of the parameter in the parameter table.
#' 
#' @param i The position of the target parameters as in the parameter table of lavaan.
#' @param sem_out The SEM output. Currently \code{lavaan} output only.
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

get_lhs_op_rhs <- function(i, sem_out) {
    if (!inherits(sem_out, "lavaan")) {
        stop("sem_out is not a supported object.")
      }
    ptable <- lavaan::parameterTable(sem_out)
    # Do not check for 
    as.data.frame(as.list(ptable[i, c("lhs", "op", "rhs")]))
  }  