#' @title lhs, op, rhs, and Other Info of a Parameter
#'
#' @description Gets lhs, op, rhs and other information necessary for
#'   uniquely identifying a parameter from an SEM output
#'
#' @details Currently supports \code{lavaan} output only.
#'
#' @return A one row data frame of lhs, op, rhs, block, and group of
#'  the parameter in the parameter table.
#'
#' By default, only return lhs, op, and rhs.
#'
#' @param i The position of the target parameters as in the parameter
#'   table of lavaan.
#'
#' @param sem_out The SEM output. Currently \code{lavaan} output only.
#'
#' @param more If `TRUE`, block and group are also returned. Default
#'  is `FALSE`.
#'
#' @examples
#'
#' # TODO
#'
#' @noRd

get_lhs_op_rhs <- function(i, sem_out, more = FALSE) {
    if (!inherits(sem_out, "lavaan")) {
        stop("sem_out is not a supported object.")
      }
    ptable <- lavaan::parameterTable(sem_out)
    # more is added to maintain compatibility with existing functions.
    # They may assume that the output has only three columns.
    if (more) {
        return(as.data.frame(
                as.list(ptable[i, c("lhs", "op", "rhs", "block", "group")])
              ))
      } else {
        return(as.data.frame(as.list(ptable[i, c("lhs", "op", "rhs")])))
      }
  }