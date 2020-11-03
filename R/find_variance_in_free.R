#'@title Find free parameters which are variances from an SEM output
#'
#'@description Compute the standardized moderation effect given the \code{lm} output.
#'
#'@details 
#' 
#' Currently supports \code{lavaan} output only.
#'
#'@return
#' A boolean vector of the same length as the number of free parameters. 
#' A position is \code{TRUE} if the corresponding free parameter 
#' is a variance (op == "~~").
#' 
#'@param sem_out The SEM output. Currently \code{lavaan} output only.
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

find_variance_in_free <- function(sem_out) {
    if (!inherits(sem_out, "lavaan")) {
        stop("sem_out is not a supported object.")
      }
    ptable <- lavaan::parameterTable(sem_out)
    # Do not check for 
    i_id <- ptable$id
    i_free <- ptable$free > 0
    id_free <- i_id[find_free(sem_out)]
    i_var <- (ptable$lhs == ptable$rhs) & (ptable$op == "~~")
    id_var <- i_id[i_var]
    id_free %in% id_var
  }