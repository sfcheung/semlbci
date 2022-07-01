#' @title Free Variances in an SEM Output
#'
#' @description Find the free variances in an SEM output
#'
#' @details Currently supports [lavaan::lavaan-class] outputs only.
#'
#' @return A boolean vector of the same length as the number of free
#'  parameters. A position is \code{TRUE} if the corresponding free
#'  parameter is a variance (op == "~~").
#'
#' @param sem_out The SEM output. Currently [lavaan::lavaan-class]
#'  outputs only.
#'
#' @examples
#' \dontrun{
#' cfa_two_factors
#'
#' mod <-
#' "
#' f1 =~ x1 + x2 + x3
#' f2 =~ x4 + x5 + x6
#' "
#'
#' fit <- lavaan::sem(mod, cfa_two_factors)
#' out <- find_variance_in_free(fit)
#' coef(fit)[out]
#' }
#' @noRd

find_variance_in_free <- function(sem_out) {
    if (!inherits(sem_out, "lavaan")) {
        stop("sem_out is not a supported object.")
      }
    ptable <- lavaan::parameterTable(sem_out)
    i_id <- ptable$id
    i_free <- ptable$free > 0
    id_free <- i_id[find_free(sem_out)]
    i_var <- (ptable$lhs == ptable$rhs) & (ptable$op == "~~")
    id_var <- i_id[i_var]
    id_free %in% id_var
  }

#' @param prop Default is .05, setting the lower bound to .05 *
#'                estimate.
#'
#' @param se_k Default is 3, the estimate minus 3 standard error. If
#'                negative, the lower bound is set using `lb_prop`.
#' @noRd

find_variance_in_free_lb <- function(sem_out, prop = .05, se_k = 3) {
    # Find the free variances and generate lower bounds
    # Used by ci_bound_wn_i
    if (!inherits(sem_out, "lavaan")) {
        stop("sem_out is not a supported object.")
      }
    ptable <- lavaan::parameterTable(sem_out)
    i_id <- ptable$id
    i_free <- ptable$free > 0
    id_free <- i_id[find_free(sem_out)]
    i_var <- (ptable$lhs == ptable$rhs) & (ptable$op == "~~")
    id_var <- i_id[i_var]
    out_est <- ptable$est[id_var]
    out_se <- ptable$se[id_var]
    out_min <- out_est * prop
    out_se3 <- out_est - out_se * se_k
    out <- ifelse(out_se3 < 0, out_min, out_se3)
    out
  }
