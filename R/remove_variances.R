#' @noRd

# Remove parameters that are variances or error variances

remove_variances <- function(pars,
                      sem_out) {
    ptable <- lavaan::parameterTable(sem_out)
    is_var <- (ptable$lhs == ptable$rhs) & (ptable$op == "~~")
    row_id <- seq_len(nrow(ptable))
    id_var <- row_id[is_var]
    out <- pars[!(pars %in% id_var)]
    out
  }