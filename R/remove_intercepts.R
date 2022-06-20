#' @noRd

# Remove parameters that are intercepts

remove_intercepts <- function(pars,
                      sem_out) {
    ptable <- lavaan::parameterTable(sem_out)
    is_intercept <- (ptable$op == "~1")
    row_id <- seq_len(nrow(ptable))
    id_var <- row_id[is_intercept]
    out <- pars[!(pars %in% id_var)]
    out
  }