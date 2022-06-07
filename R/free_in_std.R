#' @noRd

# Find parameters free in the standardized solution

free_in_std <- function(pars,
                      sem_out) {
    ptable <- lavaan::parameterTable(sem_out)
    ptable$rowid <- seq_len(nrow(ptable))
    stable <- lavaan::standardizedSolution(sem_out)
    ngroups <- lavaan::lavTech(sem_out, "ngroups")
    ptable_selected <- ptable[pars, ]
    if (ngroups == 1) {
        by_str <- c("lhs", "op", "rhs")
      } else {
        by_str <- c("lhs", "op", "rhs", "group")
      }
    p_merged <- merge(ptable_selected[, c(by_str, "rowid", "free")],
                      stable,
                      by = by_str,
                      all.x = TRUE,
                      all.y = FALSE,
                      sort = FALSE)
    out <- p_merged[!is.na(p_merged$z), ]
    out[out$free == 0 & sapply(out$se, is.numeric), "rowid"]
  }