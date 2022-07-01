#' @noRd

# Create the parameter name from a row number in the parameter table

i_to_name <- function(par_i,
                       sem_out) {
    ptable <- lavaan::parameterTable(sem_out)
    ngroups <- lavaan::lavTech(sem_out, "ngroups")
    ptable_selected <- ptable[par_i, ]
    if ((ngroups == 1) || ptable_selected$group == 0) {
        gp_label_i <- ""
      } else {
        gp_labels <- lavaan::lavInspect(sem_out, "group.label")
        gp_id <- ptable_selected$group
        gp_label_i <- gp_labels[gp_id]
        gp_label_i <- paste0(" (", gp_label_i, ")")
      }
    if (ptable_selected$label != "") {
        out <- paste0(ptable_selected$label, gp_label_i)
      } else {
        out <- paste0(ptable_selected[, c("lhs", "op", "rhs")],
                      collapse = "")
        out <- paste0(out, gp_label_i)
      }
    return(out)
  }