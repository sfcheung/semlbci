# Used by ci_bound_nm_i. Enforce equality constraints when
# when the target parameter is so constrained.

enforce_eq_by_label <- function(i_depend, p_table) {
    j_no_label <- p_table$label == ""
    p_table[j_no_label, "label"] <- 
        paste0("@@", p_table[j_no_label, "id"], "@@")
    labels <- p_table[p_table$free > 0,"label"]
    labels <- unique(labels[labels != ""])
    fct <- function(x) {
        which(p_table$label == p_table$label[x])
      }
    fct2 <- function(x) {
        rep(x, sum(p_table$label == p_table$label[x]))
      }
    out <- lapply(i_depend, fct)
    out2 <- lapply(i_depend, fct2)
    list(i_exp_target = unlist(out),
         i_exp_source = unlist(out2))
  }
