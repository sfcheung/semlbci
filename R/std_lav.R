#' @title Standardized Solution from lavaan
#'
#' @description Computes the standardized solution for a lavaan
#'  object with user supplied parameter vector
#'
#' @return A vector of the standardized solution, of the same
#'  length as the number of rows in the parameter table
#'
#' Does not yet support standardized solution for means.
#'
#' @param param A vector of parameters.
#'
#' @param sem_out A lavaan-class object.
#'
#' @noRd

std_lav <- function(param, sem_out) {

    lav_model <- sem_out@Model

    # Set parameters

    lav_model_new <- lavaan::lav_model_set_parameters(lav_model,
                                                      param)

    # Get the list of lavaan matrices with changed parameters
    # Adapted from lavaan::lav_object_inspect_modelmatrices
    # This block works regardless of the number of groups

    glist_new <- lav_model_new@GLIST
    for (i in seq_len(length(glist_new))) {
        dimnames(glist_new[[i]]) <- lav_model@dimNames[[i]]
      }
    ng <- lav_model@nblocks
    nmat <- lav_model@nmat
    gp_labels <- sem_out@Data@group.label
    if (length(gp_labels) == 0) gp_labels <- "single_group"
    out <- vector("list", length = ng)
    isum <- 0
    for (i in seq_len(ng)) {
        out[[i]] <- glist_new[seq_len(nmat[i]) + isum]
        isum <- isum + nmat[i]
      }
    names(out) <- unlist(gp_labels)

    glist_new <- out

    # Do standardization by RAM matrices

    ram <- lapply(glist_new, lav_mod_to_ram)
    ram_std <- lapply(ram, std_ram)
    glist_std <- mapply(ram_to_lav_mod, ram_std, glist_new,
                        SIMPLIFY = FALSE)

    # Update GLIST

    if (ng == 1) {
        lav_model_new@GLIST <- glist_std[[1]]
      } else {
        lav_model_new@GLIST <- unlist(glist_std,
                                    recursive = FALSE)
      }

    # Get parameter values with type = "user" and extra = TRUE

    param_std <- lavaan::lav_model_get_parameters(lav_model_new,
                                                  type = "user",
                                                  extra = TRUE)
    param_std
  }