#' @noRd
# TODO:
# - To be exported for plotting loglikelihood

loglik_user <- function(x,
                        sem_out_userp,
                        sem_out,
                        lrt_method = "default",
                        ...) {
    sem_out_userp_tmp <- sem_out_userp_run(target = x,
                                           object = sem_out_userp,
                                           ...)
    lrt_x <- tryCatch(lavaan::lavTestLRT(sem_out_userp_tmp,
                                         sem_out,
                                         method = lrt_method),
                      error = function(e) e,
                      warning = function(w) w)
    if (inherits(lrt_x, "warning")) {
        tmp <- as.character(lrt_msg)
        if (grepl("scaling factor is negative",
                  tmp, fixed = TRUE)) {
            lrt_x <- tryCatch(lavaan::lavTestLRT(sem_out_userp_tmp,
                                                 sem_out,
                                                 method = "satorra.2000"),
                              error = function(e) e,
                              warning = function(w) w)
          }
      }
    out <- lrt_x[2, "Chisq diff"] / (-2)
    attr(out, "sem_out_userp_x") <- sem_out_userp_tmp
    out
  }