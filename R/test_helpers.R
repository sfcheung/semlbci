#' @noRd

test_p <- function(fit0, fit1, ciperc, tol) {
    is_rb <- "satorra.bentler" %in% lavInspect(fit1, "options")$test
    if (is_rb) {
        out <- lavTestLRT(fit0, fit1, method = "satorra.2000", A.method = "exact")
        return(abs(out[2, "Pr(>Chisq)"] - (1 - ciperc)) < tol)
      } else {
        return(abs(anova(fit0, fit1)[2, "Pr(>Chisq)"] - (1 - ciperc)) < tol)
      }
  }


#' @noRd

get_scaling_factor <- function(lrt_out) {
    data.frame(c_p = 1 / attr(lrt_out, "scale")[2],
               c_pb = attr(lrt_out, "shift")[2],
               c_r = 1 / attr(lrt_out, "scale")[2],
               c_rb = attr(lrt_out, "shift")[2])
  }