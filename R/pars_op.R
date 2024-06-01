#' @noRd
# Convert operators to parameter syntax

pars_op <- function(pars,
                    sem_out) {
    ptable <- lavaan::parameterTable(sem_out)
    ptable$lor <- paste(ptable$lhs,
                        ptable$op,
                        ifelse(ptable$op == ":=",
                               yes = "",
                               no = ptable$rhs))
    if ("~" %in% pars) {
        pars1 <- unique(ptable$lor[ptable$op == "~"])
      } else {
        pars1 <- character(0)
      }
    if ("~~" %in% pars) {
        pars2 <- unique(ptable$lor[ptable$op == "~~"])
      } else {
        pars2 <- character(0)
      }
    if ("=~" %in% pars) {
        pars3 <- unique(ptable$lor[ptable$op == "=~"])
      } else {
        pars3 <- character(0)
      }
    if (":=" %in% pars) {
        pars4 <- unique(ptable$lor[ptable$op == ":="])
      } else {
        pars4 <- character(0)
      }
    pars_out <- c(pars[!(pars %in% c("~", "~~", "=~", ":="))],
                  pars1,
                  pars2,
                  pars3,
                  pars4)
    pars_out
  }