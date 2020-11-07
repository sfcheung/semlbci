#'@title Convert lavaan syntax to positions in the fit object parameter table
#'
#'@description Convert lavaan syntax to positions in the fit object parameter table
#'
#'@details 
#' 
#' Currently supports \code{lavaan} output only.
#'
#'@return
#' A vector of positions in the parameter table.
#' 
#' @param syntax A vector of parameters, defined as in lavaan.
#' @param sem_out The SEM output. Currently \code{lavaan} output only.
#'
#'@examples
#' library(lavaan)
#' data(cfa_two_factors)
#' mod <- 
#' "
#' f1 =~ x1 + x2 + a*x3
#' f2 =~ x4 + a*x5 + equal('f1=~x2')*x6
#' f1 ~~ 0*f2
#' asq := a^2
#' "
#' fit <- sem(mod, cfa_two_factors)
#'@export

syntax_to_i <- function(syntax,
                        sem_out) {
    if (!inherits(sem_out, "lavaan")) {
        stop("sem_out is not a supported object.")
      }
    ptable <- lavaan::parameterTable(sem_out)
    l_model <- lavaan::lavParseModelString(syntax, as.data.frame = TRUE)
    if (nrow(l_model) > 0) {
        l_model$req <- TRUE
        p_out <- merge(ptable, l_model[, c("lhs", "op", "rhs", "req")], 
                      by = c("lhs", "op", "rhs"), all.x = TRUE, sort = FALSE)
        i_par <- which(p_out$req)
      } else {
        i_par <- NULL
      }
    # User defined parameter
    syntax_def <- syntax[grepl(":=", syntax)]
    if (length(syntax_def) > 0) {
        l_def <- strsplit(syntax_def, ":=")
        l_def <- sapply(l_def, function(x) trimws(x[1]))
        # i_def <- match(l_def, p_out$label)
        i_def <- match(l_def, ptable$label)
        # p_out[i_def, "req"] <- TRUE
      } else {
        i_def <- NULL
      }
    # which(p_out$req)
    c(i_par, i_def)
  }