#' @title Generate a unique name
#'
#' @description Generate a unique name that does not duplicate with elements
#'              in a vector of strings.
#'
#' @details 
#' 
#' User supplies a vector of strings and then a name is generated that does
#' not duplicate with elements in this vector.
#' 
#' A helper function for generating names for new parameters.
#'
#' @return
#' A string
#' 
#' @param source_names A vector of strings
#'
#' @examples
#' 
#' x <- c("A", "b", "xyz")
#' 
#' gen_unique_name(x)
#' 
#' @keywords internal

gen_unique_name <- function(source_names) {
    if (!is.character(source_names)) {
        stop("The source name(s) is/are not a vector of characters")
      }
    maxi <- max(sapply(source_names, nchar)) + 1
    for (i in seq_len(maxi)) {
        out <- paste0(sample(letters, i), collapse = "")
        if (is.na(match(out, source_names))) {
            return(out)
          }
      }
    # The loop is quaranteered to be finite
    stop("Something's wrong. A unique name cannot be generated.")
  }