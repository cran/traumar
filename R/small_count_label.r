#' @title Label Small Counts Based on a Cutoff
#'
#' @description
#'
#' This function labels values in a vector as a replacement string if they are
#' smaller than a specified cutoff. The input can be numeric, and the function
#' will return either a modified version of the input vector with small values
#' replaced by a given label, or it will keep the original values otherwise.
#'
#' @param var A numeric vector. This represents the variable to be checked
#'   against the cutoff.
#' @param cutoff A numeric value representing the threshold. Values in `var`
#'   smaller than this value will be replaced.
#' @param replacement A string or a numeric value. If the value in `var` is
#'   smaller than the `cutoff`, this value will replace it. If a string is
#'   provided, it will replace the numeric values with the string. If a numeric
#'   value is provided, the replacement will also be numeric.
#'
#' @returns A vector with values from `var`. Values smaller than the `cutoff`
#'   will be replaced by the `replacement`. If `replacement` is a string, the
#'   return type will be character, otherwise, it will remain numeric.
#'
#' @export
#'
#' @examples
#' # Example usage of the small_count_label function
#' small_count_label(c(1, 5, 10), 5, "Below Cutoff")
#' small_count_label(c(1, 5, 10), 5, 0)
#'
#' @author Nicolas Foss, Ed.D., MS
#'
small_count_label <- function(var, cutoff, replacement) {
  # Check if var is numeric
  if (!is.numeric(var)) {
    cli::cli_abort(
      paste0(
        "The input {.var var} must be a numeric vector, but you supplied an object of class {.cls {class(var)}}."
      )
    )
  }

  # Check if cutoff is numeric
  if (!is.numeric(cutoff) || length(cutoff) != 1) {
    cli::cli_abort(
      paste0(
        "The {.var cutoff} must be a single numeric value, but you supplied an object of class {.cls {class(cutoff)}}."
      )
    )
  }

  # Check if replacement is of a valid type
  if (!is.character(replacement) && !is.numeric(replacement)) {
    cli::cli_abort(
      paste0(
        "The {.var replacement} must be either a string or a numeric value, but you supplied an object of class {.cls {class(replacement)}}."
      )
    )
  }

  # Perform the replacement based on the cutoff
  if (
    !is.character(replacement) ||

      (is.character(replacement) && all(var >= cutoff, na.rm = T))
  ) {
    output <- ifelse(var < cutoff, replacement, var)
  } else {
    output <- ifelse(var < cutoff, replacement, as.character(var))
  }

  return(output)
}
