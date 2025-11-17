#' Summary information for olsLM()
#'
#' This function prints a summary of an `olsLM` fitted model,
#' including coefficient table, residual standard error, and
#' R-squared statistics.
#'
#' @param object An object of class `"olsLM"`.
#' @param ... Additional arguments (ignored).
#'
#' @return The input object, invisibly.
#' @export
summary.olsLM <- function(object, ...) {

  cat("Call:\n")
  print(object$call)

  cat("\nCoefficients:\n")
  printCoefmat(object$coef_table)

  cat("\nResidual standard error:",
      sqrt(object$sigma2),
      "on", object$df_residual, "degrees of freedom\n")

  cat("Multiple R-squared:", object$r_squared,
      ", Adjusted R-squared:", object$adjusted_r_squared, "\n")

  invisible(object)
}
