#' Diagnostic plots for olsLM objects
#'
#' Produces four standard diagnostic plots for linear regression,
#' similar to `plot(lm)`:
#' 1. Residuals vs Fitted
#' 2. Normal Q-Q Plot
#' 3. Scale-Location Plot
#' 4. Residuals vs Leverage
#'
#' @param x An object of class `"olsLM"`.
#' @param ... Additional arguments (ignored).
#'
#' @export
plot.olsLM <- function(x, ...) {

  old_par <- par(mfrow = c(2,2))
  on.exit(par(old_par))

  resid  <- x$residuals
  fitted <- x$fitted
  lev    <- x$leverage
  stdres <- x$r_standard

  # 1) Residuals vs Fitted
  plot(fitted, resid,
       xlab = "Fitted values",
       ylab = "Residuals",
       main = "Residuals vs Fitted")
  abline(h = 0, col = "red")

  # 2) Normal Q-Q of standardized residuals
  qqnorm(resid,
         main = "Normal Q-Q")
  qqline(resid, col = "red")

  # 3) Scale-Location
  plot(fitted, sqrt(abs(stdres)),
       xlab = "Fitted values",
       ylab = "Sqrt(|Standardized residual|)",
       main = "Scale-Location")

  # 4) Residuals vs Leverage
  plot(lev, resid,
       xlab = "Leverage",
       ylab = "Residuals",
       main = "Residuals vs Leverage")
  abline(h = 0, col = "red")
}
