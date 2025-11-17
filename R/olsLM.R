#' Ordinary Least Squares Linear Regression
#'
#' `olsLM()` fits a linear regression model using ordinary least squares (OLS).
#' It supports both:
#'  - **formula interface**: `olsLM(y ~ x1 + x2, data)`
#'  - **matrix interface**:  `olsLM(X, y)`
#'
#' The function returns coefficients, fitted values, residuals, leverage,
#' standardized residuals, internally studentized residuals, and externally
#' studentized residuals.
#'
#' @importFrom stats model.frame model.matrix model.response pt qqnorm qqline printCoefmat
#' @importFrom graphics par abline
#' @param x A model formula (for the formula interface) or a numeric matrix
#'   of predictors (for the matrix interface).
#' @param ... Additional arguments passed to methods.
#'
#' @return An object of class `"olsLM"` containing:
#' \itemize{
#'   \item `coefficients` Estimated regression coefficients.
#'   \item `fitted` Fitted values.
#'   \item `residuals` Ordinary residuals.
#'   \item `sigma2` Residual variance estimate.
#'   \item `df_residual` Residual degrees of freedom.
#'   \item `vcov` Estimated covariance matrix of coefficients.
#'   \item `coef_table` Estimate, Std.Error, t-value, p-value.
#'   \item `r_squared` R-squared.
#'   \item `adjusted_r_squared` Adjusted R-squared.
#'   \item `leverage` Hat values.
#'   \item `r_standard` Standardized residuals.
#'   \item `r_student` Internally studentized residuals.
#'   \item `r_student_ext` Externally studentized residuals.
#' }
#'
#' @examples
#' fit1 <- olsLM(mpg ~ wt + hp, data = mtcars)
#' summary(fit1)
#'
#' X <- as.matrix(mtcars[, c("wt", "hp")])
#' y <- mtcars$mpg
#' fit2 <- olsLM(X, y)
#' summary(fit2)
#'
#' @export
olsLM <- function(x, ...) {
  UseMethod("olsLM")
}

#' @rdname olsLM
#' @param data A data frame containing variables used in the formula.
#' @export
olsLM.formula <- function(x, data, ...) {
  model <- model.frame(x, data)
  X <- model.matrix(attr(model, "terms"), model)
  y <- model.response(model)

  olsLM.default(X, y, intercept = FALSE, call = match.call())
}

#' @rdname olsLM
#' @param y Response vector (numeric).
#' @param intercept Logical; if TRUE (default) and x is a numeric matrix
#'   without an intercept column, an intercept column of 1s is added.
#' @param call The matched call (for printing).
#' @export
olsLM.default <- function(x, y, intercept = TRUE, call = NULL, ...) {
  X <- as.matrix(x)
  y <- as.numeric(y)

  if (is.null(call)) {
    call <- match.call()
  }

  if (isTRUE(intercept)) {
    if (is.null(colnames(X))) {
      colnames(X) <- paste0("x", seq_len(ncol(X)))
    }
    if (!("(Intercept)" %in% colnames(X))) {
      X <- cbind("(Intercept)" = 1, X)
    }
  }

  # OLS
  XtX     <- crossprod(X)
  XtY     <- crossprod(X, y)
  XtX_res <- solve(XtX)

  beta_hat <- XtX_res %*% XtY
  coef_vec <- as.vector(beta_hat)
  names(coef_vec) <- colnames(X)

  # fitted values and residual
  yhat  <- X %*% beta_hat
  resid <- y - yhat
  resid <- as.numeric(resid)

  # degree of freedom
  n  <- length(y)
  p  <- ncol(X)
  df <- n - p

  # variance
  sigma2 <- sum(resid^2) / df
  sigma  <- sqrt(sigma2)

  # covariance and standard error
  vcov_beta <- sigma2 * XtX_res
  std.error <- sqrt(diag(vcov_beta))

  # t test and p-value
  t_val <- as.vector(beta_hat) / std.error
  p_val <- 2 * pt(-abs(t_val), df = df)

  SSY <- sum((y - mean(y))^2)
  SSR <- sum((yhat - mean(y))^2)
  SSE <- sum(resid^2)

  # R^2 and adjusted R^2
  r2     <- SSR / SSY
  adj_r2 <- 1 - (SSE / df) / (SSY / (n - 1))

  # Leverage and residual
  H <- X %*% XtX_res %*% t(X)
  leverage <- diag(H)

  r_standard <- resid / (sigma * sqrt(1 - leverage))
  r_student  <- r_standard

  s_i2 <- (df * sigma2 - resid^2 / (1 - leverage)) / (df - 1)
  s_i2[s_i2 < 0] <- NA_real_
  s_i  <- sqrt(s_i2)
  r_student_ext <- resid / (s_i * sqrt(1 - leverage))

  # result
  result <- list(
    call               = call,
    coefficients       = coef_vec,
    fitted             = as.vector(yhat),
    residuals          = resid,
    sigma2             = sigma2,
    df_residual        = df,
    vcov               = vcov_beta,
    coef_table         = cbind(
      Estimate    = coef_vec,
      `Std.Error` = std.error,
      `t value`   = t_val,
      `Pr(>|t|)`  = p_val
    ),
    r_squared          = r2,
    adjusted_r_squared = adj_r2,
    leverage           = leverage,
    r_standard         = r_standard,
    r_student          = r_student,
    r_student_ext      = r_student_ext
  )

  class(result) <- "olsLM"
  result
}
