test_that("olsLM(formula) matches lm() on mtcars", {
  set.seed(1)

  fit_ols <- olsLM(mpg ~ wt + hp + qsec, data = mtcars)
  fit_lm  <- lm(mpg ~ wt + hp + qsec, data = mtcars)

  expect_equal(
    unname(fit_ols$coefficients),
    unname(coef(fit_lm)),
    tolerance = 1e-6
  )

  expect_equal(
    unname(fit_ols$fitted),
    unname(fitted(fit_lm)),
    tolerance = 1e-6
  )

  expect_equal(
    unname(fit_ols$residuals),
    unname(residuals(fit_lm)),
    tolerance = 1e-6
  )

  expect_equal(
    fit_ols$r_squared,
    unname(summary(fit_lm)$r.squared),
    tolerance = 1e-6
  )
})

test_that("matrix interface and formula interface give same result", {
  X <- as.matrix(mtcars[, c("wt", "hp", "qsec")])
  y <- mtcars$mpg

  fit_mat  <- olsLM(X, y)
  fit_form <- olsLM(mpg ~ wt + hp + qsec - 1, data = mtcars)

  expect_equal(
    unname(fit_mat$coefficients),
    unname(fit_form$coefficients),
    tolerance = 1e-6
  )
  expect_equal(
    unname(fit_mat$fitted),
    unname(fit_form$fitted),
    tolerance = 1e-6
  )
})

test_that("summary.olsLM runs without error", {
  fit <- olsLM(mpg ~ wt + hp, data = mtcars)
  expect_error(summary(fit), NA)
})

test_that("plot.olsLM runs without error (using temporary device)", {
  fit <- olsLM(mpg ~ wt + hp, data = mtcars)

  tmp <- tempfile(fileext = ".pdf")
  pdf(tmp)
  expect_error(plot(fit), NA)
  dev.off()

  expect_true(file.exists(tmp))
})
