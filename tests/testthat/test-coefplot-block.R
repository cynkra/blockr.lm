test_that("coefplot block expression generation - default", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")
  skip_if_not_installed("modelsummary")

  model <- lm(mpg ~ cyl + hp, data = mtcars)
  test_data <- reactive(model)

  blk <- new_coefplot_block()

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))
      expect_true(is.list(result$state))

      # Check expression
      expr_result <- result$expr()
      expect_true(inherits(expr_result, "call"))
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("modelplot", expr_text)))
      expect_true(any(grepl("conf_level", expr_text)))
    }
  )
})

test_that("coefplot block expression generation - exponentiate", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")
  skip_if_not_installed("modelsummary")

  model <- lm(mpg ~ cyl + hp, data = mtcars)
  test_data <- reactive(model)

  blk <- new_coefplot_block(exponentiate = TRUE)

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Check expression has exponentiate = TRUE
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("exponentiate = TRUE", expr_text)))
    }
  )
})

test_that("coefplot block expression generation - with intercept", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")
  skip_if_not_installed("modelsummary")

  model <- lm(mpg ~ cyl + hp, data = mtcars)
  test_data <- reactive(model)

  blk <- new_coefplot_block(omit_intercept = FALSE)

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Check expression has coef_omit = NULL
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("coef_omit = NULL", expr_text)))
    }
  )
})

test_that("coefplot block reactive state updates", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")
  skip_if_not_installed("modelsummary")

  model <- lm(mpg ~ cyl + hp, data = mtcars)
  test_data <- reactive(model)

  blk <- new_coefplot_block()

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Check initial state
      expect_equal(result$state$conf_level(), 0.95)
      expect_equal(result$state$exponentiate(), FALSE)
      expect_equal(result$state$omit_intercept(), TRUE)

      # Update conf_level
      session$setInputs(conf_level = "0.99")
      session$flushReact()
      expect_equal(result$state$conf_level(), 0.99)

      # Update exponentiate
      session$setInputs(exponentiate = TRUE)
      session$flushReact()
      expect_equal(result$state$exponentiate(), TRUE)

      # Update omit_intercept
      session$setInputs(omit_intercept = FALSE)
      session$flushReact()
      expect_equal(result$state$omit_intercept(), FALSE)
    }
  )
})

test_that("coefplot block UI generation", {
  blk <- new_coefplot_block()

  ui_output <- blk$expr_ui("test_id")

  expect_s3_class(ui_output, "shiny.tag.list")
  ui_text <- as.character(ui_output)

  # Should contain confidence level selector
  expect_true(grepl("conf_level", ui_text, ignore.case = TRUE))

  # Should contain exponentiate checkbox
  expect_true(grepl("exponentiate", ui_text, ignore.case = TRUE))

  # Should contain omit intercept checkbox
  expect_true(grepl("omit_intercept", ui_text, ignore.case = TRUE))
})

test_that("coefplot block produces valid plot", {
  skip_if_not_installed("modelsummary")

  model <- lm(mpg ~ cyl + hp + wt, data = mtcars)

  # Test modelplot directly
  plot <- modelsummary::modelplot(model, coef_omit = "(Intercept)")
  expect_s3_class(plot, "ggplot")
})
