test_that("residual explorer block expression generation - default", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  model <- lm(mpg ~ cyl + hp, data = mtcars)
  test_data <- reactive(model)

  blk <- new_residual_explorer_block(x_var = "fitted", y_var = "residuals")

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))
      expect_true(is.list(result$state))

      # Set inputs
      session$setInputs(x_var = "fitted", y_var = "residuals")
      session$flushReact()

      # Check expression
      expr_result <- result$expr()
      expect_true(inherits(expr_result, "call"))
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("create_residual_plot", expr_text)))
      expect_true(any(grepl("fitted", expr_text)))
      expect_true(any(grepl("residuals", expr_text)))
    }
  )
})

test_that("residual explorer block expression generation - standardized residuals", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  model <- lm(mpg ~ cyl, data = mtcars)
  test_data <- reactive(model)

  blk <- new_residual_explorer_block(x_var = "index", y_var = "standardized")

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Set inputs
      session$setInputs(x_var = "index", y_var = "standardized")
      session$flushReact()

      # Check expression
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("index", expr_text)))
      expect_true(any(grepl("standardized", expr_text)))
    }
  )
})

test_that("residual explorer block reactive state updates", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  model <- lm(mpg ~ cyl, data = mtcars)
  test_data <- reactive(model)

  blk <- new_residual_explorer_block(x_var = "fitted", y_var = "residuals")

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Set initial inputs
      session$setInputs(x_var = "fitted", y_var = "residuals")
      session$flushReact()

      expect_equal(result$state$x_var(), "fitted")
      expect_equal(result$state$y_var(), "residuals")

      # Update x_var
      session$setInputs(x_var = "index")
      session$flushReact()
      expect_equal(result$state$x_var(), "index")

      # Update y_var
      session$setInputs(y_var = "studentized")
      session$flushReact()
      expect_equal(result$state$y_var(), "studentized")
    }
  )
})

test_that("residual explorer block UI generation", {
  blk <- new_residual_explorer_block()

  ui_output <- blk$expr_ui("test_id")

  expect_s3_class(ui_output, "shiny.tag.list")
  ui_text <- as.character(ui_output)

  # Should contain selectInput for x_var
  expect_true(grepl("X-axis", ui_text, ignore.case = TRUE))

  # Should contain selectInput for y_var
  expect_true(grepl("Y-axis", ui_text, ignore.case = TRUE))

  # Should contain options
  expect_true(grepl("Fitted", ui_text, ignore.case = TRUE))
  expect_true(grepl("Residual", ui_text, ignore.case = TRUE))
})

test_that("create_residual_plot helper function works", {
  skip_if_not_installed("plotly")

  model <- lm(mpg ~ cyl + hp, data = mtcars)

  # Test with default parameters
  plot <- create_residual_plot(model, x_var = "fitted", y_var = "residuals")
  expect_s3_class(plot, "plotly")

  # Test with index on x-axis
  plot <- create_residual_plot(model, x_var = "index", y_var = "residuals")
  expect_s3_class(plot, "plotly")

  # Test with standardized residuals
  plot <- create_residual_plot(model, x_var = "fitted", y_var = "standardized")
  expect_s3_class(plot, "plotly")

  # Test with studentized residuals
  plot <- create_residual_plot(model, x_var = "fitted", y_var = "studentized")
  expect_s3_class(plot, "plotly")
})

test_that("create_residual_plot returns correct data structure", {
  skip_if_not_installed("plotly")

  model <- lm(mpg ~ cyl, data = mtcars)
  plot <- create_residual_plot(model)

  # Should be a plotly object
  expect_s3_class(plot, "plotly")

  # Should have x and y data
  plot_data <- plotly::plotly_data(plot)
  expect_true(nrow(plot_data) == nrow(mtcars))
  expect_true("x" %in% names(plot_data))
  expect_true("y" %in% names(plot_data))
})
