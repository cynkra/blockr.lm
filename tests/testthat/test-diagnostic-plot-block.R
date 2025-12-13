test_that("diagnostic plot block expression generation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  model <- lm(mpg ~ cyl + hp, data = mtcars)
  test_data <- reactive(model)

  blk <- new_diagnostic_plot_block()

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
      expect_true(any(grepl("create_diagnostic_plots", expr_text)))
    }
  )
})

test_that("diagnostic plot block UI generation", {
  blk <- new_diagnostic_plot_block()

  ui_output <- blk$expr_ui("test_id")

  expect_s3_class(ui_output, "shiny.tag.list")
  ui_text <- as.character(ui_output)

  # Should contain help text about diagnostics
  expect_true(grepl("diagnostic", ui_text, ignore.case = TRUE) ||
                grepl("assumptions", ui_text, ignore.case = TRUE))
})

test_that("create_diagnostic_plots helper function works", {
  skip_if_not_installed("blockr.core")

  model <- lm(mpg ~ cyl + hp, data = mtcars)

  # Should not error when called
  expect_no_error(create_diagnostic_plots(model))
})

test_that("create_diagnostic_plots with simple model", {
  model <- lm(mpg ~ cyl, data = mtcars)

  # Should produce plots without error
  expect_no_error(create_diagnostic_plots(model))
})
