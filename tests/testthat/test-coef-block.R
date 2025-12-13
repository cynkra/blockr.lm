test_that("coef block expression generation - with confidence intervals", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  # Create a model as input
  model <- lm(mpg ~ cyl + hp, data = mtcars)
  test_data <- reactive(model)

  blk <- new_coef_block(conf_int = TRUE, conf_level = 0.95)

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))
      expect_true(is.list(result$state))

      # Set inputs
      session$setInputs(conf_int = TRUE, conf_level = "0.95")
      session$flushReact()

      # Check expression
      expr_result <- result$expr()
      expect_true(inherits(expr_result, "call"))
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("broom::tidy", expr_text)))
      expect_true(any(grepl("conf.int = TRUE", expr_text)))
      expect_true(any(grepl("conf.level", expr_text)))
    }
  )
})

test_that("coef block expression generation - without confidence intervals", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  model <- lm(mpg ~ cyl + hp, data = mtcars)
  test_data <- reactive(model)

  blk <- new_coef_block(conf_int = FALSE)

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Set inputs
      session$setInputs(conf_int = FALSE, conf_level = "0.95")
      session$flushReact()

      # Check expression
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("conf.int = FALSE", expr_text)))
    }
  )
})

test_that("coef block reactive state updates", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  model <- lm(mpg ~ cyl, data = mtcars)
  test_data <- reactive(model)

  blk <- new_coef_block(conf_int = TRUE, conf_level = 0.95)

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Set initial inputs
      session$setInputs(conf_int = TRUE, conf_level = "0.95")
      session$flushReact()

      expect_true(result$state$conf_int())
      expect_equal(result$state$conf_level(), 0.95)

      # Update conf_int
      session$setInputs(conf_int = FALSE)
      session$flushReact()
      expect_false(result$state$conf_int())

      # Update conf_level
      session$setInputs(conf_level = "0.99")
      session$flushReact()
      expect_equal(result$state$conf_level(), 0.99)
    }
  )
})

test_that("coef block UI generation", {
  blk <- new_coef_block()

  ui_output <- blk$expr_ui("test_id")

  expect_s3_class(ui_output, "shiny.tag.list")
  ui_text <- as.character(ui_output)

  # Should contain checkbox for confidence intervals
  expect_true(grepl("confidence interval", ui_text, ignore.case = TRUE))

  # Should contain select for confidence level
  expect_true(grepl("0.95", ui_text))
})

# Full block server test
test_that("coef block produces tidy data frame - testServer", {
  skip_if_not_installed("blockr.core")
  skip_if_not_installed("broom")

  model <- lm(mpg ~ cyl + hp, data = mtcars)
  block <- new_coef_block(conf_int = TRUE, conf_level = 0.95)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_true("term" %in% names(result))
      expect_true("estimate" %in% names(result))
      expect_true("std.error" %in% names(result))
      expect_true("statistic" %in% names(result))
      expect_true("p.value" %in% names(result))
      expect_true("conf.low" %in% names(result))
      expect_true("conf.high" %in% names(result))

      # Should have 3 rows (Intercept, cyl, hp)
      expect_equal(nrow(result), 3)
      expect_true("(Intercept)" %in% result$term)
      expect_true("cyl" %in% result$term)
      expect_true("hp" %in% result$term)
    },
    args = list(x = block, data = list(data = function() model))
  )
})

test_that("coef block produces tidy data frame without CI - testServer", {
  skip_if_not_installed("blockr.core")
  skip_if_not_installed("broom")

  model <- lm(mpg ~ cyl, data = mtcars)
  block <- new_coef_block(conf_int = FALSE)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_true("term" %in% names(result))
      expect_true("estimate" %in% names(result))

      # Should NOT have confidence interval columns
      expect_false("conf.low" %in% names(result))
      expect_false("conf.high" %in% names(result))
    },
    args = list(x = block, data = list(data = function() model))
  )
})
