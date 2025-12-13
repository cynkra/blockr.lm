test_that("lm block expression generation - basic model", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  test_data <- reactive(data.frame(
    mpg = c(21, 21, 22.8, 21.4, 18.7),
    cyl = c(6, 6, 4, 6, 8),
    hp = c(110, 110, 93, 110, 175)
  ))

  blk <- new_lm_block(response = "mpg", predictors = c("cyl", "hp"))

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))
      expect_true(is.list(result$state))

      # Set inputs (they don't get automatically set from constructor in testServer)
      session$setInputs(response = "mpg", predictors = c("cyl", "hp"), intercept = TRUE)
      session$flushReact()

      # Check expression
      expr_result <- result$expr()
      expect_true(inherits(expr_result, "call"))
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("stats::lm", expr_text)))
      expect_true(any(grepl("mpg", expr_text)))
      expect_true(any(grepl("cyl", expr_text)))
      expect_true(any(grepl("hp", expr_text)))
    }
  )
})

test_that("lm block expression generation - no intercept", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  test_data <- reactive(data.frame(
    y = c(1, 2, 3),
    x = c(4, 5, 6)
  ))

  blk <- new_lm_block(response = "y", predictors = "x", intercept = FALSE)

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Set inputs
      session$setInputs(response = "y", predictors = "x", intercept = FALSE)
      session$flushReact()

      # Check expression contains "~ 0 +"
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("~ 0 \\+", expr_text)))
    }
  )
})

test_that("lm block expression generation - intercept only model", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  test_data <- reactive(data.frame(
    y = c(1, 2, 3),
    x = c(4, 5, 6)
  ))

  blk <- new_lm_block(response = "y", predictors = character())

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Set inputs - no predictors
      session$setInputs(response = "y", predictors = character(), intercept = TRUE)
      session$flushReact()

      # Check expression contains "~ 1"
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("~ 1", expr_text)))
    }
  )
})

test_that("lm block returns NULL when no response selected", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  test_data <- reactive(data.frame(
    y = c(1, 2, 3),
    x = c(4, 5, 6)
  ))

  blk <- new_lm_block()

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # No inputs set - should return NULL expression
      expr_result <- result$expr()
      expect_true(is.null(expr_result) || identical(expr_result, quote(NULL)))
    }
  )
})

test_that("lm block reactive state updates", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  test_data <- reactive(data.frame(
    mpg = c(21, 21, 22.8),
    cyl = c(6, 6, 4),
    hp = c(110, 110, 93)
  ))

  blk <- new_lm_block(response = "mpg", predictors = "cyl")

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Set initial inputs
      session$setInputs(response = "mpg", predictors = "cyl", intercept = TRUE)
      session$flushReact()

      expect_equal(result$state$response(), "mpg")
      expect_equal(result$state$predictors(), "cyl")
      expect_true(result$state$intercept())

      # Update predictors
      session$setInputs(predictors = c("cyl", "hp"))
      session$flushReact()
      expect_equal(result$state$predictors(), c("cyl", "hp"))

      # Update intercept
      session$setInputs(intercept = FALSE)
      session$flushReact()
      expect_false(result$state$intercept())
    }
  )
})

test_that("lm block UI generation", {
  blk <- new_lm_block(response = "mpg", predictors = c("cyl", "hp"))

  ui_output <- blk$expr_ui("test_id")

  expect_s3_class(ui_output, "shiny.tag.list")
  ui_text <- as.character(ui_output)

  # Should contain selectInput for response
  expect_true(grepl("Response variable", ui_text, ignore.case = TRUE))

  # Should contain selectizeInput for predictors
  expect_true(grepl("Predictor", ui_text, ignore.case = TRUE))

  # Should contain checkbox for intercept
  expect_true(grepl("intercept", ui_text, ignore.case = TRUE))
})

# Full block server test
test_that("lm block produces lm model - testServer", {
  skip_if_not_installed("blockr.core")

  block <- new_lm_block(response = "mpg", predictors = c("cyl", "hp"))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "lm")
      expect_true("mpg" %in% names(result$model))
      expect_true("cyl" %in% names(result$model))
      expect_true("hp" %in% names(result$model))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("lm block model has correct coefficients", {
  skip_if_not_installed("blockr.core")

  block <- new_lm_block(response = "mpg", predictors = c("cyl", "hp"))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Check that coefficients exist
      coefs <- coef(result)
      expect_true("(Intercept)" %in% names(coefs))
      expect_true("cyl" %in% names(coefs))
      expect_true("hp" %in% names(coefs))

      # Check model summary is valid
      summ <- summary(result)
      expect_true(!is.null(summ$r.squared))
      expect_true(summ$r.squared >= 0 && summ$r.squared <= 1)
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})
