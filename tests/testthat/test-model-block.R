test_that("model block creates lm with default type", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  test_data <- reactive(mtcars)

  blk <- new_model_block(
    model_type = "lm",
    response = "mpg",
    predictors = c("cyl", "hp")
  )

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))
      expect_true(is.list(result$state))

      # Check expression is an lm call
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("stats::lm", expr_text)))
      expect_true(any(grepl("mpg", expr_text)))
      expect_true(any(grepl("cyl", expr_text)))
    }
  )
})

test_that("model block creates logistic regression", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  test_data <- reactive(mtcars)

  blk <- new_model_block(
    model_type = "logistic",
    response = "am",
    predictors = c("mpg", "hp")
  )

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)

      expect_true(any(grepl("stats::glm", expr_text)))
      expect_true(any(grepl("stats::binomial", expr_text)))
    }
  )
})

test_that("model block creates poisson regression", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  test_data <- reactive(mtcars)

  blk <- new_model_block(
    model_type = "poisson",
    response = "carb",
    predictors = c("cyl", "hp")
  )

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)

      expect_true(any(grepl("stats::glm", expr_text)))
      expect_true(any(grepl("stats::poisson", expr_text)))
    }
  )
})

test_that("model block creates gamma regression", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  test_data <- reactive(mtcars)

  blk <- new_model_block(
    model_type = "gamma",
    response = "mpg",
    predictors = c("cyl", "hp")
  )

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)

      expect_true(any(grepl("stats::glm", expr_text)))
      expect_true(any(grepl("stats::Gamma", expr_text)))
    }
  )
})

test_that("model block UI generation", {
  blk <- new_model_block()

  ui_output <- blk$expr_ui("test_id")

  expect_s3_class(ui_output, "shiny.tag.list")

  # Convert to text and check for model type dropdown
  ui_text <- as.character(ui_output)
  expect_true(grepl("model_type", ui_text))
  expect_true(grepl("response", ui_text))
  expect_true(grepl("predictors", ui_text))
})

test_that("model block handles no intercept", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  test_data <- reactive(mtcars)

  blk <- new_model_block(
    model_type = "lm",
    response = "mpg",
    predictors = c("cyl"),
    intercept = FALSE
  )

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expr_text <- paste(deparse(expr_result), collapse = "")

      # Should have 0 + in formula for no intercept
      expect_true(grepl("~ 0 \\+", expr_text))
    }
  )
})

test_that("model block returns NULL with empty response", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  test_data <- reactive(mtcars)

  blk <- new_model_block()

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()

      expect_equal(expr_result, quote(NULL))
    }
  )
})

test_that("logistic model works with mtcars", {
  skip_if_not_installed("broom")

  # Fit logistic regression
  model <- glm(am ~ mpg + hp, data = mtcars, family = binomial())

  # Test that broom works
  tidy_df <- broom::tidy(model)
  glance_df <- broom::glance(model)

  expect_true(nrow(tidy_df) > 0)
  expect_true(nrow(glance_df) == 1)
})

test_that("poisson model works with mtcars", {
  skip_if_not_installed("broom")

  # Fit poisson regression
  model <- glm(carb ~ cyl + hp, data = mtcars, family = poisson())

  # Test that broom works
  tidy_df <- broom::tidy(model)
  glance_df <- broom::glance(model)

  expect_true(nrow(tidy_df) > 0)
  expect_true(nrow(glance_df) == 1)
})
