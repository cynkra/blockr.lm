test_that("model block produces a fitted lm via block_server", {
  block <- new_model_block(
    model_type = "lm",
    response = "yA",
    predictors = c("xA1", "xA2")
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "lm")
      expect_named(coef(result), c("(Intercept)", "xA1", "xA2"))
    },
    args = list(x = block, data = list(data = function() .tdf_a()))
  )
})

test_that("model block produces a fitted glmer for mixed logistic", {
  d <- .tdf_a()
  d$y_bin <- factor(ifelse(d$yA > 0, "yes", "no"))
  block <- new_model_block(
    model_type = "glmer",
    response = "y_bin",
    predictors = "xA1",
    random_effects = "fA3"
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s4_class(result, "glmerMod")
    },
    args = list(x = block, data = list(data = function() d))
  )
})

test_that("model block clears stale column state on data swap", {
  data_rv <- shiny::reactiveVal(.tdf_a())
  block <- new_model_block(
    model_type = "lm",
    response = "yA",
    predictors = c("xA1", "xA2"),
    factors = "fA3"
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_s3_class(session$returned$result(), "lm")

      # Swap to a dataset with no overlapping column names. The old
      # response / predictors / factors must be dropped from state so
      # they cannot leak into the next formula.
      data_rv(.tdf_b())
      session$flushReact()
      st <- session$returned$state
      cb <- colnames(.tdf_b())
      expect_true(length(st$response()) == 0L || all(st$response() %in% cb))
      expect_true(length(st$predictors()) == 0L || all(st$predictors() %in% cb))
      expect_true(length(st$factors()) == 0L || all(st$factors() %in% cb))
    },
    args = list(x = block, data = list(data = function() data_rv()))
  )
})

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
