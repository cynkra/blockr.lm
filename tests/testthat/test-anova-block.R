test_that("anova block expression generation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  model <- lm(mpg ~ cyl + hp, data = mtcars)
  test_data <- reactive(model)

  blk <- new_anova_block()

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
      expect_true(any(grepl("broom::tidy", expr_text)))
      expect_true(any(grepl("stats::anova", expr_text)))
    }
  )
})

test_that("anova block UI generation", {
  blk <- new_anova_block()

  ui_output <- blk$expr_ui("test_id")

  expect_s3_class(ui_output, "shiny.tag.list")
  ui_text <- as.character(ui_output)

  # Should contain help text about ANOVA
  expect_true(grepl("ANOVA", ui_text, ignore.case = TRUE) ||
                grepl("variance", ui_text, ignore.case = TRUE))
})

# Full block server test
test_that("anova block produces tidy data frame - testServer", {
  skip_if_not_installed("blockr.core")
  skip_if_not_installed("broom")

  model <- lm(mpg ~ cyl + hp, data = mtcars)
  block <- new_anova_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_true("term" %in% names(result))
      expect_true("df" %in% names(result))
      expect_true("sumsq" %in% names(result))
      expect_true("meansq" %in% names(result))
      expect_true("statistic" %in% names(result))
      expect_true("p.value" %in% names(result))

      # Should have rows for cyl, hp, and Residuals
      expect_true("cyl" %in% result$term)
      expect_true("hp" %in% result$term)
      expect_true("Residuals" %in% result$term)
    },
    args = list(x = block, data = list(data = function() model))
  )
})

test_that("anova block with single predictor - testServer", {
  skip_if_not_installed("blockr.core")
  skip_if_not_installed("broom")

  model <- lm(mpg ~ cyl, data = mtcars)
  block <- new_anova_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # Should have rows for cyl and Residuals
      expect_equal(nrow(result), 2)
      expect_true("cyl" %in% result$term)
      expect_true("Residuals" %in% result$term)
    },
    args = list(x = block, data = list(data = function() model))
  )
})
