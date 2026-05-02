test_that("parameters block emits a tibble of coefficients on lm", {
  m <- lm(yA ~ xA1 + xA2 + fA3, data = .tdf_a())
  block <- new_parameters_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "tbl_df")
      expect_true(nrow(result) >= 4L)  # intercept + 2 covs + factor levels
      expect_true(all(c("Parameter", "Coefficient", "p") %in% names(result)))
    },
    args = list(x = block, data = list(data = function() m))
  )
})

test_that("parameters block also handles t-test htest objects", {
  d <- .tdf_a()
  tt <- t.test(yA ~ fA, data = d)
  block <- new_parameters_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 1L)
    },
    args = list(x = block, data = list(data = function() tt))
  )
})
