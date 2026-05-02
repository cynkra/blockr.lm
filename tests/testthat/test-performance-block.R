test_that("performance block emits a one-row tibble of fit statistics", {
  m <- lm(yA ~ xA1 + xA2, data = .tdf_a())
  block <- new_performance_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 1L)
      expect_true(any(grepl("R2", names(result))))
    },
    args = list(x = block, data = list(data = function() m))
  )
})
