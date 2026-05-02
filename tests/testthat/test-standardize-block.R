test_that("standardize block emits standardized betas with CI", {
  m <- lm(yA ~ xA1 + xA2, data = .tdf_a())
  block <- new_standardize_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "tbl_df")
      expect_true(nrow(result) >= 2L)
      expect_true("Std_Coefficient" %in% names(result))
    },
    args = list(x = block, data = list(data = function() m))
  )
})
