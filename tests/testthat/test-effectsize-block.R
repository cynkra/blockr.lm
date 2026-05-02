test_that("effectsize block emits eta-squared per term", {
  m <- lm(yA ~ xA1 + xA2 + fA3, data = .tdf_a())
  block <- new_effectsize_block(type = "eta", partial = TRUE)
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "tbl_df")
      expect_true(nrow(result) >= 1L)
      expect_true("Parameter" %in% names(result))
    },
    args = list(x = block, data = list(data = function() m))
  )
})
