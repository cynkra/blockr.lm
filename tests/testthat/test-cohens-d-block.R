test_that("cohens_d block produces a tibble with d and CI from a t-test", {
  d <- .tdf_a()
  tt <- t.test(yA ~ fA, data = d)
  block <- new_cohens_d_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 1L)
      expect_true(any(grepl("^d$|^Cohens", names(result))))
    },
    args = list(x = block, data = list(data = function() tt))
  )
})
