test_that("frequencies block produces a long tibble of counts", {
  block <- new_frequencies_block(vars = c("fA", "fA3"))
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "tbl_df")
      # 2 levels for fA + 3 for fA3 = 5 rows
      expect_equal(nrow(result), 5L)
      expect_true(all(c("variable", "level", "n", "proportion") %in% names(result)))
    },
    args = list(x = block, data = list(data = function() .tdf_a()))
  )
})

test_that("frequencies block clears stale column state on data swap", {
  data_rv <- shiny::reactiveVal(.tdf_a())
  block <- new_frequencies_block(vars = c("fA", "fA3"))
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_equal(nrow(session$returned$result()), 5L)

      data_rv(.tdf_b())
      session$flushReact()
      expect_true(all(session$returned$state$vars() %in% colnames(.tdf_b())))
    },
    args = list(x = block, data = list(data = function() data_rv()))
  )
})
