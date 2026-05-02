test_that("descriptives block produces a tibble with one row per variable", {
  block <- new_descriptives_block(vars = c("yA", "xA1"))
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 2)
      expect_true("Variable" %in% names(result))
    },
    args = list(x = block, data = list(data = function() .tdf_a()))
  )
})

test_that("descriptives block clears stale column state on data swap", {
  data_rv <- shiny::reactiveVal(.tdf_a())
  block <- new_descriptives_block(vars = c("yA", "xA1"))
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_equal(nrow(session$returned$result()), 2)

      # Swap to a dataset with no overlapping numeric columns. State must
      # drop yA / xA1 so they don't leak into the next expression eval.
      data_rv(.tdf_b())
      session$flushReact()
      state_vars <- session$returned$state$vars()
      expect_true(all(state_vars %in% colnames(.tdf_b())))
    },
    args = list(x = block, data = list(data = function() data_rv()))
  )
})
