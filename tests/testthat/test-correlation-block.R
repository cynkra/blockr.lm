test_that("correlation block produces a long-format pairwise tibble", {
  block <- new_correlation_block(vars = c("yA", "xA1", "xA2"))
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 3L)  # 3 numeric vars -> 3 pairs
      expect_true(all(c("Parameter1", "Parameter2", "r") %in% names(result)))
    },
    args = list(x = block, data = list(data = function() .tdf_a()))
  )
})

test_that("correlation block clears stale column state on data swap", {
  data_rv <- shiny::reactiveVal(.tdf_a())
  block <- new_correlation_block(vars = c("yA", "xA1", "xA2"))
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_equal(nrow(session$returned$result()), 3L)

      data_rv(.tdf_b())
      session$flushReact()
      expect_true(all(session$returned$state$vars() %in% colnames(.tdf_b())))
    },
    args = list(x = block, data = list(data = function() data_rv()))
  )
})
