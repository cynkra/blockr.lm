test_that("normality check block produces a per-variable tibble with decision flag", {
  block <- new_normality_check_block(vars = c("yA", "xA1"))
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 2L)
      expect_true(all(c("variable", "test", "p", "decision") %in% names(result)))
    },
    args = list(x = block, data = list(data = function() .tdf_a()))
  )
})

test_that("normality check block runs per group when group is set", {
  block <- new_normality_check_block(vars = "yA", group = "fA")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      # one row per group level (fA has 2 levels)
      expect_equal(nrow(result), 2L)
    },
    args = list(x = block, data = list(data = function() .tdf_a()))
  )
})

test_that("normality check block clears stale column state on data swap", {
  data_rv <- shiny::reactiveVal(.tdf_a())
  block <- new_normality_check_block(vars = c("yA", "xA1"), group = "fA")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_equal(nrow(session$returned$result()), 4L)  # 2 vars x 2 groups

      data_rv(.tdf_b())
      session$flushReact()
      expect_true(all(session$returned$state$vars() %in% colnames(.tdf_b())))
      expect_true(all(session$returned$state$group() %in% colnames(.tdf_b())))
    },
    args = list(x = block, data = list(data = function() data_rv()))
  )
})
