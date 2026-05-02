test_that("homogeneity check block produces a single-row tibble with decision flag", {
  block <- new_homogeneity_check_block(dv = "yA", group = "fA")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 1L)
      expect_true(all(c("test", "statistic", "df", "p", "decision") %in% names(result)))
    },
    args = list(x = block, data = list(data = function() .tdf_a()))
  )
})

test_that("homogeneity check block clears stale column state on data swap", {
  data_rv <- shiny::reactiveVal(.tdf_a())
  block <- new_homogeneity_check_block(dv = "yA", group = "fA")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_equal(nrow(session$returned$result()), 1L)

      data_rv(.tdf_b())
      session$flushReact()
      st <- session$returned$state
      cb <- colnames(.tdf_b())
      expect_true(length(st$dv()) == 0L || all(st$dv() %in% cb))
      expect_true(length(st$group()) == 0L || all(st$group() %in% cb))
    },
    args = list(x = block, data = list(data = function() data_rv()))
  )
})
