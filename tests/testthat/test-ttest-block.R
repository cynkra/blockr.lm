test_that("ttest block produces an htest object on independent samples", {
  block <- new_ttest_block(
    test_type = "independent",
    dv = "yA", group = "fA"
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "htest")
      expect_true(grepl("t-test", result$method))
    },
    args = list(x = block, data = list(data = function() .tdf_a()))
  )
})

test_that("ttest block clears stale column state on data swap", {
  data_rv <- shiny::reactiveVal(.tdf_a())
  block <- new_ttest_block(
    test_type = "independent",
    dv = "yA", group = "fA", pair = "xA1"
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_s3_class(session$returned$result(), "htest")

      data_rv(.tdf_b())
      session$flushReact()
      st <- session$returned$state
      cb <- colnames(.tdf_b())
      expect_true(length(st$dv()) == 0L || all(st$dv() %in% cb))
      expect_true(length(st$group()) == 0L || all(st$group() %in% cb))
      expect_true(length(st$pair()) == 0L || all(st$pair() %in% cb))
    },
    args = list(x = block, data = list(data = function() data_rv()))
  )
})
