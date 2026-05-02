test_that("report block produces a single-column tibble of narrative text on lm", {
  m <- lm(yA ~ xA1 + xA2, data = .tdf_a())
  block <- new_report_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "tbl_df")
      expect_equal(names(result), "text")
      expect_true(nchar(result$text[[1]]) > 50L)
      expect_true(grepl("linear model", result$text[[1]], ignore.case = TRUE))
    },
    args = list(x = block, data = list(data = function() m))
  )
})

test_that("report block also handles t-test htest objects", {
  d <- .tdf_a()
  tt <- t.test(yA ~ fA, data = d)
  block <- new_report_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "tbl_df")
      expect_true(nchar(result$text[[1]]) > 50L)
    },
    args = list(x = block, data = list(data = function() tt))
  )
})

test_that("report block also handles raw data.frame input", {
  block <- new_report_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "tbl_df")
      expect_true(grepl("observations", result$text[[1]], ignore.case = TRUE))
    },
    args = list(x = block, data = list(data = function() .tdf_a()))
  )
})
