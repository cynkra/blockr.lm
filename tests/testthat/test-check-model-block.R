test_that("check_model block produces evaluated plot output", {
  m <- lm(yA ~ xA1 + xA2, data = .tdf_a())
  block <- new_check_model_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      # check_model returns a plot via evaluate::evaluate inside the
      # plot block; result is the captured evaluation output
      expect_false(is.null(result))
    },
    args = list(x = block, data = list(data = function() m))
  )
})
