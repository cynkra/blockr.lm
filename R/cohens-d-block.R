#' Cohen's d Block
#'
#' Computes a standardized mean difference (Cohen's d) from a t-test result
#' (`htest` object). Uses [effectsize::effectsize()] which dispatches to the
#' appropriate effect-size method.
#'
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A `cohens_d_block` transform block.
#'
#' @export
new_cohens_d_block <- function(...) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        list(
          expr = reactive({
            quote(
              tibble::as_tibble(
                suppressWarnings(effectsize::effectsize(data))
              )
            )
          }),
          state = list()
        )
      })
    },
    ui = function(id) tagList(),
    class = "cohens_d_block",
    ...
  )
}

#' @export
block_output.cohens_d_block <- function(x, result, session) {
  renderUI({
    if (is.null(result) || nrow(result) == 0) {
      return(htmltools::tags$em("No t-test result upstream."))
    }
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(parameters_block_css())),
      htmltools::HTML(format_easystats_table(result))
    )
  })
}

#' @export
block_ui.cohens_d_block <- function(id, x, ...) {
  tagList(uiOutput(NS(id, "result")))
}
