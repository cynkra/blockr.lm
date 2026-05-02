#' Standardize Parameters Block
#'
#' Wraps [effectsize::standardize_parameters()] as a transform block. Reports
#' standardized regression coefficients (beta) with confidence intervals.
#'
#' @param method Standardization method: "refit", "posthoc", "basic", "smart",
#'   "pseudo".
#' @param ci Numeric. Confidence level (default 0.95).
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A `standardize_block` transform block.
#'
#' @export
new_standardize_block <- function(
  method = "refit",
  ci = 0.95,
  ...
) {
  method_choices <- c("refit", "posthoc", "basic", "smart", "pseudo")

  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_method <- reactiveVal(method)
        r_ci <- reactiveVal(ci)

        observeEvent(input$method, r_method(input$method))
        observeEvent(input$ci, r_ci(input$ci))

        list(
          expr = reactive({
            mthd <- r_method()
            ci_val <- r_ci()
            expr_text <- glue::glue(
              "tibble::as_tibble(effectsize::standardize_parameters(",
              "data, method = '{mthd}', ci = {ci_val}))"
            )
            parse(text = expr_text)[[1]]
          }),
          state = list(method = r_method, ci = r_ci)
        )
      })
    },
    ui = function(id) {
      tagList(
        div(
          class = "block-container",
          selectInput(
            NS(id, "method"),
            label = "Standardization method",
            choices = method_choices,
            selected = method
          ),
          sliderInput(
            NS(id, "ci"),
            label = "Confidence level",
            min = 0.80, max = 0.99, value = ci, step = 0.01
          )
        )
      )
    },
    class = "standardize_block",
    ...
  )
}

#' @export
block_output.standardize_block <- function(x, result, session) {
  renderUI({
    if (is.null(result) || nrow(result) == 0) {
      return(htmltools::tags$em("No model - wire up a model block upstream."))
    }
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(parameters_block_css())),
      htmltools::HTML(format_easystats_table(result))
    )
  })
}

#' @export
block_ui.standardize_block <- function(id, x, ...) {
  tagList(uiOutput(NS(id, "result")))
}
