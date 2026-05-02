#' Performance Block
#'
#' Wraps [performance::model_performance()] as a transform block. Renders a
#' compact one-row model fit headline (R^2, AIC, BIC, RMSE, Sigma - and
#' family-appropriate variants for GLM and mixed models).
#'
#' @param metrics Character vector of metrics to display, or "all". Forwarded
#'   to `performance::model_performance(metrics = ...)`.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A `performance_block` transform block.
#'
#' @export
new_performance_block <- function(
  metrics = "all",
  ...
) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_metrics <- reactiveVal(metrics)
        observeEvent(input$metrics, r_metrics(input$metrics))

        list(
          expr = reactive({
            m <- r_metrics()
            metrics_str <- if (length(m) == 1 && m == "all") {
              "'all'"
            } else {
              paste0("c(", paste0("'", m, "'", collapse = ", "), ")")
            }
            expr_text <- glue::glue(
              "tibble::as_tibble(performance::model_performance(",
              "data, metrics = {metrics_str}))"
            )
            parse(text = expr_text)[[1]]
          }),
          state = list(metrics = r_metrics)
        )
      })
    },
    ui = function(id) {
      tagList(
        div(
          class = "block-container",
          selectInput(
            NS(id, "metrics"),
            label = "Metrics",
            choices = c("all", "common", "minimum"),
            selected = if (length(metrics) == 1) metrics else "all"
          )
        )
      )
    },
    class = "performance_block",
    ...
  )
}

#' @export
block_output.performance_block <- function(x, result, session) {
  renderUI({
    if (is.null(result) || nrow(result) == 0) {
      return(htmltools::tags$em("No model - wire up a model block upstream."))
    }
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(parameters_block_css())),
      htmltools::tags$div(
        class = "easystats-headline",
        style = "padding:10px 0; font-size: 11px; text-transform: uppercase; letter-spacing: 0.6px; color: #888;",
        "Model fit"
      ),
      htmltools::HTML(format_easystats_table(result))
    )
  })
}

#' @export
block_ui.performance_block <- function(id, x, ...) {
  tagList(uiOutput(NS(id, "result")))
}
