#' Effect Size Block
#'
#' Wraps [effectsize::eta_squared()] (and its variants) as a transform block.
#' Computes effect-size measures from an ANOVA decomposition of the model.
#'
#' @param type Effect size type. One of "eta", "omega", "epsilon".
#' @param partial Logical. Compute partial effect sizes (default TRUE).
#' @param ci Numeric. Confidence level (default 0.95).
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return An `effectsize_block` transform block.
#'
#' @export
new_effectsize_block <- function(
  type = "eta",
  partial = TRUE,
  ci = 0.95,
  ...
) {
  type_choices <- c("Eta-squared" = "eta", "Omega-squared" = "omega",
    "Epsilon-squared" = "epsilon")

  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_type <- reactiveVal(type)
        r_partial <- reactiveVal(partial)
        r_ci <- reactiveVal(ci)

        observeEvent(input$type, r_type(input$type))
        observeEvent(input$partial, r_partial(input$partial))
        observeEvent(input$ci, r_ci(input$ci))

        list(
          expr = reactive({
            t <- r_type()
            partial_val <- r_partial()
            ci_val <- r_ci()
            fn <- switch(t,
              "eta" = "eta_squared",
              "omega" = "omega_squared",
              "epsilon" = "epsilon_squared",
              "eta_squared")
            expr_text <- glue::glue(
              "tibble::as_tibble(effectsize::{fn}(",
              "data, partial = {partial_val}, ci = {ci_val}))"
            )
            parse(text = expr_text)[[1]]
          }),
          state = list(type = r_type, partial = r_partial, ci = r_ci)
        )
      })
    },
    ui = function(id) {
      tagList(
        div(
          class = "block-container",
          selectInput(
            NS(id, "type"),
            label = "Effect size type",
            choices = type_choices,
            selected = type
          ),
          checkboxInput(
            NS(id, "partial"),
            label = "Partial",
            value = partial
          ),
          sliderInput(
            NS(id, "ci"),
            label = "Confidence level",
            min = 0.80, max = 0.99, value = ci, step = 0.01
          )
        )
      )
    },
    class = "effectsize_block",
    ...
  )
}

#' @export
block_output.effectsize_block <- function(x, result, session) {
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
block_ui.effectsize_block <- function(id, x, ...) {
  tagList(uiOutput(NS(id, "result")))
}
