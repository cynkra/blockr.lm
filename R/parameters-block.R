#' Parameters Block
#'
#' Wraps [parameters::model_parameters()] as a transform block. Renders a
#' publication-style coefficient table with categorical level breakouts,
#' proper variable labels, and formatted CIs.
#'
#' @param ci Numeric. Confidence level (default 0.95).
#' @param exponentiate Logical. Exponentiate coefficients (for GLM odds ratios).
#' @param standardize Standardization method passed to
#'   [parameters::model_parameters()]. Default "none".
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A `parameters_block` transform block.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   m <- lm(Sepal.Length ~ Petal.Length, data = iris)
#'   serve(new_parameters_block(), list(data = m))
#' }
#'
#' @export
new_parameters_block <- function(
  ci = 0.95,
  exponentiate = FALSE,
  standardize = "none",
  ...
) {
  std_choices <- c("none", "basic", "posthoc", "smart", "pseudo", "sdy")

  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_ci <- reactiveVal(ci)
        r_exp <- reactiveVal(exponentiate)
        r_std <- reactiveVal(standardize)

        observeEvent(input$ci, r_ci(input$ci))
        observeEvent(input$exponentiate, r_exp(input$exponentiate))
        observeEvent(input$standardize, r_std(input$standardize))

        list(
          expr = reactive({
            ci_val <- r_ci()
            exp_val <- r_exp()
            std_val <- r_std()
            std_arg <- if (identical(std_val, "none")) {
              "NULL"
            } else {
              paste0("'", std_val, "'")
            }
            expr_text <- glue::glue(
              "tibble::as_tibble(parameters::model_parameters(",
              "data, ci = {ci_val}, exponentiate = {exp_val}, ",
              "standardize = {std_arg}))"
            )
            parse(text = expr_text)[[1]]
          }),
          state = list(
            ci = r_ci,
            exponentiate = r_exp,
            standardize = r_std
          )
        )
      })
    },
    ui = function(id) {
      tagList(
        div(
          class = "block-container",
          sliderInput(
            NS(id, "ci"),
            label = "Confidence level",
            min = 0.80, max = 0.99, value = ci, step = 0.01
          ),
          checkboxInput(
            NS(id, "exponentiate"),
            label = "Exponentiate (odds ratios for GLM)",
            value = exponentiate
          ),
          selectInput(
            NS(id, "standardize"),
            label = "Standardization",
            choices = std_choices,
            selected = standardize
          )
        )
      )
    },
    class = "parameters_block",
    ...
  )
}

#' @export
block_output.parameters_block <- function(x, result, session) {
  renderUI({
    if (is.null(result) || nrow(result) == 0) {
      return(htmltools::tags$em("No model - wire up a model block upstream."))
    }
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(parameters_block_css())),
      htmltools::HTML(format_easystats_table(result, "parameters-block-table"))
    )
  })
}

#' @export
block_ui.parameters_block <- function(id, x, ...) {
  tagList(uiOutput(NS(id, "result")))
}

#' Shared CSS for easystats-style tables
#' @noRd
parameters_block_css <- function() {
  ".parameters-block-table, .easystats-block-table {
     width: 100%;
     border-collapse: collapse;
     font-size: 13px;
     font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif;
   }
   .parameters-block-table th, .easystats-block-table th {
     text-align: left;
     padding: 6px 10px;
     border-bottom: 1.5px solid #333;
     font-weight: 600;
     background: #fafafa;
   }
   .parameters-block-table td, .easystats-block-table td {
     padding: 5px 10px;
     border-bottom: 1px solid #eee;
   }
   .parameters-block-table tr:last-child td,
   .easystats-block-table tr:last-child td {
     border-bottom: 1.5px solid #333;
   }
   .parameters-block-table td:not(:first-child),
   .easystats-block-table td:not(:first-child) {
     text-align: right;
     font-variant-numeric: tabular-nums;
   }"
}

#' Format an easystats result tibble as an HTML table
#' @noRd
format_easystats_table <- function(df, css_class = "easystats-block-table") {
  knitr::kable(
    df,
    format = "html",
    digits = 3,
    table.attr = sprintf('class="%s"', css_class)
  )
}
