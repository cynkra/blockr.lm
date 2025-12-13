#' Coefficients Table Block
#'
#' Extract model coefficients from a linear model object using [broom::tidy()].
#' Shows term names, estimates, standard errors, t-statistics, and p-values.
#' Optionally includes confidence intervals.
#'
#' @param conf_int Logical. Include confidence intervals (default TRUE)
#' @param conf_level Numeric. Confidence level for intervals (default 0.95)
#' @param ... Forwarded to [new_transform_block()]
#'
#' @return A transform block object of class `coef_block` that outputs a data frame
#'   with columns: term, estimate, std.error, statistic, p.value, and optionally
#'   conf.low, conf.high.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   model <- lm(mpg ~ cyl + hp, data = mtcars)
#'   serve(new_coef_block(), list(data = model))
#' }
#'
#' @export
new_coef_block <- function(
  conf_int = TRUE,
  conf_level = 0.95,
  ...
) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Initialize reactive values
          r_conf_int <- reactiveVal(conf_int)
          r_conf_level <- reactiveVal(conf_level)

          # Update reactive values when inputs change
          observeEvent(input$conf_int, {
            r_conf_int(input$conf_int)
          })

          observeEvent(input$conf_level, {
            r_conf_level(as.numeric(input$conf_level))
          })

          list(
            expr = reactive({
              incl_ci <- r_conf_int()
              ci_level <- r_conf_level()

              if (incl_ci) {
                expr_text <- glue::glue(
                  "broom::tidy(data, conf.int = TRUE, conf.level = {ci_level})"
                )
              } else {
                expr_text <- "broom::tidy(data, conf.int = FALSE)"
              }

              parse(text = expr_text)[[1]]
            }),
            state = list(
              conf_int = r_conf_int,
              conf_level = r_conf_level
            )
          )
        }
      )
    },
    ui = function(id) {
      tagList(
        shinyjs::useShinyjs(),

        # CSS utilities
        css_responsive_grid(),
        css_single_column("coef"),

        # Block-specific CSS
        tags$style(HTML(
          "
          .coef-block-container .checkbox {
            margin-top: 0;
            margin-bottom: 0;
          }
          "
        )),

        div(
          class = "block-container coef-block-container",

          div(
            class = "block-form-grid",

            div(
              class = "block-section",
              div(
                class = "block-section-grid",

                # Help text
                div(
                  class = "block-help-text",
                  "Model coefficients with standard errors, t-statistics, and p-values."
                ),

                # Confidence interval checkbox
                div(
                  class = "block-input-wrapper",
                  checkboxInput(
                    NS(id, "conf_int"),
                    label = "Include confidence intervals",
                    value = conf_int
                  )
                ),

                # Confidence level
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    NS(id, "conf_level"),
                    label = "Confidence level",
                    choices = c("0.90" = "0.90", "0.95" = "0.95", "0.99" = "0.99"),
                    selected = as.character(conf_level),
                    width = "100%"
                  )
                )
              )
            )
          )
        )
      )
    },
    dat_valid = function(data) {
      stopifnot(inherits(data, "lm"))
    },
    class = "coef_block",
    ...
  )
}
