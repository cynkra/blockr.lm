#' Coefficient Plot Block
#'
#' Create a dot-and-whisker plot of model coefficients using
#' [modelsummary::modelplot()]. Shows point estimates with confidence intervals.
#'
#' @param conf_level Confidence level for intervals (default 0.95)
#' @param exponentiate Logical. Exponentiate coefficients (for odds ratios, etc.)
#' @param omit_intercept Logical. Omit intercept from plot (default TRUE)
#' @param ... Forwarded to [new_plot_block()]
#'
#' @return A plot block object of class \code{coefplot_block}.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   model <- lm(mpg ~ cyl + hp + wt, data = mtcars)
#'   serve(new_coefplot_block(), list(data = model))
#' }
#'
#' @export
new_coefplot_block <- function(
  conf_level = 0.95,
  exponentiate = FALSE,
  omit_intercept = TRUE,
  ...
) {
  new_plot_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Initialize reactive values
          r_conf_level <- reactiveVal(conf_level)
          r_exponentiate <- reactiveVal(exponentiate)
          r_omit_intercept <- reactiveVal(omit_intercept)

          # Update reactive values when inputs change
          observeEvent(input$conf_level, {
            r_conf_level(as.numeric(input$conf_level))
          })

          observeEvent(input$exponentiate, {
            r_exponentiate(input$exponentiate)
          })

          observeEvent(input$omit_intercept, {
            r_omit_intercept(input$omit_intercept)
          })

          list(
            expr = reactive({
              conf <- r_conf_level()
              exp <- r_exponentiate()
              omit <- r_omit_intercept()

              # Build expression
              if (omit) {
                coef_omit <- "coef_omit = \"(Intercept)\""
              } else {
                coef_omit <- "coef_omit = NULL"
              }

              expr_text <- glue::glue(
                "modelsummary::modelplot(data, conf_level = {conf}, ",
                "exponentiate = {exp}, {coef_omit})"
              )
              parse(text = expr_text)[[1]]
            }),
            state = list(
              conf_level = r_conf_level,
              exponentiate = r_exponentiate,
              omit_intercept = r_omit_intercept
            )
          )
        }
      )
    },
    ui = function(id) {
      tagList(
        css_responsive_grid(),
        css_single_column("coefplot"),

        div(
          class = "block-container coefplot-block-container",

          div(
            class = "block-form-grid",

            div(
              class = "block-section",
              div(
                class = "block-section-grid",

                # Help text
                div(
                  class = "block-help-text",
                  "Coefficient plot showing point estimates with confidence intervals."
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
                ),

                # Exponentiate checkbox
                div(
                  class = "block-input-wrapper",
                  checkboxInput(
                    NS(id, "exponentiate"),
                    label = "Exponentiate (odds ratios)",
                    value = exponentiate
                  )
                ),

                # Omit intercept checkbox
                div(
                  class = "block-input-wrapper",
                  checkboxInput(
                    NS(id, "omit_intercept"),
                    label = "Omit intercept",
                    value = omit_intercept
                  )
                )
              )
            )
          )
        )
      )
    },
    class = "coefplot_block",
    ...
  )
}
