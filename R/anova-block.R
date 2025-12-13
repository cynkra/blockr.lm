#' ANOVA Table Block
#'
#' Generate an ANOVA table from a linear model object. Shows the decomposition
#' of variance for each model term with degrees of freedom, sum of squares,
#' mean squares, F-statistics, and p-values.
#'
#' @param ... Forwarded to [new_transform_block()]
#'
#' @return A transform block object of class `anova_block` that outputs a data frame
#'   with columns: term, df, sumsq, meansq, statistic, p.value.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   model <- lm(mpg ~ cyl + hp, data = mtcars)
#'   serve(new_anova_block(), list(data = model))
#' }
#'
#' @export
new_anova_block <- function(...) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive({
              parse(text = "broom::tidy(stats::anova(data))")[[1]]
            }),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      tagList(
        shinyjs::useShinyjs(),

        # CSS utilities
        css_responsive_grid(),
        css_single_column("anova"),

        div(
          class = "block-container anova-block-container",

          div(
            class = "block-form-grid",

            div(
              class = "block-section",
              div(
                class = "block-section-grid",

                # Help text
                div(
                  class = "block-help-text",
                  "ANOVA table showing the decomposition of variance. ",
                  "Tests whether each predictor significantly reduces residual variance."
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
    class = "anova_block",
    ...
  )
}
