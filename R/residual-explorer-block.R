#' Residual Explorer Block
#'
#' Interactive residual exploration using plotly. Allows users to explore
#' the relationship between fitted values and residuals, with the ability
#' to color points by a variable from the original data and identify
#' observations by clicking.
#'
#' @param x_var What to plot on x-axis: "fitted" (fitted values) or "index" (observation number)
#' @param y_var What to plot on y-axis: "residuals", "standardized", or "studentized"
#' @param ... Forwarded to [new_block()]
#'
#' @return A block object of class `residual_explorer_block` that outputs an
#'   interactive plotly plot.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   model <- lm(mpg ~ cyl + hp, data = mtcars)
#'   serve(new_residual_explorer_block(), list(data = model))
#' }
#'
#' @export
new_residual_explorer_block <- function(
  x_var = "fitted",
  y_var = "residuals",
  ...
) {
  blockr.core::new_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Initialize reactive values
          r_x_var <- reactiveVal(x_var)
          r_y_var <- reactiveVal(y_var)

          # Update reactive values when inputs change
          observeEvent(input$x_var, {
            r_x_var(input$x_var)
          })

          observeEvent(input$y_var, {
            r_y_var(input$y_var)
          })

          list(
            expr = reactive({
              x <- r_x_var()
              y <- r_y_var()

              expr_text <- glue::glue(
                "blockr.lm:::create_residual_plot(data, x_var = '{x}', y_var = '{y}')"
              )
              parse(text = expr_text)[[1]]
            }),
            state = list(
              x_var = r_x_var,
              y_var = r_y_var
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        shinyjs::useShinyjs(),

        # CSS utilities
        css_responsive_grid(),
        css_single_column("residual-explorer"),

        div(
          class = "block-container residual-explorer-block-container",

          div(
            class = "block-form-grid",

            div(
              class = "block-section",
              div(
                class = "block-section-grid",

                # Help text
                div(
                  class = "block-help-text",
                  "Interactive residual plot. Hover over points to see details. ",
                  "Use the toolbar to zoom, pan, or save the plot."
                ),

                # X-axis variable
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    NS(id, "x_var"),
                    label = "X-axis",
                    choices = c(
                      "Fitted Values" = "fitted",
                      "Observation Index" = "index"
                    ),
                    selected = x_var,
                    width = "100%"
                  )
                ),

                # Y-axis variable
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    NS(id, "y_var"),
                    label = "Y-axis",
                    choices = c(
                      "Residuals" = "residuals",
                      "Standardized Residuals" = "standardized",
                      "Studentized Residuals" = "studentized"
                    ),
                    selected = y_var,
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
    class = "residual_explorer_block",
    ...
  )
}

#' Create interactive residual plot
#'
#' Internal helper function that creates the plotly residual plot.
#'
#' @param model An lm model object
#' @param x_var What to plot on x-axis: "fitted" or "index"
#' @param y_var What to plot on y-axis: "residuals", "standardized", or "studentized"
#' @return A plotly object
#' @keywords internal
create_residual_plot <- function(model, x_var = "fitted", y_var = "residuals") {
  # Extract data from model
  fitted_vals <- fitted(model)
  resid_vals <- residuals(model)

  # Get observation names/indices
  obs_names <- names(fitted_vals)
  if (is.null(obs_names)) {
    obs_names <- seq_along(fitted_vals)
  }

  # Calculate x values
  x_vals <- if (x_var == "fitted") {
    fitted_vals
  } else {
    seq_along(fitted_vals)
  }

  x_label <- if (x_var == "fitted") "Fitted Values" else "Observation Index"

  # Calculate y values
  y_vals <- switch(
    y_var,
    "residuals" = resid_vals,
    "standardized" = rstandard(model),
    "studentized" = rstudent(model),
    resid_vals
  )

  y_label <- switch(
    y_var,
    "residuals" = "Residuals",
    "standardized" = "Standardized Residuals",
    "studentized" = "Studentized Residuals",
    "Residuals"
  )

  # Create data frame for plotting
  plot_data <- data.frame(
    x = x_vals,
    y = y_vals,
    obs = obs_names,
    fitted = fitted_vals,
    residual = resid_vals,
    stringsAsFactors = FALSE
  )

  # Create plotly scatter plot
  p <- plotly::plot_ly(
    data = plot_data,
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "markers",
    text = ~paste0(
      "Obs: ", obs, "<br>",
      "Fitted: ", round(fitted, 3), "<br>",
      "Residual: ", round(residual, 3)
    ),
    hoverinfo = "text",
    marker = list(
      size = 8,
      color = "#3498db",
      opacity = 0.7
    )
  )

  # Add horizontal line at y = 0
  p <- plotly::layout(
    p,
    xaxis = list(title = x_label),
    yaxis = list(title = y_label),
    shapes = list(
      list(
        type = "line",
        x0 = min(x_vals),
        x1 = max(x_vals),
        y0 = 0,
        y1 = 0,
        line = list(color = "#e74c3c", dash = "dash", width = 1)
      )
    ),
    hovermode = "closest"
  )

  # Configure toolbar
  p <- plotly::config(
    p,
    displayModeBar = TRUE,
    modeBarButtonsToRemove = c(
      "select2d", "lasso2d", "autoScale2d",
      "hoverClosestCartesian", "hoverCompareCartesian"
    )
  )

  p
}

#' Custom block output for residual_explorer_block
#'
#' @export
block_output.residual_explorer_block <- function(x, result, session) {
  plotly::renderPlotly(result)
}

#' Custom block UI for residual_explorer_block
#'
#' @export
block_ui.residual_explorer_block <- function(id, x, ...) {
  tagList(
    plotly::plotlyOutput(NS(id, "result"), height = "400px")
  )
}
