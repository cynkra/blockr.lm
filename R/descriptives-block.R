#' Descriptives Block
#'
#' Wraps [parameters::describe_distribution()] as a transform block. Emits a
#' tidy tibble with one row per selected variable: mean, SD, IQR, range,
#' skewness, kurtosis, n, n_missing.
#'
#' @param vars Numeric column names to summarise.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A `descriptives_block` transform block.
#'
#' @export
new_descriptives_block <- function(
  vars = character(),
  ...
) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_vars <- reactiveVal(vars)
        r_initialized <- reactiveVal(FALSE)

        observeEvent(input$vars, r_vars(input$vars))

        observe({
          if (!r_initialized() && length(colnames(data())) > 0) {
            d <- data()
            num_cols <- colnames(d)[vapply(d, is.numeric, logical(1))]
            updateSelectizeInput(session, "vars",
              choices = num_cols, selected = r_vars())
            r_initialized(TRUE)
          }
        })

        observeEvent(colnames(data()), {
          if (r_initialized()) {
            req(data())
            d <- data()
            num_cols <- colnames(d)[vapply(d, is.numeric, logical(1))]
            new_vars <- intersect(r_vars(), num_cols)
            r_vars(new_vars)
            updateSelectizeInput(session, "vars",
              choices = num_cols, selected = new_vars)
          }
        }, ignoreNULL = FALSE)

        list(
          expr = reactive({
            v <- r_vars()
            v <- v[nzchar(v)]
            if (length(v) == 0) return(quote(NULL))
            cols_str <- paste0("c(", paste0("'", v, "'", collapse = ", "), ")")
            expr_text <- glue::glue(
              "tibble::as_tibble(parameters::describe_distribution(",
              "data[, {cols_str}, drop = FALSE]))"
            )
            parse(text = expr_text)[[1]]
          }),
          state = list(vars = r_vars)
        )
      })
    },
    ui = function(id) {
      tagList(
        div(
          class = "block-container",
          selectizeInput(
            NS(id, "vars"),
            label = "Variables",
            choices = vars,
            selected = vars,
            multiple = TRUE,
            width = "100%",
            options = list(
              plugins = list("drag_drop", "remove_button"),
              placeholder = "Pick numeric variables..."
            )
          )
        )
      )
    },
    class = "descriptives_block",
    allow_empty_state = "vars",
    ...
  )
}

#' @export
block_output.descriptives_block <- function(x, result, session) {
  renderUI({
    if (is.null(result) || nrow(result) == 0) {
      return(htmltools::tags$em("Pick variables to compute descriptives."))
    }
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(parameters_block_css())),
      htmltools::HTML(format_easystats_table(result))
    )
  })
}

#' @export
block_ui.descriptives_block <- function(id, x, ...) {
  tagList(uiOutput(NS(id, "result")))
}
