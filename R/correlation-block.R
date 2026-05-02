#' Correlation Block
#'
#' Wraps [correlation::correlation()] as a transform block. Emits a tidy
#' long-format tibble of pairwise correlations with significance tests.
#'
#' @param vars Numeric column names. Must select at least two.
#' @param method Correlation method. One of "pearson", "spearman", "kendall".
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A `correlation_block` transform block.
#'
#' @export
new_correlation_block <- function(
  vars = character(),
  method = "pearson",
  ...
) {
  method_choices <- c("pearson", "spearman", "kendall")

  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_vars <- reactiveVal(vars)
        r_method <- reactiveVal(method)
        r_initialized <- reactiveVal(FALSE)

        observeEvent(input$vars, r_vars(input$vars))
        observeEvent(input$method, r_method(input$method))

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
            mthd <- r_method()
            if (length(v) < 2) return(quote(NULL))
            cols_str <- paste0("c(", paste0("'", v, "'", collapse = ", "), ")")
            expr_text <- glue::glue(
              "tibble::as_tibble(correlation::correlation(",
              "data[, {cols_str}, drop = FALSE], method = '{mthd}'))"
            )
            parse(text = expr_text)[[1]]
          }),
          state = list(vars = r_vars, method = r_method)
        )
      })
    },
    ui = function(id) {
      tagList(
        div(
          class = "block-container",
          selectizeInput(
            NS(id, "vars"),
            label = "Variables (pick at least 2)",
            choices = vars,
            selected = vars,
            multiple = TRUE,
            width = "100%",
            options = list(
              plugins = list("drag_drop", "remove_button"),
              placeholder = "Pick numeric variables..."
            )
          ),
          radioButtons(
            NS(id, "method"),
            label = "Method",
            choices = method_choices,
            selected = method,
            inline = TRUE
          )
        )
      )
    },
    class = "correlation_block",
    allow_empty_state = "vars",
    ...
  )
}

#' @export
block_output.correlation_block <- function(x, result, session) {
  renderUI({
    if (is.null(result) || nrow(result) == 0) {
      return(htmltools::tags$em(
        "Pick at least 2 numeric variables to compute correlations."
      ))
    }
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(parameters_block_css())),
      htmltools::HTML(format_easystats_table(result))
    )
  })
}

#' @export
block_ui.correlation_block <- function(id, x, ...) {
  tagList(uiOutput(NS(id, "result")))
}
