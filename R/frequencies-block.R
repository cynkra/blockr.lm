#' Frequencies Block
#'
#' Computes frequency counts and proportions for one or more categorical
#' columns. Emits a long-format tibble with columns: variable, level, n,
#' proportion.
#'
#' @param vars Categorical column names.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A `frequencies_block` transform block.
#'
#' @export
new_frequencies_block <- function(
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
            cat_cols <- colnames(d)[vapply(
              d, function(x) is.factor(x) || is.character(x), logical(1)
            )]
            updateSelectizeInput(session, "vars",
              choices = cat_cols, selected = r_vars())
            r_initialized(TRUE)
          }
        })

        observeEvent(colnames(data()), {
          if (r_initialized()) {
            req(data())
            d <- data()
            cat_cols <- colnames(d)[vapply(
              d, function(x) is.factor(x) || is.character(x), logical(1)
            )]
            updateSelectizeInput(session, "vars",
              choices = cat_cols, selected = r_vars())
          }
        }, ignoreNULL = FALSE)

        list(
          expr = reactive({
            v <- r_vars()
            v <- v[nzchar(v)]
            if (length(v) == 0) return(quote(NULL))
            cols_str <- paste0("c(", paste0("'", v, "'", collapse = ", "), ")")
            expr_text <- glue::glue(
              "do.call(rbind, lapply({cols_str}, function(col) {{",
              "  tab <- table(data[[col]], useNA = 'ifany');",
              "  tibble::tibble(",
              "    variable = col,",
              "    level = names(tab),",
              "    n = as.integer(tab),",
              "    proportion = as.numeric(tab) / sum(tab)",
              "  )",
              "}}))"
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
            label = "Categorical variables",
            choices = vars,
            selected = vars,
            multiple = TRUE,
            width = "100%",
            options = list(
              plugins = list("drag_drop", "remove_button"),
              placeholder = "Pick categorical variables..."
            )
          )
        )
      )
    },
    class = "frequencies_block",
    allow_empty_state = "vars",
    ...
  )
}

#' @export
block_output.frequencies_block <- function(x, result, session) {
  renderUI({
    if (is.null(result) || nrow(result) == 0) {
      return(htmltools::tags$em("Pick categorical variables."))
    }
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(parameters_block_css())),
      htmltools::HTML(format_easystats_table(result))
    )
  })
}

#' @export
block_ui.frequencies_block <- function(id, x, ...) {
  tagList(uiOutput(NS(id, "result")))
}
