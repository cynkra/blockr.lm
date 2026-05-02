#' Nonparametric Test Block
#'
#' Wilcoxon-family alternatives to the t-test: Wilcoxon signed-rank
#' (one-sample / paired) and Mann-Whitney U (independent two-sample).
#' Mirrors `new_ttest_block`'s adaptive type-picker pattern.
#'
#' @param test_type One of "one_sample", "paired", "independent".
#' @param dv Numeric column name.
#' @param group Categorical column with two levels (independent only).
#' @param pair Numeric column name (paired only).
#' @param test_value Numeric. Null mu for one-sample test.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A `nonparametric_block` transform block.
#'
#' @export
new_nonparametric_block <- function(
  test_type = "independent",
  dv = character(),
  group = character(),
  pair = character(),
  test_value = 0,
  ...
) {
  type_choices <- c(
    "Mann-Whitney U (independent)" = "independent",
    "Wilcoxon signed-rank (paired)" = "paired",
    "Wilcoxon signed-rank (one sample)" = "one_sample"
  )

  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_test_type <- reactiveVal(test_type)
        r_dv <- reactiveVal(dv)
        r_group <- reactiveVal(group)
        r_pair <- reactiveVal(pair)
        r_test_value <- reactiveVal(test_value)
        r_initialized <- reactiveVal(FALSE)

        observeEvent(input$test_type, r_test_type(input$test_type))
        observeEvent(input$dv, r_dv(input$dv))
        observeEvent(input$group, r_group(input$group))
        observeEvent(input$pair, r_pair(input$pair))
        observeEvent(input$test_value, r_test_value(as.numeric(input$test_value)))

        observe({
          tt <- r_test_type()
          shinyjs::toggle(id = "np_group_wrapper", condition = tt == "independent")
          shinyjs::toggle(id = "np_pair_wrapper", condition = tt == "paired")
          shinyjs::toggle(id = "np_test_value_wrapper", condition = tt == "one_sample")
        })

        observe({
          if (!r_initialized() && length(colnames(data())) > 0) {
            d <- data()
            num <- colnames(d)[vapply(d, is.numeric, logical(1))]
            cat <- colnames(d)[vapply(
              d, function(x) is.factor(x) || is.character(x), logical(1)
            )]
            updateSelectizeInput(session, "dv", choices = num, selected = r_dv())
            updateSelectizeInput(session, "pair", choices = num, selected = r_pair())
            updateSelectizeInput(session, "group", choices = cat, selected = r_group())
            r_initialized(TRUE)
          }
        })

        observeEvent(colnames(data()), {
          if (r_initialized()) {
            req(data())
            d <- data()
            num <- colnames(d)[vapply(d, is.numeric, logical(1))]
            cat <- colnames(d)[vapply(
              d, function(x) is.factor(x) || is.character(x), logical(1)
            )]
            new_dv <- intersect(r_dv(), num)
            new_pair <- intersect(r_pair(), num)
            new_group <- intersect(r_group(), cat)
            r_dv(new_dv); r_pair(new_pair); r_group(new_group)
            updateSelectizeInput(session, "dv", choices = num, selected = new_dv)
            updateSelectizeInput(session, "pair", choices = num, selected = new_pair)
            updateSelectizeInput(session, "group", choices = cat, selected = new_group)
          }
        }, ignoreNULL = FALSE)

        list(
          expr = reactive({
            tt <- r_test_type()
            d <- r_dv()
            if (length(d) == 0 || !nzchar(d)) return(quote(NULL))

            expr_text <- switch(
              tt,
              "one_sample" = glue::glue(
                "stats::wilcox.test(data[['{d}']], mu = {r_test_value()})"
              ),
              "paired" = {
                p <- r_pair()
                if (length(p) == 0 || !nzchar(p)) return(quote(NULL))
                glue::glue(
                  "stats::wilcox.test(data[['{d}']], data[['{p}']], paired = TRUE)"
                )
              },
              "independent" = {
                g <- r_group()
                if (length(g) == 0 || !nzchar(g)) return(quote(NULL))
                glue::glue(
                  "stats::wilcox.test(`{d}` ~ `{g}`, data = data)"
                )
              }
            )
            if (is.null(expr_text)) return(quote(NULL))
            parse(text = expr_text)[[1]]
          }),
          state = list(
            test_type = r_test_type,
            dv = r_dv,
            group = r_group,
            pair = r_pair,
            test_value = r_test_value
          )
        )
      })
    },
    ui = function(id) {
      tagList(
        shinyjs::useShinyjs(),
        div(
          class = "block-container",
          radioButtons(
            NS(id, "test_type"),
            label = "Test type",
            choices = type_choices,
            selected = test_type
          ),
          selectizeInput(
            NS(id, "dv"),
            label = "Dependent variable",
            choices = dv,
            selected = dv,
            multiple = FALSE,
            width = "100%"
          ),
          div(
            id = NS(id, "np_group_wrapper"),
            selectizeInput(
              NS(id, "group"),
              label = "Grouping variable",
              choices = group,
              selected = group,
              multiple = FALSE,
              width = "100%"
            )
          ),
          div(
            id = NS(id, "np_pair_wrapper"),
            selectizeInput(
              NS(id, "pair"),
              label = "Paired with",
              choices = pair,
              selected = pair,
              multiple = FALSE,
              width = "100%"
            )
          ),
          div(
            id = NS(id, "np_test_value_wrapper"),
            numericInput(
              NS(id, "test_value"),
              label = "Test value (mu)",
              value = test_value
            )
          )
        )
      )
    },
    class = "nonparametric_block",
    allow_empty_state = c("dv", "group", "pair"),
    ...
  )
}

#' @export
block_output.nonparametric_block <- function(x, result, session) {
  renderPrint({
    if (is.null(result)) {
      cat("Pick variables to run the test.")
    } else {
      print(result)
    }
  })
}

#' @export
block_ui.nonparametric_block <- function(id, x, ...) {
  tagList(verbatimTextOutput(NS(id, "result")))
}
