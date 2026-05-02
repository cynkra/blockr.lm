#' T-Test Block
#'
#' Adaptive block for one-sample / paired / independent t-tests. Emits an
#' `htest` object that downstream blocks (parameters, cohens_d, report)
#' consume via S3 dispatch.
#'
#' @param test_type One of "one_sample", "paired", "independent".
#' @param variant For independent tests: "welch" or "student".
#' @param dv Numeric column name (the dependent variable / first sample).
#' @param group Categorical column with two levels (independent only).
#' @param pair Numeric column name (paired only).
#' @param test_value Numeric. Null mean for one-sample test.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A `ttest_block` transform block.
#'
#' @export
new_ttest_block <- function(
  test_type = "independent",
  variant = "welch",
  dv = character(),
  group = character(),
  pair = character(),
  test_value = 0,
  ...
) {
  type_choices <- c(
    "Independent samples" = "independent",
    "Paired samples" = "paired",
    "One sample" = "one_sample"
  )
  variant_choices <- c("Welch (unequal variances)" = "welch",
    "Student (equal variances)" = "student")

  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_test_type <- reactiveVal(test_type)
        r_variant <- reactiveVal(variant)
        r_dv <- reactiveVal(dv)
        r_group <- reactiveVal(group)
        r_pair <- reactiveVal(pair)
        r_test_value <- reactiveVal(test_value)
        r_initialized <- reactiveVal(FALSE)

        observeEvent(input$test_type, r_test_type(input$test_type))
        observeEvent(input$variant, r_variant(input$variant))
        observeEvent(input$dv, r_dv(input$dv))
        observeEvent(input$group, r_group(input$group))
        observeEvent(input$pair, r_pair(input$pair))
        observeEvent(input$test_value, r_test_value(as.numeric(input$test_value)))

        # Toggle conditional inputs based on test type
        observe({
          tt <- r_test_type()
          shinyjs::toggle(id = "group_wrapper",
            condition = tt == "independent")
          shinyjs::toggle(id = "variant_wrapper",
            condition = tt == "independent")
          shinyjs::toggle(id = "pair_wrapper",
            condition = tt == "paired")
          shinyjs::toggle(id = "test_value_wrapper",
            condition = tt == "one_sample")
        })

        # Column choice updaters
        observe({
          if (!r_initialized() && length(colnames(data())) > 0) {
            d <- data()
            num <- colnames(d)[vapply(d, is.numeric, logical(1))]
            cat <- colnames(d)[vapply(
              d, function(x) is.factor(x) || is.character(x), logical(1)
            )]
            updateSelectizeInput(session, "dv",
              choices = num, selected = r_dv())
            updateSelectizeInput(session, "pair",
              choices = num, selected = r_pair())
            updateSelectizeInput(session, "group",
              choices = cat, selected = r_group())
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
            updateSelectizeInput(session, "dv",
              choices = num, selected = new_dv)
            updateSelectizeInput(session, "pair",
              choices = num, selected = new_pair)
            updateSelectizeInput(session, "group",
              choices = cat, selected = new_group)
          }
        }, ignoreNULL = FALSE)

        list(
          expr = reactive({
            tt <- r_test_type()
            d <- r_dv()
            if (length(d) == 0 || !nzchar(d)) return(quote(NULL))

            expr_text <- switch(
              tt,
              "one_sample" = {
                glue::glue(
                  "stats::t.test(data[['{d}']], mu = {r_test_value()})"
                )
              },
              "paired" = {
                p <- r_pair()
                if (length(p) == 0 || !nzchar(p)) return(quote(NULL))
                glue::glue(
                  "stats::t.test(data[['{d}']], data[['{p}']], paired = TRUE)"
                )
              },
              "independent" = {
                g <- r_group()
                if (length(g) == 0 || !nzchar(g)) return(quote(NULL))
                ve <- identical(r_variant(), "student")
                glue::glue(
                  "stats::t.test(`{d}` ~ `{g}`, data = data, var.equal = {ve})"
                )
              }
            )
            if (is.null(expr_text)) return(quote(NULL))
            parse(text = expr_text)[[1]]
          }),
          state = list(
            test_type = r_test_type,
            variant = r_variant,
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
          div(
            id = NS(id, "variant_wrapper"),
            selectInput(
              NS(id, "variant"),
              label = "Variant",
              choices = variant_choices,
              selected = variant
            )
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
            id = NS(id, "group_wrapper"),
            selectizeInput(
              NS(id, "group"),
              label = "Grouping variable (2 levels)",
              choices = group,
              selected = group,
              multiple = FALSE,
              width = "100%"
            )
          ),
          div(
            id = NS(id, "pair_wrapper"),
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
            id = NS(id, "test_value_wrapper"),
            numericInput(
              NS(id, "test_value"),
              label = "Test value (mu)",
              value = test_value
            )
          )
        )
      )
    },
    class = "ttest_block",
    allow_empty_state = c("dv", "group", "pair"),
    ...
  )
}

#' @export
block_output.ttest_block <- function(x, result, session) {
  renderPrint({
    if (is.null(result)) {
      cat("Pick variables to run the t-test.")
    } else {
      print(result)
    }
  })
}

#' @export
block_ui.ttest_block <- function(id, x, ...) {
  tagList(verbatimTextOutput(NS(id, "result")))
}
