#' Linear Model Block
#'
#' Fit a linear model using [stats::lm()]. This block takes a data frame as
#' input and outputs an lm model object that can be consumed by downstream
#' diagnostic blocks.
#'
#' @param response Response variable column name (single column)
#' @param predictors Predictor variable column names (can be multiple)
#' @param intercept Logical. Include intercept term in the model (default TRUE)
#' @param ... Forwarded to [new_transform_block()]
#'
#' @return A transform block object of class \code{lm_block} that outputs an lm
#' model object.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_lm_block(), list(data = mtcars))
#' }
#'
#' @export
new_lm_block <- function(
  response = character(),
  predictors = character(),
  intercept = TRUE,
  ...
) {
  ui <- function(id) {
    tagList(
      selectizeInput(
        NS(id, "response"),
        label = "Response variable (Y)",
        choices = response,
        selected = response,
        multiple = FALSE,
        width = "100%",
        options = list(placeholder = "Select response variable...")
      ),
      selectizeInput(
        NS(id, "predictors"),
        label = "Predictor variables (X)",
        choices = predictors,
        selected = predictors,
        multiple = TRUE,
        width = "100%",
        options = list(placeholder = "Select predictor variables...")
      ),
      checkboxInput(
        NS(id, "intercept"),
        label = "Include intercept",
        value = intercept
      )
    )
  }

  server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
      # initiate reactive values
      r_response   <- reactiveVal(response)
      r_predictors <- reactiveVal(predictors)
      r_intercept  <- reactiveVal(intercept)

      # update reactive values when the inputs are changed by the user
      observeEvent(input$response, r_response(input$response), ignoreNULL = FALSE)
      observeEvent(input$predictors, r_predictors(input$predictors), ignoreNULL = FALSE)
      observeEvent(input$intercept, r_intercept(input$intercept))

      # update response choices when input data changes
      observeEvent(colnames(data()),
        {
          req(data())
          resp_col  <- colnames(data())[mapply(is.numeric, data())]

          updateSelectizeInput(
            session,
            "response",
            choices = resp_col,
            selected = r_response()
          )
        },
        ignoreNULL = FALSE
      )

      # update predictor choices when input data or response variable changes
      observeEvent(list(colnames(data()), input$response),
        {
          req(data())
          pred_cols <- setdiff(colnames(data()), r_response())

          updateSelectizeInput(
            session,
            "predictors",
            choices = pred_cols,
            selected = r_predictors()
          )
        },
        ignoreNULL = FALSE
      )

      # return expression and a state
      list(
        expr = reactive({
          b0 <- r_intercept()
          y  <- r_response()
          xs <- r_predictors()

          # if no response selected return NULL
          if(is.null(y) || !nzchar(y)) {
            return(bquote(NULL))
          }

          # create model formula string
          if(!r_intercept()) {
            if (is.null(xs) || !all(nzchar(xs))) {
              # if no intercept and no predictors return NULL
              return(bquote(NULL))
            } else {
              pred_str   <- paste0("`", xs, "`", collapse = " + ")
              model_form <- paste0("`", y, "` ~ 0 + ", pred_str)
            }
          } else {
            if (is.null(xs) || !all(nzchar(xs))) {
              model_form <- paste0("`", y, "` ~ 1")
            } else {
              pred_str   <- paste0("`", xs, "`", collapse = " + ")
              model_form <- paste0("`", y, "` ~ 1 + ", pred_str)
            }
          }
          bquote(stats::lm(.(model_form), data = data))
        }),
        state = list(response = r_response, predictors = r_predictors, intercept = r_intercept)
      )
    })
  }

  new_transform_block(server, ui, "lm_block", allow_empty_state = c("response", "predictors"))
}


# .... 

#' Custom block output for lm_block
#'
#' Displays the model summary as text output.
#'
#' @export
block_output.lm_block <- function(x, result, session) {
  renderPrint({
    if (is.null(result)) {
      cat("Select response and predictor variables to fit a model.")
    } else {
      sjPlot::tab_model(result, file = stdout())
    }
  })
  renderTable({
    if (is.null(result)) {
      cat("Select response and predictor variables to fit a model.")
    } else {
      broom::tidy(result)
    }
  })
  # rgl::renderRglwidget({
  #   if (is.null(result)) {
  #     cat("Select response and predictor variables to fit a model.")
  #   } else {
  #     predict3d::predict3d(result, overlay = TRUE, show.subtitle = FALSE, radius = 20, axes = FALSE, widgh = 1000)
  #     rgl::axes3d(edges=c("x--", "y--", "z"))
  #     rgl::rglwidget()
  #   }
  # })
}

#' Custom block UI for lm_block
#'
#' @export
block_ui.lm_block <- function(id, x, ...) {
  tagList(
    htmlOutput(NS(id, "result")),
    dataTableOutput(NS(id, "result"))
    # rgl::rglwidgetOutput(NS(id, "result"))
  )
}
