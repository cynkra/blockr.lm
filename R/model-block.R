#' Model Block
#'
#' Fit statistical models including linear models (lm), generalized linear
#' models (glm), multinomial regression, ordinal regression, and mixed-effects
#' models. The block emits a fitted model object that downstream blocks
#' (parameters, anova, performance, effect sizes, diagnostics, report)
#' consume via easystats / broom S3 dispatch.
#'
#' @param model_type Model type. One of: "lm" (linear), "logistic" (binomial GLM),
#'   "poisson" (Poisson GLM), "gamma" (Gamma GLM), "multinom" (multinomial via
#'   `nnet::multinom`), "polr" (ordinal via `MASS::polr`), "lmer" (mixed linear
#'   via `lme4::lmer`), "glmer" (mixed logistic via `lme4::glmer`).
#' @param response Response variable column name (single column).
#' @param predictors Numeric predictor (covariate) column names. Multiple.
#' @param factors Categorical predictor column names. Multiple.
#' @param random_effects Categorical grouping variable column names for mixed
#'   models. One random-intercept term `(1 | group)` per name. Only used for
#'   `lmer` / `glmer`.
#' @param intercept Logical. Include intercept term (default TRUE).
#' @param ... Forwarded to [new_transform_block()].
#'
#' @return A transform block object of class \code{model_block}.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_model_block(), list(data = mtcars))
#' }
#'
#' @export
new_model_block <- function(
  model_type = "lm",
  response = character(),
  predictors = character(),
  factors = character(),
  random_effects = character(),
  intercept = TRUE,
  ...
) {
  model_choices <- c(
    "Linear (lm)" = "lm",
    "Logistic (glm)" = "logistic",
    "Multinomial" = "multinom",
    "Ordinal" = "polr",
    "Poisson (glm)" = "poisson",
    "Gamma (glm)" = "gamma",
    "Mixed Linear" = "lmer",
    "Mixed Logistic" = "glmer"
  )

  new_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          r_model_type <- reactiveVal(model_type)
          r_response <- reactiveVal(response)
          r_predictors <- reactiveVal(predictors)
          r_factors <- reactiveVal(factors)
          r_random_effects <- reactiveVal(random_effects)
          r_intercept <- reactiveVal(intercept)
          r_initialized <- reactiveVal(FALSE)

          observeEvent(input$model_type, r_model_type(input$model_type))
          observeEvent(input$response, r_response(input$response))
          observeEvent(input$predictors, r_predictors(input$predictors))
          observeEvent(input$factors, r_factors(input$factors))
          observeEvent(input$random_effects, r_random_effects(input$random_effects))
          observeEvent(input$intercept, r_intercept(input$intercept))

          # Toggle random-effects picker based on model type
          observe({
            mtype <- r_model_type()
            shinyjs::toggle(
              id = "random_effects_wrapper",
              condition = mtype %in% c("lmer", "glmer")
            )
          })

          # Helper: classify columns
          numeric_cols <- function(d) {
            cn <- colnames(d)
            cn[vapply(d, is.numeric, logical(1))]
          }
          categorical_cols <- function(d) {
            cn <- colnames(d)
            cn[vapply(d, function(x) is.factor(x) || is.character(x), logical(1))]
          }
          all_cols <- function(d) colnames(d)

          # Restore initial selection once on startup
          observe({
            if (!r_initialized() && length(colnames(data())) > 0) {
              d <- data()
              updateSelectizeInput(session, "response",
                choices = all_cols(d), selected = r_response())
              updateSelectizeInput(session, "predictors",
                choices = numeric_cols(d), selected = r_predictors())
              updateSelectizeInput(session, "factors",
                choices = categorical_cols(d), selected = r_factors())
              updateSelectizeInput(session, "random_effects",
                choices = categorical_cols(d), selected = r_random_effects())
              r_initialized(TRUE)
            }
          })

          # When data columns change, drop any selections that no longer
          # exist in the new schema, then refresh the picker UI. Updating
          # the reactive *state* (not just the widget) is what stops
          # stale column names from leaking into the formula.
          observeEvent(
            colnames(data()),
            {
              if (r_initialized()) {
                req(data())
                d <- data()
                num <- numeric_cols(d)
                cat <- categorical_cols(d)
                all <- all_cols(d)

                new_resp <- intersect(r_response(), all)
                new_preds <- intersect(r_predictors(), num)
                new_facs <- intersect(r_factors(), cat)
                new_re <- intersect(r_random_effects(), cat)

                # Update state first; the input observers will not fire
                # for "no change" or NULL emissions from the widget.
                r_response(new_resp)
                r_predictors(new_preds)
                r_factors(new_facs)
                r_random_effects(new_re)

                updateSelectizeInput(session, "response",
                  choices = all, selected = new_resp)
                updateSelectizeInput(session, "predictors",
                  choices = num, selected = new_preds)
                updateSelectizeInput(session, "factors",
                  choices = cat, selected = new_facs)
                updateSelectizeInput(session, "random_effects",
                  choices = cat, selected = new_re)
              }
            },
            ignoreNULL = FALSE
          )

          list(
            expr = reactive({
              mtype <- r_model_type()
              resp <- r_response()
              covs <- r_predictors()
              facs <- r_factors()
              re <- r_random_effects()
              incl_intercept <- r_intercept()

              if (is.null(resp) || length(resp) == 0 || resp == "") {
                return(quote(NULL))
              }

              # Response term — wrap in factor() / ordered() for multinom / polr
              resp_term <- switch(
                mtype,
                "multinom" = paste0("factor(`", resp, "`)"),
                "polr" = paste0("ordered(`", resp, "`)"),
                paste0("`", resp, "`")
              )

              # Combine numeric and factor predictors
              all_preds <- c(covs, facs)
              all_preds <- all_preds[nzchar(all_preds)]

              # Build formula
              if (length(all_preds) == 0) {
                if (incl_intercept) {
                  formula_str <- paste0(resp_term, " ~ 1")
                } else {
                  return(quote(NULL))
                }
              } else {
                pred_str <- paste0("`", all_preds, "`", collapse = " + ")
                if (incl_intercept) {
                  formula_str <- paste0(resp_term, " ~ ", pred_str)
                } else {
                  formula_str <- paste0(resp_term, " ~ 0 + ", pred_str)
                }
              }

              # Add random-effects clause for mixed models
              if (mtype %in% c("lmer", "glmer")) {
                re <- re[nzchar(re)]
                if (length(re) == 0) {
                  return(quote(NULL))
                }
                re_clause <- paste0(
                  "(1 | `", re, "`)",
                  collapse = " + "
                )
                formula_str <- paste0(formula_str, " + ", re_clause)
              }

              expr_text <- switch(
                mtype,
                "lm" = glue::glue("stats::lm({formula_str}, data = data)"),
                "logistic" = glue::glue(
                  "stats::glm({formula_str}, data = data, family = stats::binomial())"
                ),
                "poisson" = glue::glue(
                  "stats::glm({formula_str}, data = data, family = stats::poisson())"
                ),
                "gamma" = glue::glue(
                  "stats::glm({formula_str}, data = data, family = stats::Gamma())"
                ),
                "multinom" = glue::glue(
                  "nnet::multinom({formula_str}, data = data, trace = FALSE)"
                ),
                "polr" = glue::glue(
                  "MASS::polr({formula_str}, data = data, Hess = TRUE)"
                ),
                "lmer" = glue::glue("lme4::lmer({formula_str}, data = data)"),
                "glmer" = glue::glue(
                  "lme4::glmer({formula_str}, data = data, family = stats::binomial())"
                ),
                glue::glue("stats::lm({formula_str}, data = data)")
              )

              parse(text = expr_text)[[1]]
            }),
            state = list(
              model_type = r_model_type,
              response = r_response,
              predictors = r_predictors,
              factors = r_factors,
              random_effects = r_random_effects,
              intercept = r_intercept
            )
          )
        }
      )
    },
    ui = function(id) {
      tagList(
        shinyjs::useShinyjs(),
        css_responsive_grid(),
        css_single_column("model"),

        div(
          class = "block-container model-block-container",
          div(
            class = "block-form-grid",
            div(
              class = "block-section",
              div(
                class = "block-section-grid",

                div(
                  class = "block-help-text",
                  "Pick a model type and variables. Random-effects picker appears for mixed models."
                ),

                div(
                  class = "block-input-wrapper",
                  style = "grid-column: 1 / -1;",
                  selectInput(
                    NS(id, "model_type"),
                    label = "Model type",
                    choices = model_choices,
                    selected = model_type,
                    width = "100%"
                  )
                ),

                div(
                  class = "block-input-wrapper",
                  style = "grid-column: 1 / -1;",
                  selectizeInput(
                    NS(id, "response"),
                    label = "Dependent variable (Y)",
                    choices = response,
                    selected = response,
                    multiple = FALSE,
                    width = "100%",
                    options = list(placeholder = "Pick the response...")
                  )
                ),

                div(
                  class = "block-input-wrapper",
                  style = "grid-column: 1 / -1;",
                  selectizeInput(
                    NS(id, "predictors"),
                    label = "Covariates (numeric predictors)",
                    choices = predictors,
                    selected = predictors,
                    multiple = TRUE,
                    width = "100%",
                    options = list(
                      plugins = list("drag_drop", "remove_button"),
                      persist = FALSE,
                      placeholder = "Pick numeric predictors..."
                    )
                  )
                ),

                div(
                  class = "block-input-wrapper",
                  style = "grid-column: 1 / -1;",
                  selectizeInput(
                    NS(id, "factors"),
                    label = "Factors (categorical predictors)",
                    choices = factors,
                    selected = factors,
                    multiple = TRUE,
                    width = "100%",
                    options = list(
                      plugins = list("drag_drop", "remove_button"),
                      persist = FALSE,
                      placeholder = "Pick factor predictors..."
                    )
                  )
                ),

                div(
                  id = NS(id, "random_effects_wrapper"),
                  class = "block-input-wrapper",
                  style = "grid-column: 1 / -1;",
                  selectizeInput(
                    NS(id, "random_effects"),
                    label = "Random-effects grouping (mixed models only)",
                    choices = random_effects,
                    selected = random_effects,
                    multiple = TRUE,
                    width = "100%",
                    options = list(
                      plugins = list("drag_drop", "remove_button"),
                      persist = FALSE,
                      placeholder = "Pick grouping variable(s)..."
                    )
                  )
                ),

                div(
                  class = "block-input-wrapper",
                  checkboxInput(
                    NS(id, "intercept"),
                    label = "Include intercept",
                    value = intercept
                  )
                )
              )
            )
          )
        )
      )
    },
    class = "model_block",
    allow_empty_state = c("response", "predictors", "factors", "random_effects"),
    ...
  )
}

#' @export
block_output.model_block <- function(x, result, session) {
  renderPrint({
    if (is.null(result)) {
      cat("Pick variables to fit a model.")
    } else {
      summary(result)
    }
  })
}

#' @export
block_ui.model_block <- function(id, x, ...) {
  tagList(verbatimTextOutput(NS(id, "result")))
}
