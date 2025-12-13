#' Model Summary Block
#'
#' Creates an HTML summary display for any model with broom methods.
#' Shows coefficients, model statistics, and diagnostic tests in a
#' three-column layout similar to blockr.seasonal.
#'
#' Works with lm, glm, lmer, coxph, gam, and 100+ other model types
#' supported by broom.
#'
#' @param ... Forwarded to [new_transform_block()]
#'
#' @return A transform block object of class \code{model_summary_block}.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   model <- lm(mpg ~ cyl + hp + wt, data = mtcars)
#'   serve(new_model_summary_block(), list(data = model))
#' }
#'
#' @export
new_model_summary_block <- function(...) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive({
              # Return the model with summary info as attributes
              parse(
                text = "{
                  df <- broom::glance(data)
                  attr(df, \"model\") <- data
                  attr(df, \"tidy\") <- broom::tidy(data, conf.int = TRUE)
                  df
                }"
              )[[1]]
            }),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      tagList() # Empty UI, custom display handled by block_ui method
    },
    class = "model_summary_block",
    ...
  )
}

#' Generate significance badge
#' @noRd
significance_badge <- function(p) {
  if (is.na(p)) return("")

  if (p < 0.001) {
    tags$span(
      class = "badge",
      style = "background-color: #0066cc;",
      "0.1%"
    )
  } else if (p < 0.01) {
    tags$span(
      class = "badge",
      style = "background-color: #17a2b8;",
      "1%"
    )
  } else if (p < 0.05) {
    tags$span(
      class = "badge",
      style = "background-color: #6c757d;",
      "5%"
    )
  } else if (p < 0.1) {
    tags$span(
      class = "badge",
      style = "background-color: #6c757d;",
      "10%"
    )
  } else {
    ""
  }
}

#' Generate HTML coefficients table from tidy output
#' @noRd
html_model_coefs <- function(tidy_df) {
  if (is.null(tidy_df) || nrow(tidy_df) == 0) {
    return(div(
      style = "text-align: center; color: #6c757d; padding: 20px;",
      "No coefficients"
    ))
  }


  tags$table(
    class = "table table-condensed",
    style = "width: 100%; font-size: 13px;",
    tags$tbody(
      lapply(seq_len(nrow(tidy_df)), function(i) {
        row <- tidy_df[i, ]
        p_val <- if ("p.value" %in% names(row)) row$p.value else NA

        tags$tr(
          tags$td(
            style = "padding: 4px 8px 4px 0; font-weight: 500;",
            row$term
          ),
          tags$td(
            style = "padding: 4px 8px; text-align: right;",
            sprintf("%.2f", row$estimate)
          ),
          tags$td(
            style = "padding: 4px 0 4px 8px; text-align: right;",
            significance_badge(p_val)
          )
        )
      })
    )
  )
}

#' Generate HTML statistics table from glance output
#' @noRd
html_model_stats <- function(glance_df, model) {
  if (is.null(glance_df) || nrow(glance_df) == 0) {
    return(div(
      style = "text-align: center; color: #6c757d; padding: 20px;",
      "No statistics"
    ))
  }

  # Build stats items based on what's available
  stats_items <- list()

  # Model type
  model_class <- class(model)[1]
  stats_items <- c(stats_items, list(c("Model", model_class)))

  # Common statistics
  if ("nobs" %in% names(glance_df)) {
    stats_items <- c(stats_items, list(c("Observations", glance_df$nobs)))
  }

  if ("r.squared" %in% names(glance_df)) {
    stats_items <- c(stats_items, list(c("R\u00b2", sprintf("%.4f", glance_df$r.squared))))
  }

  if ("adj.r.squared" %in% names(glance_df)) {
    stats_items <- c(stats_items, list(c("Adj. R\u00b2", sprintf("%.4f", glance_df$adj.r.squared))))
  }

  if ("sigma" %in% names(glance_df)) {
    stats_items <- c(stats_items, list(c("Residual SE", sprintf("%.4f", glance_df$sigma))))
  }

  if ("AIC" %in% names(glance_df)) {
    stats_items <- c(stats_items, list(c("AIC", sprintf("%.1f", glance_df$AIC))))
  }

  if ("BIC" %in% names(glance_df)) {
    stats_items <- c(stats_items, list(c("BIC", sprintf("%.1f", glance_df$BIC))))
  }

  if ("statistic" %in% names(glance_df) && "p.value" %in% names(glance_df)) {
    stats_items <- c(stats_items, list(c("F-statistic", sprintf("%.2f", glance_df$statistic))))
  }

  if ("df" %in% names(glance_df) && "df.residual" %in% names(glance_df)) {
    stats_items <- c(stats_items, list(c("DF", paste0(glance_df$df, " / ", glance_df$df.residual))))
  }

  tags$table(
    class = "table table-condensed",
    style = "width: 100%; font-size: 13px;",
    tags$tbody(
      lapply(stats_items, function(item) {
        tags$tr(
          tags$td(
            style = "padding: 4px 8px 4px 0; font-weight: 500;",
            item[1]
          ),
          tags$td(
            style = "padding: 4px 0 4px 8px; text-align: right;",
            item[2]
          )
        )
      })
    )
  )
}

#' Generate HTML diagnostic tests
#' @noRd
html_model_tests <- function(model) {
  test_results <- list()


  # Try to get residuals for diagnostic tests
  resid <- tryCatch(stats::residuals(model), error = function(e) NULL)
  fitted_vals <- tryCatch(stats::fitted(model), error = function(e) NULL)


  if (!is.null(resid) && length(resid) >= 3) {
    # Shapiro-Wilk test for normality (max 5000 observations)
    resid_sample <- if (length(resid) > 5000) sample(resid, 5000) else resid
    sw_test <- tryCatch(
      stats::shapiro.test(resid_sample),
      error = function(e) NULL
    )

    if (!is.null(sw_test)) {
      test_results <- c(test_results, list(list(
        name = "Shapiro-Wilk",
        hypothesis = "H0: residuals are normal",
        stat = sprintf("W = %.4f", sw_test$statistic),
        p = sw_test$p.value
      )))
    }

    # Durbin-Watson test for autocorrelation
    if (length(resid) >= 3 && !is.null(fitted_vals)) {
      # Calculate DW statistic
      dw_stat <- sum(diff(resid)^2) / sum(resid^2)

      # Heuristic p-value based on distance from 2
      # DW near 2 = no autocorrelation (good)
      # DW < 1.5 or > 2.5 = potential autocorrelation (bad)
      dw_deviation <- abs(dw_stat - 2)
      dw_p <- if (dw_deviation < 0.3) {
        0.5  # OK - close to 2
      } else if (dw_deviation < 0.5) {
        0.1  # Marginal
      } else if (dw_deviation < 0.7) {
        0.05  # Concern
      } else {
        0.01  # Strong concern
      }

      test_results <- c(test_results, list(list(
        name = "Durbin-Watson",
        hypothesis = "H0: no autocorrelation",
        stat = sprintf("DW = %.3f", dw_stat),
        p = dw_p
      )))
    }

    # Breusch-Pagan style check (simplified)
    if (!is.null(fitted_vals) && length(fitted_vals) == length(resid)) {
      bp_cor <- tryCatch(
        stats::cor.test(fitted_vals, resid^2),
        error = function(e) NULL
      )

      if (!is.null(bp_cor)) {
        test_results <- c(test_results, list(list(
          name = "Heteroscedasticity",
          hypothesis = "H0: constant variance",
          stat = sprintf("r = %.3f", bp_cor$estimate),
          p = bp_cor$p.value
        )))
      }
    }
  }

  if (length(test_results) == 0) {
    return(div(
      style = "text-align: center; color: #6c757d; padding: 20px;",
      "Diagnostic tests not available"
    ))
  }

  # Build test badge
  test_badge <- function(p) {
    if (is.na(p)) {
      return(tags$span(
        class = "badge",
        style = "background-color: #6c757d;",
        "?"
      ))
    }
    if (p < 0.01) {
      tags$span(class = "badge", style = "background-color: #dc3545;", "Reject")
    } else if (p < 0.05) {
      tags$span(class = "badge", style = "background-color: #ffc107; color: #212529;", "Caution")
    } else {
      tags$span(class = "badge", style = "background-color: #28a745;", "OK")
    }
  }

  tags$table(
    class = "table table-condensed",
    style = "width: 100%; font-size: 13px;",
    tags$tbody(
      lapply(test_results, function(test) {
        tags$tr(
          tags$td(
            style = "padding: 4px 8px 4px 0;",
            tags$div(
              tags$strong(test$name),
              tags$br(),
              tags$small(style = "color: #6c757d;", test$hypothesis)
            )
          ),
          tags$td(
            style = "padding: 4px 8px; text-align: right;",
            test$stat
          ),
          tags$td(
            style = "padding: 4px 0 4px 8px; text-align: right;",
            test_badge(test$p)
          )
        )
      })
    )
  )
}

#' Generate full HTML summary for model
#' @noRd
html_model_summary <- function(model, tidy_df, glance_df) {
  if (is.null(model)) {
    return(div(
      style = "padding: 20px; text-align: center; color: #6c757d;",
      "No model available"
    ))
  }

  tagList(
    # CSS styling
    tags$style(HTML("
      .model-summary-container {
        padding: 10px 0;
        background: #ffffff;
      }
      .model-summary-container .row {
        margin-left: 0;
        margin-right: 0;
      }
      .model-summary-container .col-md-4 {
        padding-left: 0;
        padding-right: 30px;
      }
      .model-column-header {
        font-size: 14px;
        font-weight: 600;
        color: #495057;
        margin-bottom: 15px;
        padding-bottom: 8px;
        border-bottom: 1px solid #e9ecef;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      .table-condensed {
        border-collapse: collapse;
        width: 100%;
      }
      .table-condensed td, .table-condensed th {
        border-bottom: 1px solid #f0f0f0;
        transition: background-color 0.15s;
      }
      .table-condensed td:first-child {
        padding-left: 0 !important;
      }
      .table-condensed tr:hover td {
        background-color: #f8f9fa;
      }
      .table-condensed tr:last-child td {
        border-bottom: none;
      }
      .badge {
        display: inline-block;
        padding: 3px 7px;
        font-size: 10px;
        font-weight: 600;
        line-height: 1;
        color: #fff;
        text-align: center;
        white-space: nowrap;
        vertical-align: baseline;
        border-radius: 12px;
        letter-spacing: 0.3px;
      }
    ")),

    div(
      class = "model-summary-container",

      # Three column layout
      tags$div(
        class = "row",

        # Left column - Coefficients
        tags$div(
          class = "col-md-4",
          div(class = "model-column-header", "Coefficients"),
          html_model_coefs(tidy_df)
        ),

        # Middle column - Statistics
        tags$div(
          class = "col-md-4",
          div(class = "model-column-header", "Statistics"),
          html_model_stats(glance_df, model)
        ),

        # Right column - Tests
        tags$div(
          class = "col-md-4",
          div(class = "model-column-header", "Diagnostics"),
          html_model_tests(model)
        )
      )
    )
  )
}

#' @export
block_ui.model_summary_block <- function(id, x, ...) {
  uiOutput(NS(id, "result"))
}

#' @export
block_output.model_summary_block <- function(x, result, session) {
  model <- attr(result, "model")
  tidy_df <- attr(result, "tidy")
  glance_df <- result

  renderUI({
    html_model_summary(model, tidy_df, glance_df)
  })
}
