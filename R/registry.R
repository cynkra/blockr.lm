#' @importFrom blockr.core register_blocks
register_lm_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_lm_block",
      "new_coef_block",
      "new_anova_block",
      "new_diagnostic_plot_block",
      "new_residual_explorer_block"
    ),
    name = c(
      "Linear Model",
      "Coefficients Table",
      "ANOVA Table",
      "Diagnostic Plots",
      "Residual Explorer"
    ),
    description = c(
      "Fit a linear model using lm(). Select response and predictor variables.",
      "Extract model coefficients with standard errors, t-statistics, and p-values using broom::tidy().",
      "ANOVA table showing the decomposition of variance for model terms.",
      "Standard diagnostic plots: Residuals vs Fitted, Q-Q, Scale-Location, and Leverage.",
      "Interactive residual exploration with plotly. Click to identify observations."
    ),
    category = c(
      "transform",
      "transform",
      "transform",
      "plot",
      "plot"
    ),
    icon = c(
      "graph-up",
      "table",
      "bar-chart",
      "grid-3x3",
      "search"
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
