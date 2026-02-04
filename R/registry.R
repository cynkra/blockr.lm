#' @importFrom blockr.core register_blocks
register_lm_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_model_block",
      "new_lm_block",
      "new_coef_block",
      "new_anova_block",
      "new_model_summary_block",
      "new_coefplot_block",
      "new_diagnostic_plot_block",
      "new_residual_explorer_block",
      "new_check_model_block",
      "new_regression_3d_block"
    ),
    name = c(
      "Model",
      "Linear Model",
      "Coefficients Table",
      "ANOVA Table",
      "Model Summary",
      "Coefficient Plot",
      "Diagnostic Plots",
      "Residual Explorer",
      "Check Model",
      "3D Regression Plot"
    ),
    description = c(
      "Fit statistical models (lm, logistic, poisson, gamma). Select model type, response and predictor variables.",
      "Fit a linear model using lm(). Select response and predictor variables.",
      "Extract model coefficients with standard errors, t-statistics, and p-values using broom::tidy().",
      "ANOVA table showing the decomposition of variance for model terms.",
      "HTML summary with coefficients, statistics, and diagnostic tests. Works with any broom-compatible model.",
      "Dot-and-whisker plot of coefficients with confidence intervals.",
      "Standard diagnostic plots: Residuals vs Fitted, Q-Q, Scale-Location, and Leverage.",
      "Interactive residual exploration with plotly. Click to identify observations.",
      "Comprehensive model diagnostics using performance package: VIF, normality, homoscedasticity, outliers.",
      "Interactive 3D scatter plot with regression plane for models with 2 predictors."
    ),
    category = c(
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "plot",
      "plot",
      "plot",
      "plot",
      "plot"
    ),
    icon = c(
      "calculator",
      "graph-up",
      "table",
      "bar-chart",
      "clipboard-data",
      "bullseye",
      "grid-3x3",
      "search",
      "check2-square",
      "box"
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
