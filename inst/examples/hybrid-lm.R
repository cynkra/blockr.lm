# Hybrid LM Demo: Combining blockr.lm blocks with blockr.extra function blocks
#
# This example demonstrates how to extend blockr.lm's specialized modeling blocks
# with blockr.extra's new_function_block() for custom outputs like gtsummary tables.
#
# Run with: source("inst/examples/hybrid-lm.R")

library(blockr)
pkgload::load_all()
pkgload::load_all("../blockr.extra")

run_app(
  blocks = c(
    # Data source - using mtcars
    data = new_dataset_block("mtcars"),

    # === blockr.lm specialized blocks ===

    # Fit model: mpg ~ cyl + hp + wt
    model = new_model_block(
      model_type = "lm",
      response = "mpg",
      predictors = c("cyl", "hp", "wt")
    ),

    # Diagnostic plots (residuals vs fitted, Q-Q, etc.)
    plots = new_diagnostic_plot_block(),

    # Coefficient plot with confidence intervals
    coefplot = new_coefplot_block(),

    # Interactive residual explorer
    resid = new_residual_explorer_block(),

    # === blockr.extra function blocks for custom functionality ===

    # Use gtsummary for publication-ready regression table
    summary_tbl = new_function_block(
      fn = "function(data) {
        gtsummary::tbl_regression(data) |> gtsummary::as_gt()
      }"
    ),

    # Custom model: Robust regression using MASS::rlm
    # (not natively supported by blockr.lm)
    # - formula: text input for model specification
    # - psi: dropdown for robust estimation method
    robust_model = new_function_block(
      fn = "function(data,
                     formula = 'mpg ~ cyl + hp + wt',
                     psi = c('psi.huber', 'psi.hampel', 'psi.bisquare')) {
        MASS::rlm(as.formula(formula), data = data, psi = psi)
      }"
    ),

    # Coefficient plot for robust model (works with rlm objects)
    robust_coefplot = new_coefplot_block(),

    # === 3D visualization branch ===

    # Second model with 2 predictors for 3D visualization
    model_2pred = new_model_block(
      model_type = "lm",
      response = "mpg",
      predictors = c("wt", "hp")
    ),

    # 3D regression surface plot
    plot3d = new_regression_3d_block()
  ),
  links = c(
    # Main model branch
    new_link("data", "model", "data"),
    new_link("model", "plots", "data"),
    new_link("model", "coefplot", "data"),
    new_link("model", "resid", "data"),
    new_link("model", "summary_tbl", "data"),

    # Robust model branch (custom model via function block)
    new_link("data", "robust_model", "data"),
    new_link("robust_model", "robust_coefplot", "data"),

    # 3D visualization branch
    new_link("data", "model_2pred", "data"),
    new_link("model_2pred", "plot3d", "data")
  ),
  title = "Hybrid LM Demo"
)
