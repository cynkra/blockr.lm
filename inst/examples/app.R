# Demo app for blockr.lm
#
# Run with: source("inst/examples/app.R")

library(blockr)
pkgload::load_all()

# Run the demo app showing linear modeling workflow
run_app(
  blocks = c(
    # Data source - using mtcars
    data = new_dataset_block("mtcars"),

    # Fit linear model: mpg ~ cyl + hp + wt
    model = new_lm_block(
      response = "mpg",
      predictors = c("cyl", "hp", "wt")
    ),

    # Model summary (HTML display with coefficients, stats, diagnostics)
    summary = new_model_summary_block(),

    # Coefficient plot
    coefplot = new_coefplot_block(),

    # Diagnostic plots
    plots = new_diagnostic_plot_block(),

    # Interactive residual explorer
    resid = new_residual_explorer_block(x_var = "fitted", y_var = "residuals")
  ),
  links = c(
    new_link("data", "model", "data"),
    new_link("model", "summary", "data"),
    new_link("model", "coefplot", "data"),
    new_link("model", "plots", "data"),
    new_link("model", "resid", "data")
  ),
  title = "Linear Model Demo"
)
