# Demo app for blockr.lm
#
# Run with: source("inst/examples/app.R")

library(blockr)
pkgload::load_all()

# Run the demo app showing modeling workflow
run_app(
  blocks = c(
    data   = new_dataset_block("penguins"),
    model  = new_lm_block()
    ),
  links = c(new_link("data", "model", "data"))
)

 [   # Model summary (HTML display with coefficients, stats, diagnostics)
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
  title = "Model Demo"
)
