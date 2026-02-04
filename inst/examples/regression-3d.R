# Demo app for 3D Regression Plot Block
#
# Run with: source("inst/examples/regression-3d.R")

library(blockr)
pkgload::load_all()

# Run the demo app showing 3D regression visualization
run_app(
  blocks = c(
    # Data source - using mtcars
    data = new_dataset_block("mtcars"),

    # Fit model with exactly 2 predictors for 3D visualization
    model = new_model_block(
      model_type = "lm",
      response = "mpg",
      predictors = c("wt", "hp")
    ),

    # 3D regression plot
    plot3d = new_regression_3d_block()
  ),
  links = c(
    new_link("data", "model", "data"),
    new_link("model", "plot3d", "data")
  ),
  title = "3D Regression Demo"
)
