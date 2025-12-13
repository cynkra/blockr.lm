# blockr.lm Linear Modeling Demo

library(blockr)
library(blockr.lm)

# Lock the dashboard to prevent moving panels around
options(blockr.dock_is_locked = TRUE)

# Load the dashboard from JSON
dashboard_json <- jsonlite::fromJSON(
  "dashboard.json",
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
)

# Deserialize and run
board <- blockr.core::blockr_deser(dashboard_json)
blockr.dock::run_dock_app(board)
