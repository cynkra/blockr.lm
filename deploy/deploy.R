# Deployment script for blockr.lm demo to connect.cynkra.com
#
# Prerequisites:
# 1. rsconnect account configured for connect.cynkra.com

library(rsconnect)

# Check if account is configured
accounts <- rsconnect::accounts()
if (nrow(accounts) == 0) {
  stop("No rsconnect account configured.")
}

cat("Available accounts:\n")
print(accounts)

# Deploy to connect.cynkra.com (blockr.cloud)
rsconnect::deployApp(
  appDir = ".",
  appName = "blockr-lm-demo",
  account = "christoph",
  server = "connect.cynkra.com",
  forceUpdate = TRUE
)
