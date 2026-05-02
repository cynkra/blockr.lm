#' @keywords internal
"_PACKAGE"

#' @import shiny
#' @import blockr.core
#' @importFrom glue glue
#' @importFrom broom tidy
#' @importFrom stats lm anova residuals fitted sd shapiro.test ks.test bartlett.test as.formula
#' @importFrom shinyjs useShinyjs
#' @importFrom parameters model_parameters describe_distribution
#' @importFrom performance model_performance check_model
#' @importFrom effectsize eta_squared standardize_parameters effectsize cohens_d
#' @importFrom report report
#' @importFrom correlation correlation
#' @importFrom nnet multinom
#' @importFrom MASS polr
#' @importFrom lme4 lmer glmer
#' @importFrom modelsummary modelplot
#' @importFrom see plots
#' @importFrom ggplot2 ggsave
#' @importFrom htmltools tagList tags HTML
#' @importFrom knitr kable
#' @importFrom tibble as_tibble tibble
NULL

# Workaround for blockr.core serialization bug - title is character
# This should be fixed in blockr.core by adding a default method

#' @export
blockr_ser.character <- function(x, ...) {
  x
}

#' @export
blockr_ser.logical <- function(x, ...) {
  x
}

#' @export
blockr_ser.numeric <- function(x, ...) {
  x
}

#' @export
blockr_ser.integer <- function(x, ...) {
  x
}

#' @export
blockr_deser.character <- function(x, ...) {
  x
}

#' @export
blockr_deser.logical <- function(x, ...) {
  x
}

#' @export
blockr_deser.numeric <- function(x, ...) {
  x
}

#' @export
blockr_deser.integer <- function(x, ...) {
  x
}
