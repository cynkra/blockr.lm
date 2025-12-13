#' Check Model Block
#'
#' Comprehensive model diagnostics using the performance package.
#' Displays a panel of diagnostic plots including normality, homoscedasticity,
#' collinearity (VIF), and influential observations.
#'
#' @param ... Forwarded to [new_plot_block()]
#'
#' @return A plot block object of class `check_model_block`.
#'
#' @details
#' Uses `performance::check_model()` which provides:
#' - **Linearity**: Check for non-linear patterns in residuals
#' - **Homogeneity of Variance**: Check for heteroscedasticity
#' - **Influential Observations**: Cook's distance and leverage
#' - **Collinearity**: Variance Inflation Factors (VIF)
#' - **Normality of Residuals**: Q-Q plot and density
#' - **Posterior Predictive Check**: Model fit assessment
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   model <- lm(mpg ~ cyl + hp, data = mtcars)
#'   serve(new_check_model_block(), list(data = model))
#' }
#'
#' @export
new_check_model_block <- function(...) {
 new_plot_block(

server = function(id, data) {
  moduleServer(
	id,
	function(input, output, session) {
	  list(
		expr = reactive({
		  parse(text = "plot(performance::check_model(data, check = c('vif', 'qq', 'normality', 'ncv', 'homogeneity', 'outliers')))")[[1]]
		}),
		state = list()
	  )
	}
  )
},
ui = function(id) {
  tagList(
	shinyjs::useShinyjs(),
	css_responsive_grid(),
	css_single_column("check-model"),
	div(
	  class = "block-container check-model-block-container",
	  div(
		class = "block-form-grid",
		div(
		  class = "block-section",
		  div(
			class = "block-section-grid",
			div(
			  class = "block-help-text",
			  "Comprehensive model diagnostics: linearity, homoscedasticity, ",
			  "collinearity (VIF), normality, and influential observations."
			)
		  )
		)
	  )
	)
  )
},
dat_valid = function(data) {
  stopifnot(inherits(data, "lm"))
},
class = "check_model_block",
...
 )
}
