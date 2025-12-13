# blockr.lm

Linear modeling blocks for [blockr](https://github.com/blockr-org/blockr).

## Installation

```r
# Install from GitHub
pak::pak("blockr-org/blockr.lm")
```

## Blocks

| Block | Description | Input | Output |
|-------|-------------|-------|--------|
| `new_lm_block()` | Fit linear model with formula builder | data.frame | lm object |
| `new_coef_block()` | Coefficients table with CI | lm object | data.frame |
| `new_anova_block()` | ANOVA decomposition table | lm object | data.frame |
| `new_diagnostic_plot_block()` | 4-panel diagnostic plots | lm object | plot |
| `new_residual_explorer_block()` | Interactive residual plot | lm object | plotly |

## Usage

```r
library(blockr)

run_app(

blocks = c(
    data = new_dataset_block("mtcars"),
    model = new_lm_block(
      response = "mpg",
      predictors = c("cyl", "hp", "wt")
    ),
    coef = new_coef_block(conf_int = TRUE),
    anova = new_anova_block(),
    plots = new_diagnostic_plot_block(),
    resid = new_residual_explorer_block()
  ),
  links = c(
    new_link("data", "model", "data"),
    new_link("model", "coef", "data"),
    new_link("model", "anova", "data"),
    new_link("model", "plots", "data"),
    new_link("model", "resid", "data")
  )
)
```

## Workflow

```
data.frame ──► new_lm_block() ──► new_coef_block()
                    │
                    ├──► new_anova_block()
                    │
                    ├──► new_diagnostic_plot_block()
                    │
                    └──► new_residual_explorer_block()
```
