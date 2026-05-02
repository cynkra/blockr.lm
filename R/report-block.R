#' Report Block
#'
#' Wraps [report::report()] as a transform block. Generates an APA-style
#' narrative paragraph describing the model or test in plain English with
#' the relevant statistics inline. The unique wedge over jamovi for thesis
#' writers.
#'
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A `report_block` transform block.
#'
#' @export
new_report_block <- function(...) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        list(
          expr = reactive({
            quote(tibble::tibble(text = as.character(report::report(data))))
          }),
          state = list()
        )
      })
    },
    ui = function(id) {
      tagList()
    },
    class = "report_block",
    ...
  )
}

#' @export
block_output.report_block <- function(x, result, session) {
  ns <- session$ns
  text_id <- ns("report_text")

  renderUI({
    if (is.null(result) || nrow(result) == 0) {
      return(htmltools::tags$em("No upstream model or test object."))
    }
    raw <- paste(result$text, collapse = "\n\n")
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(report_block_css())),
      htmltools::tags$div(
        class = "report-card",
        htmltools::tags$div(
          class = "report-card-header",
          htmltools::tags$span(
            class = "report-card-label", "Auto-narrative"
          ),
          htmltools::tags$button(
            class = "report-copy-btn",
            type = "button",
            onclick = sprintf(
              "(function(){var el=document.getElementById('%s');if(!el)return;navigator.clipboard.writeText(el.innerText).then(function(){var b=event.target;var t=b.innerText;b.innerText='Copied';setTimeout(function(){b.innerText=t;},1200);});})()",
              text_id
            ),
            "Copy"
          )
        ),
        htmltools::tags$div(
          id = text_id,
          class = "report-narrative",
          htmltools::HTML(format_report_html(raw))
        )
      )
    )
  })
}

#' @export
block_ui.report_block <- function(id, x, ...) {
  tagList(uiOutput(NS(id, "result")))
}

#' Format raw `report::report()` text as APA-style HTML
#'
#' Splits the report text into paragraphs and bullet lists, applies APA
#' typography substitutions (italic stat letters, Greek letters, superscript
#' powers), and returns an HTML string ready to embed via `htmltools::HTML()`.
#'
#' @noRd
format_report_html <- function(raw) {
  if (length(raw) == 0 || !nzchar(raw)) return("")

  # Bullets in the raw text are separated by single newlines, not blank
  # lines -- normalise so each bullet becomes its own paragraph.
  raw <- gsub("\n(\\s*-\\s)", "\n\n\\1", raw)

  # Split into paragraphs on blank lines.
  paragraphs <- strsplit(raw, "\n\n+", perl = TRUE)[[1]]
  paragraphs <- paragraphs[nzchar(trimws(paragraphs))]

  # Walk paragraphs, group consecutive bullets into <ul>.
  is_bullet <- grepl("^\\s*-\\s", paragraphs)
  out <- character()
  i <- 1L
  while (i <= length(paragraphs)) {
    if (is_bullet[[i]]) {
      j <- i
      items <- character()
      while (j <= length(paragraphs) && is_bullet[[j]]) {
        item_text <- sub("^\\s*-\\s+", "", paragraphs[[j]])
        items <- c(items, paste0(
          "<li>", apa_format(item_text), "</li>"
        ))
        j <- j + 1L
      }
      out <- c(out, paste0(
        "<ul class=\"report-bullets\">", paste(items, collapse = ""), "</ul>"
      ))
      i <- j
    } else {
      out <- c(out, paste0("<p>", apa_format(paragraphs[[i]]), "</p>"))
      i <- i + 1L
    }
  }
  paste(out, collapse = "")
}

#' APA-style typography substitutions on a raw text segment
#'
#' HTML-escapes the input first, then applies a fixed list of regex
#' substitutions to render statistical conventions: italic stat letters
#' (p, t, F, M, SD), Greek letters (beta, eta, chi), superscript powers
#' (R^2, eta^2, chi^2).
#'
#' @noRd
apa_format <- function(x) {
  if (length(x) == 0) return("")
  x <- htmltools::htmlEscape(x)

  # Unicode characters used below (kept as \u escapes for R CMD check
  # portability):
  #   sup2 = U+00B2 superscript 2
  #   chi  = U+03C7 Greek small chi
  #   eta  = U+03B7 Greek small eta
  #   beta = U+03B2 Greek small beta
  sup2 <- "\u00b2"
  chi  <- "\u03c7"
  eta  <- "\u03b7"
  beta <- "\u03b2"

  # Squared terms -> Unicode superscript 2
  x <- gsub("\\bR2\\b", paste0("R", sup2), x)
  x <- gsub(
    paste0("\\badj\\.\\s*R", sup2, "\\b"),
    paste0("adj. <em>R</em>", sup2), x
  )
  x <- gsub(paste0("R", sup2), paste0("<em>R</em>", sup2), x)
  x <- gsub("\\bChi2\\b|\\bchi2\\b", paste0(chi, sup2), x)
  x <- gsub("\\beta2\\b", paste0(eta, sup2), x)

  # Greek letters
  x <- gsub(
    "\\bStd\\.\\s*beta\\b",
    paste0("<em>", beta, "</em><sub>std</sub>"), x
  )
  x <- gsub("\\bbeta\\b", paste0("<em>", beta, "</em>"), x)
  x <- gsub("\\beta\\b", paste0("<em>", eta, "</em>"), x)

  # Italic stat letters in standard contexts.
  # t(146) = ... | F(3, 146) = ... | p < .001 | p = 0.05
  # `<` and `>` were HTML-escaped earlier; match the entity form.
  x <- gsub("\\bt\\(([^)]+)\\)", "<em>t</em>(\\1)", x)
  x <- gsub("\\bF\\(([^)]+)\\)", "<em>F</em>(\\1)", x)
  x <- gsub(
    "(?<![A-Za-z])p\\s*(&lt;|&gt;|=)\\s*",
    "<em>p</em> \\1 ", x, perl = TRUE
  )
  x <- gsub("(?<![A-Za-z])M\\s*=", "<em>M</em> =", x, perl = TRUE)
  x <- gsub("\\bSD\\s*=", "<em>SD</em> =", x)
  x <- gsub("\\bMdn\\s*=", "<em>Mdn</em> =", x)
  x <- gsub("\\bMAD\\s*=", "<em>MAD</em> =", x)
  x <- gsub("\\bSE\\s*=", "<em>SE</em> =", x)

  x
}

#' @noRd
report_block_css <- function() {
  ".report-card {
     font-family: 'Charter', 'Iowan Old Style', Georgia, 'Times New Roman', serif;
     background: #ffffff;
     border: 1px solid #e6e6e6;
     border-radius: 6px;
     box-shadow: 0 1px 2px rgba(0,0,0,0.04);
     padding: 0;
     overflow: hidden;
   }
   .report-card-header {
     display: flex;
     align-items: center;
     justify-content: space-between;
     padding: 8px 14px;
     background: #fafafa;
     border-bottom: 1px solid #eee;
   }
   .report-card-label {
     font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif;
     font-size: 11px;
     text-transform: uppercase;
     letter-spacing: 0.6px;
     color: #888;
   }
   .report-copy-btn {
     font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif;
     font-size: 11px;
     padding: 3px 10px;
     background: #fff;
     border: 1px solid #d0d0d0;
     border-radius: 3px;
     color: #555;
     cursor: pointer;
     transition: background 0.12s ease;
   }
   .report-copy-btn:hover { background: #f0f0f0; }
   .report-copy-btn:active { background: #e0e0e0; }
   .report-narrative {
     padding: 18px 22px 22px 22px;
     font-size: 15px;
     line-height: 1.65;
     color: #2a2a2a;
     max-width: 78ch;
   }
   .report-narrative p {
     margin: 0 0 12px 0;
     text-align: justify;
     hyphens: auto;
   }
   .report-narrative p:last-child {
     margin-bottom: 0;
   }
   .report-narrative .report-bullets {
     margin: 4px 0 14px 0;
     padding-left: 22px;
   }
   .report-narrative .report-bullets li {
     margin-bottom: 8px;
     line-height: 1.55;
   }
   .report-narrative em {
     font-style: italic;
   }
   .report-narrative sub {
     font-size: 0.7em;
     vertical-align: sub;
   }"
}
