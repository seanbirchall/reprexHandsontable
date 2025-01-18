#' Create a Handsontable Widget
#'
#' Creates an interactive table widget using Handsontable library
#'
#' @param data A data frame to display in the table
#' @param colHeaders Logical or character vector for column headers
#' @param rowHeaders Logical or character vector for row headers
#' @param fixedRowsTop Number of rows to fix at the top
#' @param fixedColumnsLeft Number of columns to fix at the left
#' @param mergeCells List of cells to merge
#' @param licenseKey Handsontable license key
#' @param height Height of the widget (e.g., "400px", "100%")
#' @param width Width of the widget (e.g., "400px", "100%")
#' @param manualColumnResize Allow manual column resizing
#' @param manualRowResize Allow manual row resizing
#' @param wordWrap Enable word wrapping in cells
#' @param hideGridLines Hide the grid lines in the table
#'
#' @return A Handsontable widget object
#' @export
handsontable <- function(
    data, colHeaders = FALSE, rowHeaders = FALSE,
    fixedRowsTop = 0, fixedColumnsLeft = 0, mergeCells = 0,
    licenseKey = "non-commercial-and-evaluation", height = "400px", width = "100%",
    manualColumnResize = TRUE, manualRowResize = TRUE, wordWrap = FALSE,
    hideGridLines = FALSE
) {

  if (is.data.frame(data)) {
    if (is.null(colHeaders)) {
      colHeaders <- excel_headers(n = ncol(data))
      names(data) <- colHeaders
    }
    data <- lapply(1:nrow(data), function(i) as.list(data[i, ]))
  } else {
    stop("data must be dataframe")
  }

  htmlwidgets::createWidget(
    name = "reprexHandsontable",
    x = list(
      data = data,
      colHeaders = colHeaders,
      rowHeaders = rowHeaders,
      fixedRowsTop = fixedRowsTop,
      fixedColumnsLeft = fixedColumnsLeft,
      mergeCells = mergeCells,
      licenseKey = licenseKey,
      manualColumnResize = manualColumnResize,
      manualRowResize = manualRowResize,
      wordWrap = wordWrap
    ),
    width = width,
    height = height,
    package = "reprexHandsontable",
    dependencies = list(
      htmltools::htmlDependency(
        name = "reprexHandsontable",
        version = "15",
        src = "inst/htmlwidgets/lib",
        script = "handsontable.full.js",
        stylesheet = "handsontable.css"
      ),
      htmltools::htmlDependency(
        name = "reprexHandsontable",
        version = "15",
        src = "inst/htmlwidgets",
        script = c("handsontable.js", "handsontable.full.js")
      )
    )
  )
}

#' handsontable output
#'
#' @param outputId
#' @param width
#' @param height
#'
#' @export
handsontableOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "reprexHandsontable", width, height, package = "reprexHandsontable")
}

#' handsontable render
#'
#' @param expr
#' @param env
#' @param quoted
#'
#' @export
renderHandsontable <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) }
  htmlwidgets::shinyRenderWidget(expr, handsontableOutput, env, quoted = TRUE)
}
