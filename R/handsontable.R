#' Create a Handsontable Widget
#'
#' Creates an interactive table widget using the Handsontable JavaScript library.
#'
#' @param data A data frame to be displayed in the table
#' @param colHeaders Logical or character vector specifying column headers
#' @param rowHeaders Logical or character vector specifying row headers
#' @param fixedRowsTop Number of rows to fix at the top of the table
#' @param fixedColumnsLeft Number of columns to fix at the left of the table
#' @param mergeCells List of cell ranges to merge
#' @param licenseKey Handsontable license key for commercial use
#' @param height Height of the widget (e.g., "400px", "100%")
#' @param width Width of the widget (e.g., "400px", "100%")
#' @param manualColumnResize Allow manual column resizing
#' @param manualRowResize Allow manual row resizing
#' @param wordWrap Enable word wrapping in cells
#' @param hideGridLines Whether to hide the grid lines
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
      colHeaders <- colnames(data)  # Simplified this
    }
    data <- lapply(1:nrow(data), function(i) as.list(data[i, ]))
  } else {
    stop("data must be dataframe")
  }

  # Create widget
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
      wordWrap = wordWrap,
      hideGridLines = hideGridLines
    ),
    width = width,
    height = height,
    package = "reprexHandsontable"
  )
}

#' Create a Handsontable output element
#'
#' @param outputId The output identifier for the widget
#' @param width The width of the widget
#' @param height The height of the widget
#' @return A Shiny output element
#' @export
handsontableOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "reprexHandsontable", width, height, package = "reprexHandsontable")
}

#' Render a Handsontable widget
#'
#' @param expr An expression that returns a Handsontable widget
#' @param env The environment in which to evaluate expr
#' @param quoted Is expr a quoted expression?
#' @return A Shiny render function
#' @export
renderHandsontable <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) }
  htmlwidgets::shinyRenderWidget(expr, handsontableOutput, env, quoted = TRUE)
}
