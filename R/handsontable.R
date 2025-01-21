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
#' @param readOnly read only table
#'
#' @return A Handsontable widget object
#' @export
handsontable <- function(
    data, colHeaders = FALSE, rowHeaders = FALSE,
    fixedRowsTop = 0, fixedColumnsLeft = 0, mergeCells = 0,
    licenseKey = "non-commercial-and-evaluation", height = "400px", width = "100%",
    manualColumnResize = TRUE, manualRowResize = TRUE, wordWrap = FALSE,
    hideGridLines = FALSE, readOnly = FALSE
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
      hideGridLines = hideGridLines,
      readOnly = readOnly
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

#' Style multiple columns
#'
#' @param hot A handsontable object
#' @param cols Numeric vector of column indices to style
#' @param style List of style properties
#' @param class Character vector of class names
#' @return A modified handsontable object
#' @export
style_cols <- function(hot, cols, style = NULL, class = NULL) {
  if (is.null(style) && is.null(class)) {
    return(hot)
  }

  # Create column styles for each col
  col_styles <- lapply(cols, function(col) {
    list(
      col = col - 1,  # Convert to 0-based index
      styles = if (!is.null(style)) list(style),
      classes = class
    )
  })

  hot$x$columnStyles <- c(hot$x$columnStyles, col_styles)
  hot
}

#' Style multiple rows
#'
#' @param hot A handsontable object
#' @param rows Numeric vector of row indices to style
#' @param style List of style properties
#' @param class Character vector of class names
#' @return A modified handsontable object
#' @export
style_rows <- function(hot, rows, style = NULL, class = NULL) {
  if (is.null(style) && is.null(class)) {
    return(hot)
  }

  # Create row styles for each row
  row_styles <- lapply(rows, function(row) {
    list(
      row = row - 1,  # Convert to 0-based index
      styles = if (!is.null(style)) list(style),
      classes = class
    )
  })

  hot$x$rowStyles <- c(hot$x$rowStyles, row_styles)
  hot
}

#' Style cells or cell ranges
#'
#' @param hot A handsontable object
#' @param cells List of cell specifications, each containing row and col indices
#' @param ranges List of cell ranges, each containing start_row, end_row, start_col, end_col
#' @param style List of style properties
#' @param class Character vector of class names
#' @return A modified handsontable object
#' @export
style_cells <- function(hot, cells = NULL, ranges = NULL, style = NULL, class = NULL) {
  if (is.null(style) && is.null(class)) {
    return(hot)
  }

  cell_styles <- list()

  # Process individual cells
  if (!is.null(cells)) {
    cell_styles <- c(cell_styles, lapply(cells, function(cell) {
      list(
        row = cell$row - 1,  # Convert to 0-based index
        col = cell$col - 1,  # Convert to 0-based index
        styles = if (!is.null(style)) list(style),
        classes = class
      )
    }))
  }

  # Process ranges
  if (!is.null(ranges)) {
    for(range in ranges) {
      rows <- seq(range$start_row - 1, range$end_row - 1)  # Convert to 0-based
      cols <- seq(range$start_col - 1, range$end_col - 1)  # Convert to 0-based

      for(row in rows) {
        for(col in cols) {
          cell_styles <- c(cell_styles, list(list(
            row = row,
            col = col,
            styles = if (!is.null(style)) list(style),
            classes = class
          )))
        }
      }
    }
  }

  hot$x$cellStyles <- c(hot$x$cellStyles, cell_styles)
  hot
}

#' Clear all styles from a handsontable
#'
#' @param hot A handsontable object
#' @param what Character vector specifying what to clear: "all", "rows", "cols", "cells"
#' @return A modified handsontable object
#' @export
clear_styles <- function(hot, what = "all") {
  if (what == "all" || what == "cols") hot$x$columnStyles <- NULL
  if (what == "all" || what == "rows") hot$x$rowStyles <- NULL
  if (what == "all" || what == "cells") hot$x$cellStyles <- NULL
  hot
}
