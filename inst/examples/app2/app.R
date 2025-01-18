library(shiny)
library(reprexHandsontable)

# Sample data
df <- mtcars

ui <- fluidPage(
  titlePanel("Handsontable Demo"),

  tags$head(
    tags$link(href = "https://cdn.jsdelivr.net/npm/handsontable@15.0.0/dist/handsontable.full.min.css", rel = "stylesheet"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/handsontable@15.0.0/dist/handsontable.full.min.js"),
    tags$style("
      .header-row {
        font-weight: bold !important;
        font-style: italic !important;
      }
      .numeric-col {
        background-color: #f8f9fa !important;
      }
      .highlight-range {
        border: 2px solid #4CAF50 !important;
      }
      .special-cell {
        border: 2px solid red !important;
        font-weight: bold !important;
      }
    ")
  ),

  sidebarLayout(
    sidebarPanel(
      # Basic settings
      checkboxInput("colHeaders", "Show Column Headers", TRUE),
      checkboxInput("rowHeaders", "Show Row Headers", FALSE),
      numericInput("fixedRowsTop", "Fixed Rows at Top", 0, min = 0),
      numericInput("fixedColumnsLeft", "Fixed Columns at Left", 0, min = 0),
      checkboxInput("manualColumnResize", "Enable Column Resize", TRUE),
      checkboxInput("manualRowResize", "Enable Row Resize", TRUE),
      checkboxInput("wordWrap", "Enable Word Wrap", FALSE),
      checkboxInput("hideGridLines", "Hide Grid Lines", FALSE),

      # Dimensions
      textInput("height", "Table Height", "400px"),
      textInput("width", "Table Width", "100%"),

      # Styling controls
      h4("Styling Options"),

      # Column styling
      selectInput("styleCol", "Style Column", choices = names(df), multiple = T),

      # Row styling
      selectInput("styleRow", "Style Row", choices = c(1:nrow(df)), multiple = T),

      # Cell styling
      numericInput("styleCell_row", "Style Cell Row", 1, min = 1, max = nrow(df)),
      numericInput("styleCell_col", "Style Cell Column", 1, min = 1, max = ncol(df)),

      # Range styling
      numericInput("styleRange_startRow", "Range Start Row", 1, min = 1),
      numericInput("styleRange_endRow", "Range End Row", 2, min = 1),
      numericInput("styleRange_startCol", "Range Start Column", 1, min = 1),
      numericInput("styleRange_endCol", "Range End Column", 2, min = 1),

      actionButton("applyStyle", "Apply Styling")
    ),

    mainPanel(
      handsontableOutput("hot")
    )
  )
)

server <- function(input, output, session) {
  # Create base table
  output$hot <- renderHandsontable({
    hot <- handsontable(
      data = df,
      colHeaders = input$colHeaders,
      rowHeaders = input$rowHeaders,
      fixedRowsTop = input$fixedRowsTop,
      fixedColumnsLeft = input$fixedColumnsLeft,
      manualColumnResize = input$manualColumnResize,
      manualRowResize = input$manualRowResize,
      wordWrap = input$wordWrap,
      hideGridLines = input$hideGridLines,
      height = input$height,
      width = input$width
    )

    # Return the base table if styling hasn't been triggered
    if (input$applyStyle == 0) return(hot)

    # Apply styling using pipes
    hot |>
      # Column styling
      style_cols(
        cols = which(names(df) == input$styleCol),
        style = list(background = "#f0f0f0", fontWeight = "bold"),
        class = "styled-column"
      ) |>
      # Row styling
      style_rows(
        rows = c(as.numeric(input$styleRow)),
        style = list(background = "#e6ffe6"),
        class = "styled-row"
      ) |>
      # Cell styling
      style_cells(
        cells = list(list(
          row = input$styleCell_row,
          col = input$styleCell_col
        )),
        style = list(background = "#ffe6e6"),
        class = "styled-cell"
      ) |>
      # Range styling
      style_cells(
        ranges = list(list(
          start_row = input$styleRange_startRow,
          end_row = input$styleRange_endRow,
          start_col = input$styleRange_startCol,
          end_col = input$styleRange_endCol
        )),
        style = list(background = "#e6e6ff"),
        class = "styled-range"
      )
  })
}

shinyApp(ui = ui, server = server)
