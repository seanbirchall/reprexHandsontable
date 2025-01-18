library(shiny)
library(reprexHandsontable)

ui <- fluidPage(
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

  titlePanel("Handsontable Styling Demo"),

  sidebarLayout(
    sidebarPanel(
      # Basic table controls
      checkboxInput("hide_grid", "Hide Grid Lines", FALSE),

      # Styling sections
      hr(),
      h4("Column Styling"),
      checkboxGroupInput("cols_to_style", "Select columns to style:",
                         choices = 1:3, selected = NULL),
      actionButton("style_cols", "Style Columns"),

      hr(),
      h4("Row Styling"),
      checkboxGroupInput("rows_to_style", "Select rows to style:",
                         choices = 1:4, selected = NULL),
      actionButton("style_rows", "Style Rows"),

      hr(),
      h4("Cell Styling"),
      numericInput("cell_row", "Cell Row:", 1, min = 1, max = 4),
      numericInput("cell_col", "Cell Column:", 1, min = 1, max = 3),
      actionButton("style_cell", "Style Cell"),

      hr(),
      h4("Range Styling"),
      numericInput("range_start_row", "Start Row:", 1, min = 1, max = 4),
      numericInput("range_end_row", "End Row:", 2, min = 1, max = 4),
      numericInput("range_start_col", "Start Column:", 1, min = 1, max = 3),
      numericInput("range_end_col", "End Column:", 2, min = 1, max = 3),
      actionButton("style_range", "Style Range"),

      hr(),
      actionButton("clear_all", "Clear All Styles", class = "btn-warning")
    ),

    mainPanel(
      handsontableOutput("hot_table")
    )
  )
)

server <- function(input, output, session) {
  # Sample data
  test_data <- data.frame(
    Col1 = c("A1", "A2", "A3", "A4"),
    Col2 = c("B1", "B2", "B3", "B4"),
    Col3 = c("C1", "C2", "C3", "C4"),
    stringsAsFactors = FALSE
  )

  # Base table
  output$hot_table <- renderHandsontable({
    hot <- handsontable(
      data = test_data,
      colHeaders = TRUE,
      rowHeaders = TRUE,
      hideGridLines = input$hide_grid,
      height = "400px"
    )

    # Style columns if any are selected
    if (length(input$cols_to_style) > 0) {
      hot <- style_cols(
        hot,
        cols = as.numeric(input$cols_to_style),
        style = list(backgroundColor = "#f0f0f0"),
        class = "numeric-col"
      )
    }

    # Style rows if any are selected
    if (length(input$rows_to_style) > 0) {
      hot <- style_rows(
        hot,
        rows = as.numeric(input$rows_to_style),
        style = list(color = "#2c3e50"),
        class = "header-row"
      )
    }

    hot
  })

  # Handle single cell styling
  observeEvent(input$style_cell, {
    output$hot_table <- renderHandsontable({
      hot <- handsontable(
        data = test_data,
        colHeaders = TRUE,
        rowHeaders = TRUE,
        hideGridLines = input$hide_grid
      ) |>
        style_cells(
          cells = list(
            list(row = input$cell_row, col = input$cell_col)
          ),
          style = list(backgroundColor = "#ffcccb"),
          class = "special-cell"
        )
    })
  })

  # Handle range styling
  observeEvent(input$style_range, {
    output$hot_table <- renderHandsontable({
      hot <- handsontable(
        data = test_data,
        colHeaders = TRUE,
        rowHeaders = TRUE,
        hideGridLines = input$hide_grid
      ) |>
        style_cells(
          ranges = list(
            list(
              start_row = input$range_start_row,
              end_row = input$range_end_row,
              start_col = input$range_start_col,
              end_col = input$range_end_col
            )
          ),
          style = list(backgroundColor = "#e6ffe6"),
          class = "highlight-range"
        )
    })
  })

  # Clear all styles
  observeEvent(input$clear_all, {
    output$hot_table <- renderHandsontable({
      handsontable(
        data = test_data,
        colHeaders = TRUE,
        rowHeaders = TRUE,
        hideGridLines = input$hide_grid
      )
    })
  })
}

shinyApp(ui, server)
