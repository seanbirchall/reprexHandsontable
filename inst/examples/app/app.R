library(shiny)

# Sample data
mtcars_styled <- mtcars[1:10, 1:6]

ui <- fluidPage(
  titlePanel("Handsontable Demo"),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "handsontable.css")
  ),

  sidebarLayout(
    sidebarPanel(
      checkboxInput("show_headers", "Show Headers", TRUE),
      checkboxInput("enable_resize", "Enable Resizing", TRUE),
      checkboxInput("word_wrap", "Word Wrap", FALSE),
      numericInput("fixed_rows", "Fixed Rows", 0, min = 0, max = 5),
      numericInput("fixed_cols", "Fixed Columns", 0, min = 0, max = 3),
      actionButton("apply_style", "Apply Styling"),
      actionButton("clear_style", "Clear Styling"),
      hr(),
      verbatimTextOutput("selection_info"),
      verbatimTextOutput("change_info")
    ),

    mainPanel(
      handsontableOutput("hot_table")
    )
  ),

  tags$footer(
      tags$script(type = "text/javascript", src = "htmlwidgets.js"),
      tags$script(type = "text/javascript", src = "https://cdnjs.cloudflare.com/ajax/libs/handsontable/15.0.0/handsontable.full.js"),
      tags$script(type = "text/javascript", src = "handsontable.min.js"),
      tags$script(type = "text/javascript", src = "handsontable.js")
  )
)

server <- function(input, output, session) {
  # Reactive values for styling
  rv <- reactiveValues(
    columnStyles = NULL,
    rowStyles = NULL,
    cellStyles = NULL
  )

  # Render the table
  output$hot_table <- renderHandsontable({
    hot <- handsontable(
      data = mtcars_styled,
      colHeaders = input$show_headers,
      rowHeaders = input$show_headers,
      fixedRowsTop = input$fixed_rows,
      fixedColumnsLeft = input$fixed_cols,
      manualColumnResize = input$enable_resize,
      manualRowResize = input$enable_resize,
      wordWrap = input$word_wrap,
      height = "400px"
    )

    # Apply any stored styles
    if (!is.null(rv$columnStyles)) {
      hot$x$columnStyles <- rv$columnStyles
    }
    if (!is.null(rv$rowStyles)) {
      hot$x$rowStyles <- rv$rowStyles
    }
    if (!is.null(rv$cellStyles)) {
      hot$x$cellStyles <- rv$cellStyles
    }

    hot
  })

  # Handle style application
  observeEvent(input$apply_style, {
    # Example styling - feel free to modify!
    rv$columnStyles <- list(
      list(
        col = 0,
        styles = list(list(backgroundColor = "#f0f0f0", fontWeight = "bold")),
        classes = "numeric-col"
      )
    )

    rv$rowStyles <- list(
      list(
        row = 0,
        styles = list(list(backgroundColor = "#e6ffe6")),
        classes = "header-row"
      )
    )

    rv$cellStyles <- list(
      list(
        row = 1,
        col = 1,
        styles = list(
          list(backgroundColor = "#ffe6e6", color = "#ff0000", fontWeight = "bold")
        ),
        classes = "highlight-cell"
      )
    )
  })

  # Clear styles
  observeEvent(input$clear_style, {
    rv$columnStyles <- NULL
    rv$rowStyles <- NULL
    rv$cellStyles <- NULL
  })

  # Display selection information
  output$selection_info <- renderPrint({
    if (is.null(input$hot_table_select)) return("No selection")
    sel <- input$hot_table_select
    cat("Selected cells:\n")
    cat("From row", sel$r, "col", sel$c, "\n")
    cat("To row", sel$r2, "col", sel$c2, "\n")
  })

  # Display changes
  output$change_info <- renderPrint({
    if (is.null(input$hot_table_update)) return("No changes")
    changes <- input$hot_table_update$update
    cat("Last change:\n")
    print(changes)
  })
}

shinyApp(ui, server)
