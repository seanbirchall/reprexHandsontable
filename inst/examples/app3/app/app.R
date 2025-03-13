library(shiny)
library(reprexHandsontable)  # Your package name

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
      .negative {
        color: red !important;
      }
      .positive {
        color: green !important;
      }
    ")
  ),

  titlePanel("Handsontable Style Preservation Test"),

  fluidRow(
    column(9,
           handsontableOutput("hot_table")
    ),
    column(3,
           h4("Style Tests:"),
           actionButton("style_cols", "Style Columns"),
           actionButton("style_rows", "Style Rows"),
           actionButton("style_cells", "Style Cells"),
           actionButton("style_all", "Apply All Styles"),
           actionButton("clear_styles", "Clear Styles"),
           hr(),
           actionButton("start_timer", "Start Auto Update (5s)"),
           actionButton("stop_timer", "Stop Auto Update"),
           hr(),
           checkboxInput("use_update", "Use updateData (preserve)", TRUE),
           checkboxInput("preserve_focus", "Try to preserve focus", TRUE),
           verbatimTextOutput("update_info")
    )
  ),

  # Add JavaScript to track focus and handle timer updates
  tags$script(HTML("
    var updateTimerId = null;
    var currentSelection = null;
    var updateCount = 0;

    // Listen for Shiny messages about timer status
    Shiny.addCustomMessageHandler('timer-status', function(message) {
      if (message.status === 'started') {
        // Start a JavaScript timer that will trigger updates
        updateTimerId = setInterval(function() {
          updateCount++;
          Shiny.setInputValue('timer_tick', updateCount);
        }, 5000); // Update every 5 seconds

        console.log('Timer started');
      } else if (message.status === 'stopped') {
        // Stop the timer
        if (updateTimerId !== null) {
          clearInterval(updateTimerId);
          updateTimerId = null;
          console.log('Timer stopped');
        }
      }
    });
  "))
)

server <- function(input, output, session) {
  # Store current selection
  current_selection <- reactiveVal(NULL)

  # Timer active flag
  timer_active <- reactiveVal(FALSE)

  # Update count for display
  update_count <- reactiveVal(0)

  # Initial data
  data_reactive <- reactive({
    data.frame(
      ID = 1:5,
      Value = sample(-50:50, 5),
      Type = sample(c("A", "B", "C"), 5, replace = TRUE),
      Cost = sample(100:500, 5),
      Status = sample(c("Active", "Pending", "Completed"), 5, replace = TRUE),
      stringsAsFactors = FALSE
    )
  })

  # Create the initial table
  output$hot_table <- renderHandsontable({
    handsontable(data_reactive(),
                 colHeaders = TRUE,
                 rowHeaders = TRUE,
                 height = "300px")
  })

  # Track selection
  observeEvent(input$hot_table_select, {
    current_selection(input$hot_table_select)
  })

  # Style columns
  observeEvent(input$style_cols, {
    hot <- renderHandsontable({
      handsontable(data_reactive(),
                   colHeaders = TRUE,
                   rowHeaders = TRUE) |>
        style_cols(cols = 2:3, class = "numeric-col") |>
        style_cols(cols = 4, style = list(fontWeight = "bold", textAlign = "right"))
    })
    output$hot_table <- hot
  })

  # Style rows
  observeEvent(input$style_rows, {
    hot <- renderHandsontable({
      handsontable(data_reactive(),
                   colHeaders = TRUE,
                   rowHeaders = TRUE) |>
        style_rows(rows = c(1, 3), class = "header-row") |>
        style_rows(rows = 5, style = list(backgroundColor = "#f0f8ff"))
    })
    output$hot_table <- hot
  })

  # Style cells
  observeEvent(input$style_cells, {
    hot <- renderHandsontable({
      data <- data_reactive()

      # Create styles based on data values
      cells <- list()
      for(i in 1:nrow(data)) {
        if(data$Value[i] < 0) {
          cells <- c(cells, list(list(row = i, col = 2)))
        }
      }

      handsontable(data,
                   colHeaders = TRUE,
                   rowHeaders = TRUE) |>
        style_cells(
          cells = cells,
          class = "negative"
        ) |>
        style_cells(
          ranges = list(
            list(start_row = 1, end_row = 3, start_col = 4, end_col = 5)
          ),
          class = "highlight-range"
        ) |>
        style_cells(
          cells = list(list(row = 2, col = 3)),
          class = "special-cell"
        )
    })
    output$hot_table <- hot
  })

  # Apply all styles
  observeEvent(input$style_all, {
    data <- data_reactive()

    # Build style collections
    cells <- list()
    for(i in 1:nrow(data)) {
      if(data$Value[i] < 0) {
        cells <- c(cells, list(list(row = i, col = 2, class = "negative")))
      } else {
        cells <- c(cells, list(list(row = i, col = 2, class = "positive")))
      }
    }

    hot <- renderHandsontable({
      hot <- handsontable(data,
                          colHeaders = TRUE,
                          rowHeaders = TRUE) |>
        # Style columns
        style_cols(cols = 2:3, class = "numeric-col") |>
        style_cols(cols = 4, style = list(fontWeight = "bold", textAlign = "right")) |>
        # Style rows
        style_rows(rows = c(1, 3), class = "header-row") |>
        style_rows(rows = 5, style = list(backgroundColor = "#f0f8ff")) |>
        # Style cells
        style_cells(
          cells = list(
            list(row = 1, col = 3),
            list(row = 2, col = 4)
          ),
          class = "special-cell"
        ) |>
        style_cells(
          ranges = list(
            list(start_row = 2, end_row = 4, start_col = 3, end_col = 4)
          ),
          class = "highlight-range"
        )

      # Apply value-based styling
      for(i in 1:nrow(data)) {
        if(data$Value[i] < 0) {
          hot <- hot |> style_cells(
            cells = list(list(row = i, col = 2)),
            class = "negative"
          )
        } else {
          hot <- hot |> style_cells(
            cells = list(list(row = i, col = 2)),
            class = "positive"
          )
        }
      }

      hot
    })
    output$hot_table <- hot
  })

  # Clear styles
  observeEvent(input$clear_styles, {
    output$hot_table <- renderHandsontable({
      handsontable(data_reactive(),
                   colHeaders = TRUE,
                   rowHeaders = TRUE)
    })
  })

  # Start timer
  observeEvent(input$start_timer, {
    timer_active(TRUE)
    update_count(0)
    session$sendCustomMessage("timer-status", list(status = "started"))
  })

  # Stop timer
  observeEvent(input$stop_timer, {
    timer_active(FALSE)
    session$sendCustomMessage("timer-status", list(status = "stopped"))
  })

  # Handle timer ticks - this is where we update the data
  observeEvent(input$timer_tick, {
    req(timer_active())

    # Store current selection before update
    sel <- current_selection()

    # Generate new random data
    new_data <- data.frame(
      ID = 1:5,
      Value = sample(-50:50, 5),
      Type = sample(c("A", "B", "C"), 5, replace = TRUE),
      Cost = sample(100:500, 5),
      Status = sample(c("Active", "Pending", "Completed"), 5, replace = TRUE),
      stringsAsFactors = FALSE
    )

    # Update count for display
    update_count(as.numeric(input$timer_tick))

    # Use updateData or loadData based on checkbox
    if(input$use_update) {
      # Update with the new data (preserves styling)
      updateHandsontable(session, "hot_table", new_data)
    } else {
      # Load new data (resets styling)
      loadHandsontable(session, "hot_table", new_data)
    }

    # Try to restore focus if checkbox is checked
    if (input$preserve_focus && !is.null(sel)) {
      # Send a custom message to select the cell again
      session$sendCustomMessage("handsontable-select", list(
        id = "hot_table",
        row = sel$r - 1,  # Handsontable uses 0-based indexing
        col = sel$c - 1
      ))
    }
  })

  # Display update info
  output$update_info <- renderText({
    paste("Updates:", update_count(),
          "\nMethod:", ifelse(input$use_update, "updateData", "loadData"))
  })

  # Clean up timer on session end
  session$onSessionEnded(function() {
    session$sendCustomMessage("timer-status", list(status = "stopped"))
  })
}

shinyApp(ui, server)
