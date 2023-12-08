# Install and load required packages
if (!require("shiny")) install.packages("shiny", dependencies = TRUE)
if (!require("shinyjs")) install.packages("shinyjs", dependencies = TRUE)
library(shiny)
library(shinyjs)

# Define the UI
ui <- fluidPage(
  titlePanel("Course Marks Calculator"),
  sidebarLayout(
    sidebarPanel(
      textInput("deliverable", "Deliverable Name:"),
      numericInput("weightage", "Weightage (%):", value = 0, min = 0, max = 100),
      numericInput("max_marks", "Maximum Marks:", value = 0, min = 0),
      numericInput("scored_marks", "Scored Marks:", value = 0, min = 0),
      actionButton("add_btn", "Add Deliverable"),
      br(),
      textOutput("error_message"),
      br(),
      actionButton("calculate_btn", "Calculate Overall Percentage"),
      actionButton("clear_all_btn", "Clear All")
    ),
    mainPanel(
      tableOutput("deliverables_table"),
      br(),
      textOutput("overall_percentage")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  # Initialize reactive values
  deliverables <- reactiveValues(
    data = data.frame(
      Deliverable = character(),
      Weightage = numeric(),
      MaxMarks = numeric(),
      ScoredMarks = numeric()
    )
  )
  
  # Add deliverable button action
  observeEvent(input$add_btn, {
    if (input$deliverable != "" && input$weightage > 0 && input$weightage <= 100 &&
        input$max_marks >= 0 && input$scored_marks >= 0) {
      new_row <- data.frame(
        Deliverable = input$deliverable,
        Weightage = input$weightage,
        MaxMarks = input$max_marks,
        ScoredMarks = input$scored_marks
      )
      deliverables$data <- rbind(deliverables$data, new_row)
      clearInputs()
    } else {
      updateError("Please fill in all the fields with valid values.")
    }
  })
  
  # Calculate overall percentage button action
  observeEvent(input$calculate_btn, {
    if (sum(deliverables$data$Weightage)==100) {
      overall_percentage <- weighted.mean(
        deliverables$data$ScoredMarks/deliverables$data$MaxMarks,
        w = deliverables$data$Weightage/100
      )*100
      output$overall_percentage <- renderText({
        paste("Overall Percentage: ", round(overall_percentage, 2), "%")
      })
    } else {
      updateError("Total weightage should be 100%.")
    }
  })
  
  # Clear input fields
  clearInputs <- function() {
    updateTextInput(session, "deliverable", value = "")
    updateNumericInput(session, "weightage", value = 0)
    updateNumericInput(session, "max_marks", value = 0)
    updateNumericInput(session, "scored_marks", value = 0)
  }
  
  # Clear all button action
  observeEvent(input$clear_all_btn, {
    deliverables$data <- data.frame(
      Deliverable = character(),
      Weightage = numeric(),
      MaxMarks = numeric(),
      ScoredMarks = numeric()
    )
  })
  
  # Render the deliverables table
  output$deliverables_table <- renderTable({
    deliverables$data
  })
  
  # Update error message
  updateError <- function(message) {
    output$error_message <- renderText({
      message
    })
  }
}

# Run the Shiny app
shinyApp(ui, server)
