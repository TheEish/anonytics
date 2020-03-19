# Minimum viable example of a shiny app with a grid layout

library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Anonytics"),
  # Grid Layout
  fluidRow(column(width = 8, fileInput("file1", "Upload a Data File", accept = c(
    "text/csv","text/comma-separated-values,text/plain",
    ".csv")
  ))),
  fluidRow(column(width = 8, actionButton("upload", "Upload Data File")))
)


# Server function
server <- function(input, output) {
  
}

# Run the application
shinyApp(ui = ui, server = server)