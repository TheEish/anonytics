library(shiny)
library(readxl)
library(shinyalert)

# Define UI
ui <- fluidPage(
  useShinyalert(),
  titlePanel("Anonytics"),
  # Grid Layout
  fluidRow(column(width = 8, fileInput("inputFile", "Upload a CSV/Excel File",
    accept = c(
      "text/csv",
      "text/comma-separated-values,text/plain",
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      ".csv",
      ".xlsx"
    )))
  ),
  fluidRow(column(width = 8, actionButton("upload", "Upload CSV/Excel File")))
)

# Server function
server <- function(input, output)
{
  dataset <- reactive({
    validate(need(input$inputFile, "No file is selected."))
    inFile <- input$inputFile
    
    trycatch({
      if (tolower(file_ext(infile$datapath)) == "xlsx")
      {
        data <- read_xlsx(inFile$datapath)
      }
      else if (tolower(file_ext(infile$datapath)) == "csv")
      {
        data <- read.csv(inFile$datapath)
      }
    }, error = function(e)
      {
      shinyalert("Error", "Invalid file type.", type = "error")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
