library(shiny)
library(tools)
library(purrr)
library(readxl)
library(shinyalert)

# Define UI
ui <- fluidPage(
  useShinyalert(),
  titlePanel("Anonytics"),
  fluidRow(
    column(width = 8, fileInput("inputFile", "Upload a CSV/Excel File",
      accept = c(
       "text/csv",
       "text/comma-separated-values,text/plain",
       ".csv",
       ".xlsx"
      ))
    )
  ),
  
  fluidRow(
    column(width = 8, selectInput("template", "Template:", c(
      "None" = "none",
      "Names&Addresses" = "n&a",
      "Names" = "n",
      "Addresses" = "a"
    )))
  ),
  
  uiOutput("inputContents"),
  
  fluidRow(
    column(width = 8, textInput("saveTemplate", "Save Template", "Enter a template name"),
      actionButton("runTemplateSave", "Save")
    )
  ),
  
  fluidRow(
    column(width = 8,
      checkboxInput("include", "Include Original File", FALSE),
      actionButton("anonymize", "Anonymize Data File")
    )
  )
)

# Server function
server <- function(input, output, session)
{
  data <- eventReactive(input$inputFile, {
    inputFile <- input$inputFile
    
    tryCatch({
      if (tolower(file_ext(inputFile$datapath)) == "xlsx")
      {
        read_xlsx(inputFile$datapath)
      }
      else if (tolower(file_ext(inputFile$datapath)) == "csv")
      {
        read.csv(inputFile$datapath)
      }
      else
      {
        shinyalert("Error", "Invalid file type.", type = "error")
      }
    }, error = function(e)
    {
      shinyalert("Error", "An error occurred during file upload.", type = "error")
    })
  })
  
  vars <- reactive(colnames(data()))
  
  output$inputContents <- renderUI(
    map(vars(), ~ fluidRow(
      checkboxInput(paste(.x, "Checked"), .x, TRUE),
      selectInput(paste(.x, "Selected"), "Dataset:", c(
        "Test 1" = "t1",
        "Test 2" = "t2",
        "Test 3" = "t3"
      ))
    ))
  )
  
  observeEvent(input$anonymize, {
    checkedData <- map(vars(), ~ input[[paste(.x, "Checked")]])
    selectedData <- map(vars(), ~ input[[paste(.x, "Selected")]])
    
    for (i in 1:length(checkedData))
    {
      if (checkedData[i] == TRUE)
      {
        print(selectedData[i])
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
