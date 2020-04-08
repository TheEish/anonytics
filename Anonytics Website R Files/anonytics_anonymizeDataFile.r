library(shiny)
library(tools)
library(purrr)
library(readxl)
library(shinyalert)

datasetTypes <- c("Name" = "Name", "Address" = "Address", "SSN" = "SSN")

templatesDir <- "C:\\Users\\Matthew\\Documents\\_University\\Anonytics Website R Files\\Templates\\"
templateNames <- gsub("\\.csv$", "", list.files(templatesDir, "\\.csv$"))

displayTemplate <- function(input, output, session, vars)
{
  templateType <- input$template
  
  if (!is.null(input$inputFile))
  {
    if (templateType != "" && templateType != "None")
    {
      templateData <- read.csv(paste(templatesDir, templateType, ".csv", sep=""))
      
      checkedVar <- c()
      selectedVar <- c()
      
      for (i in 1:length(vars()))
      {
        columnName <- vars()[i]
        print(vars()[columnName])
        if (columnName %in% templateData[, 1])
        {
          # checkedVar[columnName] <- TRUE
          # selectedVar[columnName] <- templateData[grep(columnName, templateData[, 1]), 2]
          
          print(paste(templateData[grep(columnName, templateData[, 1]), 2]))
          print(paste(selectedVar[columnName]))
          # updateCheckboxInput(session, paste(columnName, "Checked"), value = TRUE)
          # updateSelectInput(session, paste(columnName, "Selected"), selected = templateData[index, 2])
        }
        else
        {
          # checkedVar[columnName] <- FALSE
          # selectedVar[columnName] <- input[[paste(columnName, "Selected")]]
          # updateCheckboxInput(session, paste(columnName, "Checked"), value = FALSE)
        }
      }
      
      output$inputContents <- renderUI(
        map(vars(), ~ fluidRow(
          checkboxInput(paste(.x, "Checked"), .x, value = checkedVar[.x]),
          selectInput(paste(.x, "Selected"), "Dataset:", datasetTypes, selected = selectedVar[.x])
        ))
      )
      
      tryCatch({
        
      }, error = function(e)
      {
        shinyalert("Error", "An error occurred during template load.", type = "error")
      })
    }
  }
}

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
    column(width = 8, selectInput("template", "Template:", c()))
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
  observe({
    updateSelectInput(session, "template", label = "Select template", choices = c("None", templateNames))
  })
  
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
      selectInput(paste(.x, "Selected"), "Dataset:", datasetTypes)
    ))
  )
  
  observeEvent(input$template, {
    displayTemplate(input, output, session, vars)
  })
  
  observeEvent(input$anonymize, {
    for (i in 1:length(vars()))
    {
      name <- vars()[i]
      if (input[[paste(name, "Checked")]] == TRUE)
      {
        print(input[[paste(name, "Selected")]])
      }
    }
  })
  
  observeEvent(input$runTemplateSave, {
    columnNames <- c()
    columnDatasets <- c()
    
    for (i in 1:length(vars()))
    {
      name <- vars()[i]
      if (input[[paste(name, "Checked")]] == TRUE)
      {
        columnNames <- c(columnNames, name)
        columnDatasets <- c(columnDatasets, input[[paste(name,"Selected")]])
      }
    }
    
    templateFrame <- data.frame(columnNames, columnDatasets)
    templateName <- input$saveTemplate
    filePath <- paste(templatesDir, paste(templateName, ".csv", sep=""), sep="")
    write.csv(templateFrame, filePath, row.names = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
