library(shiny)
columnNames <- c("Name", "Address")
dummy <- c("Jimmy John", "123 Carol")
inputTypes <- c("None" = "none", "Name" = "n", "Address" = "a", "SSN" = "social")
# Define UI
ui <- fluidPage(
  titlePanel("Anonytics"),
  # Grid Layout
  fluidRow(column(width = 8, selectInput("template", "Template:",
                                  c("None" = "none", "Names&Addresses" = "n&a", "Names" = "n", "Addresses" = "a"))
  )),
  fluidRow(column(1, checkboxGroupInput("variable", "", columnNames)
                  ),
           column(2, selectInput("mapping", "", 
                                 inputTypes),
                  selectInput("mapping", "", 
                              inputTypes)
           )
           ),
  fluidRow(column(width = 8, textInput("saveTemplate", "Save Template", "Enter a template name"),
                  actionButton("runTemplateSave", "Save")
                  )
          ),
  fluidRow(column(width = 8,
                  checkboxInput("include", "Include Original File", FALSE),
                  actionButton("anonymize", "Anonymize Data File")
                  )
           ),
  textOutput("value")
)


# Server function
server <- function(input, output) {
  anonymize <- c()
  value <- renderText({ 
    selection <- paste(input$variable, collapse = ", ")
    })
  
  for (i in 1:length(columnNames)) {
    print(columnNames[i])
    if (columnNames[i] %in% value) {
      anonymize[i] <- TRUE
    }
    else {
      anonymize[i] <- FALSE
    }
  }
  
  if (length(columnNames) == length(dummy)) {
    for (i in 1: length(columnNames)) {
      if (anonymize[i]) {
        dummy[i] <- "Potato"
      }
    }
  }
  
  anonymizeTest <- data.frame(columnNames, anonymize)
  write.csv(anonymizeTest, "D:\\College Stuff\\Spring 2020\\CIS 499\\Anonytics Website R Files\\testAnon.csv", row.names = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)