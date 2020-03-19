# Minimum viable example of a shiny app with a grid layout

library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Anonytics"),
  # Grid Layout
  fluidRow(column(width = 8, selectInput("template", "Template:",
                                  c("None" = "none", "Names&Addresses" = "n&a", "Names" = "n", "Addresses" = "a"))
  )),
  fluidRow(column(1, checkboxInput("name", "Name", FALSE),
                  checkboxInput("address", "Address", FALSE)
                  ),
           column(2, selectInput("mapping", "", 
                                 c("None" = "none", "Name" = "n", "Address" = "a", "SSN" = "social")),
                  selectInput("mapping", "", 
                              c("None" = "none", "Name" = "n", "Address" = "a", "SSN" = "social"))
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
           )
)


# Server function
server <- function(input, output) {
}

# Run the application
shinyApp(ui = ui, server = server)