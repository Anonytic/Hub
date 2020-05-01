# Minimum viable example of a shiny app with a grid layout

library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Anonytics"),
  # Grid Layout
  fluidRow(column(width = 8, "Your file has been anonymized. If you would like it to have a password, enter one below:")),
  fluidRow(column(width = 8, textInput("password", "", "Enter a password"))),
  fluidRow(column(width = 8, actionButton("cancel", "Cancel"))),
  fluidRow(column(width = 8, actionButton("export", "Export Data File")))
)


# Server function
server <- function(input, output) {
  
}

# Run the application
shinyApp(ui = ui, server = server)