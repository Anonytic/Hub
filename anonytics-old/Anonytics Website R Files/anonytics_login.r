# Minimum viable example of a shiny app with a grid layout

library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Anonytics"),
  # Grid Layout
  fluidRow(column(width = 8, textInput("username", "Username", "Enter your username"))),
  fluidRow(column(width = 8, textInput("password", "Password", "Enter your password"))),
  fluidRow(column(width = 8, actionButton("logIn", "Log In")))
)


# Server function
server <- function(input, output) {
  
}

# Run the application
shinyApp(ui = ui, server = server)