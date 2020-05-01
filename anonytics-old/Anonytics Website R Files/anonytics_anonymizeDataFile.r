library(shiny)
library(tools)
library(purrr)
library(openxlsx)
library(zip)
library(shinyalert)

datasetData <- c()

datasetsDir <- "Datasets/"
templatesDir <- "Templates/"

datasetTypes <- gsub("\\.txt$", "", list.files(datasetsDir, "\\.txt$"))
templateNames <- gsub("\\.csv$", "", list.files(templatesDir, "\\.csv$"))

displayTemplate <- function(input, output, session, vars)
{
  templateType <- input$template
  
  if (!is.null(input$inputFile))
  {
    if (templateType != "" && templateType != "None")
    {
      tryCatch({
        templateData <- read.csv(paste(templatesDir, templateType, ".csv", sep=""))
        
        checkedVar <- c()
        selectedVar <- c()
        
        for (i in 1:length(vars()))
        {
          columnName <- vars()[i]
          if (columnName %in% templateData[, 1])
          {
            checkedVar[columnName] <- TRUE
            selectedVar[columnName] <- paste(templateData[grep(columnName, templateData[, 1]), 2])
          }
          else
          {
            checkedVar[columnName] <- FALSE
            selectedVar[columnName] <- input[[paste(columnName, "Selected")]]
          }
        }
        
        output$inputContents <- renderUI(
          map(vars(), ~ fluidRow(column(width = 8,
            checkboxInput(paste(.x, "Checked"), .x, value = checkedVar[.x]),
            selectInput(paste(.x, "Selected"), "Dataset:", datasetTypes, selected = selectedVar[.x])
          )))
        )
      }, error = function(e)
      {
        shinyalert("Error", paste("An error occurred during template load: ", e), type = "error")
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
      checkboxInput("includeOriginal", "Include Original File", FALSE),
      checkboxInput("includeKeyFile", "Include Key File", FALSE),
      #fluidRow(column(width = 8,
      #  checkboxInput("includePassword", "Include Password", FALSE),
      #  textInput("password", NULL, "Enter a Password")
      #)),
      actionButton("anonymize", "Anonymize Data File")
    )
  )
)

# Server function
server <- function(input, output, session)
{
  datasetFiles <- list.files(path=datasetsDir, pattern="*.txt", full.names=TRUE, recursive=FALSE)
  for (file in datasetFiles)
  {
    name <- sub("\\.txt$", "", basename(file))
    data <- read.delim(file, comment.char="#")
    
    datasetData[name] <- data
  }
  
  observe({
    updateSelectInput(session, "template", label = "Select template", choices = c("None", templateNames))
  })
  
  data <- eventReactive(input$inputFile, {
    inputFile <- input$inputFile
    
    tryCatch({
      if (tolower(file_ext(inputFile$datapath)) == "xlsx")
      {
        read.xlsx(inputFile$datapath)
      }
      else if (tolower(file_ext(inputFile$datapath)) == "csv")
      {
        read.csv(inputFile$datapath, stringsAsFactors=FALSE)
      }
      else
      {
        shinyalert("Error", "Invalid file type.", type = "error")
      }
    }, error = function(e)
    {
      shinyalert("Error", paste("An error occurred during file upload: ", e), type = "error")
    })
  })
  
  vars <- reactive(colnames(data()))
  
  output$inputContents <- renderUI(
    map(vars(), ~ fluidRow(column(width = 8,
      checkboxInput(paste(.x, "Checked"), .x, TRUE),
      selectInput(paste(.x, "Selected"), "Dataset:", datasetTypes)
    )))
  )
  
  observeEvent(input$template, {
    displayTemplate(input, output, session, vars)
  })
  
  observeEvent(input$anonymize, {
    originalData <- data.frame(data())
    newData <- data.frame(data())
    
    anonymizedTypes <- c()
    anonymizedDatasets <- c()
    
    for (i in 1:length(vars()))
    {
      columnName <- vars()[i]
      if (input[[paste(columnName, "Checked")]] == TRUE)
      {
        dataset <- input[[paste(columnName, "Selected")]]
        dataFromDataset <- datasetData[[dataset]]
        
        anonymizedTypes <- append(anonymizedTypes, columnName)
        anonymizedDatasets <- append(anonymizedDatasets, dataset)
        
        numberOfRows <- length(data()[, i])
        datasetLength <- length(dataFromDataset)
        
        for (n in 1:numberOfRows)
        {
          newValue <- dataFromDataset[sample(1:datasetLength, 1)]
          newData[n, i] <- paste(newValue)
        }
      }
    }
    
    dataFiles <- c()
    
    if (input$includeOriginal == TRUE)
    {
      write.csv(originalData, "originalData.csv", row.names=FALSE)
      dataFiles <- append(dataFiles, "originalData.csv")
    }
    
    if (input$includeKeyFile == TRUE)
    {
      keyData <- data.frame(
        "Column Names" = anonymizedTypes,
        "Datasets" = anonymizedDatasets
      )
      write.csv(keyData, "key.csv", row.names=FALSE)
      dataFiles <- append(dataFiles, "key.csv")
    }
    
    write.csv(newData, "data.csv", row.names=FALSE)
    dataFiles <- append(dataFiles, "data.csv")
    
    #if (input$includePassword == TRUE && input$password != "")
    #{
    #  zipr("anonymizedFile.zip", files=dataFiles, flags=paste("--password", input$password))
    #}
    #else
    #{
      zipr("anonymizedFile.zip", files=dataFiles)
    #}
    unlink(dataFiles)
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
        columnDatasets <- c(colummnDatasets, input[[paste(name,"Selected")]])
      }
    }
    
    templateFrame <- data.frame(columnNames, columnDatasets)
    templateName <- input$saveTemplate
    filePath <- paste(templatesDir, paste(templateName, ".csv", sep=""), sep="")
    write.csv(templateFrame, filePath, row.names=FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
