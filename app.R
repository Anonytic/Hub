library(shiny)
library(tools)
library(purrr)
library(openxlsx)
library(zip)
library(shinyalert)

datasetData <- c()
options(shiny.maxRequestSize = 20*1024^2)

displayTemplate <- function(input, output, session, vars)
{
    template <- input$templateInputFile
    
    if (!is.null(input$inputFile))
    {
        if (!is.null(template))
        {
            tryCatch({
                templateData <- read.csv(template$datapath)
                
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
                                                  checkboxInput(paste(.x, "Checked"), .x, value = checkedVar[.x])
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
        column(width = 8,
               fileInput("inputFile", "Upload a CSV/Excel File",
                         accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv",
                             ".xlsx"
                         )
               )
        )
    ),
    
    uiOutput("templateContents"),
    
    uiOutput("inputContents"),
    
    uiOutput("templateSaveContents"),
    
    uiOutput("downloadContents")
)

# Server function
server <- function(input, output, session)
{
    data <- eventReactive(input$inputFile, {
        inputFile <- input$inputFile
        
        output$downloadContents <- renderUI(
            fluidRow(column(width = 8,
                            checkboxInput("includeOriginal", "Include Original File", FALSE),
                            downloadButton("anonymize", "Anonymize Data File")
            ))
        )
        
        output$templateContents <- renderUI(
            fluidRow(column(width = 8,
                            fileInput("templateInputFile", "Upload a Template File",
                                      accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv"
                                      )
                            )
            )),
        )
        
        output$templateSaveContents <- renderUI(
            fluidRow(
                column(width = 8,
                       downloadButton("runTemplateSave", "Save Template")
                )
            )
        )
        
        tryCatch({
            if (tolower(file_ext(inputFile$datapath)) == "xlsx")
            {
                data <- read.xlsx(inputFile$datapath)
            }
            else if (tolower(file_ext(inputFile$datapath)) == "csv")
            {
                data <- read.csv(inputFile$datapath, stringsAsFactors=FALSE)
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
                                      checkboxInput(paste(.x, "Checked"), .x, TRUE)
        )))
    )
    
    observeEvent(input$templateInputFile, {
        displayTemplate(input, output, session, vars)
    })
    
    output$anonymize <- downloadHandler(
        filename = function()
        {
            paste("anonymizedFile", "zip", sep=".")
        },
        content = function(file)
        {
            originalData <- data.frame(data())
            newData <- data.frame(data())
            
            anonymizedTypes <- c()
            anonymizedDatasets <- c()
            
            for (i in 1:length(vars()))
            {
                columnName <- vars()[i]
                if (input[[paste(columnName, "Checked")]] == TRUE)
                {
                    column <- data()[, i]
                    
                    numberOfRows <- length(column)
                    
                    for (n in 1:numberOfRows)
                    {
                        index <- sample(1:numberOfRows, 1)
                        if (index == n)
                        {
                            index = index + 1
                            if (index > numberOfRows)
                                index = 1
                        }
                        newValue <- column[index]
                        newData[n, i] <- paste(newValue)
                    }
                }
            }
            
            dataFiles <- c()
            setwd(tempdir())
            
            if (input$includeOriginal == TRUE)
            {
                fileName <- "originalData.csv"
                
                write.csv(originalData, fileName, row.names=FALSE)
                dataFiles <- append(dataFiles, fileName)
            }
            
            fileName <- "data.csv"
            
            write.csv(newData, fileName, row.names=FALSE)
            dataFiles <- append(dataFiles, fileName)
            
            zipr(file, files=dataFiles)
        },
        contentType = "application/zip"
    )
    
    output$runTemplateSave <- downloadHandler(
        filename = function()
        {
            paste("template-", Sys.Date(), ".csv", sep="")
        },
        content = function(file)
        {
            columnNames <- c()
            
            for (i in 1:length(vars()))
            {
                name <- vars()[i]
                if (input[[paste(name, "Checked")]] == TRUE)
                {
                    columnNames <- append(columnNames, name)
                }
            }
            
            templateFrame <- data.frame(columnNames)
            write.csv(templateFrame, file, row.names=FALSE)
        },
        contentType = ".csv"
    )
}

# Run the application
shinyApp(ui = ui, server = server)
