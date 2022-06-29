library(shiny)
library(ggplot2)
library(gplots)
library(vcd)
library(corrplot)

shinyApp(
  ui = fluidPage(
    titlePanel("Chi-Square Test"),
    sidebarLayout(
      sidebarPanel(
        fileInput("dataset", "Choose CSV file", multiple = FALSE,
                  accept = c("text/csv","text/comma-separated-values,text/plain",".csv,.txt"),
                  width = NULL, buttonLabel = "Browse...",
                  placeholder = "No file selected"),
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Data Table", DT::dataTableOutput("table")),
                    tabPanel("Plot",
                             h3("Ballon Plot"),
                             plotOutput("ballonplot"),
                             br(),
                             h3("Mosaic Plot"),
                             plotOutput("mosaicplot"),
                             br(),
                             h3("Mosaic Plot Using VCD"),
                             plotOutput("mosaicplotvcd")),
                    tabPanel("Chi Square",
                             h3("Chi Square Test"),
                             verbatimTextOutput("chisquare"),
                             br(),
                             h3("Chi Square Observed Values"),
                             verbatimTextOutput("chisquareobs"),
                             br(),
                             h3("Chi Square Expected Values"),
                             verbatimTextOutput("chisquarexp"),
                             br(),
                             h3("Chi Square Residual Values"),
                             verbatimTextOutput("chisquarres"),
                             br(),
                             h3("Chi Square Contribution Values"),
                             verbatimTextOutput("chisquarecontri")),
                    tabPanel("Corr Plot - Result",
                             h3("Corr Plot Residual"),
                             plotOutput("corrplotres"),
                             br(),
                             h3("Corr Plot Contribution"),
                             plotOutput("corrplotvis"))
                    
        )
      )
    )
  ),
  server = function(input,output,session) {
    data1 <- reactive({
      validate(need(input$dataset,""))
      inFile <- input$dataset
      if (is.null(inFile))
        return(NULL)
      df <- read.csv(inFile$datapath,na.strings = c("", "NA", "#N/A"))
      df2 <- df
      return(df2)
    })
    
    output$table = DT::renderDataTable({
      data1()
    })
    
    output$ballonplot <- renderPlot({
      inFile <- input$dataset
      read <- read.delim(inFile$datapath, row.names = 1)
      dt <- as.table(as.matrix(read))
      balloonplot(t(dt), xlab ="", ylab="",label = FALSE, show.margins = FALSE)
    })
    
    output$mosaicplot <- renderPlot({
      inFile <- input$dataset
      read <- read.delim(inFile$datapath, row.names = 1)
      dt <- as.table(as.matrix(read))
      mosaicplot(dt, shade = TRUE)
    })
    
    output$mosaicplotvcd <- renderPlot({
      inFile <- input$dataset
      read <- read.delim(inFile$datapath, row.names = 1)
      dt <- as.table(as.matrix(read))
      assoc(head(dt, 5), shade = TRUE)
    })
    
    output$chisquare <- renderPrint({
      inFile <- input$dataset
      read <- read.delim(inFile$datapath, row.names = 1)
      dt <- as.table(as.matrix(read))
      chisq <- chisq.test(dt)
      chisq
    })
    
    output$chisquareobs <- renderPrint({
      inFile <- input$dataset
      read <- read.delim(inFile$datapath, row.names = 1)
      dt <- as.table(as.matrix(read))
      chisq <- chisq.test(dt)
      chisq$observed
    })
    
    output$chisquarexp <- renderPrint({
      inFile <- input$dataset
      read <- read.delim(inFile$datapath, row.names = 1)
      dt <- as.table(as.matrix(read))
      chisq <- chisq.test(dt)
      round(chisq$expected,2)
    })
    
    output$chisquarres <- renderPrint({
      inFile <- input$dataset
      read <- read.delim(inFile$datapath, row.names = 1)
      dt <- as.table(as.matrix(read))
      chisq <- chisq.test(dt)
      round(chisq$residuals, 2)
    })
    
    output$chisquarecontri <- renderPrint({
      inFile <- input$dataset
      read <- read.delim(inFile$datapath, row.names = 1)
      dt <- as.table(as.matrix(read))
      chisq <- chisq.test(dt)
      contrib <- 100*chisq$residuals^2/chisq$statistic
      round(contrib, 2)
    })
    
    output$corrplotres <- renderPlot({
      inFile <- input$dataset
      read <- read.delim(inFile$datapath, row.names = 1)
      dt <- as.table(as.matrix(read))
      chisq <- chisq.test(dt)
      
      corrplot(t(chisq$residuals), is.cor = FALSE)
    })
    
    output$corrplotvis <- renderPlot({
      inFile <- input$dataset
      read <- read.delim(inFile$datapath, row.names = 1)
      dt <- as.table(as.matrix(read))
      chisq <- chisq.test(dt)
      contrib <- 100*chisq$residuals^2/chisq$statistic
      corrplot(t(contrib), is.cor = FALSE)
    })
    
  }
)
