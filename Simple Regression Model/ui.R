library(shiny)
library(shinythemes)
library(ggplot2)


fig.width <- 600
fig.height <- 450


shinyUI(fluidPage(theme =  shinytheme("cerulean"),
                  titlePanel("Linear Regression Model"),
      
                  sidebarLayout(
                    sidebarPanel(

                      fileInput('data', 'Choose file to upload', multiple = FALSE, accept = c('.text/ comma-separated-values','.csv','.xlsx','.txt','.text/ tab-separated-values')),
                      selectInput("inpx", "Select X-Axis For Plot", choices=c()),
                      selectInput("inpy", "Select Y-Axis For Plot", choices=c()),
 
                      numericInput("obs", "Observations:", 20, min = 1, max = 100),
                    ),
                    
                    mainPanel(
                      tabsetPanel(
                        
                        tabPanel("Table", DT::dataTableOutput("table")),
                        tabPanel("Plot", plotOutput("plot")),
                        tabPanel("Linear Regression", plotOutput("LinearPlot")),
                        tabPanel("Summary", verbatimTextOutput("summary")),
                        tabPanel("Linear Regression Summary", verbatimTextOutput("Linearsummary")),
                        
                      )
                    )
                    
                  )  
)
)