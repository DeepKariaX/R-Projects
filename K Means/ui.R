library(shiny)
library(shinythemes)
library(ggplot2)
library(rmarkdown)

shinyUI(
  fluidPage(
    shinythemes::themeSelector(),
    titlePanel("K-Means Clustering"),
    sidebarLayout(
      sidebarPanel(
          fileInput("dataset", "Choose CSV file", multiple = FALSE,
                    accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
                    width = NULL, buttonLabel = "Browse...",
                    placeholder = "No file selected"),
          selectInput("varselect1", "Select 1st variable For Clustering", choices=c()),
          selectInput("varselect2", "Select 2nd variable For Clustering", choices=c()),
          numericInput('k', 'Number Of Clusters', value = 3,
                       min=1, step=1)
          
          
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Data Table", DT::dataTableOutput("table")),
                    tabPanel("Summary", verbatimTextOutput("summary")),
                    tabPanel("Clusters", plotOutput("plot")),
                    tabPanel("About", 
                             h2("K-Means Clustering"),
                             p("K-Means Clustering is an Unsupervised Learning algorithm, which groups the unlabeled dataset into different clusters. Here K defines the number of pre-defined clusters that need to be created in the process, as if K=2, there will be two clusters, and for K=3, there will be three clusters, and so on."),
                             p("K-means clustering uses centroids, K different randomly-initiated points in the data, and assigns every data point to the nearest centroid. After every point has been assigned, the centroid is moved to the average of all of the points assigned to it.")
                             )
                    
        )
      )
    )
  )
)
