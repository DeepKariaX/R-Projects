library(shiny)
library(shinythemes)
library(ggplot2)
library(rmarkdown)


shinyServer(
  function(input,output,session) {
      data1 <- reactive({
          validate(need(input$dataset,""))
          inFile <- input$dataset
          if (is.null(inFile))
              return(NULL)
          df <- read.csv(inFile$datapath,na.strings = c("", "NA", "#N/A"))
          df2 <- df
          return(df2)    
      })
      
      data2 <- reactive({
          df3 <- data1()[,-1]
          updateSelectInput(session,"varselect1",choices=colnames(df3))
          updateSelectInput(session,"varselect2",choices=colnames(df3))
          return(df3)    
      })
      
      compute <- reactive({
          
          data   <- subset(data2(), select=c(input$varselect1,input$varselect2))
          colnames(data) <- c("x","y")
          
          if(input$k>nrow(unique(data))) updateNumericInput(session,"k", value = nrow(unique(data)))
          if(input$k<1)                  updateNumericInput(session,"k", value = 1)
          
          Kclust <- kmeans(data ,input$k)
          list(kmean.result = data.frame(data, cluster=as.factor(Kclust$cluster)),
               centers = as.data.frame(Kclust$centers))
          
      })
      
 output$table = DT::renderDataTable({
        data2()
      })
      
      output$summary <- renderPrint({
        summary(data2())
      })
      
     output$plot <- renderPlot({
          data=compute()
          ggplot(data=data$kmean.result, aes(x=x, y=y, color=cluster)) +
              geom_point(size=3) + geom_point(data=data$centers,
                                              aes(x=x, y=y, color='Center'), pch=17, size=7) +
              xlab(input$varselect1) + ylab(input$varselect2)
      })
     
  }
)


