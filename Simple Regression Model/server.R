library(shiny)
library(ggplot2)

shinyServer(function(input, output,session) {
  
  data1 <- reactive({
    validate(need(input$data,""))
    inFile <- input$data
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath,na.strings = c("", "NA", "#N/A"))
    df2 <- df
    return(df2)    
  })
  
  data2 <- reactive({
    df3 <- data1()[,-1]
    updateSelectInput(session,"inpx",choices=colnames(df3))
    updateSelectInput(session,"inpy",choices=colnames(df3))
    return(df3)    
  })
  
  output$table <- DT::renderDataTable({
    data2()
    
  })
  
  output$plot <- renderPlot({
    
    inFile <- input$data
  
      if(is.null(input$data))    
        return(NULL) 
      file <- read.csv(inFile$datapath)
      
      file<- file[1:input$obs,]
      x<- file[,input$inpx] 
      y<- file[,input$inpy]
    
    
    a<- input$intercept
    b<-input$slope
    regression <- lm(x~y)
    intercept1<- coef(regression)["(Intercept)"] 
    slope<- coef(regression)["y"] 
    
    plot(x, y, cex = 1, font = 3)
    points(x, y, pch = 16, cex = 0.8, col = "red",xlab = "Explanatory Variable",ylab = "Outcome Variable")
    abline(regression,col="black")
    title("Linear Regression")
    
  })
  
  
  
  output$LinearPlot <- renderPlot({
    
    inFile <- input$data
    
      
      if(is.null(input$data))     
        return(NULL) 
      file <- read.csv(inFile$datapath)
      
      file<- file[1:input$obs,]
      x<- file[,input$inpx] 
      y<- file[,input$inpy]

    
    
    par(mfrow=c(2,2))
    plot(lm(x~y),col.axis = "blue") 
    points(x, y, pch = 16, cex = 0.8, col = "red")
  })
  
  output$summary <-renderPrint({
    inFile <- input$data
      
      if(is.null(input$data))     
        return()
      
      
      file <- read.csv(inFile$datapath)
      
      file<- file[1:input$obs,]
    
    summary(file)
    
  })
  
  
  output$Linearsummary <-renderPrint({
    inFile <- input$data
    
      
      if(is.null(input$data))     
        return()
      
      file <- read.csv(inFile$datapath)
      
      file<- file[1:input$obs,]
      x<- file[,input$inpx] 
      y<- file[,input$inpy]
    
    summary(lm(x~y))
    
  })
  

})