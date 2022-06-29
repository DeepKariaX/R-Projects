library(shiny)
library(ggplot2)
library(plotly)#for plot
library(rmarkdown)
library(knitr)
library(shinythemes)


ui <- fluidPage(theme = shinytheme("flatly"),
    
    # Application title
    titlePanel("Simple Linear Regression"),
    
    withMathJax(),
    
    sidebarLayout(
        sidebarPanel(
            textInput("x", "x", value = "90, 100, 90, 80, 87, 75", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
            textInput("y", "y", value = "950, 1100, 850, 750, 950, 775", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
            checkboxInput("se", "Add confidence interval around the regression line", TRUE),
            hr(),

         
            
        ),
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Data Table", DT::dataTableOutput("tbl")),
                        tabPanel("Computation", verbatimTextOutput("summary")),
                        tabPanel("Regression Plot", uiOutput("results"), plotlyOutput("plot")),
                        tabPanel("Interpretation Plot",tags$b("Interpretation:"), uiOutput("interpretation"))
            )
            
        )
    )
)

server <- function(input, output) {
    extract <- function(text) {
        text <- gsub(" ", "", text)
        split <- strsplit(text, ",", fixed = FALSE)[[1]]
        as.numeric(split)
    }
    
    # Data output
    output$tbl <- DT::renderDataTable({
        y <- extract(input$y)
        x <- extract(input$x)
        DT::datatable(data.frame(x, y)
        )
    })
    
    output$data <- renderUI({
        y <- extract(input$y)
        x <- extract(input$x)
        if (anyNA(x) | length(x) < 2 | anyNA(y) | length(y) < 2) {
            "Invalid input or not enough observations"
        } else if (length(x) != length(y)) {
            "Number of observations must be equal for x and y"
        } else {
            withMathJax(
                #For XBar 
                paste0("\\(\\bar{x} =\\) ", round(mean(x), 3)),#Ans Of X bar
                br(),
                #For Y Bar
                paste0("\\(\\bar{y} =\\) ", round(mean(y), 3)),#Ans Of Y Bar
                br(),
                paste0("\\(n =\\) ", length(x))
            )
        }
    })
    
    output$summary <- renderPrint({
        y <- extract(input$y)
        x <- extract(input$x)
        fit <- lm(y ~ x)
        summary(fit)
    })
    
    output$results <- renderUI({
        y <- extract(input$y)
        x <- extract(input$x)
        fit <- lm(y ~ x)
        withMathJax(
            paste0(
                #Regression Plot
                "Adj. \\( R^2 = \\) ", round(summary(fit)$adj.r.squared, 3),#rsqure
                ", \\( \\beta_0 = \\) ", round(fit$coef[[1]], 3),#For Bita 0
                ", \\( \\beta_1 = \\) ", round(fit$coef[[2]], 3),#For Bita 1
                ", P-value ", "\\( = \\) ", signif(summary(fit)$coef[2, 4], 3)#For P-Value
            )
        )
    })
    
    #For Plot Showing
    output$interpretation <- renderUI({
        y <- extract(input$y)
        x <- extract(input$x)
        fit <- lm(y ~ x)
        if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
            withMathJax(
                paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
                br(),
                paste0("For a (hypothetical) value of ", input$xlab, " = 0, the mean of ", input$ylab, " = ", round(fit$coef[[1]], 3), "."),
                br(),
                paste0("For an increase of one unit of ", input$xlab, ", ", input$ylab, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), ".")
            )
        } else if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] >= 0.05) {
            withMathJax(
                paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
                br(),
                paste0("For a (hypothetical) value of ", input$xlab, " = 0, the mean of ", input$ylab, " = ", round(fit$coef[[1]], 3), "."),
                br(),
                paste0("\\( \\beta_1 \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[2, 4], 3), ") so there is no significant relationship between ", input$xlab, " and ", input$ylab, ".")
            )
        } else if (summary(fit)$coefficients[1, 4] >= 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
            withMathJax(
                paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
                br(),
                paste0("\\( \\beta_0 \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[1, 4], 3), ") so when ", input$xlab, " = 0, the mean of ", input$ylab, " is not significantly different from 0."),
                br(),
                paste0("For an increase of one unit of ", input$xlab, ", ", input$ylab, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), ".")
            )
        } else {
            withMathJax(
                paste0("((Before interpreting the coefficients, make sure the linear regression assumptions (independence, linearity, normality, and homoscedasticity) are met.)"),
                br(),
                paste0("\\( \\beta_0 \\)", " and ", "\\( \\beta_1 \\)", " are not significantly different from 0 (p-values = ", round(summary(fit)$coefficients[1, 4], 3), " and ", round(summary(fit)$coefficients[2, 4], 3), ", respectively) so the mean of ", input$ylab, " is not significantly different from 0.")
            )
        }
    })
    
    output$plot <- renderPlotly({
        y <- extract(input$y)
        x <- extract(input$x)
        fit <- lm(y ~ x)
        dat <- data.frame(x, y)
        p <- ggplot(dat, aes(x = x, y = y)) +
            geom_point() +
            stat_smooth(method = "lm", se = input$se) +
            ylab(input$ylab) +
            xlab(input$xlab) +
            theme_minimal()
        ggplotly(p)
    })
    
}

# Run the application
shinyApp(ui, server)