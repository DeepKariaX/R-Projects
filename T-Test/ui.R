if (!require("ggplot2"))
  install.packages("ggplot2")

if (!require("shinyBS"))
  install.packages("shinyBS")

shinyUI(fluidPage(
  
  titlePanel("T Test"),
  
  withMathJax(),
  

    mainPanel(
    tabsetPanel(type = "tabs",
                        
                        tabPanel("Upload Data",
                                 fluidRow(
                                   column(3,
                                          wellPanel(
                                            fileInput("file", "",accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                            bsPopover("file","Note", "Remember to select the correct data format after uploading file!  Hover over the Select data format panel for more information.",
                                                      trigger="hover",placement="right"),
                                            tags$hr(),
                                            radioButtons("datformat", strong("Select data format:"), choices=c("1-sample"=1,Stacked=2,Unstacked=3), selected=1),
                                            bsPopover("datformat","Data format", "Select Stacked for 2-sample with explanatory and response variables in two columns.  Select Unstacked with explanatory variable as column names and response variable in two columns",
                                                      trigger="hover",placement="right"),
                                            tags$hr(),
                                            

                                            )),
                                   column(9,
                                          conditionalPanel(
                                            condition="input.file!='NULL'",
                                            dataTableOutput("data.tab"))))),
                        
                        tabPanel("Visualize Data",
                                 fluidRow(
                                   column(6,
                                          plotOutput("datagraph"))
                                   )),
                tabPanel("Normality Condition",
                         column(1),
                         column(4,br(),
                                p(strong("Normality test:"),"The sampling distribution of the sample means or differences in the sample means is 1) Normal if the population(s) is Normal or 2) approximately Normal if the 
                               sample size(s) is large enough (at least 30).  In situations with small sample size(s), the approach to access 
                               whether the sample data could have came from a Normal distribution is either through a Q-Q plot or a Normality test."),
                                actionButton("normstart", label=strong("Perform normality test")),br(),br(),
                                conditionalPanel(
                                  condition="input.normstart>0",
                                  wellPanel(
                                    strong("Ho: data is from a Normal distribution"),br(),
                                    strong("Ha: data is not from a Normal distribution"),br(),br(),
                                    em("Shapiro-Wilk Normality Test:"),
                                    tableOutput("sw"),
                                    bsPopover("sw","Normality test","In a normality test, a large p-value does not provide evidence that the sample data is not from a Normal distribution.",
                                              trigger="hover",placement="bottom")))),
                         column(6,
                                conditionalPanel(
                                  condition="input.normstart>0",br(),br(),
                                  plotOutput("qqplot"),
                                  bsPopover("qqplot","Q-Q plot","In a Q-Q plot, points that resemble a diagonal line with no curvature implies that the sample data could have came from a Normal distribution.",
                                            trigger="hover",placement="left"))),
                         column(1)),
             
             tabPanel("Hypothesis Test",
                      fluidRow(
                        column(3,
                               wellPanel(
                                 conditionalPanel(
                                   condition="input.datformat==1 || input.sampdat==1",
                                   h4("Hypotheses:"),
                                   uiOutput("hypo1"),
                                   tags$hr(),
                                   numericInput("null1", label="Hypothesized value:", value=0),
                                   selectInput("alt1", "Select a direction for Ha:", choices=list("two-sided","less than","greater than"),selected="two-sided")),
                                 conditionalPanel(
                                   condition="input.datformat!=1",
                                   h4("Hypotheses:"),
                                   uiOutput("hypo2"),
                                   tags$hr(),
                                   numericInput("null2", label="Hypothesized value:", value=0),
                                   selectInput("alt2", label="Select a direction for Ha:", choices=list("two-sided","less than","greater than"),selected="two-sided")),
                                 sliderInput("alpha", label=HTML("Significance level &alpha;:"), value=.05, max=1, min=0, step=.01),
                                 tags$hr(),
                                 actionButton("teststart", strong("Perform t-test")),
                                 bsPopover("teststart","Note","Remember to check the normality condition before performing t-test.",trigger="hover",placement="right"),
                                 br(),br(),br(),
                                 
                                 )),
                        
                        column(9,
                               br(),br(),
                               conditionalPanel(
                                 condition="input.teststart>0",
                                 column(7,
                                        plotOutput("tdist"),
                                        bsPopover("tdist","p-value","The p-value is the shaded region. A large p-value indicates to fail to reject Ho and no evidence for Ha.  A small p-value indicates to reject the Ho and evidence for Ha.",
                                                  trigger="hover",placement="left"),br()),
                                 column(5,br(),
                                        strong("Test output:"),
                                        tableOutput("test"),br(),
                                        checkboxInput("showpoint","Point estimate(s):",FALSE),
                                        uiOutput("est"),
                                        checkboxInput("ci","Confidence interval:", FALSE),
                                        tableOutput("citab"),
                                        bsPopover("citab","Confidence interval sample interpretation","We are 95% confident that the true parameter is anywhere between the lower bound to the upper bound.",
                                                  trigger="hover",placement="bottom"))))))
             

             
  ) ) ) )
