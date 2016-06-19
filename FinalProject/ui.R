shinyUI(fluidPage(
    br(),
    titlePanel("Final Project - Titanic Survival Prediction"),
    br(),
    sidebarLayout(
      # Sidebar with sliders that demonstrate various available options
      sidebarPanel(
        radioButtons("model", "Model:",
                     c("Decision Tree" = "dt",
                       "SVM" = "svm")),
        br(),
        radioButtons("dataset", "Dataset:",
                          c("Training" = "train",
                            "Test" = "test")) 
        ),
      
      # Show a tabset that includes plot and result, summary
      mainPanel( tabsetPanel(type = "tabs", 
                             tabPanel("Evaluation", 
                                      h3("Accuracy with Cross Validation"),
                                      tableOutput("accuracy"),
                                      plotOutput("plot")), 
                             tabPanel("Missing Map",
                                      h3("Missing Map"),
                                      imageOutput("miss")),
                             tabPanel("Data Summary",
                                      h3("Data Summary"),
                                      verbatimTextOutput("summary")))
      )
    )
))