shinyUI(fluidPage(
    titlePanel("Yu-Wen Liu's R Homework3"),

    sidebarLayout(
      # Sidebar with sliders that demonstrate various available options
      sidebarPanel( 
        sliderInput("set", "Set Number", min=1, max=5, value=1) ),
      # Show a tabset that includes plot and result, summary
      mainPanel( tabsetPanel(type = "tabs", 
                             tabPanel("Plot", 
                                      h3("ROC Curve"),
                                      plotOutput("roc")), 
                             tabPanel("Result", 
                                      h3("Result Data"),
                                      tableOutput("set_table")))
      )
    )
))