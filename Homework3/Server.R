library(shiny)
library(ggplot2)

setTable<-function(setnum){
  f_path = paste("./data/out/set", setnum , ".csv", sep="")
  rawdata<-read.table( f_path, header=T, sep=",")
  return( rawdata )
}

roc<-function(setnum){
  target <- "male"
  method <- list.files(paste("./data/set", setnum, sep=""))
  data <- c()
  tpr_result <- c()
  fpr_result <- c()
  for( m in method){
    rawdata<-read.table( paste("./data/set", setnum, "/", m, sep=""), header=T, sep=",")
    
    # calculate precision and recall
    tf<-c( tolower(rawdata$prediction) == tolower(rawdata$reference) )
    pn<-c( tolower(rawdata$prediction) == target)
    result<-table( tf, pn)
    # result[ FN, TN, FP, TP ]
    precision <- result[4] / (result[4] + result[3])
    recall <- result[4] / ( result[4]+ result[1])
    
    # calculate sensitivity
    # Sensitivity(TPR)  = recall
    tpr<-round(recall, digit=2)
    tpr_result<-c( tpr_result, tpr)
    
    # calculate specificity
    # Specificity = TN/(TN+FP)
    fpr<-round( result[3]/(result[3]+result[2]), digit=2)
    fpr_result<-c( fpr_result, fpr)
  }
  method <- sub(".csv","", method)
  data <- data.frame( TPR=tpr_result, FPR=fpr_result, method=method)
  return( data )
}

shinyServer(function(input, output){
  # Show the values using an HTML table
  output$roc <- renderPlot({
    data_f <- roc(input$set)
    gp <- ggplot(data=data_f, aes(x=FPR, y=TPR, color=method)) + geom_point(size=3) 
    gp + ggtitle(paste("ROC: Set",input$set))
  })
  output$set_table <- renderTable({
    setTable(input$set)
  })
})