library(shiny)
library(ggplot2)

d.summary <- function( choice ){
  # loading raw data
  dtest = read.csv( "./data/test.csv", header=T)
  dtrain = read.csv( "./data/train.csv", header=T)
  
  #------------------------- Preprocessing ------------------------- #
  # convert "" into 'NA'
  convertNA <- function( data ){
    for( i in names(data) ){
      index <- which( data[[i]] == "" )
      if( length(index)> 0){
        data[[i]][index] <- NA
      }
    }
    return(data)
  }
  dtest <- convertNA(dtest)
  
  # process data
  preprocess <- function(data){
    # Pclass, SibSp, Parch, Survived: convert integer to factor
    data$Pclass <- as.factor(data$Pclass)
    data$SibSp <- as.factor(data$SibSp)
    data$Parch <- as.factor(data$Parch)
    if( "Survived" %in% colnames(data) ){
      data$Survived <- as.factor(data$Survived)
    }
    
    # Name -> Surname, Title. Firstname
    dotStart <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
    commaEnd <- dotStart + attr(dotStart, "match.length")-1
    data$Title <- as.factor(substr(data$Name, dotStart+2, commaEnd-1))
    
    # Remove Name column
    data$Name <- NULL
    data$PassengerId <- NULL
    
    # Cabin: remove (too much missing value)
    data$Cabin <- NULL
    
    # Embarked: NA -> S
    for( i in which(is.na(dtrain$Embarked))){
      data$Embarked[i] <- 'S'
    }
    # Ticket: 
    data$Ticket <- NULL
    
    return(data)
  }
  out <- c()
  if( choice == "train"){
    dtrain <- preprocess(dtrain)
    out <- dtrain
  }else{
    dtest <- preprocess(dtest)
    out <- dtest
  }
  out <- summary(out)
  return( out )
}

result <- function( choose ){
  d.dt <- read.csv( "dt.csv", header=T)
  d.svm <- read.csv( "svm.csv", header=T)
  out <- c()
  if( choose == "dt"){
    out <- d.dt
  }else{
    out <- d.svm
  }
  names(out) <- c("title",5,10,15,20)
  tmp <- c()
  d <- out[-1]
  for( i in 1:dim(d)[1]){
     index <- which(max(d[i, ])==d[i, ])
     tmp <- c( tmp, colnames(d)[index] )
  }
  out$Max <- tmp
  return(out)
}

plot <- function(choice){
  d.dt <- read.csv( "dt.csv", header=T)
  d.svm <- read.csv( "svm.csv", header=T)
  out <- c()
  if( choice == "dt"){
    out <- d.dt
  }else{
    out <- d.svm
  }
  names(out) <- c("title",5,10,15,20)
  group <- c("test","validation","train","test","validation","train","test","validation","train","test","validation","train")
  x <- c(5,5,5,10,10,10,15,15,15,20,20,20)
  y <- c()
  for( i in colnames(out[-1]) ){
    y <- c( y, out[[i]])
  }
  out <- data.frame(x,y,group)
  return( out )
}

shinyServer(function(input, output){
  # Show the values using an HTML table
  output$accuracy <- renderTable({
    result(input$model)
  })
  output$plot <- renderPlot({
    d <- plot(input$model)
    gp <- ggplot( d, aes(x=x, y=y, colour=group, group=group)) + geom_line(size=2)
    #gp <- ggplot(data=d, aes(x=FPR, y=TPR, color=method)) + geom_point(size=3) 
    gp + ggtitle(paste("Accuray: ",input$model))
  })
  output$miss <- renderImage({
    if( input$dataset == "train"){
      list(src = "train_miss.png", contentType = 'image/png', width = 700, height = 700)
    }else{
      list(src = "test_miss.png", contentType = 'image/png', width = 700, height = 700)
    }
  }, deleteFile = FALSE)
  output$summary <- renderPrint(
    d.summary(input$dataset)
  )
})