library(randomForest)
library(rpart)
library(Amelia)
library(e1071)

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
dtrain <- convertNA(dtrain)

# check missing data status
missmap( dtrain, main="Missings Map - Titanic: Training Data")
missmap( dtrain, main="")
missmap( dtest, main="Missings Map - Titanic: Testing Data")
missmap( dtest, main="")

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
  #data$Surname <- as.factor(substr(data$Name, 1, dotStart-2))
  #data$Firstname <- as.factor(substr(data$Name, commaEnd+1, length(data$Name)))
  
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
  
  # Age: 
  # 1. substitue NA according to its Title
    age.na.index <- which(is.na(data$Age))
    tmp.d <- data[-age.na.index,]
    average.name <- names(which(summary(data$Title[age.na.index])>0) )
    average <- data.frame(0,0,0,0,0)
    names(average) <- average.name
    for(i in average.name){
      average[[i]] <- mean( tmp.d$Age[ which(tmp.d$Title == i) ])
    }
    for(i in age.na.index){
      data$Age[i] <- average[[ as.character(data$Title[i]) ]]
    }
    
  # 2. mapping (0-5, 6-10, 11-15, 16-20, 21-30, 31-40, 41-50, 50 - ) = ( 1, 2, 3, 4, 5, 6, 7, 8) 
   for( i in 1:nrow(data) ){
    if( findInterval( data$Age[i], c(0,6) ) == 1){
      data$Age[i] <- 1
    }else if( findInterval( data$Age[i], c(6,11) ) == 1){
      data$Age[i] <- 2
    }else if( findInterval( data$Age[i], c(11,16) ) == 1){
      data$Age[i] <- 3
    }else if( findInterval( data$Age[i], c(16,21) ) == 1){
      data$Age[i] <- 4
    }else if( findInterval( data$Age[i], c(21, 31) ) == 1){
      data$Age[i] <- 5
    }else if( findInterval( data$Age[i], c(31, 41) ) == 1){
      data$Age[i] <- 6
    }else if( findInterval( data$Age[i], c(41, 51) ) == 1){
      data$Age[i] <- 7
    }else{
      data$Age[i] <- 8
    }
  }
    
  return(data)
}

dtrain <- preprocess(dtrain)
dtest <- preprocess(dtest)

#--------------------- Build Model & Cross Validation --------------------- #

# test, validation, train
score.dt.total <- data.frame( c(0,0,0), c(0,0,0), c(0,0,0), c(0,0,0))
names(score.dt.total) <- seq(5,20,5)
score.svm.total <- data.frame( c(0,0,0), c(0,0,0), c(0,0,0), c(0,0,0))
names(score.svm.total) <- seq(5,20,5)

tmp <- 0
result <- c()

for( n in seq(5,20,5)){
  dt_score <- c( 0, 0, 0)
  svm_score <- c( 0, 0, 0)
  id_list <- 1:n
  dtrain$id <- sample( 1:n, size = dim(dtrain)[1], replace=TRUE)
  
  for( index in 1:n ){
    # split dtrain original traing data into training data and test data
    all_dtrain <- subset( dtrain, id %in% id_list[-index] )
    t_dtest <- subset( dtrain, id %in% c(index))

    # split all training data into training data and validation data (9:1)
    all_dtrain$id <- sample( 1:10, size = dim(all_dtrain)[1], replace=TRUE)
    train_list <- 1:10
    randomNum <- sample(1:10, 1)

    t_dtrain <- subset( all_dtrain, id %in% train_list[-randomNum] )
    t_dvalidation <- subset( all_dtrain, id %in% c(randomNum) )
  
    t_dtrain <- t_dtrain[-10]
    t_dvalidation <- t_dvalidation[-10]
    t_dtest <- t_dtest[-10]
    
    # use Decision Tree to predict data
    model <- rpart( Survived ~., data = t_dtrain)
    #fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data=t_dtrain, importance=TRUE, ntree=100)
    #plot(model)
    
    # calculate Accuracy score
    dt_score[1] <- dt_score[1] + length( which( predict( model, t_dtest[-1], type="class") == t_dtest$Survived )) / nrow(t_dtest)
    dt_score[2] <- dt_score[2] + length( which( predict( model, t_dvalidation[-1], type="class") == t_dvalidation$Survived )) / nrow(t_dvalidation)
    dt_score[3] <- dt_score[3] + length( which( predict( model, t_dtrain[-1], type="class") == t_dtrain$Survived )) / nrow(t_dtrain)
    #predict( model, dtest, type="class")
    
    model <- svm( Survived ~., data = t_dtrain[-10], gamma = 0.5)
    svm_score[1] <- svm_score[1] + length( which( predict( model, t_dtest[-1], type="class") == t_dtest$Survived )) / nrow(t_dtest)
    svm_score[2] <- svm_score[2] + length( which( predict( model, t_dvalidation[-1], type="class") == t_dvalidation$Survived )) / nrow(t_dvalidation)
    svm_score[3] <- svm_score[3] + length( which( predict( model, t_dtrain[-1], type="class") == t_dtrain$Survived )) / nrow(t_dtrain)
    #if( dt_score[1] > tmp ){ result <- predict( model, dtest, type="class") }
  }
  score.svm.total[[as.character(n)]] <- svm_score/n
  score.dt.total[[as.character(n)]] <- dt_score/n
}

#--------------------- Output data to csv file --------------------- #

  title <- c( "test", "validation", "train")
  score.svm.total <- round( score.svm.total, digits=4)
  score.dt.total <- round( score.dt.total, digits=4)
  
  
  write.csv( score.svm.total, file = "svm.csv", row.names = title, quote = FALSE)
  write.csv( score.dt.total, file = "dt.csv", row.names = title,quote = FALSE)