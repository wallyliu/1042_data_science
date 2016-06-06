# Yu-Wen Liu's Data Science Homework4
# 2016/06/05

library('rpart')

# parsing argument in command line
# Sample Command: hw4_104753035_YuWenLiu.R -fold n –out performance.csv
args <- commandArgs(trailingOnly=TRUE)
n_flag <- pmatch( "-fold", args)
out_flag <- pmatch( "-out", args)
n <- args[ (n_flag+1) ]
outFile <- args[ (out_flag+1) ]

# read raw data into "d"
raw <- read.csv( "Archaeal_tfpssm.csv", header=F)

# perform cross-validation
raw$id <- sample( 1:n, size = dim(raw)[1], replace=TRUE)
id_list <- 1:n
score_test <- c()
score_validation <- c()
score_train <- c()

for( index in 1:n){
	# split raw data into all training data and test data
	dTrainAll <- subset( raw, id %in% id_list[-index] )
	dTest <- subset( raw, id %in% c(index))
	# split all training data into training data and validation data (9:1)
	dTrainAll$id <- sample( 1:10, size = dim(dTrainAll)[1], replace=TRUE)
	train_list <- 1:10
	randomNum <- sample(1:10, 1)
	dTrain <- subset( dTrainAll, id %in% train_list[-randomNum] )
	dValidation <- subset( dTrainAll, id %in% c(randomNum) )

	# use Decision Tree to predict data
	model <- rpart( dTrain$V2 ~., data = dTrain)
	# calculate Accuracy score	
	score_test <- c( score_test, length( which( predict( model, dTest[,-2], type="class") == dTest[,2] )) / nrow(dTest) )
	score_validation <- c( score_validation, length( which( predict( model, dValidation[,-2], type="class") == dValidation[,2] )) / nrow(dValidation) )
	score_train <- c( score_train, length( which( predict( model, dTrain[,-2], type="class") == dTrain[,2] )) / nrow(dTrain) )
}

title <- c( "set", "accuracy")
final_score <- round( c( mean(score_train), mean(score_validation), mean(score_test)) , digits = 2)

out_data <- cbind( c("training", "calibration", "test" ), final_score)
write.table( out_data, file = outFile, row.names = FALSE, sep=",", quote = FALSE, col.names=title)
