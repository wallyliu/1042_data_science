library('ROCR')
judge<-function(i){ which.max(i) }
significant<-function(i){
	if( i<0.05 ){ "no" }
	else{ "yes" }
}

# parsing the argument in command line
args<-commandArgs(trailingOnly=TRUE)

q_flag<-pmatch( "-query", args)
in_flag<-pmatch( "-files", args)
out_flag<-pmatch( "-out", args)
target_flag<-pmatch( "-target", args)

# use three variable "query, ifile, ofile, target" to record different arguments.
ofile<-c( args[ (out_flag+1) ])
target<-c( args[ (target_flag+1) ])

i<-q_flag+1
while( !grepl( "-", args[i]) ){ i<-i+1 }
query<-c( args[ (q_flag+1): (i-1)])

i<-in_flag+1
while( !grepl( "-", args[i]) ){ i<-i+1 }
ifile<-c( args[ (in_flag+1): (i-1)])

# read the input file and stroe in variable "raw"
# use "method" to store all method name in set
f1_result<-c()
auc_result<-c()
sen_result<-c()
spe_result<-c()
auc_result<-c()
sig_result<-c()


method<-c()
tmp<-c()
color<-c("red", "blue", "green", "brown", "yellow", "black", "pink", "grey", "purple", "orange")
dir.create(ofile)

for( file in ifile ){
	tmp<-c(list.files( paste("../data/", file, sep="")))
	for( i in tmp){
		method<-c(method, paste( file, "/", i, sep="") )
	}

	index<-0
	png( file=paste("./", ofile, "/", file, "_ROC.png", sep="" ))
	for( m in tmp ){
		index<-(index%%10)+1
		rawdata<-read.table( paste("../data/", file, "/", m, sep=""),header=T, sep=",")
		if( "F1" %in% query ){
			tf<-c( tolower(rawdata$prediction) == tolower(rawdata$reference) )
			pn<-c( tolower(rawdata$prediction) == target)
			result<-table( tf, pn)
			# result[ FN, TN, FP, TP ]
			precision <- result[4] / (result[4] + result[3])
			recall <- result[4] / ( result[4]+ result[1])
			f1 <- round( 2 * precision * recall/ (precision + recall), digit=2)
			f1_result<-c( f1_result, f1)
		}
		if( "sensitivity" %in% query ){
			# Sensitivity(TPR)  = recall
			sensitivity<-round(recall, digit=2)
			sen_result<-c( sen_result, sensitivity)
		}
		if( "specificity" %in% query ){
			# Specificity = TN/(TN+FP)
			specificity<-round( result[2]/(result[3]+result[2]), digit=2)
			spe_result<-c( spe_result, specificity)
		}

		if( "AUC" %in% query ){
			library(ROCR)
			evaluation <- prediction( rawdata$pred.score, rawdata$reference)
			perf <- performance( evaluation, 'tpr','fpr')
			if( m == tmp[1] ){
				plot(perf, main=paste("ROC Curves - ", file), col=color[index])
			}else{
				plot(perf, add=TRUE, col=color[index])
			}

			auc.tmp <- performance( evaluation,"auc")
			auc <- round(as.numeric(auc.tmp@y.values), digit=2)
			auc_result <- c( auc_result, auc)
		}

		sig <- round(fisher.test(result, conf.level = 0.95)$p.value, digit=2)
		sig_result <- c( sig_result, significant(sig<0.05) )
	}
	dev.off()
}


method<-sub( ".csv", "", method)
out_data<-data.frame( method=method, F1=f1_result, AUC=auc_result, sensitivity=sen_result, specificity=spe_result, significant=sig_result, stringsAsFactors = F)
index<-sapply(out_data[,c("F1","AUC","sensitivity","specificity")], judge)

# output file
out_data<-rbind( out_data, c( "highest", method[index], "nan") )
write.table( out_data, file=paste( "./", ofile, "/", ofile, ".csv", sep=""), sep=",", row.names=FALSE, quote=FALSE)
