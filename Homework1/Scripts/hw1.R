# parsing the argument in command line
args<-commandArgs(trailingOnly=TRUE)

func_flag<-pmatch( "-query", args)
in_flag<-pmatch( "-files", args)
out_flag<-pmatch( "–out", args)

# use three variable "func, in_file, out_file" to record different arguments.
func<-c( args[ (func_flag+1): (in_flag-1) ] )
in_file<-c( args[ (in_flag+1): (out_flag-1) ] )
out_file<-c( args[ (out_flag+1)])#: (out_flag+2) ] )

# read the input file and stroe in variable "raw"
row_name<-c('weight','Height')
col_name<-c()
out<-data.frame(row_name)

for( f_name in in_file ){
	raw<-read.csv( f_name, stringsAsFactors=F ,header=T, sep=",")
	col_name<-c( col_name, sub( "../Data/", "", f_name))
	if( grepl( func, "max") ){
		f_name<-c(max(raw[2]), max(raw[3]) )
	}else{
		f_name<-c(max(raw[2]), max(raw[3]) )
	}
	out<-data.frame( out, f_name)
}

out <- out[,-1]
colnames(out) <-col_name
rownames(out) <-row_name

result <- c()
if( grepl( func, "max") ){
	w_index<-pmatch( max(out[1,]), out[1,] )
	h_index<-pmatch( max(out[2,]), out[2,] ) 
	result <- c( result, c(colnames(out)[w_index], colnames(out)[h_index]))
	out <- data.frame( out, max=result)
}else{
	w_index<-pmatch( min(out[1,]), out[1,] )
	h_index<-pmatch( min(out[2,]), out[2,] ) 
	result <- c( result, c(colnames(out)[w_index], colnames(out)[h_index]))
	out <- data.frame( out, min=result)
}

out <- data.frame( Type=c(rownames(out)) , out)
write.table( out, file=out_file, sep=",", quote=FALSE, row.names=FALSE)
