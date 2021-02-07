get_autolabels <- function(DTm){
	finallist <- vector()
	for ( i in 1:nrow(DTm)){
		df_label <- data.frame(DTM[i,order(DTM[i,], decreasing = TRUE)])
		if(any(df_label[1,1] > 0)){
			if(any(df_label[2,1]>0)){
				if(any(df_label[3,1] >0)){
					val <- paste(rownames(df_label)[1:3], collapse="-")
				}
				else{
					val <- paste(rownames(df_label)[1:2], collapse="-")
				}
			}
			else{
				val <- paste(rownames(df_label)[i], collapse = "-")
			}
		}
		finallist[i] <- val
	}
	a1 <- finallist
	return (as[1:length(a1)])
}



				