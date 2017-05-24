getHannumEst <- function(df, keepcpgs.hannum=TRUE, showStatusHannum=FALSE){
  message("Initializing Hannum Clock age estimates...")

  hcgs <- hannumModel$marker;
  int.hcgs <- rownames(df[rownames(df) %in% hcgs,])

  vector.df <- as.data.frame(matrix(nrow=ncol(df),ncol=(length(int.hcgs)+1)))
  colnames(vector.df) <- c(int.hcgs,"Est.Age"); rownames(vector.df)<-colnames(df)

  for(i in 1:nrow(vector.df)){
    for(j in 1:(ncol(vector.df)-1)){
      cgi <- colnames(vector.df)[j]
      hcgs.weighti <- hannumModel[hannumModel$marker==cgi,]$coefficient
      vector.df[i,j]<-df[cgi,i]*hcgs.weighti
    }
    vector.df$Est.Age[i] <- sum(vector.df[i,1:(ncol(vector.df)-3)])
    if(showStatusHannum){
      message("Hannum Age Est. Status: Finished sample ",i," or ",round(100*(i/nrow(vector.df)),3),"%")
    }
  }
  dfhan <- data.frame(Hannum.Est=vector.df$Est.Age); rownames(dfhan)<-colnames(df)
  return.list <- list(dfhan); names(return.list)<-"Hannum.Clock.Est"

  if(keepcpgs.hannum){
    return.list <- append(return.list,list(rownames(df[rownames(df) %in% hcgs,])))
    names(return.list)[[length(return.list)]] <- "Hannum.CpGs.Used"
  }
  return(return.list)
}
