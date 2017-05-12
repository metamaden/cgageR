getEpiTOC <- function(df,keeptxt=TRUE){
  common.v <- intersect(rownames(df),EpiTOCcpgs);
  map.idx <- match(common.v,rownames(df));
  return.df <- data.frame(EpiTOC.Est=colMeans(df[map.idx,]));
  return.list <- list(return.df); names(return.list) <- "Epitoc.Est"

  if(keeptxt){
    txtepitoc <- paste("Number of available epiTOC CpGs: ",length(common.v),"/",length(EpiTOCcpgs),".",sep="")
    print(txtepitoc);
    return.list <- append(return.list,txtepitoc)
    names(return.list)[[length(return.list)]] <- "info"
  }

  return(return.list)
}
