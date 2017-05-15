getEpiTOC <- function(df,keepcgs.epitoc=TRUE){
  common.v <- intersect(rownames(df),EpiTOCcpgs);
  map.idx <- match(common.v,rownames(df));
  return.df <- data.frame(EpiTOC.Est=colMeans(df[map.idx,]));
  return.list <- list(return.df); names(return.list) <- "EpiTOC.Est"
  
  message("Number of available epiTOC CpGs: ",length(common.v),"/",length(EpiTOCcpgs),".",sep="")

  if(keepcpgs.epitoc){
    return.list <- append(return.list,list(common.v))
    names(return.list)[[length(return.list)]] <- "Epitoc.CpGs.Used"
  }
  return(return.list)
}
