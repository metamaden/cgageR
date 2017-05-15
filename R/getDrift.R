getDrift <- function(df, driftcg,chrage,keeptxt=TRUE,showStatus=TRUE,keepres){
  # note: chrage sample age order must correspond to order of columns in df
  message("Beginning Drift CpG residuals calculations...")

  dfdrift <- df[rownames(df) %in% driftcg,]
  reslist <- c()
  for(i in 1:nrow(dfdrift)){
    reslist[[i]] <- lm(dfdrift[i,]~chrage)$residuals
    names(reslist)[i] <- rownames(dfdrift)[i]
    if(showStatus){
      message("Drift Residuals Status: finished ",i," or ",round((100*(i/nrow(dfdrift))),3),"%.")
    }
  }

  xvecti <- c(rep(0,ncol(dfdrift)))
  for(i in 1:ncol(dfdrift)){
    xvecti[i] <- mean(sapply(reslist, "[", i))
    names(xvecti)[i] <- colnames(dfdrift)[i]
  }
  driftlm <- data.frame(Mean.Residual=xvecti,Chr.Age=chrage)
  rownames(driftlm) <- colnames(dfdrift)

  return.list <- list(driftlm)

  if(keepres){
    return.list <- append(return.list,list(reslist))
    names(return.list)[[length(return.list)]] <- "All.Drift.Residuals"
  }

  return(return.list)

}
