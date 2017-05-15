getAgeR <- function(df,epitoc=FALSE,horvath=FALSE,hannum=FALSE,drift=FALSE,driftcg,chrage,
                    keepres=FALSE,showStatusHannum=TRUE,keepcpgs.epitoc=TRUE,keepcpgs.hannum=TRUE){
  returnlist <- c()

  if(epitoc){
    message("Getting epiTOC age estimates...")
    tocout <- getEpiTOC(df,keepcpgs.epitoc)
    returnlist <- append(returnlist,tocout)
    names(returnlist)[length(returnlist)] <- "EpiTOC.output"
    message("Done! Continuing...")
  }

  if(horvath){
    suppressMessages(require(wateRmelon))
    message("Getting Horvath age estimates...")
    horvout <- as.data.frame(agep(df)); colnames(horvout)<-"Horvath.Est"
    returnlist <- append(returnlist,list(horvout))
    names(returnlist)[length(returnlist)] <- "HorvathClock.output"
    message("Done! Continuing...")
  }

  if(hannum){
    hannout <- getHannumEst(df)
    returnlist <- append(returnlist,hannout,keepcpgs.hannum,showStatusHannum)
    names(returnlist)[[length(returnlist)]] <- "HannumClock.output"
  }

  if(drift){
    driftout <- getDrift(df,driftcg,chrage,keepres)
    names(returnlist)[[length(returnlist)]] <- "DriftAge.output"
  }
  message("Age estimation complete. Returning..")

  return(returnlist)
}
