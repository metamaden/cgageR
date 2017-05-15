getAgeR <- function(df,epitoc=FALSE,horvath=FALSE,hannum=FALSE,drift=FALSE,
                    driftcg,chrage,keepres=FALSE,showStatus=TRUE){
  returnlist <- c()

  if(epitoc){
    message("Getting epiTOC age estimates...")
    tocout <- getEpiTOC(df)
    returnlist <- append(returnlist,tocout)
    names(returnlist)[[length(returnlist)]] <- "epiTOC.Age.Estimates"
    message("Done! Continuing...")
  }

  if(horvath){
    suppressMessages(require(wateRmelon))
    message("Getting Horvath age estimates...")
    horvout <- as.data.frame(agep(df)); colnames(horvout)<-"Horvath.Est"
    returnlist <- append(returnlist,list(horvout))
    names(returnlist)[[length(returnlist)]] <- "Horvath.Clock.Age.Estimates"
    message("Done! Continuing...")
  }

  if(hannum){
    hannout <- getHannumEst(df)
    returnlist <- append(returnlist,hannout)
    names(returnlist)[[length(returnlist)]] <- "Hannum.Clock.Age.Estimates"
  }

  if(drift){
    driftout <- getDrift(df,driftcg,chrage)
    names(returnlist)[[length(returnlist)]] <- "Drift.Age.Est."
  }
  message("Age estimation complete. Returning..")
return(returnlist)
}
