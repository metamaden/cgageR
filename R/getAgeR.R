getAgeR <- function(df,epitoc=FALSE,horvath=FALSE,hannum=FALSE,drift=FALSE,driftcg,chrage,keepres=FALSE,
                    showStatusHannum=TRUE,keepcpgs.epitoc=TRUE,keepcpgs.hannum=TRUE,keepcpgs.horvath=TRUE){
  returnlist <- list()

  if(epitoc){
    message("Getting epiTOC age estimates...")
    tocout <- getEpiTOC(df,keepcpgs.epitoc)
    returnlist <- append(returnlist,list(tocout))
    names(returnlist)[[length(returnlist)]] <- "EpiTOC.output"
    message("Done! Continuing...")
  }

  if(horvath){
    suppressMessages(require(wateRmelon))
    message("Getting Horvath age estimates...")
    horvout <- as.data.frame(agep(df)); colnames(horvout)<-"Horvath.Est"
    if(keepcpgs.horvath){
      int.horvath <- intersect(rownames(df),HorvathLongCGlist[,1])
      horvout <- append(list(horvout),list(int.horvath))
      names(horvout) <- c("Horvath.Est","Horvath.CpGs.Used")
    }
    returnlist <- append(returnlist,list(horvout))
    names(returnlist)[[length(returnlist)]] <- "HorvathClock.output"
    message("Done! Continuing...")
  }

  if(hannum){
    hannout <- getHannum(df,keepcpgs.hannum,showStatusHannum)
    returnlist <- append(returnlist,list(hannout))
    names(returnlist)[[length(returnlist)]] <- "HannumClock.output"
  }

  if(drift){
    driftout <- getDrift(df,driftcg,chrage,keepres)
    returnlist <- append(returnlist,list(driftout))
    names(returnlist)[[length(returnlist)]] <- "DriftAge.output"
  }
  message("Age estimation complete. Returning..")

  return(returnlist)
}
