\name{getAgeR}
\alias{getAgeR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper to retrieve any of various age estimates from methylation array data.
}
\description{
Use up to four methods to estimate sample age based on Beta-value methylation data.
}
\usage{
getAgeR(df, epitoc=TRUE, horvath=TRUE, hannum=TRUE, drift=TRUE, driftcg, chrage, showStatus=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Beta-value data frame, where columns are samples and rows are markers.
}
\item{epitoc}{
Uses the epiTOC algorithm to estimate age in terms of mean Beta-value methylation.
}
\item{horvath}{
Uses Horvath's human CpG clock, as implemented in wateRmelon, to estimate sample age.
}
\item{hannum}{
Uses Hannum's human CpG clock from blood to estimate sample age.
}
\item{drift}{
Uses user-provided drift CpG methylation to determine mean of residuals across drifting CpGs.
}
\item{driftcg}{
List of drifting CpG marker IDs to use if drift is TRUE.
}
\item{chrage}{
Numeric vector of chronologic (patient) age, in same order as columns in df.
}
}
\value{
Returns a list of specified age estimates and affiliated output.
}
\references{


1. Hannum et al. "Genome-wide Methylation Profiles Reveal Quantitative Views of Human Aging  Rates." Molecular Cell, 2014.

2. Horvath et al. "DNA methylation age of humna tissues and cell typesw." Genome Biology, 2013.

3. Pidsley et al. "A data-driven approach to preprocessing Illumina 450K methylation array data." BMC Genomics, 2013.

4. Yang et al. "Correlation of an epigenetic mitotic clock with cancer risk." Genome Biology, 2016.
}
\author{
Sean K Maden
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# require(wateRmelon); data(melon)
bval <- betas(melon)
est.ages <- getAgeR(bval,epitoc=TRUE,horvath=TRUE,hannum=TRUE,drift=FALSE,showStatus=TRUE)

## The function is currently defined as
getAgeR <- function(df,epitoc=FALSE,horvath=FALSE,hannum=FALSE,drift=FALSE,
                    driftcg,chrage,showStatus=TRUE){
  returnlist <- c()

  if(epitoc){
    message("Getting epiTOC age estimates...")
    tocout <- getEpiTOC(df)
    returnlist <- append(returnlist,tocout)
    names(returnlist)[[length(returnlist)]] <- "epiTOC.Age.Estimates"
    message("Done! Continuing...")
  }

  if(horvath){
    require(wateRmelon)
    message("Getting Horvath age estimates...")
    horvout <- as.data.frame(agep(df)); colnames(horvout)<-"Horvath.Est"
    returnlist <- append(returnlist,list(horvout))
    names(returnlist)[[length(returnlist)]] <- "Horvath.Clock.Age.Estimates"
    message("Done! Continuing...")
  }

  if(hannum){
    require(wateRmelon)
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
