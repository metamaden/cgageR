\name{getEpiTOC}
\alias{getEpiTOC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate methylation-based age using EpiTOC algorithm.
}
\description{
Uses the epiTOC algorithm described in Yang et al 2016.
}
\usage{
getEpiTOC(df,keeptxt=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Beta-value dataframe for calculation of age estimation.
}
\item{keeptxt}{
Whether to retain info on available CpGs.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
List of age estimates and user specified details.
}
\references{
Yang et al. "Correlation of an epigenetic mitotic clock with cancer risk." Genome Biology, 2016.
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(df,keeptxt=TRUE){
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
