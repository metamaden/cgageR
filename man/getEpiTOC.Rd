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
getEpiTOC(df,keepcpgs.epitoc=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Beta-value dataframe for calculation of age estimation.
}
\item{keepcpgs.epitoc}{
Whether to retain EpiTOC CpGs used in calculation.
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
## The function is currently defined as
function (df, keepcpgs.epitoc = TRUE) 
{
    common.v <- intersect(rownames(df), EpiTOCcpgs)
    map.idx <- match(common.v, rownames(df))
    return.df <- data.frame(EpiTOC.Est = colMeans(df[map.idx, 
        ]))
    return.list <- list(return.df)
    names(return.list) <- "EpiTOC.Est"
    message("Number of available epiTOC CpGs: ", length(common.v), 
        "/", length(EpiTOCcpgs), ".", sep = "")
    if (keepcpgs.epitoc) {
        return.list <- append(return.list, list(common.v))
        names(return.list)[[length(return.list)]] <- "Epitoc.CpGs.Used"
    }
    return(return.list)
}
\keyword{~models}
