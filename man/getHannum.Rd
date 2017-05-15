\name{getHannum}
\alias{getHannum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get sample age estimates using Hannum CpG Methylation Clock model.
}
\description{
Uses methylation and coefficients at specified CpGs to estimate sample age as described in Hannum et al 2013.
}
\usage{
getHannum(df, keepcpgs.hannum=TRUE, showStatusHannum=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Beta-value data frame, with samples as columns and markers as rows.
}
\item{keepcpgs.hannum}{
Wether to retain list of CpGs used (model CpGs available in provided df).
}
\item{showStatusHannum}{
Whether to display detailed status of age estimate calculations.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Hannum et al. "Genome-wide Methylation Profiles Reveal Quantitative Views of Human Aging  Rates." Molecular Cell, 2014.
}
\author{
Sean Maden
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
function (df, keepcpgs.hannum=TRUE, showStatusHannum=TRUE) 
{
    message("Initializing Hannum Clock age estimates...")
    hcgs <- hannumModel$marker
    int.hcgs <- rownames(df[rownames(df) %in% hcgs, ])
    vector.df <- as.data.frame(matrix(nrow = ncol(df), ncol = (length(int.hcgs) + 
        1)))
    colnames(vector.df) <- c(int.hcgs, "Est.Age")
    rownames(vector.df) <- colnames(df)
    for (i in 1:nrow(vector.df)) {
        for (j in 1:(ncol(vector.df) - 1)) {
            cgi <- colnames(vector.df)[j]
            hcgs.weighti <- hannumModel[hannumModel$marker == 
                cgi, ]$coefficient
            vector.df[i, j] <- df[cgi, i] * hcgs.weighti
        }
        vector.df$Est.Age[i] <- sum(vector.df[i, 1:(ncol(vector.df) - 
            3)])
        if (showStatusHannum) {
            message("Hannum Age Est. Status: Finished sample ",i," or ",round(100 * (i/nrow(vector.df)), 3),"%")
        }
    }
    dfhan <- data.frame(Hannum.Est = vector.df$Est.Age)
    return.list <- list(dfhan)
    names(return.list) <- "Hannum.Clock.Est"
    if (keepcpgs.hannum) {
        return.list <- append(return.list, list(rownames(df[rownames(df) %in% 
            hcgs, ])))
        names(return.list)[[length(return.list)]] <- "Hannum.CpGs.Used"
    }
    return(return.list)
}
