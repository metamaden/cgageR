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
getHannum(df, keeptxt=TRUE, showStatus=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Beta-value data frame, with samples as columns and markers as rows.
}
\item{keeptxt}{
Wether to retain list of CpGs used (model CpGs available in provided df).
}
\item{showStatus}{
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
