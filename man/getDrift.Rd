\name{getDrift}
\alias{getDrift}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean of residuals from drifting CpGs.
}
\description{
Calculate mean of residuals from user-specified drifting CpGs.
}
\usage{
getDrift(df, driftcg,chrage,showStatusDrift=TRUE,keepres=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{df}{
Beta-value data frame, where columns are samples and rows are markers.
}
\item{driftcg}{
List of drifting CpGs for which to calculate residuals.
}
\item{chrage}{
Sample chronological (patient) age, in same order as columns in df.
}
\item{showStatusDrift}{
Whether to show detailed output of calculation status.
}
\item{keepres}{
Whether to retain CpG-level residuals used in calculation of mean.
}
}

\value{
Returns age estimate and specified additional information.
}

\author{
Sean K Maden
}

\examples{
## The function is currently defined as
function(df, driftcg,chrage,showStatusDrift=TRUE,keepres=FALSE){
  # note: chrage sample age order must correspond to order of columns in df
  message("Beginning Drift CpG residuals calculations...")

  dfdrift <- df[rownames(df) %in% driftcg,]
  reslist <- c()
  for(i in 1:nrow(dfdrift)){
    reslist[[i]] <- lm(dfdrift[i,]~chrage)$residuals
    names(reslist)[i] <- rownames(dfdrift)[i]
    if(showStatusDrift){
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
}
\keyword{~models}
