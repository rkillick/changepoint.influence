\name{changepoint.influence-package}
\alias{changepoint.influence-package}
\alias{changepoint.influence}
\docType{package}
\title{
\packageTitle{changepoint.influence}
}
\description{
\packageDescription{changepoint.influence}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{changepoint.influence}
\packageIndices{changepoint.influence}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
\packageAuthor{changepoint.influence}

Maintainer: \packageMaintainer{changepoint.influence}
}
\references{
~~ Literature or other references for background information ~~
}
\keyword{
package
changepoint
influence
}
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
data(WelllogsStandard)

# First use the leave one out (loo) method:
library(changepoint)
welllogs.cpt=cpt.mean(WelllogsStandard,method='PELT')

par(mfrow=c(1,1))
out=loo.mean.PELT.cpts(welllogs.cpt)
par(mfrow=c(3,1))
influence.data.loo.plot(loo.object=out)
influence.data.loo.plot(loo.object=out,hist.tcpt.delete=TRUE)


# Now using the make one outlier (moo) method:
welllogs.cpt=cpt.mean(WelllogsStandard,method='PELT')

par(mfrow=c(1,1))
original.class=rep(1:(ncpts(welllogs.cpt)+1),times=seg.len(welllogs.cpt))
out=apply.many.k.outlier.mPELT(welllogs.cpt,original.class,k=1,pos=TRUE)
par(mfrow=c(3,1))
influence.data.moo.plot(moo.object=out)
influence.data.moo.plot(moo.object=out,hist.tcpt.delete=TRUE)
}
