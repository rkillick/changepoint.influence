\name{influence}
\alias{influence}

\title{Calculate inference for changepoint detection}
\description{
Calculates the influence of the data on the observed segmentation.  There are currently two methods for modifying data to assess influence on the segmentation; \code{method="delete"} and \code{method="outlier"}.
}
\usage{
influence(model,method=c("delete","outlier"),k=1, modify="all",n.modify,
pos=TRUE,same=FALSE,sd=0.01)
}
\arguments{
  \item{model}{
  Model can be the output from a supported changepoint model or a list with specific elements needed.  See 'details' for further information.
}
  \item{method}{
  Either \code{"delete"} or \code{"outlier"}.  These control what happens to the individual observations when the data is perturbed. See details for further information on the difference between the two.
  }
  \item{k}{
  Number of consecutive points to modify, defaults to 1.
  }
  \item{modify}{
  Which starting points will be modified.  Choice of: \code{"all"} (default); \code{"random"} selection of \code{n.modify} uniformly; \code{"stratified"} selection of \code{n.modify} stratified by segment; or a user-specified vector of starting values between 1 and length of data -k+1.
  }
  \item{n.modify}{
  Only used in \code{"random"} and \code{"stratified"}.  Number of data points to sample.
  }
  \item{pos}{For \code{method="outlier"} this is whether the outlier is positive (TRUE) or negative (FALSE).}
  \item{same}{For \code{method="outlier"} and multiple outliers, this is whether we want the outliers to have the same mean (TRUE) (takes the first outlier, adds 2*range(data), then jitters the others around this value) or whether we add 2*range(data) to the existing value of each datapoint (FALSE).}
  \item{sd}{For \code{method="outlier"}, the standard deviation of the jitter, see \code{same} argument.}
}

\details{
Calculates the influence of the data on the observed segmentation.  There are currently two methods implemented for this \code{method="delete"} and \code{method="outlier"}.

Both methods sequentially take each data point, modify it and then run the same changepoint algorithm on the modified data as the original data.  We record the new segmentations in an nmodifyxn matrix output of the segment number for each location 1,...,n for each of the 1,...,nmodify data modifications.

If a datapoint has no undue influence on the overall segmentation then the segmentation with that datapoint modified will be the same as the original segmentation.  We define undue influence as any unexpected variation in the segmentations when data points are modified.

The \code{method="delete"} modifies a datapoint by deleting it.  This is recorded as an NA value in the returned nxn matrix to preserve indexing.  The \code{method="outlier"} modifies a datapoint by making it an outlier (+/- 2*range(data)).  

The \code{model} object passed to this function can take different forms, currently a \code{cpt} object or a \code{list}.  \code{cpt} objects supported come directly from calls to \code{cpt.*} functions with \code{class=TRUE} (default) from the \code{changepoint} and \code{changepoint.np} packages. If a \code{list} is provided, it is expected to have the following elements:
\itemize{
  \item \code{data} - a vector of data to assess influence on
  \item \code{cpts} - a vector of changepoints from running \code{cptfuncname} on \code{data}, if this doesn't contain 0 and length(data) then these will be added.  Thus the assumption is that the changepoint is the end of the segment.
  \item \code{seglen} - a vector of segment lengths
  \item \code{cptfuncname} - the function name of the changepoint method
  \item \code{cptfuncargs} - the arguments to pass to \code{cptfuncname} in each call
  \item \code{param} - a length(data) x num parameters matrix of parameter estimates (needed for parameter stability plots)
}
}
\value{
A list containing the following elements:

$org.data, the original data input
$org.cpts, the original changepoints
$org.seglen, the original segment lengths
$modified, the start of the modification points
$k, the length of modification taken after $modified
$inputargs, a list of other arguments supplied to \code{influence()} returned for 
  reproducibility but unlikely to be needed in subsequent analysis.

$delete, if the \code{modify="delete"}
  $segment, an length(modified)xn matrix of the segment number/class at each time point (NA along the diagonal) 
  $param, an length(modified)xn matrix of the parameter at each time point (NA along the diagonal)
$outlier, if the \code{modify="outlier"}
  $segment, an nxn matrix of the segment number/class at each time point (NA along the diagonal) 
  $param, an nxn matrix of the parameter at each time point (NA along the diagonal)
}
\seealso{
 \code{\link{StabilityOverview}}, \code{\link{LocationStability}},\code{\link{ParameterStability}},\code{\link{InfluenceMap}}
}

\examples{
#### Generate Simulated data example ####
set.seed(30)
x = c(rnorm(50), rnorm(50, mean = 5), rnorm(1, mean = 15), rnorm(49, mean = 5), rnorm(50, mean = 4))
xcpt = cpt.mean(x,method='PELT') # Get the changepoints via PELT

#### Get the influence for both delete and outlier options ####
x.inf = influence(xcpt)

#### Get the influence using delete method ####
x.inf = influence(xcpt, method="delete")

#### Get the influence using outlier method ####
x.inf = influence(xcpt, method="outlier", pos=FALSE,same=FALSE) 
# no sd required as no jitter used.

}

\keyword{methods}
\keyword{influence}
\keyword{internal} 