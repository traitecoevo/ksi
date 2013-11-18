\name{ksi.nodeset}
\alias{ksi.nodeset}
\alias{ksi.group}
\title{Set of Plausible KSI Nodes}
\description{
  ...to write...
}
\usage{
ksi.nodeset(obj, idx, alpha=1/20, node=NULL)
ksi.group(obj, idx, alpha=1/20, include=1/5)
}
\arguments{
  \item{obj}{Fitted \code{ksi} object, returned by \code{\link{ksi}}.}

  \item{idx}{Index of the node to search around (1, 2, etc).}

  \item{alpha}{The quantile to include when defining plausible sets;
    \code{alpha=1/20} includes the top 5\% of nodes.}

  \item{include}{Rank up to this fraction of the nodes in the tree.}

  \item{node}{If non-NULL, centres the nodeset at a different node.}
}
\value{
  Vector of node \emph{names}.
}
\details{
  ...to write...
}
\seealso{
  \code{\link{ksi}}
}
% \examples{
% }
\author{Richard G. FitzJohn}
\keyword{util}
