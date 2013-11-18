\name{data.frame.to.vectors}
\alias{data.frame.to.vectors}
\title{Convert Data Frame to Vectors}
\description{
  Converts a data.frame with named rows and a number of data columns
  into a list of vectors, each of which is named with the rownames of
  the data.frame, and omitting any missing values.  This is the format
  expected by \code{\link{ksi}}.
}
\usage{
data.frame.to.vectors(dat)
}
\arguments{
  \item{dat}{A data.frame with named rows.}
}

% \examples{
% }
\author{Richard G. FitzJohn}
\keyword{utils}
