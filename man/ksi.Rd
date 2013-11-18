\name{ksi}
\alias{ksi}

% Need a better title.
\title{KSI}

\usage{
  ksi(tree, dat, depth=10, test=NULL, verbose=TRUE,
      multicore=FALSE, multicore.args=list())
}

\description{This is the fitting function.  The test works by
  sequentially fitting a series of (possibly nested) nodes and asking
  \dQuote{Is the trait distribution for the clade descending from this
  node different to that of the nodes neighbourhood?}.  The
  neighbourhood is defined as all the species that are in the
  'partition' of the parent of the focal node that are

  \itemize{
    \item{Not descended from that node}
    \item{Not descended from a node that was identified in a previous
      iteration (so, when fitting node \eqn{i}, we exclude all species
      descended from nodes \eqn{1..(i-1)})}
  }

  This is basically the same algorithm as MEDUSA uses for fitting
  diversification rates that vary across a phylogeny.
}

\arguments{

  \item{tree}{An phylogeny, of class \code{phylo} (ape's phylogeny
    format).  Branch lengths are not required, and are ignored if
    present.  Node labels are recommended.  If any nodes lack labels,
    labels of the form 'node.xxx' will be created.  This happens before
    removing species that do not have trait data so that these node
    labels will be the same across different analyses.}

  % TODO: Character vectors should get elevated to be factors...
  \item{dat}{A named vector of tip states.  This can be numeric, a
    factor (categorical) or logical (\code{TRUE}/\code{FALSE}).}

  \item{depth}{The number of nested nodes to identify (integer from 1 to
    the number of nodes in the tree once all taxa that lack state
    information are removed).}

  \item{test}{Force a particular test to be used.  Valid values are
    \itemize{
      \item{\code{"ks"}: do a Kolmogorov-Smirnov test on continuous
	valued distributions.}

      \item{\code{"chisq"}: do a Chi-squared test on a contingency table
        with categorical or binary traits.}

      \item{\code{"gtest"}: do a g-test on a contingency table with
        categorical or binary traits.}}

    If \code{NULL} (the default) this will be guessed from
    \code{dat}.}

  \item{verbose}{Print a (fairly small) amount of progress information
    as the fits proceed.}

  \item{multicore}{Logical: if \code{TRUE}, uses the \code{multicore}
    package to carry out some of the calculations in parallel.}

  \item{multicore.args}{Arguments to control the behaviour of
    \code{mclapply}, when \code{multicore=TRUE}.  For example
    \code{multicore.args=list(mc.cores=4, mc.preschedule=FALSE)}
    specifies that \code{mclapply} should use 4 cores, and use its
    load-balancing algorithm.}
}

\value{%

  The \code{ksi} function returns an object of class \code{ksi}.  This
  is a list with one element per node, named with the names of the nodes
  that were fit.  Each list element contains the elements:


  \describe{
    \item{\code{statistic}}{Value of the statistic against which nodes
      are ranked (for the Kolmogorov-Smirnov test, this is D scaled by
      the relative sample sizes -- see ?ks.test).}
    
    \item{\code{p.value}}{The p-value from the test (often meaninglessly
      small on large trees or uneven partitions).}

    \item{\code{n}}{A vector of length 2 with the number of species in
      the 'target' and 'neighbourhood' of the node, respectively.}
  }

  In addition, the \code{ksi} object (\code{obj}, say) has a number of
  attributes:

  \describe{
    \item{\code{attr(obj, "tree")}}{The phylogeny, after adding node
      labels and dropping species that have no trait data.}
    
    \item{\code{attr(obj, "dat")}}{The data, altered to have the same
      contents and order as attr(obj, "tree")$tip.label.}

    \item{\code{attr(obj, "contents")}}{A list with one element per
      node.  Each element is a list with elements \code{neighbourhood},
      \code{target} and \code{other}, containing the indices of the
      species that were in each test.  These are indices against
      \code{attr(obj, "dat")} for now.}

    \item{\code{attr(obj, "statistics")}}{A list with one element poer
      node.  Each element is a vector along the nodes (in ape index) with
      the statistic for each node for that round.  Nodes that were used
      in previous rounds have a value of \code{NA}.}
    
    \item{\code{attr(obj, "test")}}{Indicates which test (ks, chisq,
      gtest) was done.}
  }
}

\keyword{model}
