## Old (out of date) documentation:
## 
## There is a basic plot method that produces some useful figures.
## With output 'obj',
## 
##   plot(obj, tree.hl)
## 
## Will produce a series of plots.  The argument 'tree.hl' is a
## higher-level phylogeny (such as a tree to genus, family, or
## order).  If missing the source phylogeny will be used, but this is
## not tested properly yet (I think that it will actually break: TODO)
## 
## By default, all nodes will be plotted.  To plot a particular node
## or series of nodes, pass in the argument 'idx' with a vector of
## indices of nodes (not names).
## 
## By default, the plots for continuous data fitted with KS tests will
## alternate between a plot of the tree showing nodes fit so far and
## density plots with the distributions of the target, neighbourhood
## and all other species.  To control this behaviour, pass in
## type="trees" for just the trees, or type="density" for just the
## distributions.
## 
## The full definition with defaults is:
##   plot(x, tree.hl, idx=seq_along(x), type="both")


## Basic plot method
plot.ksi <- function(x, tree.hl=NULL, idx=seq_along(x), type="tree",
                     envelope=TRUE, cols=cols10, ...) {
  valid <- c("tree", "density", "non-nested")
  if ( !all(type %in% valid) )
    stop(sprintf("Invalid type(s): %s",
                 paste(setdiff(type, valid), collapse=", ")))
  if ( any(idx < 1 | idx > length(x)) )
    stop(sprintf("'idx' must be in range 1..%d", length(x)))

  if ( is.null(tree.hl) )
    tree.hl <- attr(x, "tree")

  cols <- rep(cols, length=max(idx) + 1)
         
  for ( i in idx ) {
    if ( "tree" %in% type )
      plot.ksi.tree(x, tree.hl, i, cols, envelope, ...)
    if ( "non-nested" %in% type )
      plot.ksi.non.nested(x, tree.hl, i, cols, ...)
    if ( "density" %in% type )
      plot.ksi.density(x, tree.hl, i, cols, ...)
  }
}

plot.ksi.tree <- function(x, tree.hl, idx, cols, envelope, ...) {
  if ( !inherits(tree.hl, "clade.tree") )
    stop("Currently only works with clade.trees, due to some plotting hacks")  

  nodes <- names(x)[seq_len(idx)]
  class <- classify.by.splits(tree.hl, nodes)
  nodes.idx <- match(nodes,
                     c(tree.hl$tip.label, tree.hl$node.label))

  n.tip <- length(tree.hl$tip.label)

  op <- par(mar=rep(0, 4))
  on.exit(par(op))
  xy <- plot(tree.hl, type="f", transform=sqrt,
             label.offset=.05, cex=.4,
             edge.color=cols[class$group], ...)

  if ( envelope ) {
    set <- setdiff(ksi.nodeset(x, idx), nodes[idx])
    set.idx <- match(set, c(tree.hl$tip.label, tree.hl$node.label))
    set.col <- ifelse(set.idx > n.tip, "#8B000080", "#00008B80")
    nodelabels(node=set.idx, pch=19, cex=2, col=set.col)
  }
      
  idx.plt <- nodes.idx[seq_len(idx)]
  nodelabels(node=idx.plt, pch=19, cex=2,
             col=ifelse(idx.plt > n.tip, "darkred", "darkblue"))
  nodelabels(seq_along(idx.plt), node=idx.plt, bg=NA, frame="none",
             cex=.5, col="white", font=2)
}

plot.ksi.non.nested <- function(x, tree.hl, idx, cols, upto=20,
                                grouped=FALSE, p.nodes=.5, alpha=1/20,
                                ...) {
  if ( !inherits(tree.hl, "clade.tree") )
    stop("Currently only works with clade.trees, due to some plotting hacks")  

  ## This is the tree slimmed down to contain only the species that
  ## contain leaf nitrogen data:
  tree <- attr(x, "tree")

  ## This is the value of the statistic for all 482 nodes in the fitted
  ## tree.
  s <- attr(x, "statistic")[[idx]]

  ## Rank the nodes and scale this so that the most important shift has
  ## value 1 and the least important shift has value 0
  r <- (rank(s, ties.method="first") - 1)/(length(s) - 1)

  ## Get the index for the 482 nodes in the full tree (not all are
  ## present, as some nodes are within families).
  set.idx <- match(tree$node.label,
                   c(tree.hl$tip.label, tree.hl$node.label))

  ## Do the plot
  op <- par(mar=rep(0, 4))
  on.exit(par(op))
  ## TODO: should to a idx-1 mapping here.
  if ( idx > 1 ) {
    nodes <- names(x)[seq_len(idx-1)]
    class <- classify.by.splits(tree.hl, nodes)
    nodes.idx <- match(nodes,
                       c(tree.hl$tip.label, tree.hl$node.label))
    edge.col <- cols[class$group]
  } else {
    edge.col <- cols[1]
  }
  xy <- plot(tree.hl, type="f", transform=sqrt, label.offset=.05,
             cex=.4, edge.color=edge.col, ...)

  ## Scale node labels so that the most important is size 2 and the
  ## least size .5:
  cex <- .5 + 1.5*r
  if ( grouped ) {
    tmp <- ksi.group(x, idx, alpha)
    g <- tmp$g
    g[g == Inf] <- max(g[is.finite(g)]) + 1
    if ( idx < 10 ) # rotate colours
      cols <- c(cols10[-seq_len(idx)], cols10[seq_len(idx)])
    cols <- add.alpha(rep(cols, length=length(unique(g)))[g], r)
    pch <- rep(rep(c(19, 15, 17), each=10),
               length=length(unique(g)))[g]
    nodelabels(node=set.idx, pch=pch, cex=cex, col=cols)
    i <- which(g <= 26)
    nodelabels(sprintf("%s.%d", letters[g[i]], tmp$r[i]),
               node=set.idx[i], bg=NA, frame="none", cex=.5,
               col="white")
  } else {
    ## Colour nodes so that the most important is opaque and the least
    ## important is transparent, 
    col <- add.alpha("red", r)
    cex <- .5 + 1.5*r
    nodelabels(node=set.idx, pch=19, cex=cex, col=col)
    nodelabels(seq_len(upto),
               node=set.idx[order(s, decreasing=TRUE)[seq_len(upto)]],
               bg=NA, frame="none", cex=.5, col="white")
  }

  ## Add labels for the top fewl up to 20:
}

plot.ksi.density <- function(x, tree.hl, idx, cols, ...) {
  if ( attr(x, "test") != "ks" )
    stop("Can't yet plot distributions for non ks fits")

  nodes <- names(x)[seq_len(idx)]
  tree <- attr(x, "tree")
  dat <- attr(x, "dat")
  class <- classify.by.splits(tree, nodes)

  states <- lapply(attr(x, "contents")[[idx]],
                   function(j) dat[j])
  if ( length(states[[3]]) > 0 )
    dens <- lapply(states, density)
  else
    dens <- c(lapply(states[1:2], density), list(NULL))
  xlim <- range(sapply(dens, function(x) x$x))
  ylim <- range(sapply(dens[1:2], function(x) x$y))

  base <- class$base[idx]
  if ( is.na(base) )
    base <- classify.by.splits(attr(x, "tree"),
                               nodes[seq_len(idx)])$base[idx]
  cols3 <- c(cols[base], cols[idx+1], "gray")

  plot(NA, xlim=xlim, ylim=ylim, xlab="Trait value", ylab="Density",
       main=nodes[i], las=1, yaxt="n")
  for ( i in length(dens):1 )
    if ( !is.null(dens[[i]]) )
      polygon(dens[[i]]$x, dens[[i]]$y, border=cols3[i],
              col=add.alpha(cols3[i], .3))
  legend("topright",
         sprintf("%s (n=%s)",
                 c("Neighbourhood", "Target", "Others"),
                 sapply(states, length)),
         fill=add.alpha(cols3, .6))
}
