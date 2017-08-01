ksi <- function(tree, dat, depth=10, test=NULL,
                verbose=TRUE,
                multicore=FALSE,
                multicore.args=list()) {
  if ( multicore ) {
    if ( !require("parallel") )
      stop("The 'parallel' package is needed to use multicore=TRUE")
    loop <- set.defaults(mclapply, defaults=multicore.args)
  } else
    loop <- lapply

  if ( is.null(test) ) {
    if ( is.logical(dat) || is.factor(dat) ||
        !is.null(check.integer(dat, FALSE)) )
      test <- "chisq"
    else if ( is.numeric(dat) )
      test <- "ks"
    else
      stop("Invalid data type")
    if ( verbose )
      cat(sprintf("Using '%s' tests\n", test))
  } else {
    test <- match.arg(test, c("ks", "chisq"))
  }
  if (is.null(tree$node.label) || any(duplicated(tree$node.label))) {
    stop("Tree must have unique node labels")
  }

  to.drop <- setdiff(tree$tip.label, names(dat))
  if(!identical(sort(tree$tip.label), sort(names(dat)))){
    tree <- diversitree:::drop.tip.fixed(tree, to.drop)
  }
  possible <- tree$node.label
  dat <- dat[tree$tip.label]

  if ( depth == Inf )
    depth <- tree$Nnode - 1
  else if ( depth < 1 || depth > tree$Nnode )
    stop(sprintf("Depth must be on [1,%d]", tree$Nnode))
  
  statistics <- contents <- res <- vector("list", depth)
  nodes <- character(depth)
  group <- base <- NULL

  for ( i in seq_len(depth) ) {
    if ( verbose )
      cat(sprintf("depth = %d...", i))

    tests <- loop(possible, test.ksi, tree, dat, group, base, test)

    stat <- sapply(tests, "[[", "statistic")

    best <- which.max(stat)
    node <- nodes[i] <- possible[best]
    res[[i]] <- tests[[best]]
    statistics[[i]] <- rep(NA_real_, tree$Nnode)
    statistics[[i]][match(possible, tree$node.label)] <- stat

    tmp <- classify.by.splits(tree, node, group, base)
    group <- tmp$group
    base <- tmp$base

    tip.type <- group[match(seq_along(tree$tip.label), tree$edge[, 2])]
    contents[[i]] <-
      list(neighbourhood=which(tip.type == base[i]),
           target=which(tip.type == i+1),
           other=which(!(tip.type %in% c(base[i], i+1))))

    if ( verbose )
      cat(sprintf("best node: %d -- %s\n", best, nodes[i]))

    possible <- possible[-best]
  }

  names(contents) <- names(res) <- names(statistics) <- nodes
  attr(res, "tree") <- tree
  attr(res, "dat") <- dat
  attr(res, "contents") <- contents
  attr(res, "statistics") <- statistics
  attr(res, "test") <- test
  class(res) <- "ksi"
  res
}

## This tests one node of the tree.
test.ksi <- function(node, tree, dat, group, base, test) {
  tmp <- classify.by.splits(tree, node, group, base)
  base.type <- tmp$base[length(tmp$base)]

  tip.type <- tmp$group[match(seq_along(tree$tip.label), tree$edge[, 2])]
  d.n <- dat[tip.type == base.type]
  d.t <- dat[tip.type == max(tip.type)]
  n <- c(length(d.n), length(d.t))

  if ( test == "ks" ) {
    n1n2 <- sqrt(prod(n)/sum(n))
    if ( min(n) == 0  )
      fit <- list(statistic=0, p.value=1)
    else if ( identical(getOption("ksi.quick"), TRUE) )
      fit <- list(statistic=ks.test.quick(d.n, d.t), p.value=0)
    else
      fit <- suppressWarnings(ks.test(d.n, d.t))
  } else {
    if ( min(n) == 0 )
      fit <- list(statistic=0, p.value=1)
    else {
      m <- rbind(neighbourhood=table(d.n),
                 target=table(d.t))
      fit <- suppressWarnings(chisq.test(m))
      
      if ( is.infinite(fit$statistic) ) {
        fit$statistic <- 0
        fit$p.value <- 1
      }
    }
  }
    
  stat <- if (test=="ks") fit$statistic * n1n2 else fit$statistic
  
  list(statistic = as.numeric(stat),
       p.value   = fit$p.value,
       n         = n,
       fit       = fit)
}


print.ksi <- function(x, ...) {
  str <- c(sprintf("Nested distributions; %d nodes, split at:",
                   length(x)),
           strwrap(paste(names(x), collapse=", "), indent=4))
  cat(paste(str, collapse="\n"), "\n")
}

summary.ksi <- function(object, ...) {
  nodesets <- sapply(seq_along(object), function(i)
                     paste(ksi.nodeset(object, i), collapse="; "))
  stat <- sapply(object, "[[", "statistic")
  ret <- data.frame(node=names(object),
                    rank=seq_along(object),
                    statistic=stat,
                    statistic.rel=stat / max(stat),
                    nodesets=nodesets,
                    stringsAsFactors=FALSE)
  rownames(ret) <- NULL
  ret
}

## Find all contiguous nodes that are in the 5% tail of the
## distribution.  This is a fun little algorithm that probably
## deserves documentation.
ksi.nodeset <- function(obj, idx, alpha=1/20, node=NULL) {
  if ( is.null(node) )
    node <- names(obj)[idx]
  else if ( is.integer(node) )
    node <- attr(obj, "tree")$node.label[node]
  else if ( !is.character(node) )
    stop("Invalid argument for 'node'")
  
  tree <- attr(obj, "tree")
  stat <- attr(obj, "statistic")[[idx]]
  ok <- stat > quantile(stat, 1-alpha, na.rm=TRUE)

  n.tip <- length(tree$tip.label)
  edge <- tree$edge
  idx <- match(node, tree$node.label) + n.tip

  ## Down first, towards root.
  set.down <- idx
  repeat {
    idx <- edge[match(idx, edge[,2]),1]
    if ( is.na(idx) || is.na(ok[idx - n.tip]) || !ok[idx - n.tip] )
      break
    set.down <- c(set.down, idx)
  }

  ## Then down
  set.up <- idx <- set.down
  repeat {
    idx <- edge[edge[,1] %in% idx,2]
    idx <- idx[idx > n.tip]
    idx <- idx[!is.na(idx) &
               ok[idx - n.tip] &
               !is.na(ok[idx - n.tip])]
    if ( length(idx) == 0 )
      break
    set.up <- c(set.up, idx)
  }

  tree$node.label[unique(set.up) - n.tip]
}

ksi.group <- function(obj, idx, alpha=1/20, include=1/5) {
  tree <- attr(obj, "tree")
  ss <- s <- attr(obj, "statistic")[[idx]]
  g <- rep(NA, length(s))
  for ( i in seq_along(g) ) {
    ns <- ksi.nodeset(obj, idx, alpha, which.max(s))
    j <- match(ns, tree$node.label)
    g[j] <- i
    attr(obj, "statistic")[[idx]][j] <- NA
    s[j] <- 0
    if ( mean(!is.na(g)) > include )
      break
  }
  g[is.na(g) & !is.na(s)] <- Inf

  tmp <- data.frame(s=ss, g, idx=seq_along(s))
  tmp <- tmp[order(tmp$g, -tmp$s),]
  r <- c(sequence(table(g)),
         rep(NA, sum(is.na(g))))
  tmp <- cbind(tmp, rank=r)
  tmp[order(tmp$idx),]
}

