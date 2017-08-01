<!-- README.md is generated from README.Rmd. Please edit that file -->
Finding Distinctive Clades with the Komogorov-Smirnov Importance Statistic
--------------------------------------------------------------------------

[![Build Status](https://travis-ci.org/traitecoevo/ksi.svg?branch=master)](https://travis-ci.org/traitecoevo/ksi)

This is the package that we used in [Cornwell et al 2014](http://onlinelibrary.wiley.com/doi/10.1111/1365-2745.12208/abstract). The idea is to find clades that are a mix of large and unusual with respect to trait values. The weighting of largeness and unusualness is done via the classic Komogorov-Smirnov test statistic. All nodes on the tree are tested for "distinctiveness". This can be done iteratively as shown below.

The package can be installed using [devtools](https://github.com/hadley/devtools), which itself can be installed from CRAN with

``` r
# install.packages("devtools") # uncomment this line if you don't have devtools installed
library(devtools)
install_github("traitecoevo/ksi")
library(ksi)
```

and the package will be installed.

There is really only one useful function in the package: `ksi`. See the help page `?ksi` for more information once installed and loaded.

The key thing to know is that node labels are essential for this process, so they should be as informative as possible in your input tree. `Depth` is the number of distinctive clades that the algorithm returns.

``` r
library(ape)
library(knitr)
tree <- rtree(1000)
tree$node.label <- paste0("nd", seq_len(tree$Nnode))
vals <- setNames(runif(1000), tree$tip.label)
output <- ksi(tree, vals, depth = 2,verbose = FALSE)
kable(summary(output))
```

| node  |  rank|  statistic|  statistic.rel| nodesets                          |
|:------|-----:|----------:|--------------:|:----------------------------------|
| nd197 |     1|   1.977782|      1.0000000| nd197; nd196; nd195; nd198; nd199 |
| nd904 |     2|   1.737567|      0.8785437| nd904; nd903; nd902; nd929; nd930 |

This output gives the two most distinctive clades for out made-up phylogeny. Because of the nature of phylogenies, this returns the node from which the most distinctive single clade descendended, but also includes neighboring nodes which are very difficult to distinguish statistically.

There is more statistical detail inside the returned object, as well as the data split in a useful way for easy visualization:

``` r
str(output)
```

    ## List of 2
    ##  $ nd197:List of 4
    ##   ..$ statistic: num 1.98
    ##   ..$ p.value  : num 0.000103
    ##   ..$ n        : int [1:2] 994 6
    ##   ..$ fit      :List of 5
    ##   .. ..$ statistic  : Named num 0.81
    ##   .. .. ..- attr(*, "names")= chr "D"
    ##   .. ..$ p.value    : num 0.000103
    ##   .. ..$ alternative: chr "two-sided"
    ##   .. ..$ method     : chr "Two-sample Kolmogorov-Smirnov test"
    ##   .. ..$ data.name  : chr "d.n and d.t"
    ##   .. ..- attr(*, "class")= chr "htest"
    ##  $ nd904:List of 4
    ##   ..$ statistic: num 1.74
    ##   ..$ p.value  : num 0.00477
    ##   ..$ n        : int [1:2] 936 58
    ##   ..$ fit      :List of 5
    ##   .. ..$ statistic  : Named num 0.235
    ##   .. .. ..- attr(*, "names")= chr "D"
    ##   .. ..$ p.value    : num 0.00477
    ##   .. ..$ alternative: chr "two-sided"
    ##   .. ..$ method     : chr "Two-sample Kolmogorov-Smirnov test"
    ##   .. ..$ data.name  : chr "d.n and d.t"
    ##   .. ..- attr(*, "class")= chr "htest"
    ##  - attr(*, "tree")=List of 5
    ##   ..$ edge       : int [1:1998, 1:2] 1001 1002 1003 1004 1004 1003 1005 1006 1006 1007 ...
    ##   ..$ tip.label  : chr [1:1000] "t921" "t220" "t594" "t723" ...
    ##   ..$ edge.length: num [1:1998] 0.977 0.911 0.202 0.902 0.121 ...
    ##   ..$ Nnode      : int 999
    ##   ..$ node.label : chr [1:999] "nd1" "nd2" "nd3" "nd4" ...
    ##   ..- attr(*, "class")= chr "phylo"
    ##   ..- attr(*, "order")= chr "cladewise"
    ##  - attr(*, "dat")= Named num [1:1000] 0.6338 0.3025 0.7538 0.1336 0.0945 ...
    ##   ..- attr(*, "names")= chr [1:1000] "t921" "t220" "t594" "t723" ...
    ##  - attr(*, "contents")=List of 2
    ##   ..$ nd197:List of 3
    ##   .. ..$ neighbourhood: int [1:994] 1 2 3 4 5 6 7 8 9 10 ...
    ##   .. ..$ target       : int [1:6] 190 191 192 193 194 195
    ##   .. ..$ other        : int(0) 
    ##   ..$ nd904:List of 3
    ##   .. ..$ neighbourhood: int [1:936] 1 2 3 4 5 6 7 8 9 10 ...
    ##   .. ..$ target       : int [1:58] 902 903 904 905 906 907 908 909 910 911 ...
    ##   .. ..$ other        : int [1:6] 190 191 192 193 194 195
    ##  - attr(*, "statistics")=List of 2
    ##   ..$ nd197: num [1:999] 0 0.645 1.298 0.522 1.293 ...
    ##   ..$ nd904: num [1:999] 0 0.661 1.322 0.526 1.316 ...
    ##  - attr(*, "test")= chr "ks"
    ##  - attr(*, "class")= chr "ksi"
