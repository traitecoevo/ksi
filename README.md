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

| node  |  rank|  statistic|  statistic.rel| nodesets                                                      |
|:------|-----:|----------:|--------------:|:--------------------------------------------------------------|
| nd145 |     1|   1.887620|      1.0000000| nd145; nd136; nd134; nd133; nd132; nd121; nd146; nd154; nd149 |
| nd929 |     2|   1.724547|      0.9136093| nd929; nd927; nd926; nd925; nd937; nd938; nd939               |

This output gives the two most distinctive clades for out made-up phylogeny. Because of the nature of phylogenies, this returns the most distinctive single clade.

There is more statistical detail inside the returned object, as well as the data split in a useful way for easy visualization:

``` r
str(output)
```

    ## List of 2
    ##  $ nd145:List of 4
    ##   ..$ statistic: num 1.89
    ##   ..$ p.value  : num 0.00161
    ##   ..$ n        : int [1:2] 989 11
    ##   ..$ fit      :List of 5
    ##   .. ..$ statistic  : Named num 0.572
    ##   .. .. ..- attr(*, "names")= chr "D"
    ##   .. ..$ p.value    : num 0.00161
    ##   .. ..$ alternative: chr "two-sided"
    ##   .. ..$ method     : chr "Two-sample Kolmogorov-Smirnov test"
    ##   .. ..$ data.name  : chr "d.n and d.t"
    ##   .. ..- attr(*, "class")= chr "htest"
    ##  $ nd929:List of 4
    ##   ..$ statistic: num 1.72
    ##   ..$ p.value  : num 0.00522
    ##   ..$ n        : int [1:2] 971 18
    ##   ..$ fit      :List of 5
    ##   .. ..$ statistic  : Named num 0.41
    ##   .. .. ..- attr(*, "names")= chr "D"
    ##   .. ..$ p.value    : num 0.00522
    ##   .. ..$ alternative: chr "two-sided"
    ##   .. ..$ method     : chr "Two-sample Kolmogorov-Smirnov test"
    ##   .. ..$ data.name  : chr "d.n and d.t"
    ##   .. ..- attr(*, "class")= chr "htest"
    ##  - attr(*, "tree")=List of 5
    ##   ..$ edge       : int [1:1998, 1:2] 1001 1002 1003 1004 1005 1006 1007 1008 1009 1010 ...
    ##   ..$ tip.label  : chr [1:1000] "t603" "t509" "t501" "t812" ...
    ##   ..$ edge.length: num [1:1998] 0.49 0.943 0.638 0.252 0.391 ...
    ##   ..$ Nnode      : int 999
    ##   ..$ node.label : chr [1:999] "nd1" "nd2" "nd3" "nd4" ...
    ##   ..- attr(*, "class")= chr "phylo"
    ##   ..- attr(*, "order")= chr "cladewise"
    ##  - attr(*, "dat")= Named num [1:1000] 0.2363 0.0482 0.5596 0.9388 0.7891 ...
    ##   ..- attr(*, "names")= chr [1:1000] "t603" "t509" "t501" "t812" ...
    ##  - attr(*, "contents")=List of 2
    ##   ..$ nd145:List of 3
    ##   .. ..$ neighbourhood: int [1:989] 1 2 3 4 5 6 7 8 9 10 ...
    ##   .. ..$ target       : int [1:11] 138 139 140 141 142 143 144 145 146 147 ...
    ##   .. ..$ other        : int(0) 
    ##   ..$ nd929:List of 3
    ##   .. ..$ neighbourhood: int [1:971] 1 2 3 4 5 6 7 8 9 10 ...
    ##   .. ..$ target       : int [1:18] 927 928 929 930 931 932 933 934 935 936 ...
    ##   .. ..$ other        : int [1:11] 138 139 140 141 142 143 144 145 146 147 ...
    ##  - attr(*, "statistics")=List of 2
    ##   ..$ nd145: num [1:999] 0 0.545 0.504 0.858 0.879 ...
    ##   ..$ nd929: num [1:999] 0 0.572 0.454 0.659 0.676 ...
    ##  - attr(*, "test")= chr "ks"
    ##  - attr(*, "class")= chr "ksi"
