<!-- README.md is generated from README.Rmd. Please edit that file -->
### Finding Distinctive Clades with the Komogorov-Smirnov Importance Statistic

This is the package that we used in [Cornwell et al 2014](10.1111/1365-2745.12208).

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
tree <- rtree(1000)
tree$node.label <- paste0("nd", seq_len(tree$Nnode))
vals <- setNames(runif(1000), tree$tip.label)
output<-ksi(tree, vals, depth = 5)
```

    ## Using 'ks' tests
    ## depth = 1...best node: 910 -- nd910
    ## depth = 2...best node: 656 -- nd656
    ## depth = 3...best node: 286 -- nd286
    ## depth = 4...best node: 65 -- nd65
    ## depth = 5...best node: 219 -- nd220
