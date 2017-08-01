context("ksi")

## checking that it runs
test_that("returns normal", {
  tree <- rtree(1000)
  tree$node.label <- paste0("nd", seq_len(tree$Nnode))
  vals1 <- setNames(runif(1000), tree$tip.label)
  ksi(tree, vals1)
  
  vals2 <- setNames(runif(100), tree$tip.label[1:100])
  ksi(tree, vals2)
  
  tree <- rtree(100)
  tree$node.label <- paste0("nd", seq_len(tree$Nnode))
  names(vals1)[1:50]<-tree$tip.label[1:50]
  ksi(tree, vals1)
})