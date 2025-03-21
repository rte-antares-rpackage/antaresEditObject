# >=860 ----

testthat::test_that("remove st storage", {
  # # RemoveClusterST (if no cluster => function read return error => see readClusterDesc tests)
  # opts_test <- removeClusterST(area = area_test_clust, "cluster1", 
  #                              opts = opts_test)
  # 
  # testthat::expect_false(paste(area_test_clust, "cluster1", sep = "_") %in% 
  #                          levels(readClusterSTDesc(opts = opts_test)$cluster))
})