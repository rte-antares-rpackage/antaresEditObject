
# v860 ----
test_that("edit st-storage clusters (only for study >= v8.6.0" , {
  # global params for structure v8.6 ----
  opts_test <-createStudy(path = tempdir(), 
              study_name = "edit-cluster-st", 
              antares_version = "8.6.0")
  area_test = "be"
  opts_test <- createArea(name = area_test, opts = opts_test)

  ## 
  # INIT : create tests clusters
  ##
  opts_test <- createClusterST(area_test, 
                  "cluster-st-1", 
                  opts = opts_test) 
  
  opts_test <- createClusterST(area_test, 
                  "cluster-st-2", 
                  opts = opts_test) 
  
  st_clusters <- readClusterSTDesc(opts = opts_test)
  
  ## basics errors cases ----
  testthat::expect_error(editClusterST(area = area_test, 
                                       cluster_name = "cluster-st-1", 
                                       opts = "toto"), 
                         regexp = "inherit from class simOptions")
  opts_fake <- opts_test
  opts_fake$antaresVersion <- 820
  testthat::expect_error(editClusterST(area = area_test, 
                                       cluster_name = "cluster-st-1", 
                                       opts = opts_fake), 
                         regexp = "only available if using Antares >= 8.6.0")
  testthat::expect_error(editClusterST(area = "area_test", 
                                       cluster_name = "cluster-st-1", 
                                       opts = opts_test), 
                         regexp = "is not a valid area name")
  testthat::expect_error(editClusterST(area = area_test, 
                                       cluster_name = levels(st_clusters$cluster)[1], 
                                       group = "new group", 
                                       add_prefix = FALSE, 
                                       opts = opts_test), 
                         regexp = "is not a valid group recognized by Antares")
  testthat::expect_error(editClusterST(area = area_test, 
                                       cluster_name = "casper", 
                                       group = "Other1",
                                       add_prefix = FALSE, 
                                       opts = opts_test), 
                         regexp = "'casper' doesn't exist")
  
  ## default edition cluster ----
    # if all parameters are NULL => no edition of ini and data .txt
  testthat::expect_warning(editClusterST(area = area_test, 
                                         cluster_name = levels(st_clusters$cluster)[1],
                                         add_prefix = FALSE,
                                         opts = opts_test), 
                           regexp = "No edition for 'list.ini' file")
  
  ## edit list ini ----
    # edit only group value
  name_cluster_test <- levels(st_clusters$cluster)[1]
  # case insensitive
  expect_no_error(editClusterST(area = toupper(area_test),
                                cluster_name = toupper(name_cluster_test),
                                group = "Other5",
                                add_prefix = FALSE,
                                storage_parameters = list("efficiency" = 0.789),
                                opts = opts_test))

  opts_test <- editClusterST(area = area_test, 
                             cluster_name = name_cluster_test,
                             group = "Other2", 
                             add_prefix = FALSE,
                             opts = opts_test)
  
    # check update "group"
  st_clusters <- readClusterSTDesc(opts = opts_test)
  group_test <- st_clusters[cluster %in% name_cluster_test, 
                            .SD, .SDcols= "group"]
  testthat::expect_equal("Other2", as.character(group_test$group))

  
  
    # edit values (only 2 parameters)
  name_cluster_test <- levels(st_clusters$cluster)[2]
  list_params <- storage_values_default()[1:2]
  list_params$efficiency <- 0.5
  list_params$reservoircapacity <- 50
  
  initial_values <- st_clusters[cluster %in% name_cluster_test, 
                                .SD, 
                                .SDcols= c("efficiency", "reservoircapacity")]
  
  opts_test <- editClusterST(area = area_test, 
                             cluster_name = name_cluster_test, 
                             storage_parameters = list_params,
                             opts = opts_test, 
                             add_prefix = FALSE)
  
  
  st_clusters <- readClusterSTDesc(opts = opts_test)
  value_to_test <- st_clusters[cluster %in% name_cluster_test, 
                               .SD, 
                               .SDcols= c("group", 
                                          "efficiency", 
                                          "reservoircapacity")]
  
  # test value group is default
  testthat::expect_equal("Other1", as.character(value_to_test$group))
  
  # test parameters are updated
  value_to_test <- as.list(value_to_test[, .SD, 
                                         .SDcols= c("efficiency", 
                                                    "reservoircapacity")])
  testthat::expect_equal(list_params, value_to_test)
  
 
  ## edit DATA ----
  val <- 0.007
  opts_test <- editClusterST(area = area_test, 
                             cluster_name = levels(st_clusters$cluster)[1],  
                             PMAX_injection = matrix(val, 8760), 
                             PMAX_withdrawal = matrix(val, 8760),
                             inflows =  matrix(0.007, 8760), 
                             lower_rule_curve = matrix(val, 8760), 
                             upper_rule_curve = matrix(val, 8760),
                             opts = opts_test, 
                             add_prefix = FALSE)
  
  # test data value (with fread_antares)
  path_dir_test <- file.path(opts_test$inputPath, "st-storage", "series", area_test, 
                             paste(area_test, "cluster-st-1", sep = "_"))
  files_test <- list.files(path_dir_test, full.names = TRUE)
  l_file_series <- lapply(files_test, antaresRead:::fread_antares, opts = opts_test)
  
  value_test <- mean(sapply(l_file_series, 
                       function(.x){
                         mean(.x$V1)}))
  
  testthat::expect_equal(value_test, val)
  
  # test data value with readInputTS
  st_ts <- readInputTS(st_storage = "all", opts = opts_test)
  
  # check clusters
  testthat::expect_true(all(levels(st_clusters$cluster) %in% unique(st_ts$cluster)))
  
  # delete study
  unlink(opts_test$studyPath, recursive = TRUE)
})

# v880 ----
test_that("Edit short-term storage cluster (new feature v8.8.0)",{
  ## basics errors cases ----
  suppressWarnings(
    createStudy(path = tempdir(), 
                study_name = "st-storage880", 
                antares_version = "8.8.0"))
  
  # default area with st cluster
  area_test_clust = "al" 
  createArea(name = area_test_clust)
  
  # default 
  createClusterST(area = area_test_clust, 
                  cluster_name = "default")
  
  # edit 
  list_params <- storage_values_default()
  list_params$efficiency <- 0.5
  list_params$reservoircapacity <- 50
  list_params$enabled <- FALSE
  
  editClusterST(area = area_test_clust, 
                cluster_name = "default", 
                storage_parameters = list_params)
  
  # read properties 
  st_params <- readClusterSTDesc()
  
  # "enabled" must be present 
  testthat::expect_true("enabled"%in%names(st_params))
  testthat::expect_true(st_params$enabled[1]%in%FALSE)
  
  # test restrictions on 'group' parameter
  testthat::test_that("static 'group",{
    testthat::expect_warning(
      editClusterST(area = area_test_clust, 
                      cluster_name = "default", 
                      group = "not_allowed"), 
      regexp = paste0(
        "Group: '", "not_allowed", "' is not a valid name recognized by Antares,"
      )
    )
    
  })
  
  deleteStudy()
})


# >=9.1 ---- 
testthat::test_that("Allow dynamic `group`",{
  suppressWarnings(
    createStudy(path = tempdir(), 
                study_name = "st-storage9.2", 
                antares_version = "9.2"))
  
  # default area with st cluster
  area_test_clust = "al" 
  createArea(name = area_test_clust)
  
  # create 
  createClusterST(area = area_test_clust, 
                  cluster_name = "dynamic_grp", 
                  group = "toto")
  
  # edit
  editClusterST(area = area_test_clust, 
                cluster_name = "dynamic_grp", 
                group = "titi")
  
  # read properties
  opts_ <- simOptions()
  st_path <- file.path("input",
                       "st-storage", 
                       "clusters", 
                       area_test_clust, 
                       "list")
  
  st_file <- readIni(pathIni = st_path)
  
  # group has no restrictions
  testthat::expect_equal(st_file[[names(st_file)]][["group"]], "toto")
  
  deleteStudy()
})


