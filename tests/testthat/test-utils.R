test_that("Control the short-term storage existence",{
  
  ant_version <- "8.7.0"
  study_name <- paste0("my_study_870_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = study_name, antares_version = ant_version))

  nb_areas <- 5
  ids_areas <- seq(1,nb_areas)
  my_areas <- paste0("zone",ids_areas)

  lapply(my_areas, FUN = function(area){createArea(name = area, opts = simOptions())})
  
  st_clusters <- c("batterie", "pondage")
  my_clusters <- expand.grid("area" = my_areas, "cluster_name" = st_clusters)

  apply(my_clusters[,c("area","cluster_name")],
        MARGIN = 1,
        FUN = function(row){
          createClusterST(area = as.character(row[1]),
                          cluster_name = as.character(row[2]),
                          add_prefix = FALSE,
                          opts = simOptions()
        )
      }
  )

  createClusterST(area = "zone1", cluster_name = "vehicle", add_prefix = FALSE, opts = simOptions())
  exists_st_cluster <- check_cluster_name(area = "zone1", cluster_name = "vehicle", add_prefix = FALSE, opts = simOptions())
  expect_true(exists_st_cluster)
  exists_st_cluster <- check_cluster_name(area = "zone3", cluster_name = "vehicle", add_prefix = FALSE, opts = simOptions())
  expect_false(exists_st_cluster)
})
