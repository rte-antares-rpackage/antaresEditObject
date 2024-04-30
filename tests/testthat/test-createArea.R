

context("Function createArea")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, 1)
  
  
  test_that("Cannot initialize a new area if not in 'Input' mode", {
    expect_error(createArea(name = "myarea"))
  })
  
  
  # set simulation path in mode input
  opts <- antaresRead::setSimulationPath(studyPath, 'input')
  
  
  
  
  
  test_that("Backup study/input", {
    skip_on_cran() # issue 115
    backupStudy(what = "study", extension = ".zip")
    backupStudy(what = "input", extension = ".tar.gz")
    expect_true(file.exists(paste0(opts$studyPath, ".zip")))
    expect_true(file.exists(paste0(opts$inputPath, ".tar.gz")))
  })
  
  
  
  test_that("Initialize a new area", {
    n_before <- length(getOption("antares")$areaList)
    createArea(name = "myarea")
    n_after <- length(getOption("antares")$areaList)
    expect_equal(n_before + 1, n_after)
    expect_true("myarea" %in% getOption("antares")$areaList)
  })
  
  
  test_that("nodal optimization options are properly written", {
    createArea(
      name = "testarea",
      nodalOptimization = nodalOptimizationOptions(
        non_dispatchable_power = FALSE,
        dispatchable_hydro_power = TRUE,
        other_dispatchable_power = FALSE,
        spread_unsupplied_energy_cost = 10,
        spread_spilled_energy_cost = 3.14,
        average_unsupplied_energy_cost = 239,
        average_spilled_energy_cost = 1000
      )
    )
    
    optim_testarea <- readIniFile(file.path(opts$inputPath, "areas", "testarea", "optimization.ini"))
    expect_equal(optim_testarea$`nodal optimization`$`dispatchable-hydro-power`, TRUE)
    expect_equal(optim_testarea$`nodal optimization`$`spread-unsupplied-energy-cost`, 10)
    expect_equal(optim_testarea$`nodal optimization`$`non-dispatchable-power`, FALSE)
    expect_equal(optim_testarea$`nodal optimization`$`other-dispatchable-power`, FALSE)
    expect_equal(optim_testarea$`nodal optimization`$`spread-spilled-energy-cost`, 3.14)
    
    thermal_areas <- readIniFile(file.path(opts$inputPath, "thermal", "areas.ini"))
    expect_equal(thermal_areas$spilledenergycost$testarea, 1000)
    expect_equal(thermal_areas$unserverdenergycost$testarea, 239)

    
  })
  
  # Initialize hydro.ini values for a new area 
  test_that("Check if hydro.ini default values are initialized when creating an area", {
    
    default_hydro_params <- get_default_hydro_ini_values()
    
    hydro_ini_path <- file.path("input", "hydro", "hydro.ini")
    hydro_ini_data <- readIni(pathIni = hydro_ini_path, opts = opts)
    
    new_area <- "area_check_hydro"
    createArea(new_area)
    
    hydro_ini_data_after_area_creation <- readIni(pathIni = hydro_ini_path, opts = opts)
    
    # Check if each section is in the hydro.ini file
    name_ref <- setdiff(names(default_hydro_params), "reservoir capacity")
    name_ref <- name_ref[order(name_ref)]
    name_ <- names(hydro_ini_data_after_area_creation)
    name_ <- name_[order(name_)]
    
    expect_equal(name_, name_ref)
    
    # Check if new_area has an entry for each section
    check_by_section <- sapply(names(hydro_ini_data_after_area_creation),
                               FUN = function(name) new_area %in% names(hydro_ini_data_after_area_creation[[name]])
    )
    expect_true(all(check_by_section))
    
    # Check if the value for new_area is the default value
    value_by_section <- sapply(names(hydro_ini_data_after_area_creation),
                               FUN = function(name) hydro_ini_data_after_area_creation[[name]][[new_area]],
                               simplify = FALSE
    )
    
    for(key in name_ref){
      expect_equal(default_hydro_params[[key]], value_by_section[[key]])
    }
    
  })
  
  
  test_that("Remove an area", {
    area2remove <- "myareatoremove"
    createArea(name = area2remove)
    
    ra <- checkRemovedArea(area = area2remove)
    expect_true(length(ra$areaResiduFiles) > 0)
    expect_true(length(ra$areaResidus) > 0)
    
    removeArea(name = area2remove)
    ra <- checkRemovedArea(area = area2remove)
    expect_length(ra$areaResiduFiles, 0)
    expect_length(ra$areaResidus, 0)
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})


test_that("adequacy patch options are properly written", {
  
  tmp <- tempfile()
  suppressWarnings({
    createStudy(path = tmp, antares_version = "8.3.0")
    opts <- antaresRead::setSimulationPath(tmp)
  })
  
  #activateRES(quietly = TRUE)
  createArea(
    name = "testarea_adq",
    adequacy = adequacyOptions(
      adequacy_patch_mode = "inside"
    )
  )
  
  adq_testarea <- readIniFile(file.path(opts$inputPath, "areas", "testarea_adq", "adequacy_patch.ini"))
  expect_equal(adq_testarea$`adequacy-patch`$`adequacy-patch-mode`, "inside")
  
  unlink(tmp, recursive = TRUE)
})


# v860 ----
test_that("create area / st-storage in 8.6.0", {
  
  tmp <- tempfile()
  suppressWarnings({
    temp_study_860 <- createStudy(path = tmp, antares_version = "8.6.0")
    opts <- antaresRead::setSimulationPath(tmp)
  })
  
  # check area 
  createArea(name = "myarea", 
             opts = temp_study_860)
  
  areas_created <- getAreas()
  expect_true("myarea" %in% areas_created)
  
  expect_true(dir.exists(file.path(tmp,"input","st-storage","clusters","myarea")))
  
  # check ini file
  ini_path_file <- file.path(temp_study_860$inputPath, 
                             "st-storage","clusters","myarea", "list.ini")
  expect_true(file.exists(ini_path_file))
  
  # check mingen.txt file (hydro)
  mingen_path_file <- file.path(temp_study_860$inputPath, 
                             "hydro", "series","myarea", "mingen.txt")
  expect_true(file.exists(mingen_path_file))
  
  # REMOVE TESTS
  test_that("Remove an area in 8.6.0", {
    area2remove <- "myareatoremove"
    createArea(name = area2remove, 
               opts = temp_study_860)
    
    ra <- checkRemovedArea(area = area2remove)
    expect_true(length(ra$areaResiduFiles) > 0)
    expect_true(length(ra$areaResidus) > 0)
    
    removeArea(name = area2remove)
    ra <- checkRemovedArea(area = area2remove)
    expect_length(ra$areaResiduFiles, 0)
    expect_length(ra$areaResidus, 0)
    
    expect_equal(getAreas(select = area2remove), character(0))
  })
  
  # remove temp study
  unlink(tmp, recursive = TRUE)
})


test_that("removeArea() in 8.2.0 : check that properties.ini are all there", {
  
  ## V8
  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  
  # prop is taken cause it is a substring included in the char properties.ini
  area <- "prop"
  area2 <- "zone51"
  area3 <- "zone52"
  
  createArea(name = area, opts = opts)
  createArea(name = area2, opts = opts)
  createArea(name = area3, opts = opts)
  
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  createLink(from = area, to = area2, opts = opts)
  createLink(from = area, to = area3, opts = opts)
  createLink(from = area2, to = area3, opts = opts)
  
  opts <- setSimulationPath(opts$studyPath, simulation = "input")
  
  removeArea(name = area, opts = opts)
  
  links_path <- file.path(opts$inputPath, "links")
  dirs_links <- list.dirs(path = links_path, full.names = TRUE, recursive = FALSE)
  files_properties <- list.files(path = links_path, full.names = TRUE, recursive = TRUE, pattern = "properties.ini$")
  
  expect_true(length(dirs_links) == length(files_properties))
  expect_true(all(file.exists(file.path(dirs_links, "properties.ini"))))
  
  unlink(opts$studyPath, recursive = TRUE)
  
  ## V7
  ant_version <- "7.1.0"
  st_test <- paste0("my_study_710_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  
  # prop is taken cause it is a substring included in the char properties.ini
  area <- "prop"
  area2 <- "zone51"
  area3 <- "zone52"
  
  createArea(name = area, opts = opts)
  createArea(name = area2, opts = opts)
  createArea(name = area3, opts = opts)
  
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  createLink(from = area, to = area2, opts = opts)
  createLink(from = area, to = area3, opts = opts)
  createLink(from = area2, to = area3, opts = opts)
  
  opts <- setSimulationPath(opts$studyPath, simulation = "input")
  
  removeArea(name = area, opts = opts)
  
  links_path <- file.path(opts$inputPath, "links")
  dirs_links <- list.dirs(path = links_path, full.names = TRUE, recursive = FALSE)
  files_properties <- list.files(path = links_path, full.names = TRUE, recursive = TRUE, pattern = "properties.ini$")
  
  expect_true(length(dirs_links) == length(files_properties))
  expect_true(all(file.exists(file.path(dirs_links, "properties.ini"))))
  
  unlink(opts$studyPath, recursive = TRUE)
})


# Area in binding constraint not removed ----
test_that("removeArea(): check that area is removed if it is not referenced in a binding constraint and not removed if the area is referenced in a binding constraint", {
  
  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  
  # Areas
  nb_areas <- 5
  ids_areas <- seq(1,nb_areas)
  my_areas <- paste0("zone",ids_areas)

  lapply(my_areas, FUN = function(area){createArea(name = area, opts = simOptions())})

  # Links
  my_links <- expand.grid("from" = ids_areas, "to" = ids_areas)
  my_links <- my_links[my_links$from < my_links$to,]
  my_links$from <- paste0("zone", my_links$from)
  my_links$to <- paste0("zone", my_links$to)
  
  apply(my_links[,c("from","to")],
        MARGIN = 1,
        FUN = function(row){
          createLink(as.character(row[1]),as.character(row[2]), opts = simOptions())
    } 
  )

  # Clusters
  clusters <- c("nuclear", "gas", "coal")
  my_clusters <- expand.grid("area" = my_areas, "cluster_name" = clusters)
  my_clusters$cluster_name_prefixed <- paste0(my_clusters$area, "_", my_clusters$cluster_name)
  my_clusters$cluster_name_binding <- paste0(my_clusters$area, ".", my_clusters$cluster_name_prefixed)
  lst_clusters <- split(my_clusters[,c("cluster_name_binding")], my_clusters$cluster_name)
  
  apply(my_clusters[,c("area","cluster_name")],
        MARGIN = 1,
        FUN = function(row){
          createCluster(area = as.character(row[1]),
                        cluster_name = as.character(row[2]),
                        add_prefix = TRUE,
                        opts = simOptions())
    }
  )

  suppressWarnings(opts <- setSimulationPath(path = opts$studyPath, simulation = "input"))

  # Binding constraints
  # Link
  all_areas <- getAreas(opts = opts)
  all_links <- as.character(getLinks(opts = opts))
  all_links <- gsub(pattern = " - ", replacement = "%", x = all_links)
  nb_cols_per_matrix <- 3
  nb_hours_per_year <- 8784
  nb_values_per_matrix <- nb_hours_per_year * nb_cols_per_matrix
  for (area in all_areas) {
    
    links_area <- all_links[startsWith(all_links, area)]
    if (length(links_area) > 0) {
      coefs <- seq_len(length(links_area))
      names(coefs) <- links_area
      createBindingConstraint(name = paste0("bc_",area),
                              timeStep = "hourly",
                              operator = "less",
                              coefficients = coefs,
                              values = matrix(rep(0, nb_values_per_matrix), ncol = nb_cols_per_matrix),
                              opts = opts)
    }
  }

  # Cluster
  for (cluster in names(lst_clusters)) {
    names_coefs_bc <- lst_clusters[[cluster]]
    coefs <- seq_len(length(names_coefs_bc))
    names(coefs) <- names_coefs_bc
    createBindingConstraint(name = paste0("bc_",cluster),
                            timeStep = "hourly",
                            operator = "less",
                            coefficients = coefs,
                            values = matrix(rep(0, nb_values_per_matrix), ncol = nb_cols_per_matrix),
                            opts = opts)
  }
  
  new_area <- "zzone_bc_link"
  
  # Area
  opts <- createArea(name = new_area, opts = simOptions())
  expect_no_error(removeArea(name = new_area, opts = simOptions()))
  
  # Area + Link
  opts <- createArea(name = new_area, opts = simOptions())
  opts <- createLink(from = "zone1", to = new_area, opts = simOptions())
  expect_no_error(removeArea(name = new_area, opts = simOptions()))
  
  # Area + Link + Binding Constraint
  opts <- createArea(name = new_area, opts = simOptions())
  opts <- createLink(from = "zone1", to = new_area, opts = simOptions())
  coefs <- c(1)
  names(coefs) <- paste0("zone1", "%", new_area)
  name_bc <- "bc_new_area_link"
  opts <- createBindingConstraint(name = name_bc,
                                  timeStep = "hourly",
                                  operator = "less",
                                  coefficients = coefs,
                                  values = matrix(rep(0, nb_values_per_matrix), ncol = nb_cols_per_matrix),
                                  opts = simOptions())
  expect_error(removeArea(name = new_area, opts = simOptions()),
               regexp = paste0("Can not remove the area ", new_area)
  )
  
  removeBindingConstraint(name = name_bc, opts = simOptions())
  expect_no_error(removeArea(name = new_area, opts = simOptions()))
  
  new_area <- "zzone_bc_cluster"
  
  # Area
  opts <- createArea(name = new_area, opts = simOptions())
  expect_no_error(removeArea(name = new_area, opts = simOptions()))
  
  # Area + Cluster
  opts <- createArea(name = new_area, opts = simOptions())
  opts <- createCluster(area = new_area, cluster_name = "nuclear", add_prefix = TRUE, opts = simOptions())
  expect_no_error(removeArea(name = new_area, opts = simOptions()))
  
  # Area + Cluster + Binding Constraint
  opts <- createArea(name = new_area, opts = simOptions())
  cl_name <- "nuclear"
  opts <- createCluster(area = new_area, cluster_name = cl_name, add_prefix = TRUE, opts = simOptions())
  coefs <- c(1)
  names(coefs) <- paste0(new_area, ".", paste0(new_area, "_", cl_name))
  name_bc <- "bc_new_area_cluster"
  opts <- createBindingConstraint(name = name_bc,
                                  timeStep = "hourly",
                                  operator = "less",
                                  coefficients = coefs,
                                  values = matrix(rep(0, nb_values_per_matrix), ncol = nb_cols_per_matrix),
                                  opts = simOptions())
  expect_error(removeArea(name = new_area, opts = simOptions()),
               regexp = paste0("Can not remove the area ", new_area)
  )
  
  removeBindingConstraint(name = name_bc, opts = simOptions())
  expect_no_error(removeArea(name = new_area, opts = simOptions()))
  
  new_area <- "zzone_bc_cluster_link"
  
  # Area + Cluster + Link + Binding Constraint : every coefficient has the area to remove
  opts <- createArea(name = new_area, opts = simOptions())
  opts <- createLink(from = "zone1", to = new_area, opts = simOptions())
  opts <- createCluster(area = new_area, cluster_name = cl_name, add_prefix = TRUE, opts = simOptions())
  
  coefs <- c(1,2)
  names(coefs) <- c(paste0(new_area, ".", paste0(new_area, "_", cl_name)), paste0("zone1", "%", new_area))
  name_bc <- "bc_new_area_cluster_link"
  opts <- createBindingConstraint(name = name_bc,
                                  timeStep = "hourly",
                                  operator = "less",
                                  coefficients = coefs,
                                  values = matrix(rep(0, nb_values_per_matrix), ncol = nb_cols_per_matrix),
                                  opts = simOptions())
  expect_error(removeArea(name = new_area, opts = simOptions()),
               regexp = paste0("Can not remove the area ", new_area)
  )
  
  removeBindingConstraint(name = name_bc, opts = simOptions())
  expect_no_error(removeArea(name = new_area, opts = simOptions()))
  
  new_area <- "zzone_bc_cluster_link_2"
  
  # Area + Cluster + Link + Binding Constraint : at least one coefficient has the area to remove
  opts <- createArea(name = new_area, opts = simOptions())
  opts <- createLink(from = "zone1", to = new_area, opts = simOptions())
  opts <- createCluster(area = new_area, cluster_name = cl_name, add_prefix = TRUE, opts = simOptions())
  
  coefs <- c(1,2,3,4)
  names(coefs) <- c(paste0(new_area, ".", paste0(new_area, "_", cl_name)), paste0("zone1", "%", new_area), paste0("zone1", "%", "zone2"), paste0("zone2", ".", "zone2_gas"))
  name_bc <- "bc_new_area_cluster_link_2"
  opts <- createBindingConstraint(name = name_bc,
                                  timeStep = "hourly",
                                  operator = "less",
                                  coefficients = coefs,
                                  values = matrix(rep(0, nb_values_per_matrix), ncol = nb_cols_per_matrix),
                                  opts = simOptions())
  expect_error(removeArea(name = new_area, opts = simOptions()),
               regexp = paste0("Can not remove the area ", new_area)
  )
  
  removeBindingConstraint(name = name_bc, opts = simOptions())
  expect_no_error(removeArea(name = new_area, opts = simOptions()))
  
  # standard areas
  for (area in my_areas) {
    expect_error(removeArea(name = area, opts = simOptions()),
               regexp = paste0("Can not remove the area ", area)
    )
  }
  
  unlink(opts$studyPath, recursive = TRUE)
})
