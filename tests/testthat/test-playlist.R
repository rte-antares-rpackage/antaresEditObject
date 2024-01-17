

context("Functions get and set playlist")

sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  test_that("Update playlist", {
    
    setPlaylist(playlist = 1, opts = opts)
    expect_length(getPlaylist(opts = opts), 1)
    expect_equal(getPlaylist(opts = opts), 1)
    
    setPlaylist(playlist = 2, opts = opts)
    expect_length(getPlaylist(opts = opts), 1)
    expect_equal(getPlaylist(opts = opts), 2)
    
    
    setPlaylist(playlist = c(1,2), opts = opts)
    expect_length(getPlaylist(opts = opts), 2)
    expect_equal(getPlaylist(opts = opts), c(1,2))
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})


test_that("Check if setPlaylist() is disabled when playlist = mcYear and enabled when playlist is customized", {
  
  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  
  # Default behaviour
  expect_false(opts$parameters$general$`user-playlist`)
  
  # Set parameters
  updateGeneralSettings(nbyears = 10)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  nbyears <- opts$parameters$general$nbyears
  mc_years <- seq(nbyears)
  
  # With custom playlist - user-playlist is enabled
  playlist_to_write <- sort(sample(mc_years, size = nbyears/2, replace = FALSE))
  setPlaylist(playlist = playlist_to_write, opts = opts)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  playlist_in_file <- getPlaylist(opts = opts)
  
  expect_true(opts$parameters$general$`user-playlist`)
  expect_equal(playlist_to_write, playlist_in_file)
  
  # No playlist if length(playlist) == length(mcYear) - user-playlist is disabled
  setPlaylist(playlist = mc_years, opts = opts)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  expect_false(opts$parameters$general$`user-playlist`)
})
