

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


test_that("Update playlist with weight", {
  
  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  
  updateGeneralSettings(nbyears = 10)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  mcYears = sample(1:5,3)
  weights = c(0.1,0.4,0.5)
  dfw = data.frame(mcYears, weights)
  
  setPlaylist(playlist = mcYears, weights = dfw, opts = opts)
  expect_length(getPlaylist(opts = opts), 2)
  
  expect_equal(getPlaylist(opts = opts)$activate_mc, sort(mcYears))
  expect_equal(getPlaylist(opts = opts)$weights$mcYears, mcYears)
  expect_equal(getPlaylist(opts = opts)$weights$weights, weights)
  
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
  opts <- setPlaylist(playlist = playlist_to_write, opts = opts)
  playlist_in_file <- getPlaylist(opts = opts)
  
  expect_true(opts$parameters$general$`user-playlist`)
  playlist_opts <- opts$parameters$playlist
  playlist_opts <- playlist_opts[which(names(playlist_opts) == "playlist_year +")]
  expect_true(all(unlist(playlist_opts) %in% (playlist_to_write - 1)))
  expect_equal(playlist_to_write, playlist_in_file)
  
  # No playlist if length(playlist) == length(mcYear) - user-playlist is disabled
  setPlaylist(playlist = mc_years, opts = opts)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  expect_false(opts$parameters$general$`user-playlist`)
})
