

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


test_that("playlist V8", {
  
  
  opts <- setSimulationPath(file.path(V8dir))
    playlist <- getPlaylist()
  testthat::expect_true(all(playlist == 1:10))
  
  setPlaylist(playlist = c(1:8))
  testthat::expect_true(all(getPlaylist()$activate_mc == 1:8))
  
  weights_clean <- data.table(mcYears = c(1:8), weights = c(seq(from = 0.1, to = 0.7, length.out = 7), 14))
  setPlaylist(playlist = c(1:8), weights = weights_clean)
  testthat::expect_true(all(getPlaylist()$weights$mcYears == 1:8))
  testthat::expect_true(all(getPlaylist()$weights$weights ==  c(seq(from = 0.1, to = 0.7, length.out = 7), 14)))
  
  weights_partial <- data.table(mcYears = c(1, 3, 5), weights = c(0.1, 0.3, 14))
  testthat::expect_error(setPlaylist(playlist = c(1:8), weights = weights_partial))
  
  
  too_many_weights <- data.table(mcYears = c(1:9), weights = c(seq(from = 0.1, to = 0.8, length.out = 8), 14))
  testthat::expect_error(setPlaylist(playlist = c(1:8), weights = too_many_weights))
  
  setPlaylist(playlist = c(1:8))
  testthat::expect_true(all(getPlaylist()$activate_mc == 1:8))
  

})

