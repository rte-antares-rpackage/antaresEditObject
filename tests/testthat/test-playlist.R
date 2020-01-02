

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
  unlink(x = file.path(path, "test_case"), recursive = TRUE)
  
})
