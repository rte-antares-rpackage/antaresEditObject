context("Function editArea")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  expect_error(editArea("azerty", opts = opts))
  
  
  editArea("a", opts = opts)

  editArea("a", localization = c(1, 1), opts = opts)

  color_loc_ini <- readIniFile(file.path(studyPath, "input", "areas", "a", "ui.ini"))
  
  
  ##Edit position
  expect_true(color_loc_ini$layerX$`0` == 1)
  expect_true(color_loc_ini$layerY$`0` == 1)
  
  expect_true(color_loc_ini$ui$x == 1)
  expect_true(color_loc_ini$ui$y == 1)
  
  
  ## Edit color
  editArea("a", color = grDevices::rgb(230, 108, 44, max = 255), opts = opts)
  
  color_loc_ini <- readIniFile(file.path(studyPath, "input", "areas", "a", "ui.ini"))
  expect_true(color_loc_ini$layerColor$`0` == "230, 108,  44")
    
  expect_true(color_loc_ini$ui$color_r == 230)
  expect_true(color_loc_ini$ui$color_g == 108)
  expect_true(color_loc_ini$ui$color_b == 44)
  expect_true(color_loc_ini$layerColor$`0` == "230, 108,  44")
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})