#Copyright © 2016 RTE Réseau de transport d’électricité


context("Function createLink")



# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, 'input')




# Tests -------------------------------------------------------------------

test_that("Create a new link", {
  
  areas <- sort(sample(x = getOption("antares")$areaList, size = 2))
  
  createLink(from = areas[1], to = areas[2], overwrite = TRUE)
  
  expect_true(paste(areas, collapse = " - ") %in% levels(antaresRead::getLinks()))
})


test_that("Create a new link - respect alphabetical order", {
  
  areas <- sort(sample(x = getOption("antares")$areaList, size = 2))
  
  createLink(from = areas[2], to = areas[1], overwrite = TRUE)
  expect_true(paste(areas, collapse = " - ") %in% levels(antaresRead::getLinks()))
  
  removeLink(from = areas[2], to = areas[1])
  expect_false(paste(areas, collapse = " - ") %in% levels(antaresRead::getLinks()))
})




areas <- sort(sample(x = getOption("antares")$areaList, size = 2))


test_that("Create a link with default properties", {
  
  if (is_antares_v7(opts)) {
    dataLink <- matrix(
      data = c(rep(0, 8760), rep(7500, 8760), rep(0, 8760*6)),
      ncol = 8
    ) 
  } else {
    dataLink <- matrix(
      data = c(rep(0, 8760), rep(7500, 8760), rep(0, 8760*3)),
      ncol = 5
    )
  }
  
  createLink(
    from = areas[1], 
    to = areas[2], 
    propertiesLink = propertiesLinkOptions(hurdles_cost = FALSE, transmission_capacities = "enabled"), 
    dataLink = dataLink,
    overwrite = TRUE
  )
  
  expect_true(paste(areas, collapse = " - ") %in% levels(antaresRead::getLinks()))
})




test_that("Remove a link", {
  
  removeLink(from = areas[1], to = areas[2])
  
  expect_false(paste(areas, collapse = " - ") %in% levels(antaresRead::getLinks()))
})




test_that("Remove a link that doesn't exist", {
  expect_message(removeLink(from = "myimaginaryarea", to = "myimaginaryareabis"))
})






# End ---------------------------------------------------------------------


# remove temporary study
unlink(x = file.path(path, "test_case"), recursive = TRUE)
