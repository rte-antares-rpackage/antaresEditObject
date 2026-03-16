test_that("backup study works with zip", {
  # given
  path <- file.path(tempdir(), "backupstudy_v930")
  suppressWarnings(
    opts <- createStudy(path, antares_version = "9.3")
  )
  
  # when 
  path_default_zip_file <- backupStudy()
  
  # then
  dir_extract_path <- file.path(tempdir(), "mydir")
  dir.create(path = dir_extract_path)
  zip::unzip(zipfile = path_default_zip_file, exdir = dir_extract_path)
  
  expect_true("backupstudy_v930" %in% dir(dir_extract_path)) 
  expected_list_files <- list.files(opts$studyPath, recursive = TRUE)
  unziped_study_list_files <- list.files(
    file.path(dir_extract_path, "backupstudy_v930"), recursive = TRUE)
  expect_equal(expected_list_files, unziped_study_list_files)
  
  deleteStudy()
})

test_that("backup study works with .tar.gz", {
  # given
  path <- file.path(tempdir(), "backupstudy_targz_v930")
  suppressWarnings(
    opts <- createStudy(path, antares_version = "9.3")
  )
  
  # when 
  path_default_tar_file <- backupStudy(extension = ".tar.gz")
  
  # then
  dir_extract_path <- file.path(tempdir(), "mydirtar")
  dir.create(path = dir_extract_path)
  utils::untar(tarfile = path_default_tar_file, exdir = dir_extract_path)
  
  expect_true("backupstudy_targz_v930" %in% dir(dir_extract_path)) 
  expected_list_files <- list.files(opts$studyPath, recursive = TRUE)
  untar_study_list_files <- list.files(
    file.path(dir_extract_path, "backupstudy_targz_v930"), recursive = TRUE)
  expect_equal(expected_list_files, untar_study_list_files)
  
  deleteStudy()
})
  