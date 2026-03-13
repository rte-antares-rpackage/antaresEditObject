test_that("backup study works", {
  path <- file.path(tempdir(), "backupstudy_v930")
  suppressWarnings(
    opts <- createStudy(path, antares_version = "9.3")
  )
  
  backupStudy()
  backupStudy(backupfile = "titi")
  backupStudy(extension = ".tar.gz")
  
  
  deleteStudy()
})