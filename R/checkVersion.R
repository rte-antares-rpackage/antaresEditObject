
is_antares_v7 <- function(opts) {
  new_version <- getOption("antares.version.new", default = 700)
  new_version <- as.numeric(new_version)
  isTRUE(opts$antaresVersion >= new_version)
}
