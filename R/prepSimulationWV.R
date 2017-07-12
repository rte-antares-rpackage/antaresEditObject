#' Prepare inputs to run a simulation for calculate water values
#'
#' @param area The area studied
#' @param fictive_area Name of the fictive area to create
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return nothing yet
#' @export
#' 
#' @importFrom antaresRead simOptions
#' @importFrom utils read.table write.table
#'
#' @examples
#' \dontrun{
#' prepSimulationWV("fr")
#' }
prepSimulationWV <- function(area, fictive_area = paste0("WaterValue_", area), opts = antaresRead::simOptions()) {

  if (!area %in% opts$areaList)
    stop(paste(area, "is not a valid area"))

  # Input path
  inputPath <- opts$inputPath


  # Create fictive area ----
  initializeArea(name = fictive_area, overwrite = TRUE, opts = opts)


  # Hydro storage ----
  path_hydro_storage <- file.path(inputPath, "hydro", "series", area, "mod.txt")

  if (file.exists(path_hydro_storage)) {

    # file's copy
    res_copy <- file.copy(
      from = path_hydro_storage,
      to = file.path(inputPath, "hydro", "series", area, "mod_backup.txt")
    )
    if (!res_copy)
      stop("Impossible to backup hydro storage file")

    # read hydro storage series and initialize at 0
    hydro_storage <- read.table(file = path_hydro_storage)
    hydro_storage[] <- 0
    utils::write.table(x = hydro_storage, file = path_hydro_storage, row.names = FALSE, col.names = FALSE)

  } else {

    stop("No hydro storage series for this area")

  }


  # Create thermal cluster ----
  hydro_storage_max <- utils::read.table(
    file = file.path(inputPath, "hydro", "common", "capacity", paste0("maxpower_", area, ".txt"))
  )
  createCluster(
    area = area, cluster_name = fictive_area,
    group = "other",
    unitcount = 1,
    nominalcapacity = max(hydro_storage_max),
    `min-down-time` = 0,
    `marginal-cost` = 0.010000,
    `market-bid-cost` = 0.010000,
    opts = opts
  )

  # Create serie
  # daily => hourly
  hydro_storage_avg_hourly <- hydro_storage_max[rep(seq_len(365), each = 24), 2, drop = FALSE]
  utils::write.table(
    x = hydro_storage_avg_hourly, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "thermal", "series", area, fictive_area, "series.txt")
  )

  
  # Create link ----
  
  dagtaLink <- matrix(
    data = c(rep(0, 8760), rep(max(hydro_storage_max), 8760), rep(0, 8760*3)),
    ncol = 5
  )
  
  createLink(
    from = area, 
    to = fictive_area, 
    propertiesLink = propertiesLink(hurdles_cost = FALSE, transmission_capacities = "enabled"), 
    dataLink = dataLink,
    opts = opts
  )
}

