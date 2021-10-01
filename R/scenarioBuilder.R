

#' Read, create & update scenario builder
#'
#' @param n_scenario Number of scenario.
#' @param n_mc Number of Monte-Carlo years.
#' @param areas Areas to use in scenario builder, if `NULL` (default), areas in Antares study are used.
#' @param areas_rand Areas for which to use `"rand"`.
#' @param opts
#'   List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()]
#'
#' @return `scenarioBuilder` : a `matrix`
#' @export
#' 
#' @importFrom antaresRead getAreas simOptions
#' 
#' @name scenario-builder
#'
#' @examples
#' \dontrun{
#' 
#' library(antaresRead)
#' library(antaresEditObject)
#' 
#' # simulation path
#' setSimulationPath(
#'   path = "pat/to/simulation",
#'   simulation = "input"
#' )
#' 
#' # Create a scenario builder matrix
#' sbuilder <- scenarioBuilder(
#'   n_scenario = 51,
#'   n_mc = 2040,
#'   areas_rand = c("fr", "be")
#' )
#' sbuilder[, 1:6]
#' dim(sbuilder)
#' 
#' 
#' # Read previous scenario builder
#' # in a matrix format
#' prev_sb <- readScenarioBuilder()
#' 
#' 
#' # Update scenario builder
#' 
#' # for load serie
#' updateScenarioBuilder(ldata = sbuilder, series = "load")
#' 
#' # equivalent as
#' updateScenarioBuilder(ldata = list(l = sbuilder))
#' 
#' 
#' # update several series
#' 
#' # same input
#' sbuilder
#' updateScenarioBuilder(
#'   ldata = sbuilder, 
#'   series = c("load", "hydro", "solar")
#' )
#' 
#' # different input
#' updateScenarioBuilder(ldata = list(
#'   l = load_sb,
#'   h = hydro_sb,
#'   s = solar_sb
#' ))
#' 
#' }
scenarioBuilder <- function(n_scenario, 
                            n_mc = NULL,
                            areas = NULL,
                            areas_rand = NULL,
                            opts = antaresRead::simOptions()) {
  if (is.null(areas)) {
    areas <- antaresRead::getAreas(opts = opts)
  } else {
    areas <- unique(c(areas, areas_rand))
  }
  if (!all(areas_rand %in% areas)) {
    warning("Some 'areas_rand' are not Antares' areas", call. = FALSE)
  }
  if (is.null(n_mc)) {
    n_mc <- opts$parameters$general$nbyears
  } else {
    if (n_mc != opts$parameters$general$nbyears) {
      warning("Specified number of Monte-Carlo years differ from the one in Antares general parameter", call. = FALSE)
    }
  }
  sb <- matrix(
    data = rep(seq_len(n_scenario), length(areas) * ceiling(n_mc/n_scenario)),
    byrow = TRUE, 
    nrow = length(areas),
    dimnames = list(areas, NULL)
  )
  sb[areas %in% areas_rand, ] <- apply(
    X = sb[areas %in% areas_rand, , drop = FALSE],
    MARGIN = 1,
    FUN = function(x) "rand"
  )
  return(sb)
}




#' @param ruleset Ruleset to read.
#' @param as_matrix If `TRUE` (default) return a `matrix`, else a `list`.
#'
#' @return `readScenarioBuilder` : a `list` of `matrix` or `list` according to `as_matrix` parameters.
#' @export
#' 
#' @rdname scenario-builder
readScenarioBuilder <- function(ruleset = "Default Ruleset",
                                as_matrix = TRUE,
                                opts = antaresRead::simOptions()) {
  pathSB <- file.path(opts$studyPath, "settings", "scenariobuilder.dat")
  sb <- readIniFile(file = pathSB)
  if (!ruleset %in% names(sb)) {
    ruleset1 <- names(sb)[1]
    warning(sprintf("Ruleset '%s' not found, returning: '%s'", ruleset, ruleset1), call. = FALSE)
    ruleset <- ruleset1
  }
  sb <- sb[[ruleset]]
  if (is.null(sb))
    return(list())
  extract_el <- function(l, indice) {
    res <- strsplit(x = names(l), split = ",")
    res <- lapply(res, `[`, i = indice)
    unlist(res)
  }
  types <- extract_el(sb, 1)
  sbt <- split(x = sb, f = types)
  if (!is_active_RES(opts))
    sbt$r <- NULL
  lapply(
    X = sbt,
    FUN = function(x) {
      areas <- unique(extract_el(x, 2))
      years <- unique(extract_el(x, 3))
      if (as_matrix) {
        # x <- unlist(x)
        if(length(x) < length(areas)*length(years)){
          x <- rep(x, length(years))
        }
        matrix(
          data = x[1:(length(areas) * length(years))], byrow = TRUE,
          nrow = length(areas),
          ncol = length(years),
          dimnames = list(areas, NULL)
        )
      } else {
        x
      }
    }
  )
}


#' @param ldata A `matrix` obtained with `scenarioBuilder`, 
#'  or a named list of matrices obtained with `scenarioBuilder`, names must be 
#'  'l', 'h', 'w', 's', 't' or 'r', depending on the series to update.
#' @param series Name(s) of the serie(s) to update if `ldata` is a single `matrix`.
#'
#' @export
#' 
#' @rdname scenario-builder
updateScenarioBuilder <- function(ldata, 
                                  ruleset = "Default Ruleset", 
                                  series = NULL,
                                  opts = antaresRead::simOptions()) {
  prevSB <- readScenarioBuilder(ruleset = ruleset, as_matrix = FALSE, opts = opts)
  if (!is.list(ldata)) {
    if (!is.null(series)) {
      series <- match.arg(
        arg = series,
        choices = c("load", "hydro", "wind", "solar", "thermal", "renewables"),
        several.ok = TRUE
      )
      series <- substr(series, 1, 1)
    } else {
      stop("If 'ldata' isn't a named list, you must specify which serie(s) to use!", call. = FALSE)
    }
    sbuild <- lapply(
      X = series,
      FUN = listify_sb,
      mat = ldata,
      opts = opts
    )
    prevSB[series] <- NULL
  } else {
    series <- names(ldata)
    if (!all(series %in% c("l", "h", "w", "s", "t", "r"))) {
      stop("'ldata' must be 'l', 'h', 'w', 's', 't' or 'r'", call. = FALSE)
    }
    sbuild <- lapply(
      X = series,
      FUN = function(x) {
        listify_sb(ldata[[x]], x, opts = opts)
      }
    )
    prevSB[series] <- NULL
  }
  names(prevSB) <- NULL
  res <- unlist(c(sbuild, prevSB))
  res <- list(as.list(res))
  names(res) <- ruleset
  
  pathSB <- file.path(opts$studyPath, "settings", "scenariobuilder.dat")
  writeIni(listData = res, pathIni = pathSB, overwrite = TRUE)
  if (interactive())
    cat("\u2713", "Scenario Builder updated\n")
  return(invisible(res))
} 

#' Converts a scenarioBuilder matrix to a list
#' 
#' @param mat A matrix obtained from scenarioBuilder().
#' @param series Name of the series, among 'l', 'h', 'w', 's', 't' and 'r'.
#' @param opts Simulation options.
#'
#' @importFrom data.table as.data.table melt := .SD
#' @importFrom antaresRead readClusterDesc
#' @importFrom utils packageVersion getFromNamespace
#' @noRd
listify_sb <- function(mat, series = "l", opts = antaresRead::simOptions()) {
  dtsb <- as.data.table(mat, keep.rownames = TRUE)
  dtsb <- melt(data = dtsb, id.vars = "rn")
  dtsb[, variable := as.numeric(gsub("V", "", variable)) - 1]
  dtsb <- dtsb[value != "rand"]
  dtsb[, value := as.integer(value)]
  
  if (identical(series, "t")) {
    cluster_desc <- readClusterDesc(opts = opts)
    dtsb <- merge(
      x = dtsb, 
      y = cluster_desc[, .SD, .SDcols = c("area", "cluster")],
      by.x = "rn",
      by.y = "area", 
      allow.cartesian = TRUE
    )
  }
  if (identical(series, "r")) {
    check_active_RES(opts)
    if (packageVersion("antaresRead") < "2.2.8")
      stop("You need to install a more recent version of antaresRead (>2.2.8)", call. = FALSE)
    if (!exists("readClusterResDesc", where = "package:antaresRead", mode = "function"))
      stop("You need to install a more recent version of antaresRead (>2.2.8)", call. = FALSE)
    read_cluster_res_desc <- getFromNamespace("readClusterResDesc", ns = "antaresRead")
    cluster_desc <- read_cluster_res_desc(opts = opts)
    dtsb <- merge(
      x = dtsb, 
      y = cluster_desc[, .SD, .SDcols = c("area", "cluster")],
      by.x = "rn",
      by.y = "area", 
      allow.cartesian = TRUE
    )
  }
  
  dtsb <- dtsb[order(rn, variable)]
  
  lsb <- as.list(dtsb$value)
  if (series %in% c("r", "t")) {
    names(lsb) <- paste(series, dtsb$rn, dtsb$variable, dtsb$cluster, sep = ",")
  } else{
    names(lsb) <- paste(series, dtsb$rn, dtsb$variable, sep = ",")
  }
  
  return(lsb)
} 


