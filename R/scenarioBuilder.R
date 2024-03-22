#' @title Read, create, update & deduplicate scenario builder
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Read, create, update & deduplicate scenario builder.
#'
#' @param n_scenario Number of scenario.
#' @param n_mc Number of Monte-Carlo years.
#' @param areas Areas to use in scenario builder, if `NULL` (default) all areas in Antares study are used.
#' @param areas_rand Areas for which to use `"rand"`.
#' @param group_bc `character` Bindgind constraints's groups names to use.
#' @param group_bc_rand `character` Bindgind constraints which to use `"rand"`.
#' @param mode `character` "bc" to edit binding constraints.
#' @param coef_hydro_levels Hydro levels coefficients.
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
#' # Create a scenario builder matrix for hydro levels (use case 1)
#' sbuilder <- scenarioBuilder(
#'   n_mc = opts$parameters$general$nbyears,
#'   areas = c("fr", "be"),
#'   coef_hydro_levels = c(0.1, 0.9)
#' )
#'
#' # Create a scenario builder matrix for hydro levels (use case 2)
#' sbuilder <- scenarioBuilder(
#'   n_mc = opts$parameters$general$nbyears,
#'   areas = c("fr", "be"),
#'   coef_hydro_levels = c(runif(opts$parameters$general$nbyears)
#'   , runif(opts$parameters$general$nbyears)
#'   )
#' )
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
#' # Deduplicate scenario builder
#' 
#' deduplicateScenarioBuilder()
#' }
scenarioBuilder <- function(n_scenario = NULL, 
                            n_mc = NULL,
                            areas = NULL,
                            areas_rand = NULL,
                            group_bc = NULL, 
                            group_bc_rand = NULL,
                            coef_hydro_levels = NULL,
                            mode = NULL,
                            opts = antaresRead::simOptions()) {
  
  
  if (is_api_study(opts) && is_api_mocked(opts)) {
    stopifnot("In mocked API mode, n_mc cannot be NULL" = !is.null(n_mc))
    stopifnot("In mocked API mode, areas cannot be NULL" = !is.null(areas))
  }
  
  # check version >=870 from group parameter
  if(!opts$antaresVersion >= 870 & !is.null(group_bc))
    stop("Parameter 'group_bc' is only for Antares study version >= v8.7.0", 
         call. = FALSE)
  if(!opts$antaresVersion >= 870 & !is.null(group_bc_rand))
    stop("Parameter 'group_bc_rand' is only for Antares study version >= v8.7.0", 
         call. = FALSE)
  
  # >=v870
  if(opts$antaresVersion >= 870){
    # update with bc
    if(mode %in% "bc"){
      # read groups
      if(is.null(group_bc)){
        group_bc <- readBindingConstraints(opts = opts)
        group_bc <- sapply(group_bc, function(x){
          x$properties$group
        })
      }
      else 
        group_bc <- unique(c(group_bc, group_bc_rand))
      
      # check parameters
      if (!all(group_bc_rand %in% group_bc)) 
        warning("Some 'group_bc_rand' are not Antares 'group_bc'", call. = FALSE)
      
      # n_mc parameter
      if (is.null(n_mc)) 
        n_mc <- opts$parameters$general$nbyears
      else 
        if (isTRUE(n_mc != opts$parameters$general$nbyears)) 
          warning("Specified number of Monte-Carlo years differ from the one in Antares general parameter", call. = FALSE)
    }
    
    # write data
    data_mat <- rep(rep_len(seq_len(n_scenario), 
                        n_mc),
                    length(group_bc))
    
    sb <- matrix(
      data = data_mat,
      byrow = TRUE, 
      nrow = length(group_bc),
      dimnames = list(group_bc, NULL)
    )
    sb[group_bc %in% group_bc_rand, ] <- "rand"
    
    return(sb)
  }
  else # without bc 
    .manage_parameter(n_scenario = n_scenario,
                      n_mc = n_mc,
                      areas = areas,
                      areas_rand = areas_rand,
                      coef_hydro_levels = coef_hydro_levels,
                      opts = opts)
  
  # if (is.null(areas)) {
  #   areas <- antaresRead::getAreas(opts = opts)
  # } else {
  #   areas <- unique(c(areas, areas_rand))
  # }
  # if (!all(areas_rand %in% areas)) {
  #   warning("Some 'areas_rand' are not Antares' areas", call. = FALSE)
  # }
  # if (is.null(n_mc)) {
  #   n_mc <- opts$parameters$general$nbyears
  # } else {
  #   if (isTRUE(n_mc != opts$parameters$general$nbyears)) {
  #     warning("Specified number of Monte-Carlo years differ from the one in Antares general parameter", call. = FALSE)
  #   }
  # }
  # 
  # if (!is.null(coef_hydro_levels)) {
  #   nb_areas <- length(areas)
  #   nb_coef_hydro_levels <- length(coef_hydro_levels)
  #   if (nb_coef_hydro_levels == nb_areas) {
  #     data_mat <- rep(coef_hydro_levels, each = n_mc)
  #   } else if(nb_coef_hydro_levels == nb_areas * n_mc) {
  #     data_mat <- coef_hydro_levels
  #   } else {
  #     stop("Please check the number of areas and the number of coefficients for hydro levels that you provided.")
  #   }
  # } else {
  #   data_mat <- rep_len(seq_len(n_scenario), length(areas) * n_mc)
  # }
  # 
  # sb <- matrix(
  #   data = data_mat,
  #   byrow = TRUE, 
  #   nrow = length(areas),
  #   dimnames = list(areas, NULL)
  # )
  # sb[areas %in% areas_rand, ] <- "rand"
  # 
  # return(sb)
}

.manage_parameter <- function(..., opts){
  args <- list(...)
  if (is.null(args$areas)) {
    args$areas <- antaresRead::getAreas(opts = opts)
  } else {
    args$areas <- unique(c(args$areas, args$areas_rand))
  }
  if (!all(args$areas_rand %in% args$areas)) {
    warning("Some 'areas_rand' are not Antares' areas", call. = FALSE)
  }
  if (is.null(args$n_mc)) {
    args$n_mc <- opts$parameters$general$nbyears
  } else {
    if (isTRUE(args$n_mc != opts$parameters$general$nbyears)) {
      warning("Specified number of Monte-Carlo years differ from the one in Antares general parameter", call. = FALSE)
    }
  }
  
  if (!is.null(args$coef_hydro_levels)) {
    nb_areas <- length(args$areas)
    nb_coef_hydro_levels <- length(args$coef_hydro_levels)
    if (nb_coef_hydro_levels == nb_areas) {
      data_mat <- rep(args$coef_hydro_levels, each = args$n_mc)
    } else if(nb_coef_hydro_levels == nb_areas * args$n_mc) {
      data_mat <- args$coef_hydro_levels
    } else {
      stop("Please check the number of areas and the number of coefficients for hydro levels that you provided.")
    }
  } else {
    data_mat <- rep_len(seq_len(args$n_scenario), 
                        length(args$areas) * args$n_mc)
  }
  
  sb <- matrix(
    data = data_mat,
    byrow = TRUE, 
    nrow = length(args$areas),
    dimnames = list(args$areas, NULL)
  )
  sb[args$areas %in% args$areas_rand, ] <- "rand"
  
  return(sb)
}


#' @title Create the correspondence data frame between the symbol and the type in scenario builder
#' @return a `data.frame`.
create_referential_series_type <- function(){

  ref_series <- data.frame("series" = c("l", "h", "w", "s", "t", "r", "ntc", "hl", "bc"),
                           "choices" = c("load", "hydro", "wind", "solar", "thermal", "renewables", "ntc", "hydrolevels", "binding")
                         )

  return(ref_series)
}


#' @param ruleset Ruleset to read.
#' @param as_matrix If `TRUE` (default) return a `matrix`, else a `list`.
#'
#' @return `readScenarioBuilder` : a `list` of `matrix` or `list` according to `as_matrix` parameters.
#' @export
#' 
#' @rdname scenario-builder
#' 
#' @importFrom data.table data.table CJ dcast
#' @importFrom antaresRead readClusterDesc getAreas 
readScenarioBuilder <- function(ruleset = "Default Ruleset",
                                as_matrix = TRUE,
                                opts = antaresRead::simOptions()) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  # read existing scenariobuilder.dat
  if (is_api_study(opts)) {
    if (is_api_mocked(opts)) {
      sb <- list("Default Ruleset" = NULL)
    } else {
      sb <- readIni("settings/scenariobuilder", 
                    opts = opts, default_ext = ".dat")
    }
  } else {
    sb <- readIni("settings/scenariobuilder", 
                  opts = opts, default_ext = ".dat")
  }
  
  # check structure in top of file scenariobuilder.dat
  if (!ruleset %in% names(sb)) {
    warning(sprintf("Ruleset '%s' not found, possible values are: %s", 
                    ruleset, paste(names(sb), collapse = ", ")), 
            call. = FALSE)
    sb <- NULL
  } else {
    sb <- sb[[ruleset]]
  }
  if (is.null(sb))
    return(list())
 
  types <- extract_el(sb, 1)
  sbt <- split(x = sb, f = types)
  if (is_active_RES(opts)) {
    sbt$w <- NULL
    sbt$s <- NULL
  } else {
    sbt$r <- NULL
  }
  lapply(
    X = sbt,
    FUN = function(x) {
      type <- extract_el(x, 1)[1]
      areas <- extract_el(x, 2)
      if (type %in% c("t", "r")) {
        clusters <- extract_el(x, 4)
        areas <- paste(areas, clusters, sep = "_")
        # all_areas <- areas # for the moment
        if (type == "t") {
          clusdesc <- readClusterDesc(opts = opts)
        } else {
          if (packageVersion("antaresRead") < "2.2.8")
            stop("You need to install a more recent version of antaresRead (>2.2.8)", call. = FALSE)
          if (!exists("readClusterResDesc", where = "package:antaresRead", mode = "function"))
            stop("You need to install a more recent version of antaresRead (>2.2.8)", call. = FALSE)
          read_cluster_res_desc <- getFromNamespace("readClusterResDesc", ns = "antaresRead")
          clusdesc <- read_cluster_res_desc(opts = opts)
        }
        all_areas <- paste(clusdesc$area, clusdesc$cluster, sep = "_")
      } else {
        all_areas <- getAreas(opts = opts)
      }
      if (type %in% c("ntc")) {
        areas2 <- extract_el(x, 3)
        areas <- paste(areas, areas2, sep = "%")
        years <- extract_el(x, 4)
      } else {
        years <- extract_el(x, 3)
      }
      
      if (as_matrix) {
        SB <- data.table(
          areas = areas,
          years = as.numeric(years) + 1,
          values = unlist(x, use.names = FALSE)
        )
        if (!type %in% c("ntc")) {
          SB <- SB[CJ(areas = all_areas, years = seq_len(opts$parameters$general$nbyears)), on = c("areas", "years")]
        }
        SB <- dcast(data = SB, formula = areas ~ years, value.var = "values")
        mat <- as.matrix(SB, rownames = 1)
        colnames(mat) <- NULL
        mat
      } else {
        x
      }
    }
  )
}

extract_el <- function(l, indice) {
  res <- strsplit(x = names(l), split = ",")
  res <- lapply(res, `[`, i = indice)
  unlist(res)
}


#' @param ldata A `matrix` obtained with `scenarioBuilder`, 
#'  or a named list of matrices obtained with `scenarioBuilder`, names must be 
#'  'l', 'h', 'w', 's', 't', 'r', 'ntc', 'hl' or 'bc', depending on the series to update.
#' @param series Name(s) of the serie(s) to update if `ldata` is a single `matrix`.
#' @param clusters_areas A `data.table` with two columns `area` and `cluster`
#'  to identify area/cluster couple to update for thermal or renewable series.
#'  Default is to read clusters description and update all couples area/cluster.
#' @param links Links to use if series is `"ntc"`.
#'  Either a simple vector with links described as `"area01%area02` or a `data.table` with two columns `from` and `to`.
#'  Default is to read existing links and update them all.
#'  
#'  
#' @note
#' `series = "hl"` each value must be between 0 and 1.
#'   
#' `series = "ntc"` is only available with Antares >= 8.2.0.  
#' 
#' `series = "bc"` is only available with Antares >= 8.7.0. 
#'
#' @export
#' 
#' @rdname scenario-builder
updateScenarioBuilder <- function(ldata, 
                                  ruleset = "Default Ruleset", 
                                  series = NULL,
                                  clusters_areas = NULL,
                                  links = NULL,
                                  opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  suppressWarnings(
    prevSB <- readScenarioBuilder(ruleset = ruleset, 
                                  as_matrix = FALSE, 
                                  opts = opts))
  
  ref_series <- create_referential_series_type()
  possible_series <- ref_series$series
  
  if (!is.list(ldata)) {
    if (!is.null(series)) {
      series <- ref_series[possible_series %in% series, "choices"]
      if (isTRUE("ntc" %in% series) & isTRUE(opts$antaresVersion < 820))
        stop("updateScenarioBuilder: cannot use series='ntc' with Antares < 8.2.0", call. = FALSE)
      series <- ref_series[ref_series$choices %in% series, "series"]
    } else {
      stop("If 'ldata' isn't a named list, you must specify which serie(s) to use!", call. = FALSE)
    }
    sbuild <- lapply(
      X = series,
      FUN = listify_sb,
      mat = ldata,
      clusters_areas = clusters_areas, 
      links = links,
      opts = opts
    )
    prevSB[series] <- NULL
  } else {
    series <- names(ldata)
    if (!all(series %in% possible_series)) {
      stop("'ldata' must be one of ", paste0(possible_series, collapse = ", "), call. = FALSE)
    }
    if (isTRUE("ntc" %in% series) & isTRUE(opts$antaresVersion < 820))
      stop("updateScenarioBuilder: cannot use series='ntc' with Antares < 8.2.0", call. = FALSE)
    sbuild <- lapply(
      X = series,
      FUN = function(x) {
        listify_sb(
          mat = ldata[[x]], 
          series = x,
          opts = opts, 
          clusters_areas = clusters_areas, 
          links = links
        )
      }
    )
    prevSB[series] <- NULL
  }
  names(prevSB) <- NULL
  res <- unlist(c(sbuild, prevSB))
  res <- list(as.list(res))
  names(res) <- ruleset
  
  if (is_api_study(opts)) {
    cmd <- api_command_generate(
      action = "update_config",
      target = paste0("settings/scenariobuilder/", ruleset),
      data = res[[1]]
    )
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "{.emph update_config (scenariobuilder)}: {msg_api}"),
      cli_command_registered("update_config")
    )
    
    return(update_api_opts(opts))
  } else {
    pathSB <- file.path(opts$studyPath, "settings", "scenariobuilder.dat")
    writeIni(listData = res, pathIni = pathSB, overwrite = TRUE, default_ext = ".dat")
    if (interactive())
      cat("\u2713", "Scenario Builder updated\n")
    return(invisible(res))
  }
} 


#' @export
#' 
#' @rdname scenario-builder
clearScenarioBuilder <- function(ruleset = "Default Ruleset",
                                 opts = antaresRead::simOptions()) {
  if (is_api_study(opts)) {
    cmd <- api_command_generate(
      action = "update_config",
      target = paste0("settings/scenariobuilder/", ruleset),
      data = list()
    )
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "{.emph update_config (clearScenarioBuilder)}: {msg_api}"),
      cli_command_registered("update_config")
    )
    return(update_api_opts(opts))
  } else {
    pathSB <- file.path(opts$studyPath, "settings", "scenariobuilder.dat")
    sb <- readIniFile(file = pathSB)
    if (!isTRUE(ruleset %in% names(sb))) {
      warning("Invalid ruleset provided.")
      return(invisible(FALSE))
    }
    sb[[ruleset]] <- list()
    writeIni(listData = sb, pathIni = pathSB, overwrite = TRUE, default_ext = ".dat")
    if (interactive())
      cat("\u2713", "Scenario Builder cleared\n")
    return(invisible(TRUE))
  }
}



#' Converts a scenarioBuilder matrix to a list
#' 
#' @param mat A matrix obtained from scenarioBuilder().
#' @param series Name of the series, among 'l', 'h', 'w', 's', 't', 'r', 'ntc', 'hl', 'bc'.
#' @param clusters_areas A `data.table` with two columns `area` and `cluster`
#'  to identify area/cluster couple to use for thermal or renewable series.
#' @param links Either a simple vector with links described as `"area01%area02` or a `data.table` with two columns `from` and `to`.
#' @param opts Simulation options.
#'
#' @importFrom data.table as.data.table melt := .SD
#' @importFrom antaresRead readClusterDesc getLinks
#' @importFrom utils packageVersion getFromNamespace
#' @noRd
listify_sb <- function(mat,
                       series = "l", 
                       clusters_areas = NULL, 
                       links = NULL,
                       opts = antaresRead::simOptions()) {
  
  dtsb <- as.data.table(mat, keep.rownames = TRUE)
  dtsb <- melt(data = dtsb, id.vars = "rn")
  dtsb[, variable := as.numeric(gsub("V", "", variable)) - 1]
  dtsb <- dtsb[value != "rand"]
  
  if (identical(series, "hl")) {
    dtsb[, value := as.numeric(value)]
    if(min(dtsb$value) < 0 | max(dtsb$value) > 1) {
      stop("Every coefficient for hydro levels must be between 0 and 1.", call. = FALSE)
    }
  } else {
    dtsb[, value := as.integer(value)]
  }
  
  # Thermal
  if (identical(series, "t")) {
    if (is.null(clusters_areas))
      clusters_areas <- readClusterDesc(opts = opts)
    dtsb <- merge(
      x = dtsb, 
      y = clusters_areas[, .SD, .SDcols = c("area", "cluster")],
      by.x = "rn",
      by.y = "area", 
      allow.cartesian = TRUE
    )
  }
  
  # Renewables
  if (identical(series, "r")) {
    check_active_RES(opts)
    if (packageVersion("antaresRead") < "2.2.8")
      stop("You need to install a more recent version of antaresRead (>2.2.8)", call. = FALSE)
    if (!exists("readClusterResDesc", where = "package:antaresRead", mode = "function"))
      stop("You need to install a more recent version of antaresRead (>2.2.8)", call. = FALSE)
    read_cluster_res_desc <- getFromNamespace("readClusterResDesc", ns = "antaresRead")
    if (is.null(clusters_areas))
      clusters_areas <- read_cluster_res_desc(opts = opts)
    dtsb <- merge(
      x = dtsb, 
      y = clusters_areas[, .SD, .SDcols = c("area", "cluster")],
      by.x = "rn",
      by.y = "area", 
      allow.cartesian = TRUE
    )
  }
  
  # Links
  if (identical(series, "ntc")) {
    if (is.null(links))
      links <- getLinks(namesOnly = FALSE, opts = opts)
    if (is.character(links))
      links <- linksAsDT(links)
    dtsb <- merge(
      x = dtsb, 
      y = links[, .SD, .SDcols = c("from", "to")],
      by.x = "rn",
      by.y = "from"
    )
  }
  
  dtsb <- dtsb[order(rn, variable)]
  
  lsb <- as.list(as.character(dtsb$value))
  if (series %in% c("r", "t")) {
    names(lsb) <- paste(series, dtsb$rn, dtsb$variable, dtsb$cluster, sep = ",")
  } else if (series %in% c("ntc")) {
    names(lsb) <- paste(series, dtsb$rn, dtsb$to, dtsb$variable, sep = ",")
  } else {
    names(lsb) <- paste(series, dtsb$rn, dtsb$variable, sep = ",")
  }
  
  return(lsb)
} 



#' @importFrom data.table as.data.table transpose
#' @importFrom stats setNames
linksAsDT <- function(x) {
  x <- strsplit(x = as.character(x), split = " - |%")
  x <- lapply(x, sort)
  x <- transpose(x)
  x <- setNames(x, c("from", "to"))
  as.data.table(x)
}


#' @title Keep the last element of a named list
#' 
#' @param row of a data frame with 2 columns : key of the scenario builder and its frequency in the scenariobuilder.dat file
#' @param prevldata a named list
#'
#' @noRd
keep_last_element_from_named_list <- function(row, prevldata){
  
  newldata <- list()
  
  key <- as.character(row[1])
  nb_values <- as.numeric(row[2])
  
  prevldata_key <- prevldata[which(names(prevldata) == key)]
  newldata[[key]] <- prevldata_key[[nb_values]]
  
  if(nb_values > 1){
    cat("The following lines will be removed from scenariobuilder.dat\n")
    for(i in seq(1, nb_values-1)){
      cat(key, "=", prevldata_key[[i]], "\n")
    }
  }
  
  return(newldata)
}


#' @title Deduplicate the scenariobuilder.dat file
#' 
#' @param ruleset Ruleset to read.
#' @param opts
#'   List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()]
#'
#' @export
#' 
#' @rdname scenario-builder
deduplicateScenarioBuilder <- function(ruleset = "Default Ruleset", 
                                       opts = antaresRead::simOptions()){
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  prevSB <- readScenarioBuilder(ruleset = ruleset, opts = opts, as_matrix = FALSE)
  lnewSB <- lapply(prevSB, FUN = function(x){
    table_freq <- as.data.frame(table(names(x)))
    newSBkey <- apply(table_freq, MARGIN = 1, FUN = keep_last_element_from_named_list, prevldata = x)
    newSBkey <- do.call("c", newSBkey)
  })
  
  res <- do.call("c", c(lnewSB, use.names = FALSE))
  newSB <- list()
  newSB[[ruleset]] <- res
  pathSB <- file.path(opts$studyPath, "settings", "scenariobuilder.dat")
  writeIni(listData = newSB, pathIni = pathSB, overwrite = TRUE, default_ext = ".dat")
  cat("\u2713", "Scenario Builder deduplicated\n")
}
