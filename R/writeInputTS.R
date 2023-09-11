#' @title Write input time series
#'
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' This function writes input time series in an Antares project.
#'
#' @param data A 8760*N matrix of hourly time series, except when `type` is
#'  `"hydroSTOR"`.
#'  In this latter case `"hydroSTOR"` data must have either be 365 rows
#'  (Antares v7) or 12 rows (v6 and earlier).
#'  
#'  
#' @param type Serie to write: `"load"`, `"hydroROR"`, `"hydroSTOR"`,
#'  `"wind"`, `"solar"`, `"tsLink"` or `"mingen"`.
#'  
#'  If type == `"mingen"`, `"antaresVersion"` should be >= 860.
#'  Refers to note section below.
#'  
#' @param area The area where to write the input time series.
#' @param link Link for which writing transmission capacities time series, 
#'  must like `"area01%area02"` or `c("area01", "area02")`.
#' @param overwrite Logical. Overwrite the values if a file already exists.
#' 
#' 
#' @note
#' For an **Antares version >= 860**, the `mingen.txt` file is created.
#' 
#' The `mingen.txt` file can be created under two conditions:   
#'   - The number of columns must be equal to either `1` or the number in `mod.txt`  
#'   - If the `mod.txt` file is empty or has one column, then there is no dimension constraint
#'   
#' @template opts
#'
#' @export
#' 
#' @section Warning:
#'
#' You cannot use `area` and `link` arguments at the same time.
#'
#' For an **Antares version >= 860**, control of data consistency between `mingen.txt` and `mod.txt` can be executed.
#'
#' These controls depend on the values you find in `hydro.ini` file.
#'
#' @importFrom antaresRead simOptions
#' @importFrom assertthat assert_that
#' @importFrom data.table fwrite
#'
#' @examples
#' \dontrun{
#'
#' # Write solar time series
#' writeInputTS(
#'   area = "fictive_area",
#'   type = "solar",
#'   data = matrix(rep(4, 8760*2), nrow = 8760)
#' )
#'
#' }
writeInputTS <- function(data, 
                         type = c("load", "hydroROR", "hydroSTOR", "mingen", "wind", "solar", "tsLink"),
                         area = NULL, 
                         link = NULL,
                         overwrite = TRUE, 
                         opts = antaresRead::simOptions()) {
  
  type <- match.arg(type)
  # No control on area possible for area with type = "tsLink"
  if (type != "tsLink") {
    check_area_name(area, opts)
  }
  
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  #Check for version. 'mingen' data can be writed only for antaresVersion >= 860.
  if (type == "mingen" & (opts$antaresVersion < 860 )){
    stop("antaresVersion should be >= v8.6.0 to write mingen 'data'.", call. = FALSE)
  }
  
  # Data validation
  if (!is.null(area) & !is.null(link)) {
    stop("Cannot use area and link simultaneously.")
  }
  
  if (type %in% c("load", "hydroROR", "wind", "solar", "mingen")) {
    if (NROW(data) != 8760)
      stop("'data' must be a 8760*N matrix.", call. = FALSE)
    
  } else if(type %in% "hydroSTOR") {
    if (is_antares_v7(opts)) {
      if (NROW(data) != 365)
        stop("'data' must be a 365*N matrix.", call. = FALSE)
    } else {
      if (NROW(data) != 12)
        stop("'data' must be a 12*N matrix.", call. = FALSE)
    }
    
    # v860
      # "mod.txt" dimension depends on file "mingen.txt". 
      # The file can be created only in version >= 8.6.0. 
      # We do not need to put version condition here.
    if(opts$antaresVersion >= 860){
      path_mingen_file <- file.path(opts$inputPath,
                                    "hydro","series",area,"mingen.txt")
      
      if (file.exists(path_mingen_file)){
        #read the mingen.txt data table
        mingen_data <- antaresRead:::fread_antares(opts = opts,
                                                   file = path_mingen_file)
        
        #initialize the number of columns to the data input
        dim_column <- dim(data)[2]
        
        #If mingen.txt has more than 1 column, mod must have either 1 or same width than mingen.txt 
        if (dim(mingen_data)[2] > 1) {
          dim_column = c(1, dim(mingen_data)[2])
          
          #If the dimensions does not match, we alert
          if (!(dim(data)[2] %in% dim_column)){
            warning("mod 'data' must be either a ", NROW(data),"*1 or ",
                    NROW(data), "*", dim(mingen_data)[2],
                    " matrix. You should adapt the format of either mingen or mod to match the number of columns",
                    call. = FALSE)
            
          }
        }
      }
      
    }
    
    
  }
  
  # v860 - mingen dimension depends on file "mod.txt"
  if (type == "mingen"){
    path_mod_file <- file.path(opts$inputPath,
                                  "hydro","series",area,"mod.txt")
    
    #read the mod.txt data table
    mod_data <- antaresRead:::fread_antares(opts = opts,
                                            file = path_mod_file)
    
    #initialize the number of columns to the data input
    dim_column <- dim(data)[2]
    
    #If mod.txt has more than 1 column, mingen must have either 1 or same width than mod.txt 
    if (dim(mod_data)[2] > 1) {
      dim_column <- c(1, dim(mod_data)[2])
      
      #If the dimensions does not match, we stop
      if (!(dim(data)[2] %in% dim_column)){
        stop(paste0("mingen 'data' must be either a 8760*1 or 8760*", dim(mod_data)[2], " matrix."), 
             call. = FALSE)
        
      } else {
        #The data number of columns is correct
        dim_column <- dim(data)[2]
      }
    }
  }
  
  # tsLink block (file & API)
  if (!is.null(link)) {
    stopifnot(
      "link must be a character, like 'area01%area02' or c('area01', 'area02')" = is.character(link)
    )
    if (length(link) == 1)
      link <- strsplit(x = link, split = "%")[[1]]
    
    stopifnot(
      "Invalid link specification, must be 'area01%area02' or c('area01', 'area02')" = length(link) == 2
    )
    
    from <- tolower(as.character(link[1]))
    to <- tolower(as.character(link[2]))
    
    check_area_name(from, opts)
    check_area_name(to, opts)
    
    if (!is_api_study(opts)) {
      inputPath <- opts$inputPath
      tsLink_file <- file.path(inputPath, "links", from, "capacities", paste0(to, "_direct.txt"))
      if (file.exists(tsLink_file) & !overwrite) {
        stop(
          "NTC files already exist for this area. Use overwrite=TRUE if you want to overwrite them.",
          call. = FALSE
        )
      }
    }
    
    opts <- editLink(from = from, to = to, tsLink = data)
    return(invisible(opts))
  }
  
  if (identical(type, "tsLink"))
    stop("type = \"tsLink\" can only be used if link argument is provided")
  
  
  # API block
  if (is_api_study(opts)) {
    
    l_area <- tolower(area)
    
    if (type %in% c("load", "wind", "solar")) {
      target_type <- sprintf("input/%s/series/%s_%s", type, type, l_area)
    } else if (type == "hydroROR") {
      target_type <- sprintf("input/hydro/series/%s/ror", l_area)
    } else if (type == "hydroSTOR") {
      target_type <- sprintf("input/hydro/series/%s/mod", l_area)
    } else if (type == "mingen") {
      target_type <- sprintf("input/hydro/series/%s/mingen", l_area)
    }
    cmd <- api_command_generate(
        action = "replace_matrix",
        target = target_type,
        matrix = as.matrix(data)
    )
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "Writing time-series: {msg_api}"),
      cli_command_registered("replace_matrix")
    )
    
    return(update_api_opts(opts))
  }
  
  # File block
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  if (type %in% c("load", "wind", "solar")) {
    path <- file.path(inputPath, type, "series", paste0(type, "_", tolower(area), ".txt"))
  } else if (type == "hydroROR") {
    path <- file.path(inputPath, "hydro", "series", area, "ror.txt")
  } else if (type == "hydroSTOR") {
    path <- file.path(inputPath, "hydro", "series", area, "mod.txt")
  } else if (type == "mingen") {
    path <- file.path(inputPath, "hydro", "series", area, "mingen.txt")
  }
  
  if (isTRUE(file.size(path) > 0) && !overwrite)
    stop(
      "Time series already exist for this area. Use overwrite=TRUE if you want to overwrite them.",
      call. = FALSE
    )
  
  should_check_mingen_data <- opts$antaresVersion >= 860 & type %in% c("mingen", "hydroSTOR")
  # v860 - save the original data
  if (should_check_mingen_data) {
    filename <- switch(type,
                       "mingen" = "mingen.txt",
                       "hydroSTOR" = "mod.txt"
                      )
    data_ori <- NULL
    if (!is.null(filename)) {
      path_ori_file <- file.path(inputPath, "hydro", "series", area, filename)
      if (file.size(path) > 0) {
        data_ori <- antaresRead:::fread_antares(opts = opts,
                                                file = path_ori_file) 
      }
    }
  }
  
  #Writing or creation if file does not exist.
  fwrite(
    x = as.data.table(data),
    row.names = FALSE, 
    col.names = FALSE, 
    sep = "\t",
    file = path
  )
  
  # v860 - rollback to original data if necessary
  if (should_check_mingen_data) {
    comp_mingen_vs_hydro_storage <- list("check" = TRUE, "msg" = "")
    comp_mingen_vs_maxpower <- list("check" = TRUE, "msg" = "")
    if (type == "mingen") {
      comp_mingen_vs_hydro_storage <- check_mingen_vs_hydro_storage(area, opts)
      comp_mingen_vs_maxpower <- check_mingen_vs_maxpower(area, opts)
    }
    if (type == "hydroSTOR") {
      comp_mingen_vs_hydro_storage <- check_mingen_vs_hydro_storage(area, opts)
    }
    if (!comp_mingen_vs_hydro_storage$check){
      cat(comp_mingen_vs_hydro_storage$msg)
      rollback_to_previous_data(area = area, prev_data = data_ori, rollback_type = type, opts = opts)
    }
    if (!comp_mingen_vs_maxpower$check){
      cat(comp_mingen_vs_maxpower$msg)
      rollback_to_previous_data(area = area, prev_data = data_ori, rollback_type = type, opts = opts)
    }
  }
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
