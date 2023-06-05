#' @title Write Hydro Values
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Write waterValues, reservoirLevels, maxpower, inflowPattern, creditModulations and mingen data for a given area.
#'
#' @param area The area where to add the values.
#' @param type Type of hydro file, it can be "waterValues", "reservoir", "maxpower", "inflowPattern" or "creditmodulations".
#' @param data The data must have specific dimension depending on the type of file :
#' \itemize{
#'   \item{waterValues}{: a 365x101 numeric matrix:
#'   marginal values for the stored energy based on date (365 days)
#'   and on the reservoir level (101 round percentage values ranging from
#'   0% to 100%). OR a 3-column matrix with 365x101 rows. In this latter case the 3 columns must
#'   be 'date', 'level' and 'value' (in this order), and the rows must be sorted by:
#'   ascending day, ascending level.}
#'   \item{reservoir}{: a 365x3 numeric matrix. The columns contains respectively the levels min, avg and max.}
#'   \item{maxpower}{: a 365x4 numeric matrix.}
#'   \item{inflowPattern}{: a 365x1 numeric matrix.}
#'   \item{creditmodulations}{: a 2x101 numeric matrix.}
#'   }
#'   
#' @param overwrite Logical. Overwrite the values if a file already exists.
#' @param opts List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()].
#'
#' @export
#'
#' @importFrom antaresRead simOptions
#' @importFrom assertthat assert_that
#' @importFrom data.table dcast data.table fwrite
#'
#' @examples
#' \dontrun{
#'
#' writeHydroValues("fictive_area", type = "inflowPattern", data = matrix(rep(0, 365*1), nrow = 365))
#'
#' }
writeHydroValues <- function(area,
                             type,
                             data, 
                             overwrite = TRUE,
                             opts = antaresRead::simOptions()) {
  
  check_area_name(area, opts)
  
  type <- match.arg(type, c("waterValues", "reservoir", "maxpower", "inflowPattern", "creditmodulations","mingen"))
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  #Check for version
  if (type == "mingen" && opts$antaresVersion < 860 ){
    stop("antaresVersion should be >= v8.6.0 to write mingen 'data'.", call. = FALSE)
  }
  
  #mingen dimension depends on file "mod.txt"
  if (type == "mingen"){
    #read the mod.txt data table
    mod_data <- suppressWarnings(antaresRead:::fread_antares(opts = opts, file = file.path(opts$studyPath,"input","hydro","series",area,"mod.txt")))
    
    #initialize the number of columns to the data input
    dim_column = dim(data)[2]
    
    #If mod.txt has more than 1 column, mingen must have either 1 or same width than mod.txt 
    if (dim(mod_data)[2] > 1) {
      dim_column = c(1, dim(mod_data)[2])
      
      #If the dimensions does not match, we stop
      if (!(dim(data)[2] %in% dim_column)){
        stop("mingen 'data' must be either a 8760*1 or 8760*", dim(mod_data)[2], " matrix.", call. = FALSE)
    
      } else {
        #The data number of columns is correct
        dim_column = dim(data)[2]
      }
    }
  }
  dims <- switch(type,
                 "reservoir" = c(365L, 3L),
                 "maxpower" = c(365L, 4L),
                 "inflowPattern" = c(365L, 1L),
                 "creditmodulations" = c(2L, 101L),
                 "mingen" = c(8760L,dim_column))
  
  #Case waterValues    
  if (type=="waterValues"){
    if (!(identical(dim(data), c(365L, 101L)) || identical(dim(data), c(36865L, 3L))))
      stop("waterValues 'data' must be either a 365*101 or (365*101)*3 matrix.", call. = FALSE)
    
    if (ncol(data) == 3) {
      data <- data.table::data.table(data)
      names(data) <- c("date", "level", "value")
      data <- data.table::dcast(data, formula = date ~ level, value.var = "value")
      data$date <- NULL
    }
  } else {
    #Other cases
    if (!(identical(dim(data), dims)))
      stop(type, " 'data' must be a ", 
           do.call(paste, as.list(c(dims, sep = "*"))), " matrix.", call. = FALSE)
  }
  
  # API block
  if (is_api_study(opts)) {
    cmd <- api_command_generate(
      action = "replace_matrix",
      target = sprintf("input/hydro/common/capacity/%s_%s", type, area),
      matrix = data
    )
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = sprintf("Write %s values: {msg_api}", type)),
      cli_command_registered("replace_matrix")
    )
    
    return(invisible(opts))
  }  
  
  # Input path
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  values_file <- file.path(inputPath, "hydro", "common", "capacity", paste0(type, "_", tolower(area), ".txt"))
  
  if (isTRUE(file.size(values_file) > 0) && !overwrite)
    stop(type," Data already exist for this area. Use overwrite=TRUE if you want to overwrite them.",
         call. = FALSE)
  
  fwrite(x = data, row.names = FALSE, col.names = FALSE, sep = "\t", file = values_file)
}
