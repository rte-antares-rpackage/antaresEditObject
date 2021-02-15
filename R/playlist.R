#' Get the playlist of an Antares study
#' 
#' \code{getPlaylist} gives the identifier of the MC years which
#' will be simulated in the Antares study, taking into account the potential use of a 
#' playlist which can skip some MC years
#' 
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'   
#' @return 
#' Returns a vector of the identifier of the simulated MC year
#' 
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions
#' @export
#' 
#' 
getPlaylist <- function(opts = antaresRead::simOptions())
{
  
  

  # reload opts
  if(is.null(opts$simPath))
  {
    suppressWarnings({
      opts2 <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
    })
  }
  else
  {
    suppressWarnings({
      opts2 <- antaresRead::setSimulationPath(path = opts$simPath)
    })
  }
  version_study <- substr(opts2$antaresVersion,1,1)
  version_study <- as.numeric(version_study)
  
  
  # get all MC years
  mc_years <- 1:opts2$parameters$general$nbyears
  
  # if no playlist is used, return all mc years
  if(opts2$parameters$general$`user-playlist` == FALSE)
  {
    return(mc_years)
  }
  
  
  # otherwise, update the vector of mc_years by removing disabled years
  playlist_update_type <- names(opts2$parameters$playlist)
  playlist_update_value <- opts2$parameters$playlist
  
  # untouched playlist - no modification have been made 
  if(length(playlist_update_type) == 0)
  {
    return(mc_years)
  }
  
  # modified playlist - take into account the modifications
  assertthat::assert_that(all(playlist_update_type %in% c("playlist_reset", "playlist_year +", "playlist_year -", "playlist_year_weight")))
  activated <- rep(TRUE, length(mc_years))
  
  for(i in 1:length(playlist_update_type))
  {
    # playlist_reset means that we start from a playlist where every MC year is disactivated
    if(playlist_update_type[i] == "playlist_reset")
    {
      activated <- rep(FALSE, length(mc_years))
    }
    # playlist_year + means that the corresponding year is added to the playlist
    if(playlist_update_type[i] == "playlist_year +")
    {
      activated[playlist_update_value[[i]]+1] <- TRUE
    }
    # playlist_year - means that the corresponding year is removed from the playlist
    if(playlist_update_type[i] == "playlist_year -")
    {
      activated[playlist_update_value[[i]]+1] <- FALSE
    } 
  }
  activate_mc <- mc_years[activated]
  
  if(version_study<8){

  return(activate_mc)
  
  }else{
    if(!"playlist_year_weight" %in% playlist_update_type){
      return(activate_mc)
    }else{
      vect_value_weigth = unlist(playlist_update_value[names(playlist_update_value) == "playlist_year_weight"])
      mat_play_list <- data.table(t(cbind.data.frame(strsplit(vect_value_weigth, ","))))
      mat_play_list$V1 <- as.numeric(mat_play_list$V1) + 1
      mat_play_list$V2 <- as.numeric(mat_play_list$V2)
      setnames(mat_play_list, "V1", "mcYears")
      setnames(mat_play_list, "V2", "weights")
      return(list(activate_mc = activate_mc, weights = mat_play_list))
    }
  }
}



#' Set the playlist of an Antares Study
#' 
#' \code{set_playlist} is a function which modifies the input file of an ANTARES
#' study and set the playlist in order to simulate only the MC years given in input
#' 
#' 
#' @param playlist
#'   vector of MC years identifier to be simulated can be a list (V8 compatibility) but not recommended
#' @param weights
#'   data.table, 2 columns : mcYears and weights. Only with after antares V8
#'   
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'   
#' @return 
#' The function does not return anything. It is  used to modify the input of an 
#' Antares study
#' 
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions
#' @export
#' 
setPlaylist <- function(playlist, weights = NULL, opts = antaresRead::simOptions())
{
  
  
  version_study <- substr(opts$antaresVersion,1,1)
  version_study <- as.numeric(version_study)
  
  if(version_study < 8 & !is.null(weights)){
    stop("weights can be use only for antares > V8, please convert your studie before")
  }
  
  
  
  ##For portability (V7, V8)
  if(is.list(playlist)){
    if('activate_mc' %in% names(playlist)){
      playlist <- playlist$activate_mc
    }else{
      stop("List provide must contain activate_mc columns")
    }
  }
  
  # get all MC years
  mc_years <- 1:opts$parameters$general$nbyears
  assertthat::assert_that(all(playlist %in% mc_years))
  playlist <- sort(playlist)
  playlist <- unique(playlist)
  
  # load setting file and check if it exists
  general_parameters_file_name <- paste(opts$studyPath,"/settings/generaldata.ini",sep="")
  assertthat::assert_that(file.exists(general_parameters_file_name))
  assertthat::assert_that(file.info(general_parameters_file_name)$size !=0)
  
  # read file
  param_data <- scan(general_parameters_file_name, what=character(), sep="/", quiet = TRUE)
  
  # find line describing if the playlist is used
  index_p <- grep("user-playlist =",param_data,  fixed = TRUE)
  assertthat::assert_that(length(index_p) == 1)
  
  
  # if all mc_years must be simulated, desactive playlist
  if(length(playlist) == length(mc_years))
  {
    
    if(version_study < 8 | is.null(weights)){

    # update line to disable the playlist
    param_data[index_p] <- paste0("user-playlist = false")
    # write updated file
    write(param_data, general_parameters_file_name, sep = "/")
    }else{
      #Edit playlist
      param_data <- .createNewMc(param_data, weights, playlist, index_p)
      
      # write updated file
      write(param_data, general_parameters_file_name, sep = "/")
    }
    
    
  }
  
  # otherwise, set the playlist
  else
  {
   
    #Edit playlist
    param_data <- .createNewMc(param_data, weights, playlist, index_p)
    
    # write updated file
    write(param_data, general_parameters_file_name, sep = "/")
    
  }
}

#' @noRd
.createNewMc <- function(param_data, weights, playlist, index_p){
  # update line to enable the playlist
  param_data[index_p] = paste0("user-playlist = true")
  
  # delete lines with current playlist
  
  index_d <- grep("playlist",param_data,  fixed = TRUE)
  index_d <- index_d[index_d != index_p]
  if(length(index_d) >= 1)
  {
    param_data <- param_data[- index_d]
  }
  
  # create new plalist
  new_playlist <- c("[playlist]", 
                    "playlist_reset = false",
                    sapply(playlist,FUN = function(x){paste0("playlist_year + = ", x-1)}))
  
  if(!is.null(weights)){
    new_playlist <- c(new_playlist, apply(weights, 1, function(X){
      paste0("playlist_year_weight = " , X[1] - 1,",",format(round(X[2], 6), nsmall = 6))
    })) 
  }
  
  # add new playlist to the parameters description
  param_data <- c(param_data, new_playlist)
  param_data
}
