
#' @title Get the playlist of an Antares study
#'  `r antaresEditObject:::badge_api_ok()`
#'
#' @description \code{getPlaylist} gives the identifier of the MC years which
#' will be simulated in the Antares study, taking into account the potential use of a
#' playlist which can skip some MC years
#'
#' @template opts-arg
#'
#' @return
#'  * `getPlaylist` returns a vector of the identifier of the simulated MC year.
#'
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions
#' @export
#'
#' @name playlist
#' 
#' @examples 
#' \dontrun{
#' setSimulationPath("PATH/TO/STUDY/")
#' # or 
#' setSimulationPathAPI(
#'   host = "http://localhost:8080",
#'   study_id = "6f98a393-155d-450f-a581-8668dc355235",
#'   token = NULL,
#'   simulation = "input"
#' )
#' 
#' # augment number of MC years
#' updateGeneralSettings(nbyears = 10)
#' 
#' # Get the actual playlist
#' getPlaylist()
#' # [1] 2 4 6
#' 
#' # set a new playlist
#' setPlaylist(c(3, 5, 7))
#' }
getPlaylist <- function(opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  api_study <- is_api_study(opts)
  # read general parameters
  parameters <- readIni("settings/generaldata", opts = opts)
  
  version_study <- substr(opts$antaresVersion, 1, 1)
  version_study <- as.numeric(version_study)
  
  # get all MC years
  mc_years <- seq_len(parameters$general$nbyears)
  
  # if no playlist is used, return all mc years
  if (parameters$general$`user-playlist` == FALSE) {
    return(mc_years)
  }
  
  # otherwise, update the vector of mc_years by removing disabled years
  playlist_update_type <- names(parameters$playlist)
  playlist_update_value <- parameters$playlist
  if (api_study){
    
    for (type in playlist_update_type){
      no_uptdate <- grepl(pattern = "reset", type)
      if (!no_uptdate){
        if (type == "playlist_year +"){
          transform_values <- gsub(pattern = "\\[|\\]", 
                                   x = playlist_update_value[[type]],
                                   replacement = "")
          transform_values <- unlist(strsplit(x = transform_values, split = "\\,"))
        }
        else if (type == "playlist_year_weight"){
          transform_values <- gsub(pattern = "\\['|'|'\\]", 
                                   x = playlist_update_value[[type]],
                                   replacement = "")
          transform_values <- unlist(strsplit(x = transform_values, split = "\\,"))
        }
        
        
        playlist_update_value[[type]] <- as.numeric(transform_values)
      } 
      
    }
  }
  
  # untouched playlist - no modification have been made
  if (length(playlist_update_type) == 0) {
    return(mc_years)
  }
  
  # modified playlist - take into account the modifications
  assertthat::assert_that(all(
    playlist_update_type %in% c(
      "playlist_reset",
      "playlist_year +",
      "playlist_year -",
      "playlist_year_weight"
    )
  ))
  activated <- rep(TRUE, length(mc_years))
  
  for (i in seq_along(playlist_update_type)) {
    # playlist_reset means that we start from a playlist where every MC year is disactivated
    if (playlist_update_type[i] == "playlist_reset") {
      activated <- rep(FALSE, length(mc_years))
    }
    # playlist_year + means that the corresponding year is added to the playlist
    if (playlist_update_type[i] == "playlist_year +") {
      activated[playlist_update_value[[i]] + 1] <- TRUE
    }
    # playlist_year - means that the corresponding year is removed from the playlist
    if (playlist_update_type[i] == "playlist_year -") {
      activated[playlist_update_value[[i]] + 1] <- FALSE
    }
  }
  activate_mc <- mc_years[activated]
  
  if (version_study < 8) {
    return(activate_mc)
    
  } else {
    if (!"playlist_year_weight" %in% playlist_update_type) {
      return(activate_mc)
    } else{
      vect_value_weigth = unlist(playlist_update_value[names(playlist_update_value) == "playlist_year_weight"])
      if(api_study){
        mcYears <- vect_value_weigth[seq(1, length(vect_value_weigth), by = 2)] + 1
        weights <- vect_value_weigth[seq(2, length(vect_value_weigth), by = 2)]
        mat_play_list <- data.table(mcYears, weights)
      }
      else {
        mat_play_list <- data.table(t(cbind.data.frame(
          strsplit(vect_value_weigth, ","))))
        mat_play_list$V1 <- as.numeric(mat_play_list$V1) + 1
        mat_play_list$V2 <- as.numeric(mat_play_list$V2)
        setnames(mat_play_list, "V1", "mcYears")
        setnames(mat_play_list, "V2", "weights")
      }
      return(list(activate_mc = activate_mc, weights = mat_play_list))
    }
  }
}



#' Set the playlist of an Antares Study
#'
#' \code{setPlaylist} is a function which modifies the input file of an ANTARES
#' study and set the playlist in order to simulate only the MC years given in input
#'
#'
#' @param playlist
#'   vector of MC years identifier to be simulated can be a list (V8 compatibility) but not recommended
#' @param weights
#'   data.table, 2 columns : mcYears and weights. Only with after antares V8
#' @template opts-arg
#'
#' @return
#'  * `setPlaylist` does not return anything. It is  used to modify the input of an Antares study.
#'
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions readIni
#' @export
#'
#' @rdname playlist
setPlaylist <- function(playlist,
                        weights = NULL,
                        opts = antaresRead::simOptions()) {
  
  api_study <- is_api_study(opts)
  
  version_study <- substr(opts$antaresVersion, 1, 1)
  
  if (as.numeric(version_study) < 8 & !is.null(weights)) {
    stop("weights can be use only for antares > V8, please convert your studie before")
  }
  
  ##For portability (V7, V8)
  if (is.list(playlist)) {
    if ("activate_mc" %in% names(playlist)) {
      playlist <- playlist$activate_mc
    } else {
      stop("List provided must contain activate_mc columns")
    }
  }
  
  # get all MC years
  mc_years <- seq_len(opts$parameters$general$nbyears)
  assertthat::assert_that(all(playlist %in% mc_years))
  playlist <- sort(playlist)
  playlist <- unique(playlist)
  
  # read general data parameters
  generaldata <- readIni("settings/generaldata", opts = opts)
  existing_playlist_weight <- "playlist_year_weight" %in% names(generaldata$playlist)
  
  # if all mc_years must be simulated, desactive playlist
  if (length(playlist) == length(mc_years)) {
    # update line to disable the playlist
    generaldata$general$`user-playlist` <- FALSE
    if (api_study) {
      # To minimize the number of queries, we reduce the list to the updated items
      generaldata <- generaldata[which(names(generaldata) == "general")]
      generaldata$general[which(names(generaldata$general) != "user-playlist")] <- NULL
    }
  } else { # otherwise, set the playlist
    # update line to enable the playlist
    generaldata$general$`user-playlist` <- TRUE
    # delete lines with current playlist
    generaldata$playlist <- NULL
    
    # create new playlist (+ patch double to integer)
    new_playlist <- setNames(as.list(sort(as.integer(playlist - 1))), rep("playlist_year +", length(playlist)))
    playlist_weights <- .format_playlist_weights(weights = weights, api_mode = api_study)
    
    if (api_study) {
      new_playlist$sep <- ", "
      new_playlist <- list("playlist_year +" = paste0("[", do.call(paste, new_playlist), "]"))
      if (existing_playlist_weight & is.null(playlist_weights)) {
        weights <- data.table("mcYears" = playlist, "weights" = rep(1, length(playlist))) # 1 is the default weight
        playlist_weights <- .format_playlist_weights(weights = weights, api_mode = TRUE) 
      }
      new_playlist[["playlist_year_weight"]] <- playlist_weights
    } else {
      new_playlist <- c(new_playlist, playlist_weights)
    }
    
    new_playlist <- c(list("playlist_reset" = FALSE), new_playlist)
    
    if (api_study) {
      # To minimize the number of queries, we reduce the list to the updated items
      generaldata <- generaldata[which(names(generaldata) == "general")]
      generaldata$general[which(names(generaldata$general) != "user-playlist")] <- NULL
    }
    # add new playlist to the parameters description
    generaldata$playlist <- new_playlist
  }
  
  writeIni(listData = generaldata, pathIni = "settings/generaldata", overwrite = TRUE, opts = opts)
  
  if (api_study) {
    suppressWarnings(
      res <- antaresRead::setSimulationPathAPI(host = opts$host,
                                               study_id = opts$study_id, 
                                               token = opts$token, 
                                               simulation = "input")
    )
  } else {
    suppressWarnings(
      res <- antaresRead::setSimulationPath(path = opts$studyPath, 
                                            simulation = "input")
    )
  }
  
  return(invisible(res))
}


#' Generate playlist_year_weight section in the appropriate format.
#' 
#' @param weights
#'   data.table, 2 columns : mcYears and weights. Only with after antares V8
#' @param api_mode Boolean to identify an api study
#'
#' @return The playlist_year_weight section formatted.
#'
.format_playlist_weights <- function(weights, api_mode) {
  
  playlist_year_weight <- NULL 
  
  if (!is.null(weights)) {
    if (api_mode) {
      playlist_year_weight <- paste0(weights$mcYears - 1, ",", weights$weights)
      playlist_year_weight <- paste("\'", playlist_year_weight, "\'", collapse  = ",", sep = "")
      playlist_year_weight <- paste("[", playlist_year_weight, "]", sep = "")
    } else {
      playlist_year_weight <- paste0(weights$mcYears - 1, ",", format(round(weights$weights, 6), nsmall = 6))
      playlist_year_weight <- setNames(playlist_year_weight, rep("playlist_year_weight", length(playlist_year_weight)))
    }
  }
  
  return(playlist_year_weight)
}
