
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
      mat_play_list <-
        data.table(t(cbind.data.frame(strsplit(
          vect_value_weigth, ","
        ))))
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
#' @importFrom antaresRead simOptions
#' @export
#'
#' @rdname playlist
setPlaylist <- function(playlist,
                        weights = NULL,
                        opts = antaresRead::simOptions()) {
  
  version_study <- substr(opts$antaresVersion, 1, 1)
  version_study <- as.numeric(version_study)
  
  if (version_study < 8 & !is.null(weights)) {
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
  
  # if all mc_years must be simulated, desactive playlist
  if (length(playlist) == length(mc_years)) {
    # update line to disable the playlist
    generaldata$general$`user-playlist` <- FALSE
    # write updated file
    writeIni(listData = generaldata, pathIni = "settings/generaldata", overwrite = TRUE, opts = opts)
    
  } else { # otherwise, set the playlist
    # update line to enable the playlist
    generaldata$general$`user-playlist` <- TRUE
    
    # delete lines with current playlist
    generaldata$playlist <- NULL
    
    # create new playlist (+ patch double to integer)
    new_playlist <- setNames(as.list(as.integer(playlist - 1)), rep("playlist_year +", length(playlist)))
    new_playlist <- c(list(playlist_reset = FALSE), new_playlist)
    
    if (!is.null(weights)) {
      new_playlist <- c(
        new_playlist, 
        setNames(apply(
          X = weights, 
          MARGIN = 1,
          FUN = function(X) {
            paste0(
              X[1] - 1,
              ",",
              format(round(X[2], 6), nsmall = 6)
            )
          }
        ), rep("playlist_year_weight", length(weights)))
      )
    }
    
    # add new playlist to the parameters description
    generaldata$playlist <- new_playlist
    
    # write updated file
    writeIni(listData = generaldata, pathIni = "settings/generaldata", overwrite = TRUE, opts = opts)
  }
}
