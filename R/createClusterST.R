#' @title Create a short-term storage cluster 
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Create a new ST-storage cluster for >= v8.6.0 Antares studies.
#'
#' @param area The area where to create the cluster.
#' @param cluster_name Name for the cluster, it will prefixed by area name, unless you set `add_prefix = FALSE`.
#' @param group Group of the cluster, one of : *{PSP_open, PSP_closed, Pondage, Battery, Other}*. 
#' It corresponds to the type of stockage (**dynamic name for Antares version >= 9.2**).
#' @param storage_parameters `list ` Parameters to write in the Ini file (see `Note`). 
#' @param PMAX_injection Modulation of charging capacity on an 8760-hour basis. `numeric` \{0;1\} (8760*1).
#' @param PMAX_withdrawal Modulation of discharging capacity on an 8760-hour basis. `numeric` \{0;1\} (8760*1).
#' @param inflows Algebraic deviation of the state of charge of the storage, which does not induce any power 
#' generation or consumption on the system `numeric` \{<0;>0\} (8760*1).
#' @param lower_rule_curve This is the lower limit for filling the stock imposed each hour. `numeric` \{0;1\} (8760*1).
#' @param upper_rule_curve This is the upper limit for filling the stock imposed each hour. `numeric` \{0;1\} (8760*1).
#' @param cost_injection NULL
#' @param cost_withdrawal NULL
#' @param cost_level NULL
#' @param cost_variation_injection NULL
#' @param cost_variation_withdrawal NULL
#' @param add_prefix If `TRUE` (the default), `cluster_name` will be prefixed by area name.
#' @param overwrite `logical`, overwrite the cluster or not.
#' 
#' @template opts
#' 
#' @name createClusterST
#' 
#' @section note:
#'    
#' To write parameters to the `list.ini` file. You have function `storage_values_default()` who is called by default.
#' This function return `list` containing properties according study version for cluster `st-storage`.  
#'   
#' Study version >= "8.6.0" :  
#'  - efficiency = 1  (`numeric` \{0;1\})  
#'  - reservoircapacity = 0  (`integer` >= 0)  
#'  - initiallevel = 0  (`numeric` \{0;1\})  
#'  - withdrawalnominalcapacity = 0  (`integer` >= 0)  
#'  - injectionnominalcapacity = 0  (`integer` >= 0)  
#'  - initialleveloptim = FALSE (`logical` TRUE/FALSE)  
#'    
#'    
#' Study version >= "8.8.0" (update + new parameter) :  
#'  - initiallevel = 0.5  (`numeric` \{0;1\})  
#'  - enabled = TRUE (`logical` TRUE/FALSE)  
#'  
#' By default, these values don't allow you to have an active cluster (See example section.)  
#' 
#' 
#'  
#' @seealso All the functions needed to manage a storage cluster, 
#' [antaresRead::readClusterSTDesc()], [editClusterST()], [removeClusterST()].
#' 
#' @export
#' 
#' @importFrom antaresRead simOptions
#' @importFrom stats setNames
#' @importFrom utils read.table write.table
#' @importFrom data.table setcolorder year yday month setnames fwrite
#'
#' @examples
#' \dontrun{
#' 
#' # list for cluster parameters : 
#' storage_values_default()
#' 
#' # create a cluster by default (with default parameters values + default data values):
#' createClusterST(area = "my_area", 
#'                "my_cluster") 
#'   
#' # Read cluster in study                            
#'  # by default, cluster name is prefixed 
#'  # by the area name
#' levels(readClusterSTDesc()$cluster)
#' # > "my_area_my_cluster"
#' 
#' # create cluster with custom parameter and data
#'   # use the function to create your own list of parameters (no Antares optim)
#'   # if you want optim (my_parameters$initialleveloptim <- TRUE)
#' my_parameters <- storage_values_default()
#' my_parameters$efficiency <- 0.5
#' my_parameters$initiallevel <- 10
#' my_parameters$withdrawalnominalcapacity <- 100
#' my_parameters$injectionnominalcapacity <- 1000
#' my_parameters$reservoircapacity <- 10000
#' 
#'   # time series 
#' inflow_data <- matrix(3, 8760)
#' ratio_data <- matrix(0.7, 8760)
#' 
#' createClusterST(area = "my_area", 
#'                 "my_cluster",
#'                 storage_parameters = my_parameters,
#'                 PMAX_withdrawal = ratio_data, 
#'                 inflows = inflow_data, 
#'                 PMAX_injection = ratio_data, 
#'                 lower_rule_curve = ratio_data, 
#'                 upper_rule_curve = ratio_data)
#' }
#'
createClusterST <- function(area,
                            cluster_name, 
                            group = "Other1",
                            storage_parameters = storage_values_default(),
                            PMAX_injection = NULL,
                            PMAX_withdrawal = NULL,
                            inflows = NULL,
                            lower_rule_curve = NULL,
                            upper_rule_curve = NULL,
                            cost_injection = NULL,
                            cost_withdrawal = NULL,
                            cost_level = NULL,
                            cost_variation_injection = NULL,
                            cost_variation_withdrawal = NULL,
                            add_prefix = TRUE, 
                            overwrite = FALSE,
                            opts = antaresRead::simOptions()) {

  ## check study opts parameters ----
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  ## check study version ----
  check_active_ST(opts = opts)
  
  ## check group ----
  .check_group_st(group = group, opts = opts)
  
  ##  check area ----
    # exsiting in current study ?
  check_area_name(area, opts)  
  
  ## tolower area----
  area <- tolower(area)
  
  ##
  ## check parameters ----
  ##
  assertthat::assert_that(inherits(storage_parameters, "list"))
  
  # static name of list parameters 
  names_parameters <- names(storage_values_default(opts = opts))
  
  if(!all(names(storage_parameters) %in% names_parameters))
    stop(append("Parameter 'st-storage' must be named with the following elements: ", 
                paste0(names_parameters, collapse= ", ")))
  
  # check values parameters
  .st_mandatory_params(list_values = storage_parameters, opts = opts)
  
  # According to Antares Version 
    # default values associated with TS + .txt names files
  storage_value <- .default_values_st_TS(opts = opts)
  
  ## check dim data ----
  for (name in names(storage_value)){
    if (!is.null(dim(get(name))))
      if (!identical(dim(get(name)), c(8760L, 1L)))
        stop(paste0("Input data for ", name, " must be 8760*1"))
  }
  
  ## Standardize params ----
  params_cluster <- hyphenize_names(storage_parameters)
  
  ## cluster name prefix ----
  if (add_prefix)
    cluster_name <- generate_cluster_name(area = area, 
                                          cluster_name = cluster_name, 
                                          add_prefix = add_prefix)

  # all properties of cluster standardized
  params_cluster <- c(list(name = cluster_name, 
                           group = group),
                      params_cluster)
  
  ################# -
  ##  API block ----
  ################# -
  if (is_api_study(opts)) {
    # format name for API 
    cluster_name <- transform_name_to_id(cluster_name)
    
    ##
    # POST only for properties (creation with default TS values)
    ##
    
    # adapt parameter names
    list_properties <- list(
      "group" = params_cluster[["group"]],
      "name" = cluster_name,
      "injectionNominalCapacity" = params_cluster[["injectionnominalcapacity"]],
      "withdrawalNominalCapacity" = params_cluster[["withdrawalnominalcapacity"]],
      "reservoirCapacity" = params_cluster[["reservoircapacity"]],
      "efficiency" = params_cluster[["efficiency"]],
      "initialLevel" = params_cluster[["initiallevel"]],
      "initialLevelOptim" = params_cluster[["initialleveloptim"]],
      "enabled" = params_cluster[["enabled"]])
    
    list_properties <- dropNulls(list_properties)
    
    # make json file
    body <- jsonlite::toJSON(list_properties,
                             auto_unbox = TRUE)
    
    # send request (without coeffs/term)
    result <- api_post(opts = opts, 
                       endpoint = file.path(opts$study_id, 
                                            "areas", 
                                            area,
                                            "storages"), 
                       body = body, 
                       encode = "raw")
    
    cli::cli_alert_success("Endpoint {.emph {'Create ST-storage (properties)'}} {.emph 
                      {.strong {cluster_name}}} success")
    
    ##
    # PUT api call for each TS value
    ##
    
    # adapt list name TS 
    list_value_ts <- list(pmax_injection = PMAX_injection,
                          pmax_withdrawal = PMAX_withdrawal,
                          inflows = inflows,
                          lower_rule_curve = lower_rule_curve,
                          upper_rule_curve = upper_rule_curve)
    
    list_value_ts <- dropNulls(list_value_ts)
    
    if(length(list_value_ts)!=0){
      lapply(names(list_value_ts), function(x){
        body = jsonlite::toJSON(list(data=list_value_ts[[x]],
                                     index=0, 
                                     columns=0),
                                auto_unbox = FALSE)
        
        endpoint <- file.path(opts$study_id, 
                              "areas", 
                              area, 
                              "storages",
                              cluster_name,
                              "series", 
                              x)
        
        # update
        api_put(opts = opts, 
                endpoint =  endpoint, 
                body = body, 
                encode = "raw")
        
        cli::cli_alert_success("Endpoint {.emph {'Create ST-storage (TS value)'}} {.emph 
                      {.strong {x}}} success")
      })
     
    }
    return(invisible(opts))
  }
  ########################## -
  
  
  ##
  ## write properties ----
  ##
  
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  # path to ini file containing clusters' name and parameters
  path_clusters_ini <- file.path(inputPath, 
                                 "st-storage", 
                                 "clusters", 
                                 area, 
                                 "list.ini")
  
  # read previous content of ini
  previous_params <- readIniFile(file = path_clusters_ini)
  
  # already exists ? 
  if (tolower(cluster_name) %in% tolower(names(previous_params)) 
      & !overwrite)
    stop(paste(cluster_name, "already exist"))
    
  # overwrite
  if(overwrite){
    if(tolower(cluster_name) %in% tolower(names(previous_params))){
      ind_cluster <- which(tolower(names(previous_params)) %in% 
                             tolower(cluster_name))[1]
      previous_params[[ind_cluster]] <- params_cluster
      names(previous_params)[[ind_cluster]] <- cluster_name
    }
  }
    
  # add properties 
  previous_params[[cluster_name]] <- params_cluster
  
  # write properties (all properties are overwritten)
  writeIni(
    listData = previous_params,
    pathIni = path_clusters_ini,
    overwrite = TRUE
  )
  
  ##
  ## write TS ----
  ##
  
  # initialize series
  dir.create(
    path = file.path(inputPath, 
                     "st-storage", 
                     "series", 
                     area, 
                     tolower(cluster_name)),
    recursive = TRUE, showWarnings = FALSE
  )
  
  # write every TS values
  for (name in names(storage_value)){
    if (is.null(get(name))) 
      # to suppress messages in fwrite "conversion... matrix in data.table"
      data_values <- as.data.table(
        matrix(storage_value[[name]]$N,8760))
    else {
      # to suppress messages in fwrite "conversion... matrix in data.table"
      data_values <- get(name)
      if(!"data.table" %in% class(data_values))
        data_values <- as.data.table(data_values)
    }
    # write data 
    fwrite(
      x = data_values, row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(inputPath, "st-storage", "series", 
                       area, 
                       tolower(cluster_name), 
                       paste0(storage_value[[name]]$string, ".txt")))
  }
  
  # Update simulation options object
  suppressWarnings({
    res <- antaresRead::setSimulationPath(
      path = opts$studyPath, 
      simulation = "input")
  })
  
  invisible(res)
  
}


# check parameters (`list`)
.st_mandatory_params <- function(list_values, opts){
  .is_ratio(list_values[["efficiency"]], 
            "efficiency")
  
  .check_capacity(list_values[["reservoircapacity"]], 
                  "reservoircapacity")
  
  .is_ratio(list_values[["initiallevel"]], 
            "initiallevel")
  
  .check_capacity(list_values[["withdrawalnominalcapacity"]], 
                  "withdrawalnominalcapacity")
  
  .check_capacity(list_values[["injectionnominalcapacity"]], 
                  "injectionnominalcapacity")
  
  if(!is.null(list_values[["initialleveloptim"]]))
    assertthat::assert_that(inherits(list_values[["initialleveloptim"]], 
                                     "logical"))
  
  if (opts$antaresVersion >= 880)
    if(!is.null(list_values[["enabled"]]))
      assertthat::assert_that(inherits(list_values[["enabled"]], 
                                       "logical"))
  
  if (opts$antaresVersion >= 920){
    .is_ratio(list_values[["efficiencywithdrawal"]], 
              "efficiencywithdrawal")
    if(!is.null(list_values[["penalize-variation-injection"]]))
      assertthat::assert_that(inherits(list_values[["penalize-variation-injection"]], 
                                       "logical"))
    if(!is.null(list_values[["penalize-variation-withdrawal"]]))
      assertthat::assert_that(inherits(list_values[["penalize-variation-withdrawal"]], 
                                       "logical"))
  }
}

.is_ratio <- function(x, mess){
  if(!is.null(x)){
    assertthat::assert_that(inherits(x, "numeric"))
    if(!(x>=0 && x<=1))
      stop(paste0(mess, " must be in range 0-1"), 
           call. = FALSE)
  }
}

.check_capacity <- function(x, mess){
  if(!is.null(x)){
    assertthat::assert_that(inherits(x, "numeric"))
    if(!(x>=0))
      stop(paste0(mess, " must be >= 0"), 
           call. = FALSE)
  }
}

#' Short Term Storage Property List
#'
#' @description
#' Default values are returned according to study version
#'
#' @template opts
#' @return a named list
#' @export
#'
#' @examples
#' \dontrun{
#' storage_values_default()
#' }
storage_values_default <- function(opts = simOptions()) {
  lst_parameters <- list(efficiency = 1,
                         reservoircapacity = 0,
                         initiallevel = 0,
                         withdrawalnominalcapacity = 0,
                         injectionnominalcapacity = 0,
                         initialleveloptim = FALSE)
  
  if (opts$antaresVersion >= 880){
    lst_parameters[["initiallevel"]] <- 0.5
    lst_parameters[["enabled"]] <- TRUE
  }
  
  if (opts$antaresVersion >= 920){
    lst_parameters[["efficiencywithdrawal"]] <- 1
    lst_parameters[["penalize-variation-injection"]] <- FALSE
    lst_parameters[["penalize-variation-withdrawal"]] <- FALSE
  }
  return(lst_parameters)
}


#' function to check 'group' parameter according to version study
#' no check 'group' for version >= 9.2
#' it is used in editClusterST()
#' @noRd
.check_group_st <- function(group, opts){
  # `group` is dynamic (>=v9.2)
  if(opts$antaresVersion<920){
    # statics groups
    st_storage_group <- c("PSP_open", 
                          "PSP_closed", 
                          "Pondage", 
                          "Battery",
                          paste0("Other", 
                                 seq(1,5)))
    
    # check group
    if (!is.null(group) && !tolower(group) %in% tolower(st_storage_group))
      stop(
        "Group: '", group, "' is not a valid name recognized by Antares,",
        " you should be using one of: ", paste(st_storage_group, collapse = ", "), 
        call. = FALSE
      )
  }
}

# name parameter of function associated with .txt names files
  # mutualized with editClusterST()  
.default_values_st_TS <- function(opts){
  # TS DATA parameters : default value + name txt file
  storage_value <- list(PMAX_injection = list(N=1, string = "PMAX-injection"),
                        PMAX_withdrawal = list(N=1, string = "PMAX-withdrawal"),
                        inflows = list(N=0, string = "inflows"),
                        lower_rule_curve = list(N=0, string = "lower-rule-curve"),
                        upper_rule_curve = list(N=1, string = "upper-rule-curve"))
  
  if (opts$antaresVersion >= 920){
    # add new TS
    list_value_920 <- list(
      cost_injection = list(N=0, string = "cost-injection"),
      cost_withdrawal = list(N=0, string = "cost-withdrawal"),
      cost_level = list(N=0, string = "cost-level"),
      cost_variation_injection = list(N=0, string = "cost-variation-injection"),
      cost_variation_withdrawal = list(N=0, string = "cost-variation-withdrawal"))
    
    storage_value <- append(storage_value, list_value_920)
  }
  return(storage_value)
}
