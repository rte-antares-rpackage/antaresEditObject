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
#' @param PMAX_injection Modulation of charging capacity on an 8760-hour basis. `numeric` \{0;1\} (8760*1)
#' (**`numeric` \{0;1\} (8760*N) noting that N >= 1 for Antares version >= 9.3**).
#' @param PMAX_withdrawal Modulation of discharging capacity on an 8760-hour basis. `numeric` \{0;1\} (8760*1)
#' (**`numeric` \{0;1\} (8760*N) noting that N >= 1 for Antares version >= 9.3**).
#' @param inflows Algebraic deviation of the state of charge of the storage, which does not induce any power 
#' generation or consumption on the system `numeric` \{<0;>0\} (8760*1)
#' (**``numeric` \{<0;>0\} (8760*1) noting that N >= 1 for Antares version >= 9.3**).
#' @param lower_rule_curve This is the lower limit for filling the stock imposed each hour. `numeric` \{0;1\} (8760*1)
#' (**`numeric` \{0;1\} (8760*N) noting that N >= 1 for Antares version >= 9.3**).
#' @param upper_rule_curve This is the upper limit for filling the stock imposed each hour. `numeric` \{0;1\} (8760*1)
#' (**`numeric` \{0;1\} (8760*N) noting that N >= 1 for Antares version >= 9.3**).
#' @param cost_injection Penalizes the injection flowrate at each hour (&euro;/MWh) `numeric` \{>0\} (8760*1)
#' (**`numeric` \{>0\}  (8760*N) noting that N >= 1 for Antares version >= 9.3**).
#' @param cost_withdrawal Penalizes the withdrawal flowrate at each hour (&euro;/MWh) `numeric` \{>0\} (8760*1)
#' (**`numeric` \{>0\}  (8760*N) noting that N >= 1 for Antares version >= 9.3**).
#' @param cost_level Penalizes the volume of stored energy at each hour (&euro;/MWh) `numeric` \{<0;>0\} (8760*1)
#' (**`numeric` \{<0;>0\}  (8760*N) noting that N >= 1 for Antares version >= 9.3**).
#' @param cost_variation_injection Penalizes injection flowrate variation every hour (&euro;/MWh) `numeric` \{>0\} (8760*1)
#' (**`numeric` \{>0\}  (8760*N) noting that N >= 1 for Antares version >= 9.3**).
#' @param cost_variation_withdrawal Penalizes the withdrawal variation every hour (&euro;/MWh) `numeric` \{>0\} (8760*1)
#' (**`numeric` \{>0\}  (8760*N) noting that N >= 1 for Antares version >= 9.3**).
#' @param constraints_properties `list ` Parameters (see example)
#' @param constraints_ts `list ` of time series (see example)
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
#' Study version >= "9.2" (new parameters) :  
#'  - efficiencywithdrawal = 1 (`numeric` \{0;1\})
#'  - `penalize-variation-injection` = FALSE (`logical` TRUE/FALSE)
#'  - `penalize-variation-withdrawal` = FALSE `logical` TRUE/FALSE)
#'  
#' Study version >= "9.3" (new parameters) :  
#'  - `allow-overflow` = FALSE (`logical` TRUE/FALSE)
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
#'                 
#' # for a study version >= 9.2 (new parameters)
#' my_parameters <- storage_values_default()
#' my_parameters$efficiencywithdrawal <- 0.5
#' my_parameters$`penalize-variation-injection` <- TRUE
#' my_parameters$`penalize-variation-withdrawal` <- TRUE
#' 
#' 
#' createClusterST(area = "my_area", 
#'                 "my_cluster",
#'                 storage_parameters = my_parameters)
#'                
#'   # time series                  
#' ratio_value <- matrix(0.7, 8760)
#'
#' # default properties with new optional TS
#' createClusterST(area = "fr", 
#'                 cluster_name = "good_ts_value", 
#'                 cost_injection = ratio_value, 
#'                 cost_withdrawal = ratio_value, 
#'                 cost_level = ratio_value, 
#'                 cost_variation_injection = ratio_value,
#'                 cost_variation_withdrawal = ratio_value)         
#'                 
#' # Add optional constraints properties 
#'     
#' name_no_prefix <- "add_constraints"
#' 
#' constraints_properties <- list(
#'   "withdrawal-1"=list(
#'     variable = "withdrawal",
#'     operator = "equal",
#'     hours = c("[1,3,5]", 
#'               "[120,121,122,123,124,125,126,127,128]")
#'   ),
#'   "netting-1"=list(
#'     variable = "netting",
#'     operator = "less",
#'     hours = c("[1, 168]")
#'   ))
#' 
#' # create a cluster with constraint properties (no need to provide TS)
#' createClusterST(area = area_test_clust, 
#'                 cluster_name = name_no_prefix, 
#'                 constraints_properties = constraints_properties)         
#' 
#'    # Add optional constraints properties + TS 
#'    
#' constraints_properties <- list(
#'   "withdrawal-2"=list(
#'     variable = "withdrawal",
#'     operator = "equal",
#'     hours = c("[1,3,5]", 
#'               "[120,121,122,123,124,125,126,127,128]")
#'   ),
#'   "netting-2"=list(
#'     variable = "netting",
#'     operator = "less",
#'     hours = c("[1, 168]")
#'   ))    
#'    
#' good_ts <- matrix(0.7, 8760)
#' constraints_ts <- list(
#'   "withdrawal-2"=good_ts,
#'   "netting-2"=good_ts)
#' 
#' # create a cluster with constraint properties + TS
#' createClusterST(area = area_test_clust, 
#'                 cluster_name = name_no_prefix, 
#'                 constraints_properties = constraints_properties, 
#'                 constraints_ts = constraints_ts)
#'   
#' # for a study version >= 9.3 (new parameters)
#' my_parameters <- storage_values_default()
#' my_parameters$`allow-overflow` <- TRUE
#' 
#'   # time series                  
#' ratio_value <- matrix(0.7, 8760, N)
#'
#' # default properties with new optional TS
#' createClusterST(area = "fr", 
#'                 cluster_name = "good_ts_value", 
#'                 cost_injection = ratio_value)         
#'                 
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
                            constraints_properties = NULL, 
                            constraints_ts = NULL,
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
                paste0(names_parameters, collapse= ", ")), 
         call. = FALSE)
  
  # check values parameters
  .st_mandatory_params(list_values = storage_parameters, opts = opts)
  
  # According to Antares Version 
  # default values associated with TS + .txt names files
  storage_value <- .default_values_st_TS(opts = opts)
  
  # names of the 10 inputs to validate
  TS_names <- c(
    "PMAX_injection", "PMAX_withdrawal", "inflows",
    "lower_rule_curve", "upper_rule_curve",
    "cost_injection", "cost_withdrawal", "cost_level",
    "cost_variation_injection", "cost_variation_withdrawal"
  )
  
  # grab those args from createClusterST()'s environment (NULL if not provided)
  TS_vals <- mget(TS_names, envir = environment(), inherits = FALSE,
                  ifnotfound = rep(list(NULL), length(TS_names)))
  TS_vals <- Filter(Negate(is.null), TS_vals)
  
  
  # class check on provided (non-NULL) ones
  for (nm in TS_names) {
    x <- TS_vals[[nm]]
    if (!is.null(x)) .check_class(x)
  }
  
  # dimension check (only non-NULL), Antares version handled inside
  .check_dimension(TS_vals, opts = opts)
  
  ## Standardize params ----
  params_cluster <- hyphenize_names(storage_parameters)
  
  ## Standardize cluster name + prefix ----
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
      "enabled" = params_cluster[["enabled"]],
      "penalizeVariationInjection"= params_cluster[["penalize-variation-injection"]],
      "penalizeVariationWithdrawal"= params_cluster[["penalize-variation-withdrawal"]],
      "efficiencyWithdrawal"= params_cluster[["efficiencywithdrawal"]],
      "allowOverflow"= params_cluster[["allow-overflow"]])
    
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
    # we build all the series
    # 1) mapping between key and suffix
    keys <- c("PMAX_injection", "PMAX_withdrawal", "inflows", "lower_rule_curve", "upper_rule_curve")
    suffixes <- tolower(keys)  
    
    # 2) Building
    ST_time_series <- setNames(
      lapply(seq_along(keys), function(i) {
        list(
          path   = sprintf("input/st-storage/series/%s/%s/%s", "%s", "%s", suffixes[i]),
          matrix = get(keys[i], inherits = TRUE)  
        )
      }),
      keys
    )
    
    if (opts$antaresVersion >= 920) {
      # Names of the series
      keys <- c(
        "cost_injection",
        "cost_withdrawal",
        "cost_level",
        "cost_variation_injection",
        "cost_variation_withdrawal"
      )
      
      # Building the ST_time_series_920 list
      ST_time_series_920 <- setNames(
        lapply(keys, function(k) {
          list(
            path   = sprintf("input/st-storage/series/%s/%s/%s", "%s", "%s", k),
            matrix = get(k, inherits = TRUE) 
          )
        }),
        keys
      )
      
      ST_time_series <- append(ST_time_series, ST_time_series_920)
      
      # POST Constraints (robust: we retrieve the exact cluster ID)
      if (!is.null(constraints_properties)) {
        # force a JSON array even for a single value
        # Parse each occurrence, whether given as a string "[1,3,5]" or as a vector c(1L,3L,5L)
        .to_hours_list <- function(x) {
          if (is.character(x)) x <- jsonlite::fromJSON(x)
          as.list(as.integer(x))  
        }
        
        .make_occ <- function(pr) {
          h <- pr$hours
          if (is.null(h)) return(list())
          # h can be: vector of strings ("[1,3]","[120,121]") OR list of vectors (c(1,3), c(120,121))
          elems <- if (is.list(h)) h else as.list(h)
          lapply(elems, function(v) list(hours = .to_hours_list(v)))
        }
        
        constraints_payload <- lapply(names(constraints_properties), function(nm) {
          pr <- constraints_properties[[nm]]
          list(
            name        = nm,
            variable    = pr$variable,
            operator    = pr$operator,
            occurrences = .make_occ(pr),
            enabled     = if (!is.null(pr$enabled)) isTRUE(pr$enabled) else TRUE
          )
        })
        
        # Endpoint constraints (with the exact ID)
        endpoint_constraints <- file.path(
          opts$study_id, "areas", tolower(area), "storages",tolower(cluster_name),
          "additional-constraints"
        )
        # update
        api_post(opts = opts, 
                 endpoint =  endpoint_constraints, 
                 body = constraints_payload, 
                 encode = "json")
        
        cli::cli_alert_success(
          "Endpoint {.emph {'Create ST-storage (constraints)'}} pour {.strong {cluster_name}} OK"
        )
      }
      
      #  Constraints time series (rhs_<name>) via replace_matrix
      if (!is.null(constraints_ts) && length(constraints_ts) > 0) {
        actions_rhs <- lapply(names(constraints_ts), function(nm) {
          list(
            target = sprintf("input/st-storage/constraints/%s/%s/rhs_%s",
                             tolower(area), tolower(cluster_name), nm),
            matrix = constraints_ts[[nm]]
          )
        })
        actions_rhs <- setNames(actions_rhs, rep("replace_matrix", length(actions_rhs)))
        cmd_rhs <- do.call(api_commands_generate, actions_rhs)
        api_command_register(cmd_rhs, opts = opts)
        if (should_command_be_executed(opts)) {
          api_command_execute(cmd_rhs, opts = opts,
                              text_alert = "Writing constraint TS (rhs_*): {msg_api}")
        } else {
          cli_command_registered("replace_matrix")
        }
      }
    }
    #Matrix
    not_null_matrix <- sapply(ST_time_series, FUN = function(l) {!is.null(l[["matrix"]])})
    ST_time_series <- ST_time_series[not_null_matrix]
    cmd <- NULL
    if (length(ST_time_series) > 0) {
      actions <- lapply(
        X = seq_along(ST_time_series),
        FUN = function(i) {
          list(
            target = sprintf(ST_time_series[[i]][["path"]], tolower(area), tolower(cluster_name)),
            matrix = ST_time_series[[i]][["matrix"]]
          )
        }
      )
      actions <- setNames(actions, rep("replace_matrix", length(actions)))
      cmd <- do.call(api_commands_generate, actions)
    }
    
    if (!is.null(cmd)) {
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts),
        api_command_execute(cmd, opts = opts, text_alert = "Writing short-term's series: {msg_api}"),
        cli_command_registered("replace_matrix")
      )
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
  
  ## check cluster already exists ----
  if (cluster_name %in% tolower(names(previous_params)) 
      & !overwrite)
    stop(paste(cluster_name, "already exist"))
  
  ## overwrite ----
  if(overwrite){
    if(cluster_name %in% tolower(names(previous_params))){
      ind_cluster <- which(tolower(names(previous_params)) %in% 
                             cluster_name)[1]
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
                     cluster_name),
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
                       cluster_name, 
                       paste0(storage_value[[name]]$string, ".txt")))
  }
  
  ## add constraint(s) ----
  if(!is.null(constraints_properties))
    .add_storage_constraint(area = area, 
                            cluster_name = cluster_name, 
                            constraints_properties = constraints_properties, 
                            constraints_ts = constraints_ts, 
                            overwrite = overwrite, 
                            opts = opts)
  
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
    if(!is.null(list_values[["efficiencywithdrawal"]])){
      assertthat::assert_that(inherits(list_values[["efficiencywithdrawal"]], "numeric"))
    }
    if(!is.null(list_values[["penalize-variation-injection"]]))
      assertthat::assert_that(inherits(list_values[["penalize-variation-injection"]], 
                                       "logical"))
    if(!is.null(list_values[["penalize-variation-withdrawal"]]))
      assertthat::assert_that(inherits(list_values[["penalize-variation-withdrawal"]], 
                                       "logical"))
  }
  
  if (opts$antaresVersion >= 930){
    if(!is.null(list_values[["allow-overflow"]]))
      assertthat::assert_that(inherits(list_values[["allow-overflow"]], 
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
  if (opts$antaresVersion >= 930){
    lst_parameters[["allow-overflow"]] <- FALSE
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

#' Add constaint to a st-storage
#' 
#' @inheritParams createClusterST
#' @noRd
.add_storage_constraint <- function(area, 
                                    cluster_name, 
                                    constraints_properties, 
                                    constraints_ts, 
                                    overwrite,
                                    opts){
  # constraints/<area id>/cluster/additional-constraints.ini
  
  # create dir 
  dir_path <- file.path(opts$inputPath, 
                        "st-storage", 
                        "constraints", 
                        area, 
                        cluster_name)
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  
  ## write properties ----
  
  # TODO check mandatory list params
  
  # read previous content of ini (if exists)
  path_contraint_ini <- file.path(dir_path, 
                                  "additional-constraints.ini")
  
  if(file.exists(path_contraint_ini)){
    previous_params <- readIniFile(file = path_contraint_ini)
    
    constraints_names <- names(constraints_properties)
    
    ## check constraint(s) already exist(s) ----
    if (any(
      constraints_names %in% 
      tolower(names(previous_params)) & 
      !overwrite))
      stop(paste(constraints_names, " already exist "), 
           call. = FALSE)
    
    ## overwrite prop ----
    if(overwrite){
      if(any(
        constraints_names %in% tolower(names(previous_params))
      )){
        # insert/overwrite
        ind_cluster <- which(tolower(names(previous_params)) %in% 
                               constraints_names)
        previous_params[ind_cluster] <- constraints_properties
        constraints_properties <- previous_params
        # names(previous_params)[ind_cluster] <- constraints_names
      }else
        constraints_properties <- append(previous_params, constraints_properties) 
    }else
      constraints_properties <- append(previous_params, constraints_properties)
  }
  # write properties (all properties are overwritten)
  writeIni(
    listData = constraints_properties,
    pathIni = path_contraint_ini,
    overwrite = TRUE
  )
  
  ## write ts values ----
  # check if ts already exist
  if(!is.null(constraints_ts)){
    ts_name <- paste0("rhs_", names(constraints_ts), ".txt")
    path_ts_files <- file.path(dir_path, 
                               ts_name)
    
    if(any(file.exists(path_ts_files))){
      if(!overwrite)
        stop(paste(constraints_names, " already exist "), 
             call. = FALSE)
    }
    lapply(names(constraints_ts), 
           function(x){
             fwrite(
               x = constraints_ts[[x]], 
               row.names = FALSE, 
               col.names = FALSE, 
               sep = "\t",
               file = file.path(dir_path, 
                                paste0("rhs_", x, ".txt")))
           })
  }
}


#' Check the class of the matrix
#'
#' @param x 
#'
#' @noRd
.check_class <- function(x) {
  if (inherits(x, c("matrix", "data.frame", "data.table"))) {
    return(invisible(TRUE))
  }
  stop("The object must be of class matrix, data.frame, or data.table", call. = FALSE)
}


#' Check the dimensions of the matrix
#'
#' @param in_vals 
#' @param allow_multi 
#'
#' @noRd
.check_dimension <- function(in_vals, opts) {
  allow_multi <- isTRUE(opts$antaresVersion >= 930)

  for (nm in names(in_vals)) {
    x <- in_vals[[nm]]
    if (is.null(x)) next
    d <- dim(x)
    if (allow_multi) {
      if (d[1L] != 8760L || d[2L] < 1L)
        stop(sprintf("Input data for %s must be 8760*N (N>=1)", nm), call. = FALSE)
    } else {
      if (!identical(d, c(8760L, 1L)))
        stop(sprintf("Input data for %s must be 8760*1", nm), call. = FALSE)
    }
  }
  invisible(TRUE)
}
