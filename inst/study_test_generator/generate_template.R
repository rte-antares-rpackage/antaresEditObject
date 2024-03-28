library(waldo)
library(antaresEditObject)


# create template study for next release

# params ----
version <- "8.6.0"
name <- "test_case"

# create study ----
createStudy(path = tempdir(), 
            study_name = name, 
            antares_version = version)

# areas ----
lapply(c("fr", "it", "at"), 
       createArea)

# thermal clusters ----
pollutants_tests_values <- list_pollutants_values(multi_values = 0.3)

  # folder prepro/area/cluster/{data, mudulation} 
  # are created with default values

capacity <- 500
count <- 3L

  # data TS (data + modulation with default)
cluster_ts_data <- matrix(count*capacity, 8760, 1)
cluster_ts_data <- cbind(cluster_ts_data,
                         matrix(count*capacity*1.25, 8760, 1))

createCluster(area = getAreas()[1], 
              cluster_name = "gas", 
              group = "Gas", 
              unitcount = count, 
              nominalcapacity = capacity,
              min_stable_power = 180,
              min_up_time = 3L,
              marginal_cost = 135.9,
              market_bid_cost = 135.9,
              list_pollutants = pollutants_tests_values, 
              time_series = cluster_ts_data)

createCluster(area = getAreas()[2], 
              cluster_name = "oil", 
              group = "Oil", 
              unitcount = count, 
              nominalcapacity = capacity,
              min_stable_power = 280,
              min_up_time = 3L,
              marginal_cost = 535.9,
              market_bid_cost = 535.9,
              list_pollutants = pollutants_tests_values, 
              time_series = cluster_ts_data)

createCluster(area = getAreas()[3], 
              cluster_name = "nuc", 
              group = "Nuclear", 
              unitcount = count, 
              nominalcapacity = capacity,
              min_stable_power = 280,
              min_up_time = 3L,
              marginal_cost = 835.9,
              market_bid_cost = 835.9,
              list_pollutants = pollutants_tests_values, 
              time_series = cluster_ts_data)

# renewables ----
cluster_ts_res_data <- cluster_ts_data/2

  # production factor
createClusterRES(area = getAreas()[1], 
              cluster_name = "res_1", 
              group = "Other RES 1", 
              unitcount = count, 
              nominalcapacity = capacity/2,
              enabled = TRUE,
              ts_interpretation = "production-factor",
              time_series = cluster_ts_res_data)

  # power generation
createClusterRES(area = getAreas()[2], 
                 cluster_name = "res_2", 
                 group = "Other RES 2", 
                 unitcount = count, 
                 nominalcapacity = capacity/2,
                 enabled = TRUE,
                 ts_interpretation = "power-generation",
                 time_series = cluster_ts_res_data)

# load ----
  # calculated with cluster params (for every area)
load_value <- count*capacity
load_value_2 <- load_value*0.75

data_load <- matrix(c(
  rep(load_value, 8760),
  rep(load_value_2, 8760)), ncol = 2)

lapply(getAreas(), 
       writeInputTS, 
       data=data_load, 
       type="load")

#writeInputTS(data = data_load, area = getAreas()[1])

# links ----
  # set properties
link_properties <- propertiesLinkOptions()

  # data link
ts_link <- matrix(rep(count*capacity/3, 8760*2), ncol = 2)

createLink(from = getAreas()[1], 
           to = getAreas()[2], 
           propertiesLink = link_properties, 
           tsLink = ts_link)

createLink(from = getAreas()[2], 
           to = getAreas()[3], 
           propertiesLink = link_properties, 
           tsLink = ts_link)

# binding constraints ----
less <- rep(200, 8760)
greater <- rep(300, 8760)
equal <- rep(400, 8760)
data_bc <- matrix(cbind(less, greater, equal), 
                  ncol = 3)

createBindingConstraint(name = "bc_1", 
                        values = data_bc, 
                        timeStep = "hourly", 
                        operator = "less", 
                        filter_year_by_year = "hourly", 
                        filter_synthesis = "hourly",
                        coefficients = c("at%fr" = 1))

createBindingConstraint(name = "bc_2", 
                        values = data_bc[1:365,], 
                        timeStep = "weekly", 
                        operator = "greater", 
                        filter_year_by_year = "hourly", 
                        filter_synthesis = "hourly",
                        coefficients = c("fr%it" = 1))

createBindingConstraint(name = "bc_3", 
                        values = data_bc[1:365,], 
                        timeStep = "weekly", 
                        operator = "equal", 
                        filter_year_by_year = "hourly", 
                        filter_synthesis = "hourly",
                        coefficients = c("fr%it" = 1))

createBindingConstraint(name = "bc_4", 
                        values = NULL, 
                        timeStep = "daily", 
                        operator = "both", 
                        filter_year_by_year = "hourly", 
                        filter_synthesis = "hourly")

# st-storage ----
inflows_data <- matrix(3, 8760)
ratio_values <- matrix(0.7, 8760)

list_params_st <- storage_values_default()
list_params_st$efficiency <- 0.9
list_params_st$reservoircapacity <- 500
list_params_st$injectionnominalcapacity <- 100
list_params_st$withdrawalnominalcapacity <- 100
list_params_st$initiallevel <- 0.1

# creation with data default values 
createClusterST(area = getAreas()[1], 
                cluster_name = "st_batt", 
                group = "Battery", 
                storage_parameters = list_params_st)

createClusterST(area = getAreas()[2], 
                cluster_name = "st_other1", 
                group = "Other1", 
                storage_parameters = list_params_st)

createClusterST(area = getAreas()[3], 
                cluster_name = "st_pondage", 
                group = "Pondage", 
                storage_parameters = list_params_st)


# hydro ----
  # properties hydro.ini (use writeIniHydro())
  # /series (fichiers mod, ror, mingen => writeInputTS())
  # /common/capacity data maxpower (use writeHydroValues())
  # /prepro (no need for mingen)
  # /allocation (no need for mingen)

hydro_ini <- readIni(pathIni = "input/hydro/hydro")

  # for every areas 
hydro_params <- c('use heuristic', 'follow load', "reservoir")
hydro_ini[hydro_params]

# weekly rules for mingen checks
hydro_ini$`follow load`[[getAreas()[1]]] <- FALSE

# annual rules for mingen checks
hydro_ini$reservoir[[getAreas()[2]]] <- TRUE

  # create section [reservoir capacity]
section_name <- "reservoir capacity"
hydro_ini[[section_name]] <- list(area_name = 11840000)

names(hydro_ini[[section_name]]) <- getAreas()[2]

# add new section to write
hydro_params <- append(hydro_params, section_name)

# last area is on monthly mode

  ## write properties ----
lapply(getAreas(), function(x){
  writeIniHydro(area = x, 
                params= lapply(hydro_ini[hydro_params], 
                               `[[`, 
                               x))
})

  ## write data ----
  ### TS + max power + mingen ----
  # write TS (mod file only) + max power + mingen
mod_data = matrix(60,365,5)

# max power is only first column
study_path <- simOptions()
study_path <- study_path$inputPath
max_power_file_path <- file.path(study_path, 
                                 "hydro", 
                                 "common", 
                                 "capacity", 
                                 "maxpower_at.txt")  
maxpower_data_origin <- antaresRead:::fread_antares(opts = simOptions(), 
                                                    file = max_power_file_path)
maxpower_data <- rep(80,365)
maxpower_data_upgrade <- maxpower_data_origin
maxpower_data_upgrade$V1 <- maxpower_data

# mingen data
mingen_data = matrix(2,8760,5)

lapply(getAreas(), function(x){
  writeInputTS(area = x, type = "hydroSTOR", 
               data = mod_data, 
               overwrite = TRUE)
  
  writeHydroValues(area = x, 
                   type = "maxpower", 
                   data = maxpower_data_upgrade)
  
  writeInputTS(area = x, type = "mingen", 
               data = mingen_data, 
               overwrite = TRUE)
})


# wind ----

# solar ----

# general data ----

general_data_file <- readIni("settings/generaldata")
  
  # /!\/!\/!\
  # click "run time series" to run simulation
  # this part is for time series generated by solver for a simulation
  # Input time series are generated 

# values_generate <- c("thermal, hydro", "load", "st-storage")
value_nb_year <- 2
active_year_by_year <- "true"

# section [general]
# updateGeneralSettings(generate = values_generate)
updateGeneralSettings(nbyears = value_nb_year, 
                      year.by.year = active_year_by_year)

# section [input]
# updateInputSettings(import = values_generate)

# read generaldata 
general_data_file_updated <- readIni("settings/generaldata")

# compare files
waldo::compare(general_data_file, 
               general_data_file_updated)

# scenario builder ----


# delete study ----
# deleteStudy()


##
# POST UPDATE ----
##

# edit binding values to v870 format
  # provide antares study in your $HOME env
study_path <- file.path("~", name)
setSimulationPath(path = study_path)

file.path(study_path, "input", "bindingconstraints", "bc_1_lt.txt")
path_file_bc <- file.path(study_path, "input", "bindingconstraints", "bc_1_lt.txt")
#  update biding file to add time series

bc_1 <- antaresRead:::fread_antares(file = path_file_bc, 
                            opts = simOptions())

bc_up <- cbind(bc_1, as.integer(c(equal, rep(0, 24))))

data.table::fwrite(x = bc_up, 
                   file = path_file_bc, 
                   sep = "\t", 
                   col.names = FALSE)

  # /!\/!\/!\ edit group to "group_test" in study

# edit scenariobuilder file

# bc,group_test,0 = 1
# bc,group_test,1 = 2

# make tar.gz archive with following name 
  # "test_case_study_v870"
