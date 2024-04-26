# create study version 8.7.0 for test

# params ----
version <- "8.7.0"
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