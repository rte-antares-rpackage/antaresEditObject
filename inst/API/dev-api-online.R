
# ONLINE ------------------------------------------------------------------

pkgload::load_all()


# Path to our study
opts <- antaresRead::setSimulationPathAPI(
  host = "http://localhost:8080",
  study_id = "0b26b7e9-57d3-47cb-82c6-d118427eaf7d",
  token = NULL,
  simulation = "input"
)
# temp (implemented in dev version of antaresRead)
opts$host <- "http://localhost:8080"
opts$study_id <- "0b26b7e9-57d3-47cb-82c6-d118427eaf7d"
options(antares = opts)

# send commands to API
setAPImode("sync")

# createVariant("myvariant")
useVariant("myvariant")

createArea(name = "area01")
getVariantCommands()
createArea(name = "area02")
createArea(name = "area03")

getVariantCommands()

createLink(from = "area01", to = "area02")
editLink(from = "area01", to = "area02", dataLink = matrix(data = c(rep(9, 8760*2), rep(6, 8760*6)), ncol = 8))

createLink(from = "area02", to = "area03")

getVariantCommands()

createCluster(
  area = "area01", 
  cluster_name = "clus05",
  unitcount = 1L,
  marginal_cost = 50,
  ts_interpretation = "production-factor",
  group = "Nuclear",
  add_prefix = FALSE,
  prepro_data = matrix(
    data = c(rep(9, times = 365 * 2),
             rep(7, times = 365 * 4)), 
    ncol = 6
  ),
  prepro_modulation = matrix(
    data = c(rep(8, times = 365 * 24 * 3),
             rep(6, times = 365 * 24 * 1)),
    ncol = 4
  ),
  time_series = matrix(
    data = c(rep(22, times = 365 * 24 * 8),
             rep(44, times = 365 * 24 * 4)),
    ncol = 12
  )
)


createCluster(
  area = "area02", 
  cluster_name = "clus01"
)
createCluster(
  area = "area02", 
  cluster_name = "clus02"
)

editCluster(
  area = "area02", 
  cluster_name = "clus02",
  unitcount = 12
)

editCluster(
  area = "area02", 
  cluster_name = "clus01",
  prepro_data = matrix(
    data = c(rep(9, times = 365 * 2),
             rep(7, times = 365 * 4)), 
    ncol = 6
  )
)


removeCluster("area02", "clus01")

ts <- matrix(rep(4, 8760*2), nrow = 8760)
writeInputTS("area01", type = "wind", data = ts)
writeInputTS("area01", type = "hydroROR", data = ts)



updateGeneralSettings(mode = "Adequacy")
updateGeneralSettings(generate = c("thermal", "hydro"))
updateGeneralSettings(mode = "Economy")


updateGeneralSettings(mode = "Draft", nbyears = 42)
getVariantCommands()

createBindingConstraint(
  name = "myconstraint2",
  # values = matrix(data = rep(0, 8760 * 3), ncol = 3),
  values = NULL,
  enabled = FALSE,
  timeStep = "hourly",
  operator = "both",
  coefficients = c("area01%area02" = 1)
)


getVariantCommands()



writeWaterValues("area01", data = matrix(rep(0, 365*101), nrow = 365))






