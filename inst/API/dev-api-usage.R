
# Path to our study
# opts <- antaresRead::setSimulationPathAPI(
#   host = "http://localhost:8080",
#   study_id = "70a08fae-da67-444a-b2ed-df4c0f956a31", 
#   token = NULL, 
#   simulation = "input"
# )
# 
# # temp
# opts$host <- "http://localhost:8080"
# opts$study_id <- "70a08fae-da67-444a-b2ed-df4c0f956a31"
# options(antares = opts)

mockSimulationAPI()

# Set mode you want to use
# setAPImode("sync") # send all commands to API
# setAPImode("async") # just record all commands


# Create a New Variant
createVariant("variant-1")

# or use a pre-existing one:
# useVariant("variant-2")


# Create a new area
createArea(name = "earth")

# List commands recorded (one normally at this point)
getVariantCommands()

# Create another area with more options
createArea(
  name = "moon", 
  filtering = filteringOptions(filter_synthesis = c("hourly", "daily"))
)

# List commands
getVariantCommands()




