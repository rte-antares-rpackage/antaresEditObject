
# context("Function createDSR")
# 
# 
# sapply(studies, function(study) {
#   
#   setup_study(study, sourcedir)
#   opts <- antaresRead::setSimulationPath(studyPath, "input")
#   
#   
#   test_that("Create a new DSR ", {
#     dsrData<-data.frame(area = c("a", "b"), unit = c(10,20), nominalCapacity = c(100, 120), marginalCost = c(52, 65), hour = c(3, 7))
#     
#     #create virtual area                    
#     optsRes<-createDSR(dsrData)
#     expect_true("a_dsr_3h" %in% getAreas())
#     expect_true("b_dsr_7h" %in% getAreas())
#     
#     #create virtual link 
#     linkADsr<-"a - a_dsr_3h"
#     linkBDsr<-"b - b_dsr_7h"
#     expect_true(linkADsr %in% getLinks())
#     expect_true(linkBDsr %in% getLinks())
#     capaLink<-antaresRead::readInputTS(linkCapacity = c("a - a_dsr_3h", "b - b_dsr_7h"), showProgress = FALSE)
#     expect_equal(unique(capaLink[link==linkADsr, transCapacityIndirect]), dsrData[dsrData$area=="a",]$unit*dsrData[dsrData$area=="a",]$nominalCapacity)
#     expect_equal(unique(capaLink[link==linkBDsr, transCapacityIndirect]), dsrData[dsrData$area=="b",]$unit*dsrData[dsrData$area=="b",]$nominalCapacity)
#     
#     #create a virtual bindingConstraint
#     bindingList<-antaresRead::readBindingConstraints(opts = optsRes)
#     expect_true("a_dsr_3h" %in% names(bindingList))
#     expect_true("b_dsr_7h" %in% names(bindingList))
#     expect_equal(bindingList$a_dsr_3h$enabled, TRUE)
#     expect_equal(bindingList$a_dsr_3h$timeStep, "daily")
#     expect_equal(bindingList$a_dsr_3h$operator, "less")
#     expect_equal(as.double(bindingList$a_dsr_3h$coefs["a%a_dsr_3h"]), -1)
#     expect_equal(as.double(bindingList$b_dsr_7h$coefs["b%b_dsr_7h"]), -1)
#     expect_equal(nrow(bindingList$a_dsr_3h$values), 366)
# 		expect_equal(nrow(bindingList$b_dsr_7h$values), 366)
# 		
#     expect_equal(unique(bindingList$a_dsr_3h$values$less)[1], dsrData[dsrData$area=="a",]$unit*dsrData[dsrData$area=="a",]$nominalCapacity*dsrData[dsrData$area=="a",]$hour)
#     expect_equal(unique(bindingList$b_dsr_7h$values$less)[1], dsrData[dsrData$area=="b",]$unit*dsrData[dsrData$area=="b",]$nominalCapacity*dsrData[dsrData$area=="b",]$hour)
#     
#     #create a virtual cluster
#     clusterList <- antaresRead::readClusterDesc(opts = optsRes)
#     expect_equal(as.character(clusterList[area == "a_dsr_3h"]$cluster), "a_dsr_3h_cluster")
#     expect_equal(as.character(clusterList[area == "a_dsr_3h"]$group), "Other")
#     expect_equal(clusterList[area == "a_dsr_3h"]$enabled, TRUE)
#     expect_equal(clusterList[area == "a_dsr_3h"]$unitcount, dsrData[dsrData$area=="a",]$unit)
#     expect_equal(clusterList[area == "a_dsr_3h"]$spinning, 2)
#     expect_equal(clusterList[area == "a_dsr_3h"]$nominalcapacity, dsrData[dsrData$area=="a",]$nominalCapacity)
#     expect_equal(clusterList[area == "a_dsr_3h"]$marginal.cost, dsrData[dsrData$area=="a",]$marginalCost)
#     
#   })
#   
#   # test_that("overwrite a DSR ", {
#     # dsrData<-data.frame(area = c("a", "b"), unit = c(52,36), nominalCapacity = c(956, 478), marginalCost = c(52, 65), hour = c(3, 7))
#     
#     # expect_error(suppressWarnings(createDSR(dsrData)), "The link a - a_dsr_3h already exist, use overwrite.")
#     
#     # createDSR(dsrData, overwrite = TRUE)
#     # linkADsr <- "a - a_dsr_3h"
#     # linkBDsr <- "b - b_dsr_7h"
#     # expect_true(linkADsr %in% getLinks())
#     # expect_true(linkBDsr %in% getLinks())
#     # capaLink<-antaresRead::readInputTS(linkCapacity = c("a - a_dsr_3h", "b - b_dsr_7h"), showProgress = FALSE)
#     # expect_equal(unique(capaLink[link==linkADsr, transCapacityIndirect]), dsrData[dsrData$area=="a",]$unit*dsrData[dsrData$area=="a",]$nominalCapacity)
#     # expect_equal(unique(capaLink[link==linkBDsr, transCapacityIndirect]), dsrData[dsrData$area=="b",]$unit*dsrData[dsrData$area=="b",]$nominalCapacity)
#     
#     # #edit spinning
#     # optsRes <- createDSR(dsrData, overwrite = TRUE, spinning = 3)
#     # clusterList <- antaresRead::readClusterDesc(opts = optsRes)
#     # expect_equal(as.character(clusterList[area == "a_dsr_3h"]$cluster), "a_dsr_3h_cluster")
#     # expect_equal(as.character(clusterList[area == "a_dsr_3h"]$group), "Other")
#     # expect_equal(as.double(clusterList[area == "a_dsr_3h"]$spinning), 3)
#     
#   # })
#   
#   test_that("test input data DSR", {
#     #area
#     dsrData<-data.frame(zone = c("a", "b"), unit = c(10,20), nominalCapacity = c(100, 120), marginalCost = c(52, 65), hour = c(3, 7))
#     expect_error(createDSR(dsrData), "areasAndDSRParam must be a data.frame with a column area, unit, nominalCapacity, marginalCost and hour")
#     #unit
#     dsrData<-data.frame(area = c("a", "b"), un = c(10,20), nominalCapacity = c(100, 120), marginalCost = c(52, 65), hour = c(3, 7))
#     expect_error(createDSR(dsrData), "areasAndDSRParam must be a data.frame with a column area, unit, nominalCapacity, marginalCost and hour")
#     #nominalCapacity
#     dsrData<-data.frame(area = c("a", "b"), unit = c(10,20), nominalapacity = c(100, 120), marginalCost = c(52, 65), hour = c(3, 7))
#     expect_error(createDSR(dsrData), "areasAndDSRParam must be a data.frame with a column area, unit, nominalCapacity, marginalCost and hour")
#     #marginalCost
#     dsrData<-data.frame(area = c("a", "b"), unit = c(10,20), nominalCapacity = c(100, 120), marginlCost = c(52, 65), hour = c(3, 7))
#     expect_error(createDSR(dsrData), "areasAndDSRParam must be a data.frame with a column area, unit, nominalCapacity, marginalCost and hour")
#     #hour
#     dsrData<-data.frame(area = c("a", "b"), unit = c(10,20), nominalCapacity = c(100, 120), marginalCost = c(52, 65), houor = c(3, 7))
#     expect_error(createDSR(dsrData), "areasAndDSRParam must be a data.frame with a column area, unit, nominalCapacity, marginalCost and hour")
#     #class
#     dsrData<-c(area = c("a", "b"), unit = c(10,20), nominalCapacity = c(100, 120), marginalCost = c(52, 65), hour = c(3, 7))
#     expect_error(createDSR(dsrData), "areasAndDSRParam must be a data.frame")
#     #area zz not in getAreas
#     dsrData<-data.frame(area = c("zz", "b"), unit = c(10,20), nominalCapacity = c(100, 120), marginalCost = c(52, 65), hour = c(3, 7))
#     expect_error(createDSR(dsrData), "zz is not a valid area.") 
#     #spinning 
#     dsrData<-data.frame(area = c("a", "b"), unit = c(10,20), nominalCapacity = c(100, 120), marginalCost = c(52, 65), hour = c(3, 7))
#     expect_error(createDSR(dsrData, overwrite = TRUE, spinning = "fr"), "spinning is not a double.") 
#     expect_error(createDSR(dsrData, overwrite = TRUE, spinning = NULL), "spinning is set to NULL")
#   })
#   
#   # test_that("getCapacityDSR and editDSR", {
#     # dsrData<-data.frame(area = c("a", "b"), unit = c(50,40), nominalCapacity = c(200, 600), marginalCost = c(52, 65), hour = c(3, 7))
#     # createDSR(dsrData, overwrite = TRUE)
#     
#     # expect_equal(getCapacityDSR("a"),  dsrData[dsrData$area=="a",]$nominalCapacity *  dsrData[dsrData$area=="a",]$unit )
#     # expect_equal(getCapacityDSR("b"),  dsrData[dsrData$area=="b",]$nominalCapacity *  dsrData[dsrData$area=="b",]$unit )
#     
#     # optsRes<-editDSR(area = "a", 
#                      # unit = 2, 
#                      # nominalCapacity = 500,
#                      # marginalCost = 40,
#                      # spinning = 50)
#     
#     # #change for "a" but not for "b" 
#     # expect_equal(getCapacityDSR("a"), 2 * 500)
#     # expect_equal(getCapacityDSR("b"),  dsrData[dsrData$area=="b",]$nominalCapacity *  dsrData[dsrData$area=="b",]$unit )
#     # #get the new values
#     # clusterList <- antaresRead::readClusterDesc(opts = optsRes)
#     # dsrName <- "a_dsr_3h"
#     # expect_equal(as.character(clusterList[area == dsrName]$cluster), paste0(dsrName, "_cluster"))
#     # expect_equal(as.character(clusterList[area == dsrName]$group), "Other")
#     # expect_equal(clusterList[area == dsrName]$enabled, TRUE)
#     # expect_equal(clusterList[area == dsrName]$unitcount, 2)
#     # expect_equal(clusterList[area == dsrName]$spinning, 50)
#     # expect_equal(clusterList[area == dsrName]$nominalcapacity, 500)
#     # expect_equal(clusterList[area == dsrName]$marginal.cost, 40)
#   # })
#   
#   
#   
#   # remove temporary study
#   unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
#   
# })

