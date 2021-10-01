

context("Function createPSP")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  test_that("Create a new weekly PSP ", {
    pspData<-data.frame(area=c("a", "b"), installedCapacity=c(800,900))
    
    opts <- antaresRead::setSimulationPath(studyPath, 'input')
    createPSP(areasAndCapacities=pspData, efficiency = 0.8, opts = opts)
    
    expect_true("psp_in_w" %in% antaresRead::getAreas())
    expect_true("psp_out_w" %in% antaresRead::getAreas())
    expect_true("a - psp_in_w" %in% antaresRead::getLinks())
    expect_true("a - psp_out_w" %in% antaresRead::getLinks())
    
    opts <- antaresRead::setSimulationPath(studyPath, 'input')
    capaPSP<-readInputTS(linkCapacity = "a - psp_out_w", showProgress = FALSE, opts = opts)
    expect_equal(unique(capaPSP$transCapacityIndirect), 800)
    expect_equal(unique(capaPSP$hurdlesCostIndirect), 0.0005)
    
    opts <- antaresRead::setSimulationPath(studyPath, 'input')
    binding<-readBindingConstraints(opts = opts)
    #for R CMD Check 
    if (is.na(binding$a_psp_weekly$coefs["a%psp_in_w"])){
      efficiencyTest<-as.double(binding$a_psp_weekly$coefs["psp_in_w%a"])
    } else{
      efficiencyTest<-as.double(binding$a_psp_weekly$coefs["a%psp_in_w"])
    }
    
    expect_equal(efficiencyTest, 0.8)
    expect_equal(binding$a_psp_weekly$operator, "equal")
    expect_equal(binding$a_psp_weekly$timeStep, "weekly")
    expect_equal(binding$a_psp_weekly$enabled, TRUE)
    
  })
  
  test_that("Overwrite a PSP ",{
    pspData<-data.frame(area=c("a", "b"), installedCapacity = c(800, 900))
    createPSP(pspData, efficiency = 0.75, overwrite = TRUE, hurdleCost = 0.1, opts = opts)
    
    opts <- antaresRead::setSimulationPath(studyPath, 'input')
    capaPSP<-readInputTS(linkCapacity = "a - psp_out_w", showProgress = FALSE, opts = opts)
    expect_equal(unique(capaPSP$hurdlesCostIndirect), 0.1)
    
    opts <- antaresRead::setSimulationPath(studyPath, 'input')
    binding<-readBindingConstraints(opts = opts)
    efficiencyTest<-as.double(as.double(binding$a_psp_weekly$coefs["a%psp_in_w"])+as.double(binding$a_psp_weekly$coefs["psp_in_w%a"]))
    
    #for R CMD Check 
    if (is.na(binding$a_psp_weekly$coefs["a%psp_in_w"])){
      efficiencyTest<-as.double(binding$a_psp_weekly$coefs["psp_in_w%a"])
    } else{
      efficiencyTest<-as.double(binding$a_psp_weekly$coefs["a%psp_in_w"])
    }
    expect_equal(efficiencyTest, 0.75)
  })
  
  test_that(" create a daily PSP ", {
    pspData<-data.frame(area=c("a", "b"), installedCapacity=c(600,523))
    createPSP(pspData, efficiency = 0.66, timeStepBindConstraint = "daily", hurdleCost = 5)
    
    expect_true("psp_in_d" %in% antaresRead::getAreas())
    expect_true("psp_out_d" %in% antaresRead::getAreas())
    expect_true("b - psp_in_d" %in% antaresRead::getLinks())
    expect_true("b - psp_out_d" %in% antaresRead::getLinks())
    
    capaPSP<-readInputTS(linkCapacity = "b - psp_out_d", showProgress = FALSE)
    expect_equal(unique(capaPSP$transCapacityIndirect), 523)
    expect_equal(unique(capaPSP$hurdlesCostIndirect), 5)
    
    binding<-readBindingConstraints()
    
    #for R CMD Check 
    if (is.na(binding$b_psp_daily$coefs["b%psp_in_d"])){
      efficiencyTest<-as.double(binding$b_psp_daily$coefs["psp_in_d%b"])
    } else{
      efficiencyTest<-as.double(binding$b_psp_daily$coefs["b%psp_in_d"])
    }
    expect_equal(efficiencyTest, 0.66)
    expect_equal(binding$b_psp_daily$operator, "equal")
    expect_equal(binding$b_psp_daily$timeStep, "daily")
    expect_equal(binding$b_psp_daily$enabled, TRUE)
    
  })
  
  test_that(" test incorrect data ", {
    pspData<-data.frame(area=c("a", "b"), installedCapacity=c(800,900))
    
    #incorrect timeStepBindConstraint
    expect_error(createPSP(pspData, efficiency = 0.75, timeStepBindConstraint = "annual"), 
                 "timeStepBindConstraint is not equal to weekly or daily.")
    expect_error(createPSP(pspData, efficiency = 0.75, timeStepBindConstraint = 988), 
                 "timeStepBindConstraint is not equal to weekly or daily.")
    
    #incorrect efficency 
    expect_error(createPSP(pspData), "efficiency is set to NULL")
    expect_error(createPSP(pspData, efficiency = "Batman"), "efficiency is not a double.")
    
    #wrong pspData
    pspDataWrong<-data.frame(area=c("apop", "ssb"), installedCapacity=c(800,900))
    expect_error(createPSP(pspDataWrong, efficiency = 0.75), "apop is not a valid area.")
    
    #incorrect pumping name 
    expect_error(createPSP(pspData, efficiency = 0.75, namePumping = 988), 
                 "One of the pumping or turbining name is not a character.")
    expect_error(createPSP(pspData, efficiency = 0.75, namePumping = NULL), 
                 "One of the pumping or turbining name is set to NULL")
    
    #incorrect hurdle cost
    expect_error(createPSP(pspData, efficiency = 0.75, hurdleCost = "988"), 
                 "hurdleCost is not a double.")
    
    #incorrect areasAndCapacities
    expect_error(createPSP(c(5,9), efficiency = 0.75), 
                 "areasAndCapacities must be a data.frame")
    
    expect_error(createPSP(data.frame(voiture=c(87,98)), efficiency = 0.75), 
                 "areasAndCapacities must be a data.frame with a column area")
    expect_error(createPSP(data.frame(area=c(87,98)), efficiency = 0.75), 
                 "areasAndCapacities must be a data.frame with a column installedCapacity")
    
  })
  
  test_that("create a psp with a long name ", {
    #after p, we change the link direction
    areaName<-"suisse"
    createArea(areaName, overwrite = TRUE)
    pspData<-data.frame(area=c(areaName), installedCapacity=c(9856))
    createPSP(pspData, efficiency = 0.5, overwrite = TRUE, timeStepBindConstraint = "daily")
    
    expect_true("psp_in_d" %in% antaresRead::getAreas())
    expect_true("psp_out_d" %in% antaresRead::getAreas())
    expect_true("psp_in_d - suisse" %in% antaresRead::getLinks())
    expect_true("psp_out_d - suisse" %in% antaresRead::getLinks())
    
    capaPSP<-readInputTS(linkCapacity = "psp_out_d - suisse", showProgress = FALSE)
    expect_equal(unique(capaPSP$transCapacityDirect), 9856)
    expect_equal(unique(capaPSP$hurdlesCostIndirect), 0.0005)
    
    binding<-readBindingConstraints()
    expect_equal(as.double(binding$suisse_psp_daily$coefs["psp_in_d%suisse"]), 0.5)
    expect_equal(binding$suisse_psp_daily$operator, "equal")
    expect_equal(binding$suisse_psp_daily$timeStep, "daily")
    expect_equal(binding$suisse_psp_daily$enabled, TRUE)
  })
  
  test_that("Get and set the PSP ", {
    
    expect_error(editPSP("lp"))
    
    #after p, we change the link direction
    areaName<-"suisse"
    createArea(areaName, overwrite = TRUE)
    pspData<-data.frame(area=c(areaName), installedCapacity=c(9856))
    opts <- antaresRead::setSimulationPath(studyPath, 'input')
    createPSP(pspData, efficiency = 0.5, overwrite = TRUE, timeStepBindConstraint = "daily")
    expect_equal(getCapacityPSP(areaName, timeStepBindConstraint = "daily"), 9856)
    
    opts <- antaresRead::setSimulationPath(studyPath, 'input')
    pspData<-data.frame(area=c("a", "b"), installedCapacity = c(800, 900))
    createPSP(pspData, efficiency = 0.75, overwrite = TRUE, hurdleCost = 0.1, opts = opts)
    opts2<-editPSP("a", 8000)
    #ERROR in R CMD check 
    #expect_equal(getCapacityPSP("a", opts = opts2), 8000)
    
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})
