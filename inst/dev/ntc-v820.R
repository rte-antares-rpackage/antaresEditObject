

pkgload::load_all()

createStudy(path = "../etude_antares/", study_name = "ntc006", antares_version = 820)

opt <- setSimulationPath("../etude_antares/ntc006/")


createArea(name = "area01", localization = c(-100, -100))
createArea(name = "area02", localization = c(1, 1))
createArea(name = "area03", localization = c(100, 100))
createArea(name = "area04", localization = c(0, 200))
createArea(name = "area05", localization = c(0, -400))

createLink(
  from = "area01", 
  to = "area02", 
  dataLink = matrix(data = c(rep(9, 8760*2), rep(6, 8760*6)), ncol = 8)
)

createLink(
  from = "area02", 
  to = "area03", 
  dataLink = NULL
)


createLink(
  from = "area03", 
  to = "area04", 
  dataLink = matrix(data = c(rep(4, 8760*6)), ncol = 6)
)

createLink(
  from = "area04", 
  to = "area05", 
  dataLink = matrix(data = c(rep(6, 8760*6)), ncol = 6),
  tsLink = matrix(data = c(rep(9, 8760*10), rep(6, 8760*10)), ncol = 20)
)






