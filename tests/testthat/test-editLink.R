
test_that("Edit a link filters", {
  
  pasteVectorItemsWithComma <- function(x) paste(x,collapse=", ")
  
  setup_study_860(sourcedir860)
  suppressWarnings(
    opts_test <- setSimulationPath(study_temp_path,simulation="input")
  )
  
  opts_test <- createArea(name="area1",opts=opts_test)
  opts_test <- createArea(name="area2",opts=opts_test)
  opts_test <- createLink(from="area1",to="area2",opts=opts_test)
  
  new_filtering_synthesis <- c("hourly","daily")
  new_filtering_year_by_year <- c("hourly","daily")
  
  
  link_test <- getGeographicTrimming(areas="area1",opts=opts_test)$links$`area1 - area2`
  
  testthat::expect_false(
    link_test$`filter-synthesis`==pasteVectorItemsWithComma(new_filtering_synthesis) &&
      link_test$`filter-year-by-year`==pasteVectorItemsWithComma(new_filtering_year_by_year)
  )
  
  
  opts_test <- editLink(
    from="area1",
    to="area2",
    filter_year_by_year=new_filtering_year_by_year,
    filter_synthesis=new_filtering_synthesis,
    opts=opts_test
  ) 
  
  new_link_test <- getGeographicTrimming(areas="area1",opts=opts_test)$links$`area1 - area2`
  
  testthat::expect_true(
    new_link_test$`filter-synthesis`==pasteVectorItemsWithComma(new_filtering_synthesis) &&
      new_link_test$`filter-year-by-year`==pasteVectorItemsWithComma(new_filtering_year_by_year)
  )
  
})