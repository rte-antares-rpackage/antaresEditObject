
test_that("Edit a link filters", {
  
  pasteVectorItemsWithComma <- function(x) paste(x,collapse=", ")

  opts_test <- suppressWarnings(createStudy(path = tempdir(), 
                                            study_name = "edit-link", 
                                            antares_version = "8.6.0"
                                            )
                                )

  opts_test <- createArea(name = "area1", opts = opts_test)
  opts_test <- createArea(name = "area2", opts = opts_test)
  opts_test <- createArea(name = "area3", opts = opts_test)
  opts_test <- createLink(from = "area1", to = "area2", opts = opts_test)
  opts_test <- createLink(from = "area1", to = "area3", opts = opts_test)
  
  new_filtering_synthesis <- c("hourly", "daily")
  new_filtering_year_by_year <- c("hourly", "daily")
  
  
  link_test <- getGeographicTrimming(areas = "area1", opts = opts_test)[["links"]][["area1 - area2"]]
  
  testthat::expect_false(
    link_test$`filter-synthesis`==pasteVectorItemsWithComma(new_filtering_synthesis) &&
      link_test$`filter-year-by-year`==pasteVectorItemsWithComma(new_filtering_year_by_year)
  )
  
  opts_test <- editLink(
    from = "area1",
    to = "area2",
    filter_year_by_year = new_filtering_year_by_year,
    filter_synthesis = new_filtering_synthesis,
    opts = opts_test
  ) 
  
  new_link_test <- getGeographicTrimming(areas = "area1", opts = opts_test)[["links"]][["area1 - area2"]]
  
  testthat::expect_true(
    new_link_test$`filter-synthesis`==pasteVectorItemsWithComma(new_filtering_synthesis) &&
      new_link_test$`filter-year-by-year`==pasteVectorItemsWithComma(new_filtering_year_by_year)
  )
  
  # Default case : filter_synthesis/filter_year_by_year NULL
  # The goal is to test that those two properties are not overwritten if NULL is provided.
  geo_before <- getGeographicTrimming(areas = "area1", opts = opts_test)
  geo_before_target_link <- geo_before[["links"]][["area1 - area3"]]
  
  ncol <- 2
  new_tsLink <- matrix(rep(1, 8760 * ncol), ncol = ncol) 
  opts_test <- editLink(
    from = "area1",
    to = "area3",
    tsLink = new_tsLink,
    opts = opts_test
  ) 
  
  geo_after <- getGeographicTrimming(areas = "area1", opts = opts_test)
  geo_after_target_link <- geo_after[["links"]][["area1 - area3"]]
  
  expect_true(geo_before_target_link[["filter-year-by-year"]] == geo_after_target_link[["filter-year-by-year"]])
  expect_true(geo_before_target_link[["filter-synthesis"]] == geo_after_target_link[["filter-synthesis"]])
})