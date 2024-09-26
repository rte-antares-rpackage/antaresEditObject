
test_that("Control the basic behaviour of .format_ini_rhs()",{
  
  res <- .format_ini_rhs(value = TRUE)
  expect_true(res == "true")
  
  res <- .format_ini_rhs(value = FALSE)
  expect_true(res == "false")
  
  res <- .format_ini_rhs(value = "fake_value")
  expect_true(res == "fake_value")
  
  res <- .format_ini_rhs(value = letters[seq(1,5)])
  expect_true(res == paste(letters[seq(1,5)], collapse = ", "))  
})
