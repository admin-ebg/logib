# Test the expected output of a salary analysis using the internal test data
test_that(paste0("standard_analysis() returns an object of type ",
                 "standard_analysis_model"), {
                   results <- standard_analysis(test_data, 2019, 2, 1)
                   expect_s3_class(results, "standard_analysis_model")
                 })
test_that("standard_analysis() returns a list object", {
  results <- standard_analysis(test_data, 2019, 2, 1)
  expect_type(results, "list")
})
