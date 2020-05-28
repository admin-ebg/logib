# Test the expected output of a salary analysis using the internal test data
test_that(paste0("analysis() returns an object of type ",
                 "analysis_model"), {
                   results <- standard_analysis(test_data, 2019, 2, 1)
                   expect_s3_class(results, "analysis_model")
                 })
test_that("analysis() returns a list object", {
  results <- analysis(test_data, 2019, 2, 1)
  expect_type(results, "list")
})
