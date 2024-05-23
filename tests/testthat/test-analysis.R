# Test the expected output of a salary analysis using the internal test data
test_that(paste0("analysis() returns an object of type ",
                 "analysis_model"), {
                   results <- analysis(datalist_example, 1, 2019, "F", "M")
                   expect_s3_class(results, "analysis_model")
                 })

test_that("analysis() returns a list object", {
  results <- analysis(datalist_example, 1, 2019, "F", "M")
  expect_type(results, "list")
})
