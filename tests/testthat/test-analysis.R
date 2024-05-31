# Test the expected output of a salary analysis using the internal test data
test_that(paste0("analysis() returns an object of type ",
                 "analysis_model"), {
                   results <- analysis(datalist_example, reference_month = 1,
                                       reference_year = 2019,
                                       usual_weekly_hours = 40,
                                       female_spec = 1, male_spec = 2)
                   expect_s3_class(results, "analysis_model")
                 })

test_that("analysis() returns a list object", {
  results <- analysis(datalist_example, reference_month = 1,
                      reference_year = 2019,
                      usual_weekly_hours = 40,
                      female_spec = 1, male_spec = 2)
  expect_type(results, "list")
})

test_that("analysis() returns the correct results", {
  results <- analysis(read_data(testthat::test_path("Exportfile_M1_de.xlsx")),
                      reference_month = 1, reference_year = 2019,
                      usual_weekly_hours = 40, female_spec = "F",
                      male_spec = "M")
  coef_sex_f <- results$results$coefficients["sexF"]
  se_sex_f <- summary(results$results)$coefficients["sexF", 2]
  expect_true(all(round(coef_sex_f, 3) == -0.042,
                  round(se_sex_f, 3) == 0.019,
                  summary(results$results)$df[2] == 148,
                  round(summary(results$results)$r.squared, 3) == 0.88,
                  round(abs(summary(results$results)$coefficients["sexF", 3]), 3) == 2.172,
                  as.numeric(sprintf("%.3f", get_kennedy_estimator(coef_sex_f, se_sex_f^2))) == -0.041))
})
