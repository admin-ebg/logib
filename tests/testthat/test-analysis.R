# Test the expected output of a salary analysis using the internal data
test_that(paste0("analysis() returns an object of type ",
                 "analysis_model"), {
                   results <- analysis(datalist_example, reference_month = 1,
                                       reference_year = 2019,
                                       usual_weekly_hours = 40,
                                       female_spec = "F", male_spec = "M",
                                       age_spec = "age")
                   expect_s3_class(results, "analysis_model")
                 })

test_that("analysis() returns a list object", {
  results <- analysis(datalist_example, reference_month = 1,
                      reference_year = 2019,
                      usual_weekly_hours = 40,
                      female_spec = "F", male_spec = "M",
                      age_spec = "age")
  expect_type(results, "list")
})

test_that("analysis() returns the correct results using internal data", {
  results <- analysis(datalist_example, reference_month = 1,
                      reference_year = 2019,
                      usual_weekly_hours = 40,
                      female_spec = "F", male_spec = "M",
                      age_spec = "age")
  coef_sex_f <- results$results$coefficients["sexF"]
  se_sex_f <- summary(results$results)$coefficients["sexF", 2]
  expect_true(all(round(coef_sex_f, 3) == -0.026,
                  round(se_sex_f, 3) == 0.003,
                  summary(results$results)$df[2] == 267,
                  round(summary(results$results)$r.squared, 3) == 0.991,
                  round(abs(summary(results$results)$coefficients["sexF", 3]), 3) == 7.925,
                  as.numeric(sprintf("%.3f", get_kennedy_estimator(coef_sex_f, se_sex_f^2))) == -0.026))
})

# Test the expected output of a salary analysis using the downloaded example data
test_that("analysis() returns the correct results using downloaded example data DE", {
  download_example_datalist(testthat::test_path("Beispiel_Datenblatt_M1.xlsx"),
                            language = "de")
  datalist_download <- read_data(testthat::test_path("Beispiel_Datenblatt_M1.xlsx"))
  results <- analysis(datalist_example,
                      reference_month = 1, reference_year = 2019,
                      usual_weekly_hours = 40,
                      female_spec = "F", male_spec = "M",
                      age_spec = "age")
  coef_sex_f <- results$results$coefficients["sexF"]
  se_sex_f <- summary(results$results)$coefficients["sexF", 2]
  expect_true(all(round(coef_sex_f, 3) == -0.026,
                  round(se_sex_f, 3) == 0.003,
                  summary(results$results)$df[2] == 267,
                  round(summary(results$results)$r.squared, 3) == 0.991,
                  round(abs(summary(results$results)$coefficients["sexF", 3]), 3) == 7.925,
                  as.numeric(sprintf("%.3f", get_kennedy_estimator(coef_sex_f, se_sex_f^2))) == -0.026))
})

test_that("analysis() returns the correct results using downloaded example data FR", {
  download_example_datalist(testthat::test_path("Exemple_feuille_de_donnees_M1.xlsx"),
                            language = "fr")
  datalist_download <- read_data(testthat::test_path("Exemple_feuille_de_donnees_M1.xlsx"))
  results <- analysis(datalist_example,
                      reference_month = 1, reference_year = 2019,
                      usual_weekly_hours = 40,
                      female_spec = "F", male_spec = "M",
                      age_spec = "age")
  coef_sex_f <- results$results$coefficients["sexF"]
  se_sex_f <- summary(results$results)$coefficients["sexF", 2]
  expect_true(all(round(coef_sex_f, 3) == -0.026,
                  round(se_sex_f, 3) == 0.003,
                  summary(results$results)$df[2] == 267,
                  round(summary(results$results)$r.squared, 3) == 0.991,
                  round(abs(summary(results$results)$coefficients["sexF", 3]), 3) == 7.925,
                  as.numeric(sprintf("%.3f", get_kennedy_estimator(coef_sex_f, se_sex_f^2))) == -0.026))
})

test_that("analysis() returns the correct results using downloaded example data IT", {
  download_example_datalist(testthat::test_path("Esempio_foglio_di_dati_M1.xlsx"),
                            language = "it")
  datalist_download <- read_data(testthat::test_path("Esempio_foglio_di_dati_M1.xlsx"))
  results <- analysis(datalist_example,
                      reference_month = 1, reference_year = 2019,
                      usual_weekly_hours = 40,
                      female_spec = "F", male_spec = "M",
                      age_spec = "age")
  coef_sex_f <- results$results$coefficients["sexF"]
  se_sex_f <- summary(results$results)$coefficients["sexF", 2]
  expect_true(all(round(coef_sex_f, 3) == -0.026,
                  round(se_sex_f, 3) == 0.003,
                  summary(results$results)$df[2] == 267,
                  round(summary(results$results)$r.squared, 3) == 0.991,
                  round(abs(summary(results$results)$coefficients["sexF", 3]), 3) == 7.925,
                  as.numeric(sprintf("%.3f", get_kennedy_estimator(coef_sex_f, se_sex_f^2))) == -0.026))
})

test_that("analysis() returns the correct results using downloaded example data EN", {
  download_example_datalist(testthat::test_path("Example_data_sheet_M1.xlsx"),
                            language = "en")
  datalist_download <- read_data(testthat::test_path("Example_data_sheet_M1.xlsx"))
  results <- analysis(datalist_example,
                      reference_month = 1, reference_year = 2019,
                      usual_weekly_hours = 40,
                      female_spec = "F", male_spec = "M",
                      age_spec = "age")
  coef_sex_f <- results$results$coefficients["sexF"]
  se_sex_f <- summary(results$results)$coefficients["sexF", 2]
  expect_true(all(round(coef_sex_f, 3) == -0.026,
                  round(se_sex_f, 3) == 0.003,
                  summary(results$results)$df[2] == 267,
                  round(summary(results$results)$r.squared, 3) == 0.991,
                  round(abs(summary(results$results)$coefficients["sexF", 3]), 3) == 7.925,
                  as.numeric(sprintf("%.3f", get_kennedy_estimator(coef_sex_f, se_sex_f^2))) == -0.026))
})

