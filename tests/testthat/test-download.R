# Test whether datalists can be downloaded and have correct sheet and variable names
test_that("Datalist DE can be downloaded and has correct sheet names", {
  url <- "https://logib.admin.ch/assets/Data/Datalist_d.xlsx"
  download.file(url, testthat::test_path("Datalist_d.xlsx"), mode = "wb")
  sheet_names <- readxl::excel_sheets(testthat::test_path("Datalist_d.xlsx"))
  expect_true(all(sheet_names == c("Generelle_Angaben", "Vorlage_Datenblatt")))
})

test_that("Datalist DE can be downloaded and has correct variable names", {
  url <- "https://logib.admin.ch/assets/Data/Datalist_d.xlsx"
  download.file(url, testthat::test_path("Datalist_d.xlsx"), mode = "wb")
  var_names_download <- names(readxl::read_excel(testthat::test_path("Datalist_d.xlsx"), sheet = 2))
  var_names <- names(readxl::read_excel(testthat::test_path("datalist_de.xlsx"), sheet = 2))
  expect_true(all(var_names_download == var_names))
})

test_that("Datalist FE can be downloaded and has correct sheet names", {
  url <- "https://logib.admin.ch/assets/Data/Datalist_f.xlsx"
  download.file(url, testthat::test_path("Datalist_f.xlsx"), mode = "wb")
  sheet_names <- readxl::excel_sheets(testthat::test_path("Datalist_f.xlsx"))
  expect_true(all(sheet_names == c("Donn\u00E9es_g\u00E9n\u00E9rales", "mod\u00E8le_de_feuille_de_donn\u00E9es")))
})

test_that("Datalist FR can be downloaded and has correct variable names", {
  url <- "https://logib.admin.ch/assets/Data/Datalist_f.xlsx"
  download.file(url, testthat::test_path("Datalist_f.xlsx"), mode = "wb")
  var_names_download <- names(readxl::read_excel(testthat::test_path("Datalist_f.xlsx"), sheet = 2))
  var_names <- names(readxl::read_excel(testthat::test_path("datalist_fr.xlsx"), sheet = 2))
  expect_true(all(var_names_download == var_names))
})

test_that("Datalist IT can be downloaded and has correct sheet names", {
  url <- "https://logib.admin.ch/assets/Data/Datalist_i.xlsx"
  download.file(url, testthat::test_path("Datalist_i.xlsx"), mode = "wb")
  sheet_names <- readxl::excel_sheets(testthat::test_path("Datalist_i.xlsx"))
  expect_true(all(sheet_names == c("Dati_generali", "modello_del_foglio_di_dati")))
})

test_that("Datalist IT can be downloaded and has correct variable names", {
  url <- "https://logib.admin.ch/assets/Data/Datalist_i.xlsx"
  download.file(url, testthat::test_path("Datalist_i.xlsx"), mode = "wb")
  var_names_download <- names(readxl::read_excel(testthat::test_path("Datalist_i.xlsx"), sheet = 2))
  var_names <- names(readxl::read_excel(testthat::test_path("datalist_it.xlsx"), sheet = 2))
  expect_true(all(var_names_download == var_names))
})

test_that("Datalist EN can be downloaded and has correct sheet names", {
  url <- "https://logib.admin.ch/assets/Data/Datalist_e.xlsx"
  download.file(url, testthat::test_path("Datalist_e.xlsx"), mode = "wb")
  sheet_names <- readxl::excel_sheets(testthat::test_path("Datalist_e.xlsx"))
  expect_true(all(sheet_names == c("General_information", "data_sheet_template")))
})

test_that("Datalist EN can be downloaded and has correct variable names", {
  url <- "https://logib.admin.ch/assets/Data/Datalist_e.xlsx"
  download.file(url, testthat::test_path("Datalist_e.xlsx"), mode = "wb")
  var_names_download <- names(readxl::read_excel(testthat::test_path("Datalist_e.xlsx"), sheet = 2))
  var_names <- names(readxl::read_excel(testthat::test_path("datalist_en.xlsx"), sheet = 2))
  expect_true(all(var_names_download == var_names))
})

