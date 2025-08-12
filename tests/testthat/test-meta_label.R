
# # ---------- Tests for meta_label() ----------
#
# --- Created: 2025-08-12
#
# --- Notes ---
#
# testing functionality, errors, and warnings (works as of 08/12)
# check with "devtools::test()" in console
#
# --- - - - - - - - - - -

## load tools ----------

# libraries
library(testthat)
library(wcmtools)
library(labelled)


# --- - - - - - - - - - -

# ---------- Functionality ----------
test_that("meta_label applies labels correctly", {

  yaml_path <- system.file("extdata", "codebook.yaml", package = "wcmtools")
  df <- data.frame(
    age = c(23, 25, 42),
    gender = c(1, 2, 1),
    education = c(2, 3, 3)
  ) |>
    meta_label(yaml_path)

  # labels applied correctly
  expect_equal(var_label(df$age), "Age (y)")
  expect_equal(var_label(df$gender), "Sex")

  # NULL label for unnamed variables
  expect_null(var_label(df$notthere))
})


# ---------- Errors ----------
test_that("meta_label errors for invalid inputs", {

  df <- data.frame(x = 1)

  # invalid data entry
  expect_error(
    "no data" |> meta_label("somepath.yaml")
  )

  # invalid YAML entry
  expect_error(
    df |> meta_label("no_file.yaml")
  )

  # YAML file without 'labels' element
  tmp_yaml <- tempfile(fileext = ".yaml")
  writeLines("foo: bar", tmp_yaml)
  expect_error(df |> meta_label(tmp_yaml))
})


# ---------- Warnings ----------
test_that("meta_label warns missing vars when warn_missing=TRUE", {
  df <- data.frame(age = c(1,2))
  yaml_path <- system.file("extdata", "codebook.yaml", package = "wcmtools")

  expect_warning(
    df |> meta_label(yaml_path, warn_missing = TRUE),
    "Some metadata variables had no match"
  )
})
