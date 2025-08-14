
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
library(bookworm)
library(labelled)


# --- - - - - - - - - - -

# ---------- Functionality ----------
test_that("meta_label applies labels correctly", {

  yaml_path <- system.file("extdata", "codebook.yaml", package = "bookworm")
  df <- data.frame(
    age = c(23, 25, 42, 38, 29),
    gender = c(1, 2, 1, 1, 2),
    education = c(2, 3, 3, 5, 4),
    diabetes = c("N", "N", "2", "1", "N"),
    dx = c(1, 0, 0, 0, 1)
  ) |>
    meta_label(yaml_path)

  # labels applied correctly
  expect_equal(var_label(df$age), "Age (y)")
  expect_equal(var_label(df$education), "Highest level of education")

  # NULL label for unnamed variables
  expect_null(var_label(df$dx))
})


test_that("meta_label leaves variables unchanged when values = none", {
  yaml_path <- system.file("extdata", "codebook.yaml", package = "bookworm")
  df <- data.frame(gender = c(2, 1)) |>
    meta_label(yaml_path, values = "none")

  expect_false(is.factor(df$gender))
  expect_equal(var_label(df$gender), "Sex")
})


test_that("meta_label applies value labels with overwrite", {

  yaml_path <- system.file("extdata", "codebook.yaml", package = "bookworm")
  df <- data.frame(gender = c(2, 1)) |>
    meta_label(yaml_path, values = "overwrite")

  expect_s3_class(df$gender, "factor")
  expect_equal(levels(df$gender), c("Male", "Female", "Other"))
  expect_equal(as.character(df$gender), c("Female", "Male"))
  expect_equal(var_label(df$gender), "Sex")
})


test_that("meta_label applies value labels with new factor and custom suffix", {

  yaml_path <- system.file("extdata", "codebook.yaml", package = "bookworm")
  df <- data.frame(gender = c(2, 1)) |>
    meta_label(yaml_path, values = "new", values_tag = "_cats")

  expect_false("gender" %in% names(df) && is.factor(df$gender))
  expect_true("gender_cats" %in% names(df))
  expect_s3_class(df$gender_cats, "factor")
  expect_equal(levels(df$gender_cats), c("Male", "Female", "Other"))
  expect_equal(var_label(df$gender_cats), "Sex")
  expect_equal(as.character(df$gender_cats), c("Female", "Male"))
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
  yaml_path <- system.file("extdata", "codebook.yaml", package = "bookworm")

  expect_warning(
    df |> meta_label(yaml_path, warn_missing = TRUE),
    "Some metadata variables had no match"
  )
})
