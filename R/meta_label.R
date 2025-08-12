
#' Attach labels from YAML metadata to a data frame.
#'
#' @description
#' Reads variable labels from a YAML file and applies them to the corresponding columns of a data frame.
#' Data frame may contain variables not found in metadata, and vice versa.
#'
#' @param data A data frame you want to label.
#' @param yaml_path Path to YAML file containing variable labels.
#' @param warn_missing Logical, controls warnings for metadata variables not found in data frame.
#'
#' @return Original data frame with labels attached
#' @export
#' @importFrom yaml read_yaml
#' @importFrom labelled var_label
#'
#' @examples
#' \dontrun{
#' yaml_path <- system.file("extdata", "codebook.yaml", package = "wcmtools")
#'
#' df <- data.frame(
#'   age = c(23, 25, 42, 38, 29),
#'   gender = c(1, 2, 1, 1, 2),
#'   education = c(2, 3, 3, 5, 4)
#' )
#'
#' labeled_df <- meta_label(df, yaml_path)
#' labelled::var_label(labeled_df$age)
#' }

meta_label <- function(data, yaml_path, warn_missing = FALSE) {

  ## initial errors from user inputs ----------
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame or tibble")
  }
  if (!file.exists(yaml_path)) {
    stop("Specified metadata file does not exist: ", yaml_path)
  }

  # load metadata
  metadata <- yaml::read_yaml(yaml_path)

  if (!"labels" %in% names(metadata)) {
    stop("No labels found in metadata")
  }

  # check what variables in metadata were found in data frame
  missing_vars <- setdiff(names(metadata$labels), names(data))
  if (warn_missing && length(missing_vars) > 0) {
    warning(
      "Some metadata variables had no match: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # apply variable labels
  for (var in intersect(names(metadata$labels), names(data))) {
    labelled::var_label(data[[var]]) <- metadata$labels[[var]]
  }

  return(data)
}
