
#' Attach labels from YAML metadata to a data frame.
#'
#' @description
#' Reads variable labels from a YAML file and applies them to the corresponding columns of a data frame.
#' Data frame may contain variables not found in metadata, and vice versa.
#'
#' @param data A data frame you want to label.
#' @param yaml_path Path to YAML file containing variable labels.
#' @param warn_missing Logical; controls warnings for metadata variables not found in data frame.
#' @param values Character (none, overwrite, new); controls how to handle factor values.
#' @param values_tag Character, string for variable suffix when values = "new."
#'
#' @return Original data frame with labels attached
#' @export
#' @importFrom yaml read_yaml
#' @importFrom labelled var_label
#'
#' @examples
#' \dontrun{
#' yaml_path <- system.file("extdata", "codebook.yaml", package = "bookworm")
#'
#' df <- data.frame(
#'   age = c(23, 25, 42, 38, 29),
#'   gender = c(1, 2, 1, 1, 2),
#'   education = c(2, 3, 3, 5, 4),
#'   diabetes = c("Ngt", "Ngt", "2", "1", "Ngt")
#' )
#'
#' labels <- df |> annotate(yaml_path, values = "overwrite")
#' }

annotate <- function(
    data,
    yaml_path,
    warn_missing = FALSE,
    values = c("none", "overwrite", "new"),
    values_tag = "_factor"
  ) {

  ## initial errors from user inputs ----------

  values <- match.arg(values) # validates argument

  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame or tibble")
  }
  if (!file.exists(yaml_path)) {
    stop("Specified metadata file does not exist: ", yaml_path)
  }
  if (!is.character(values_tag) || length(values_tag) != 1) {
    stop("`values_tag` must be a single string")
  }

  # load metadata
  metadata <- yaml::read_yaml(yaml_path)

  if (!"labels" %in% names(metadata)) {
    stop("No labels found in metadata")
  }


  ## labels ----------

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


  ## levels ----------

  # apply variable levels (optional, based on user preference)
  if (values != "none" && "values" %in% names(metadata)) {
    for (var in intersect(names(metadata$values), names(data))) {
      val_map <- metadata$values[[var]]

      # match as character (avoids type mismatch)
      var_factor <- factor(
        as.character(data[[var]]),
        levels = names(val_map),
        labels = unname(val_map)
      )

      if (values == "overwrite") {
        data[[var]] <- var_factor
        if (var %in% names(metadata$labels)) { # keep original label
          labelled::var_label(data[[var]]) <- metadata$labels[[var]]
        }

      } else if (values == "new") {
        var_new <- paste0(var, values_tag)
        data[[var_new]] <- var_factor
        if (var %in% names(metadata$labels)) { # keep original label
          labelled::var_label(data[[var_new]]) <- metadata$labels[[var]]
        }
      }
    }
  }

  return(data)
}
