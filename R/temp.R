
#' Apply a YAML codebook to a data frame (labels, values, meta)
#'
#' @description
#' Reads a YAML codebook and applies it to a data frame: variable labels,
#' value maps (as factors), and simple meta (missing codes, factor order,
#' reference level, type coercion). `path` can be a file path **or** a
#' preloaded YAML object (a list).
#'
#' @param data A data.frame/tibble to annotate.
#' @param path File path to a YAML codebook **or** a preloaded YAML list
#'   (e.g., the result of `yaml::read_yaml()`).
#' @param values One of "none","overwrite","new"; how to handle value maps.
#' @param values_tag Suffix used when `values = "new"`.
#' @param mismatch Either a single policy for all scopes — one of
#'   "ignore","warn","error" — or a named list/character vector with any of
#'   `vars`, `codes`, `meta` set individually, e.g.
#'   `list(vars="warn", codes="error", meta="ignore")`.
#' @param normalize_codes Logical; trim whitespace and case-fold before matching codes to `values`.
#' @param case One of "lower","upper","asis" for code matching when `normalize_codes = TRUE`.
#' @param apply_meta Logical; apply `meta$missing_codes`, `meta$levels_order`, `meta$reference_level`,
#'   and simple type coercion from `meta$type`.
#'
#' @return The annotated `data` (attributes `codebook_source`, `codebook_path`, `bookworm_version`).
#' @export
#' @importFrom yaml read_yaml
#' @importFrom labelled var_label
annotate <- function(
    data,
    path,
    values = c("none", "overwrite", "new"),
    values_tag = "_factor",
    mismatch = "warn",
    normalize_codes = TRUE,
    case = c("lower","upper","asis"),
    apply_meta = TRUE
) {
  values <- match.arg(values)
  case   <- match.arg(case)

  if (!inherits(data, "data.frame")) stop("`data` must be a data.frame or tibble")
  if (!is.character(values_tag) || length(values_tag) != 1) stop("`values_tag` must be a single string")

  `%||%` <- function(x, y) if (is.null(x)) y else x
  .valid <- c("ignore","warn","error")

  # --- mismatch policy --------------------------------------------------------
  normalize_mismatch <- function(x) {
    pol <- list(vars = "warn", codes = "warn", meta = "warn")
    if (is.character(x) && length(x) == 1 && x %in% .valid) { pol[] <- x; return(pol) }
    if ((is.list(x) || is.character(x)) && length(x) >= 1) {
      nx <- names(x)
      if (is.null(nx) || !all(nx %in% c("vars","codes","meta")))
        stop("`mismatch` must be 'ignore'|'warn'|'error' or a named list/char vector with any of: vars, codes, meta.")
      for (k in nx) {
        val <- if (is.list(x)) x[[k]] else x[[k]]
        if (!is.character(val) || length(val) != 1 || !(val %in% .valid))
          stop("`mismatch$", k, "` must be one of: ", paste(.valid, collapse=", "))
        pol[[k]] <- val
      }
      return(pol)
    }
    stop("`mismatch` must be 'ignore'|'warn'|'error' or a named list/char vector like list(vars='warn', codes='error').")
  }
  .signal <- function(policy, msg) switch(policy,
                                          ignore = invisible(NULL),
                                          warn   = warning(msg, call. = FALSE),
                                          error  = stop(msg)
  )

  # --- YAML input: path OR preloaded list ------------------------------------
  .read_yaml_safe <- function(p) {
    tryCatch(yaml::read_yaml(p, eval.expr = FALSE),
             error = function(e) yaml::yaml.load_file(p, eval.expr = FALSE))
  }
  .as_codebook <- function(x) {
    if (is.list(x)) {
      list(codebook = x, source = "object", path = NA_character_)
    } else if (is.character(x) && length(x) == 1) {
      if (!file.exists(x)) stop("Specified metadata file does not exist: ", x)
      list(codebook = .read_yaml_safe(x), source = "file", path = normalizePath(x, winslash = "/", mustWork = TRUE))
    } else {
      stop("`path` must be a file path (character scalar) or a preloaded YAML list.")
    }
  }

  policy <- normalize_mismatch(mismatch)
  cb_in  <- .as_codebook(path)
  metadata <- cb_in$codebook

  # --- codebook structure -----------------------------------------------------
  if (!"labels" %in% names(metadata)) stop("No `labels` found in metadata")

  labels     <- metadata$labels %||% list()
  values_map <- metadata$values %||% list()
  meta       <- metadata$meta   %||% list()

  # --- helpers for mapping/coercion ------------------------------------------
  .norm <- function(x) {
    if (!normalize_codes) return(x)
    x <- trimws(as.character(x))
    switch(case, lower = tolower(x), upper = toupper(x), asis = x)
  }
  .apply_label_vec <- function(df, lab) {
    ix <- intersect(names(lab), names(df))
    if (length(ix)) labelled::var_label(df)[ix] <- lab[ix]
    df
  }
  .apply_missing_codes <- function(x, miss) {
    if (is.null(miss) || !length(miss)) return(x)
    x[x %in% miss] <- NA
    x
  }
  .coerce_type <- function(x, m) {
    if (is.null(m$type)) return(x)
    type <- tolower(m$type); fmt <- m$format %||% NA_character_
    if (type == "date")      return(as.Date(x, format = ifelse(is.na(fmt), "%Y-%m-%d", fmt)))
    if (type %in% c("datetime","posixct","timestamp"))
      return(as.POSIXct(x, format = ifelse(is.na(fmt), "%Y-%m-%d %H:%M:%S", fmt), tz = "UTC"))
    if (type %in% c("integer","int"))    return(as.integer(x))
    if (type %in% c("double","numeric")) return(as.numeric(x))
    if (type %in% c("logical","bool"))   return(as.logical(x))
    if (type %in% c("character","chr","string")) return(as.character(x))
    x
  }
  .map_to_factor <- function(vec, val_map, policy_codes, levels_order = NULL, ref = NULL) {
    keys <- as.character(names(val_map))
    labs <- as.character(unname(val_map))
    raw    <- .norm(vec)
    keys_n <- .norm(keys)

    # codes mismatches
    seen <- unique(raw[!is.na(raw)])
    unmatched <- setdiff(seen, keys_n)
    if (length(unmatched)) {
      msg <- paste0("Code mismatch (unmapped codes): ",
                    paste(utils::head(unmatched, 10), collapse = ", "),
                    if (length(unmatched) > 10) " …")
      .signal(policy_codes, msg)
    }

    lab_lookup <- stats::setNames(labs, keys_n)
    out_chr <- ifelse(is.na(raw), NA_character_, unname(lab_lookup[raw]))

    levs <- if (!is.null(levels_order) && length(levels_order)) levels_order else unique(labs)
    f <- factor(out_chr, levels = levs)
    if (!is.null(ref) && ref %in% levels(f)) f <- stats::relevel(f, ref)
    f
  }

  # --- VARS: YAML labels vs data ---------------------------------------------
  missing_vars <- setdiff(names(labels), names(data))
  if (length(missing_vars)) {
    .signal(policy$vars,
            paste("Variable mismatch (in YAML, not in data):", paste(missing_vars, collapse = ", ")))
  }
  # Optional: also check data vars not in YAML (comment in if you want)
  # extra_vars <- setdiff(names(data), names(labels))
  # if (length(extra_vars)) {
  #   .signal(policy$vars,
  #           paste("Variable mismatch (in data, not in YAML):", paste(extra_vars, collapse = ", ")))
  # }

  # apply labels
  data <- .apply_label_vec(data, labels)

  # --- META: meta vars vs data + basic consistency ---------------------------
  meta_vars <- names(meta %||% list())
  if (length(meta_vars)) {
    missing_meta <- setdiff(meta_vars, names(data))
    if (length(missing_meta)) {
      .signal(policy$meta,
              paste("Meta mismatch (in YAML `meta`, not in data):", paste(missing_meta, collapse = ", ")))
    }
    for (v in intersect(meta_vars, names(data))) {
      m <- meta[[v]]
      if (!is.null(m$reference_level)) {
        known_levels <- c(
          as.character(unname(values_map[[v]] %||% character())),
          as.character(m$levels_order %||% character())
        )
        if (length(known_levels) && !(m$reference_level %in% known_levels)) {
          .signal(policy$meta,
                  sprintf("Meta mismatch for `%s`: reference_level '%s' not found in defined levels.",
                          v, m$reference_level))
        }
      }
      if (!is.null(m$allowed_range) &&
          !tolower(m$type %||% "") %in% c("double","numeric","integer","int")) {
        .signal(policy$meta,
                sprintf("Meta mismatch for `%s`: allowed_range present but type is '%s'.",
                        v, m$type %||% "unspecified"))
      }
    }
  }

  # --- VALUES + META transforms ----------------------------------------------
  if ((values != "none") || apply_meta) {
    vars <- intersect(unique(c(names(values_map), names(meta))), names(data))
    for (v in vars) {
      v_meta <- meta[[v]] %||% list()

      if (!is.null(v_meta$missing_codes)) data[[v]] <- .apply_missing_codes(data[[v]], v_meta$missing_codes)
      if (apply_meta && !is.null(v_meta$type)) data[[v]] <- .coerce_type(data[[v]], v_meta)

      if (values != "none" && !is.null(values_map[[v]])) {
        f <- .map_to_factor(
          vec = data[[v]],
          val_map = values_map[[v]],
          policy_codes = policy$codes,
          levels_order = v_meta$levels_order %||% NULL,
          ref          = v_meta$reference_level %||% NULL
        )
        if (values == "overwrite") {
          data[[v]] <- f
          if (!is.null(labels[[v]])) labelled::var_label(data[[v]]) <- labels[[v]]
        } else { # "new"
          v_new <- paste0(v, values_tag)
          if (v_new %in% names(data)) warning("Overwriting existing column: ", v_new, call. = FALSE)
          data[[v_new]] <- f
          if (!is.null(labels[[v]])) labelled::var_label(data[[v_new]]) <- labels[[v]]
        }
      } else if (apply_meta && !is.null(v_meta$levels_order) && is.factor(data[[v]])) {
        data[[v]] <- factor(data[[v]], levels = v_meta$levels_order)
        if (!is.null(v_meta$reference_level)) data[[v]] <- stats::relevel(data[[v]], v_meta$reference_level)
      }
    }
  }

  # --- provenance -------------------------------------------------------------
  attr(data, "codebook_source")   <- cb_in$source   # "file" or "object"
  attr(data, "codebook_path")     <- cb_in$path     # path or NA
  attr(data, "bookworm_version")  <- tryCatch(utils::packageVersion("bookworm"), error = function(e) NA)

  data
}
