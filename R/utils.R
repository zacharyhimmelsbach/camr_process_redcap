#' Null coalescing operator
#'
#' Returns the left-hand side if it is not NULL, otherwise returns the
#' right-hand side. Similar to the `??` operator in other languages.
#'
#' @param x Value to check for NULL.
#' @param y Default value to return if x is NULL.
#'
#' @return x if not NULL, otherwise y.
#'
#' @examples
#' NULL %||% "default"
#' # Returns "default"
#'
#' "value" %||% "default"
#' # Returns "value"
#'
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Make a safe target name from a form/instrument
#' @keywords internal
camr_safe_name <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) {
    x <- "form"
  }
  tolower(x)
}

#' Get first non-missing value from a vector
#'
#' Returns the first non-NA (and non-empty for character) value from a vector.
#' Useful for collapsing multiple rows per subject to a single value.
#'
#' @param x A vector (character, numeric, or factor).
#'
#' @return The first non-missing value, or NA if all values are missing.
#'
#' @keywords internal
camr_first_value <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.character(x)) {
    idx <- which(!is.na(x) & x != "")
  } else {
    idx <- which(!is.na(x))
  }
  if (length(idx) == 0) {
    return(NA)
  }
  x[[idx[[1]]]]
}

#' Merge two vectors, preferring primary values
#'
#' Combines two vectors by taking values from the primary vector when available,
#' falling back to the fallback vector for missing values.
#'
#' @param primary A vector of primary values.
#' @param fallback A vector of fallback values (same length as primary).
#'
#' @return A vector with primary values where available, fallback values elsewhere.
#'
#' @keywords internal
camr_merge_values <- function(primary, fallback) {
  if (is.factor(primary)) {
    primary <- as.character(primary)
  }
  if (is.factor(fallback)) {
    fallback <- as.character(fallback)
  }
  if (is.character(primary) || is.character(fallback)) {
    primary <- as.character(primary)
    fallback <- as.character(fallback)
    out <- primary
    idx <- is.na(out) | out == ""
    out[idx] <- fallback[idx]
    return(out)
  }
  out <- primary
  idx <- is.na(out)
  out[idx] <- fallback[idx]
  out
}

#' Resolve fields to keep in extracted datasets
#'
#' Computes the configured keep-field set, with defaults for backward
#' compatibility and guaranteed inclusion of `id_field`.
#'
#' @param config Configuration list.
#' @return Character vector of field names to keep.
#' @keywords internal
camr_keep_fields <- function(config) {
  defaults <- c("record_id", "record_status", "record_subject_id", "record_cam_id")
  keep <- config$keep_fields %||% defaults
  keep <- as.character(keep %||% character(0))
  keep <- keep[!is.na(keep) & nzchar(keep)]
  id_field <- as.character(config$id_field %||% "")
  if (nzchar(id_field) && !id_field %in% keep) {
    keep <- c(id_field, keep)
  }
  unique(keep)
}

#' Get last non-missing value from a vector
#'
#' Returns the last non-NA (and non-empty for character) value from a vector.
#' Useful for getting the most recent status or measurement.
#'
#' @param x A vector (character, numeric, or factor).
#'
#' @return The last non-missing value, or NA if all values are missing.
#'
#' @keywords internal
camr_last_value <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.character(x)) {
    idx <- which(!is.na(x) & x != "")
  } else {
    idx <- which(!is.na(x))
  }
  if (length(idx) == 0) {
    return(NA)
  }
  x[[idx[[length(idx)]]]]
}

#' Append unique lines to a file
#'
#' Adds lines to a file only if they don't already exist. Used for safely
#' updating `.gitignore` without duplicating entries.
#'
#' @param path Character. Path to the file.
#' @param lines Character vector. Lines to append if not already present.
#'
#' @return Invisibly returns the file path.
#'
#' @keywords internal
camr_append_unique_lines <- function(path, lines) {
  lines <- unique(as.character(lines))
  if (file.exists(path)) {
    existing <- readLines(path, warn = FALSE)
  } else {
    existing <- character(0)
  }
  missing <- setdiff(lines, existing)
  if (length(missing) > 0) {
    writeLines(c(existing, missing), path)
  }
  invisible(path)
}
