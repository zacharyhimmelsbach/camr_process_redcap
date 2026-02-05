#' Null coalescing helper
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
