#' Validate REDCap config against metadata
#'
#' @param config Config list.
#' @param metadata REDCap metadata.
#' @return Config list (possibly normalized).
#' @export
camr_validate_config <- function(config, metadata) {
  if (is.null(config)) {
    stop("config is required.")
  }
  if (is.null(metadata) || nrow(metadata) == 0) {
    warning("Metadata is empty; config validation skipped.")
    return(config)
  }

  metadata <- tibble::as_tibble(metadata)
  status_field <- config$status_field %||% ""

  if (!"field_name" %in% names(metadata)) {
    warning("Metadata is missing field_name; config validation skipped.")
    return(config)
  }

  if (!nzchar(status_field)) {
    warning("config$status_field is empty; status validation skipped.")
    return(config)
  }

  if (!status_field %in% metadata$field_name) {
    warning("status_field not found in metadata; status validation skipped.")
    return(config)
  }

  choices <- camr_choices_table(metadata) |> dplyr::filter(.data$field_name == status_field)
  if (nrow(choices) == 0) {
    warning("No choices found for status_field; status validation skipped.")
    return(config)
  }

  codes <- as.character(choices$code)
  labels <- as.character(choices$label)
  mapping <- stats::setNames(labels, codes)

  normalize_values <- function(values, label) {
    values <- as.character(values %||% character(0))
    if (length(values) == 0) {
      return(values)
    }

    if (all(values %in% labels)) {
      return(values)
    }

    if (all(values %in% codes)) {
      return(unname(mapping[values]))
    }

    warning(
      sprintf(
        "Some %s values do not match REDCap choices. Valid labels: %s. Valid codes: %s.",
        label,
        paste(unique(labels), collapse = ", "),
        paste(unique(codes), collapse = ", ")
      )
    )
    values
  }

  config$status_active_values <- normalize_values(config$status_active_values, "status_active_values")
  config$status_dropout_values <- normalize_values(config$status_dropout_values, "status_dropout_values")

  config
}
