#' Parse REDCap choice strings into codes and labels
#'
#' Parses the pipe-delimited choice strings used by REDCap for dropdown,
#' radio, and checkbox fields into a tidy tibble of codes and labels.
#'
#' @param choices A REDCap choice string in the format "code1, label1 | code2, label2".
#'   For example: "1, Yes | 2, No | 3, Unknown".
#'
#' @return A tibble with two columns:
#'   \describe{
#'     \item{code}{Character. The numeric or text code for each choice.}
#'     \item{label}{Character. The human-readable label for each choice.}
#'   }
#'
#' @examples
#' camr_parse_choices("1, Yes | 2, No")
#' camr_parse_choices("0, Inactive | 1, Active | 2, Completed | 3, Dropout")
#'
#' @export
camr_parse_choices <- function(choices) {
  if (is.na(choices) || !nzchar(choices)) {
    return(tibble::tibble(code = character(), label = character()))
  }
  parts <- stringr::str_split(choices, "\\s*\\|\\s*")[[1]]
  parsed <- purrr::map_dfr(parts, function(part) {
    if (!nzchar(part)) {
      return(tibble::tibble(code = character(), label = character()))
    }
    bits <- stringr::str_split(part, "\\s*,\\s*", n = 2)[[1]]
    code <- bits[[1]] %||% ""
    label <- bits[[2]] %||% ""
    tibble::tibble(code = code, label = label)
  })
  parsed
}

#' Convert checkbox code to REDCap field suffix
#'
#' REDCap checkbox fields are stored as separate columns with the naming
#' convention `fieldname___code`. This function converts special characters
#' in codes to underscores to match REDCap's column naming.
#'
#' @param code Character. The checkbox choice code.
#'
#' @return Character. The sanitized code suitable for use as a field suffix.
#'
#' @keywords internal
camr_checkbox_code_to_field <- function(code) {
  stringr::str_replace_all(code, "[^A-Za-z0-9]", "_")
}

#' Build a lookup table of all choices from REDCap metadata
#'
#' Extracts all choice options from dropdown, radio, checkbox, yesno, and
#' truefalse fields in the REDCap metadata and returns them as a single
#' lookup table.
#'
#' @param metadata A tibble of REDCap metadata as returned by
#'   \code{\link{camr_redcap_metadata}}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{field_name}{Character. The REDCap field name.}
#'     \item{field_type}{Character. The field type (dropdown, radio, checkbox, yesno, truefalse).
#'     }
#'     \item{code}{Character. The choice code.}
#'
#'     \item{label}{Character. The choice label.}
#'   }
#'
#' @keywords internal
camr_choices_table <- function(metadata) {
  metadata <- tibble::as_tibble(metadata)
  metadata <- dplyr::filter(metadata, .data$field_type %in% c("dropdown", "radio", "checkbox", "yesno", "truefalse"))

  yesno_fields <- metadata$field_name[metadata$field_type == "yesno"]
  truefalse_fields <- metadata$field_name[metadata$field_type == "truefalse"]

  yesno_tbl <- purrr::map_dfr(yesno_fields, function(field) {
    tibble::tibble(
      field_name = field,
      field_type = "yesno",
      code = c("0", "1"),
      label = c("No", "Yes")
    )
  })

  truefalse_tbl <- purrr::map_dfr(truefalse_fields, function(field) {
    tibble::tibble(
      field_name = field,
      field_type = "truefalse",
      code = c("0", "1"),
      label = c("False", "True")
    )
  })

  mc_tbl <- metadata |>
    dplyr::filter(.data$field_type %in% c("dropdown", "radio", "checkbox")) |>
    dplyr::mutate(parsed = purrr::map(.data$select_choices_or_calculations, camr_parse_choices)) |>
    tidyr::unnest(.data$parsed)

  dplyr::bind_rows(
    mc_tbl |> dplyr::select(.data$field_name, .data$field_type, .data$code, .data$label),
    yesno_tbl,
    truefalse_tbl
  )
}

#' Apply REDCap choice labels as factors
#'
#' Converts coded values in REDCap data to labeled factors using the choice
#' definitions from the project metadata. Handles dropdown, radio, checkbox,
#' yesno, and truefalse field types.
#'
#' @param data A data frame or tibble of REDCap records, typically from
#'   \code{\link{camr_redcap_read}}.
#' @param metadata A tibble of REDCap metadata as returned by
#'   \code{\link{camr_redcap_metadata}}.
#' @param checkbox_false_label Character. Label to use for unchecked (0) checkbox
#'   values. Defaults to "0".
#'
#' @return A tibble with multiple-choice fields converted to factors with
#'   human-readable labels.
#'
#' @details
#' For checkbox fields, REDCap stores each option as a separate column named
#' `fieldname___code` with values 0 (unchecked) or 1 (checked). This function

#' converts these to factors with the checkbox label for checked values.
#'
#' @seealso \code{\link{camr_parse_choices}}, \code{\link{camr_redcap_metadata}}
#'
#' @export
camr_apply_labels <- function(data, metadata, checkbox_false_label = "0") {
  data <- tibble::as_tibble(data)
  choices <- camr_choices_table(metadata)
  if (nrow(choices) == 0) {
    return(data)
  }

  for (field in unique(choices$field_name)) {
    field_choices <- dplyr::filter(choices, .data$field_name == field)
    field_type <- field_choices$field_type[[1]]

    if (field_type == "checkbox") {
      for (i in seq_len(nrow(field_choices))) {
        code <- as.character(field_choices$code[[i]])
        label <- as.character(field_choices$label[[i]])
        col <- paste0(field, "___", camr_checkbox_code_to_field(code))
        if (!col %in% names(data)) {
          next
        }
        vals <- data[[col]]
        data[[col]] <- factor(
          as.character(vals),
          levels = c("0", "1"),
          labels = c(checkbox_false_label, label)
        )
      }
    } else {
      if (!field %in% names(data)) {
        next
      }
      field_codes <- as.character(field_choices$code)
      field_labels <- as.character(field_choices$label)
      label_to_code <- stats::setNames(field_codes, field_labels)
      vals <- as.character(data[[field]])
      is_already_labeled <- vals %in% names(label_to_code)
      vals[is_already_labeled] <- label_to_code[vals[is_already_labeled]]
      data[[field]] <- factor(
        vals,
        levels = field_codes,
        labels = field_labels
      )
    }
  }

  data
}
