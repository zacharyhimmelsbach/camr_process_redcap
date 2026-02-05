#' Parse REDCap choice strings into codes and labels
#'
#' @param choices A REDCap choice string (e.g. "1, Yes | 2, No").
#' @return A tibble with columns code and label.
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

camr_checkbox_code_to_field <- function(code) {
  stringr::str_replace_all(code, "[^A-Za-z0-9]", "_")
}

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
#' @param data Data frame of REDCap records.
#' @param metadata REDCap metadata.
#' @param checkbox_false_label Label to use for unchecked checkbox values.
#' @return Data frame with labeled factor columns.
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
      data[[field]] <- factor(
        as.character(data[[field]]),
        levels = as.character(field_choices$code),
        labels = as.character(field_choices$label)
      )
    }
  }

  data
}
