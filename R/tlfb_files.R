#' Identify TLFB file upload fields from REDCap metadata
#'
#' @param metadata A tibble of REDCap metadata.
#' @param field_pattern Regex pattern used to identify TLFB-related fields.
#'
#' @return Character vector of matching REDCap file field names.
#' @keywords internal
camr_tlfb_file_fields <- function(metadata, field_pattern = "(?i)tlfb") {
  metadata <- tibble::as_tibble(metadata)
  if (!all(c("field_name", "field_type") %in% names(metadata))) {
    return(character(0))
  }

  file_md <- metadata |>
    dplyr::filter(.data$field_type == "file") |>
    dplyr::mutate(
      search_text = paste(
        .data$field_name,
        dplyr::coalesce(.data$field_label, ""),
        dplyr::coalesce(.data$form_name, "")
      )
    )

  if (!nzchar(field_pattern)) {
    return(unique(file_md$field_name))
  }

  matched <- file_md |>
    dplyr::filter(stringr::str_detect(.data$search_text, field_pattern))

  unique(matched$field_name)
}

#' Build an index of TLFB files available in REDCap exports
#'
#' @param data A tibble of REDCap records as returned by \code{\link{camr_redcap_read}}.
#' @param metadata A tibble of REDCap metadata as returned by
#'   \code{\link{camr_redcap_metadata}}.
#' @param config Config list with `id_field` and `event_name_field`.
#' @param field_pattern Regex pattern to identify TLFB-related file fields.
#'
#' @return A tibble with one row per available TLFB file reference.
#' @export
camr_tlfb_index <- function(data, metadata, config, field_pattern = "(?i)tlfb") {
  data <- tibble::as_tibble(data)
  metadata <- tibble::as_tibble(metadata)

  id_field <- config$id_field
  event_field <- config$event_name_field
  if (!id_field %in% names(data)) {
    stop("id_field not found in data.")
  }

  tlfb_fields <- camr_tlfb_file_fields(metadata, field_pattern = field_pattern)
  tlfb_fields <- tlfb_fields[tlfb_fields %in% names(data)]
  if (!length(tlfb_fields)) {
    return(tibble::tibble())
  }

  idx <- purrr::map_dfr(tlfb_fields, function(field) {
    vals <- as.character(data[[field]])
    keep <- !is.na(vals) & nzchar(vals) & vals != "0"
    if (!any(keep)) {
      return(tibble::tibble())
    }

    out <- tibble::tibble(
      record = as.character(data[[id_field]][keep]),
      field_name = field,
      doc_id = vals[keep]
    )

    out$event_name <- if (event_field %in% names(data)) {
      as.character(data[[event_field]][keep])
    } else {
      NA_character_
    }
    out$redcap_repeat_instrument <- if ("redcap_repeat_instrument" %in% names(data)) {
      as.character(data$redcap_repeat_instrument[keep])
    } else {
      NA_character_
    }
    out$redcap_repeat_instance <- if ("redcap_repeat_instance" %in% names(data)) {
      as.character(data$redcap_repeat_instance[keep])
    } else {
      NA_character_
    }

    out
  })

  if (!nrow(idx)) {
    return(tibble::tibble())
  }

  idx |>
    dplyr::distinct(
      .data$record,
      .data$event_name,
      .data$redcap_repeat_instrument,
      .data$redcap_repeat_instance,
      .data$field_name,
      .data$doc_id
    )
}

#' Export a REDCap file-upload field
#'
#' @param redcap_url REDCap API URL.
#' @param api_token API token.
#' @param record Record ID.
#' @param field_name REDCap file upload field name.
#' @param event_name Optional event name for longitudinal projects.
#' @param repeat_instrument Optional repeating instrument name.
#' @param repeat_instance Optional repeating instrument instance number.
#'
#' @return Raw vector with file bytes.
#' @keywords internal
camr_redcap_export_file <- function(redcap_url,
                                    api_token,
                                    record,
                                    field_name,
                                    event_name = NULL,
                                    repeat_instrument = NULL,
                                    repeat_instance = NULL) {
  body <- list(
    token = api_token,
    content = "file",
    action = "export",
    record = record,
    field = field_name
  )

  if (!is.null(event_name) && !is.na(event_name) && nzchar(event_name)) {
    body$event <- event_name
  }
  if (!is.null(repeat_instrument) && !is.na(repeat_instrument) && nzchar(repeat_instrument) &&
      !is.null(repeat_instance) && !is.na(repeat_instance) && nzchar(as.character(repeat_instance))) {
    body$repeat_instance <- repeat_instance
    body$instrument <- repeat_instrument
  }

  resp <- httr::POST(
    redcap_url,
    body = body,
    encode = "form",
    httr::content_type("application/x-www-form-urlencoded")
  )
  httr::stop_for_status(resp)

  file_raw <- httr::content(resp, as = "raw")
  txt <- tryCatch(rawToChar(file_raw), error = function(e) NA_character_)
  if (!is.na(txt) && stringr::str_starts(txt, "ERROR")) {
    stop(txt)
  }
  file_raw
}

#' Download TLFB files as local JSON files
#'
#' @param tlfb_index A tibble from \code{\link{camr_tlfb_index}}.
#' @param redcap_url REDCap API URL.
#' @param api_token API token.
#' @param out_dir Output directory for downloaded files.
#' @param overwrite Whether to overwrite existing files.
#'
#' @return Character vector of downloaded file paths.
#' @export
camr_download_tlfb_files <- function(tlfb_index,
                                     redcap_url,
                                     api_token,
                                     out_dir = "tlfb_files",
                                     overwrite = FALSE) {
  tlfb_index <- tibble::as_tibble(tlfb_index)
  if (!nrow(tlfb_index)) {
    return(character(0))
  }

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  paths <- character(0)
  for (i in seq_len(nrow(tlfb_index))) {
    row <- tlfb_index[i, , drop = FALSE]

    name_bits <- c(
      camr_safe_name(as.character(row$record[[1]])),
      camr_safe_name(as.character(row$event_name[[1]])),
      camr_safe_name(as.character(row$field_name[[1]])),
      camr_safe_name(as.character(row$redcap_repeat_instrument[[1]])),
      camr_safe_name(as.character(row$redcap_repeat_instance[[1]])),
      camr_safe_name(as.character(row$doc_id[[1]]))
    )
    name_bits <- name_bits[name_bits != "form"]
    file_name <- paste(name_bits, collapse = "__")
    if (!nzchar(file_name)) {
      file_name <- paste0("tlfb_", i)
    }
    out_path <- file.path(out_dir, paste0(file_name, ".json"))

    if (file.exists(out_path) && !isTRUE(overwrite)) {
      paths <- c(paths, out_path)
      next
    }

    file_raw <- camr_redcap_export_file(
      redcap_url = redcap_url,
      api_token = api_token,
      record = as.character(row$record[[1]]),
      field_name = as.character(row$field_name[[1]]),
      event_name = as.character(row$event_name[[1]]),
      repeat_instrument = as.character(row$redcap_repeat_instrument[[1]]),
      repeat_instance = as.character(row$redcap_repeat_instance[[1]])
    )
    writeBin(file_raw, out_path)
    paths <- c(paths, out_path)
  }

  unique(paths)
}
