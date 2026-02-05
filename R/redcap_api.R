#' Low-level REDCap API POST
#'
#' @param redcap_url REDCap API URL.
#' @param api_token REDCap API token.
#' @param body Named list of API parameters.
#' @return A character string response body.
#' @keywords internal
camr_redcap_post <- function(redcap_url, api_token, body, format = "json") {
  if (is.null(redcap_url) || is.na(redcap_url) || !nzchar(redcap_url)) {
    stop("redcap_url is required.")
  }
  if (is.null(api_token) || is.na(api_token) || !nzchar(api_token)) {
    stop("api_token is required.")
  }
  payload <- c(
    list(token = api_token, format = format),
    body
  )
  resp <- httr::POST(redcap_url, body = payload, encode = "form")
  httr::stop_for_status(resp)
  text <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (length(text) == 0 || is.na(text)) {
    return("")
  }
  if (stringr::str_starts(text, "ERROR")) {
    stop(text)
  }
  text
}

#' Export REDCap metadata (data dictionary)
#'
#' @param redcap_url REDCap API URL.
#' @param api_token REDCap API token.
#' @return A tibble with metadata.
#' @export
camr_redcap_metadata <- function(redcap_url, api_token) {
  text <- camr_redcap_post(
    redcap_url,
    api_token,
    list(content = "metadata")
  )
  if (!nzchar(text)) {
    return(tibble::tibble())
  }
  json <- jsonlite::fromJSON(text, simplifyDataFrame = TRUE)
  tibble::as_tibble(json)
}

#' Export REDCap records in raw form
#'
#' @param redcap_url REDCap API URL.
#' @param api_token REDCap API token.
#' @param fields Optional vector of field names.
#' @param records Optional vector of record IDs.
#' @return A tibble with raw REDCap data.
#' @export
camr_redcap_read <- function(redcap_url, api_token, fields = NULL, records = NULL) {
  body <- list(
    content = "record",
    type = "flat",
    rawOrLabel = "raw",
    rawOrLabelHeaders = "raw",
    exportCheckboxLabel = "false",
    exportSurveyFields = "true",
    exportDataAccessGroups = "true"
  )
  if (!is.null(fields)) {
    body$fields <- fields
  }
  if (!is.null(records)) {
    body$records <- records
  }
  text <- camr_redcap_post(redcap_url, api_token, body)
  if (!nzchar(text)) {
    return(tibble::tibble())
  }
  json <- jsonlite::fromJSON(text, simplifyDataFrame = TRUE)
  tibble::as_tibble(json)
}

#' Export REDCap event metadata
#'
#' @param redcap_url REDCap API URL.
#' @param api_token REDCap API token.
#' @return A tibble with event names, labels, and numbers.
#' @export
camr_redcap_events <- function(redcap_url, api_token) {
  text <- camr_redcap_post(
    redcap_url,
    api_token,
    list(content = "event")
  )
  if (!nzchar(text)) {
    return(tibble::tibble())
  }
  json <- jsonlite::fromJSON(text, simplifyDataFrame = TRUE)
  events <- tibble::as_tibble(json)

  name_col <- NULL
  if ("unique_event_name" %in% names(events)) {
    name_col <- "unique_event_name"
  } else if ("event_name" %in% names(events)) {
    name_col <- "event_name"
  }
  if (!is.null(name_col)) {
    events <- dplyr::rename(events, event_name = !!rlang::sym(name_col))
  }
  if (!"event_name" %in% names(events)) {
    stop("Event metadata does not include an event name column.")
  }

  if ("custom_event_label" %in% names(events)) {
    events <- dplyr::mutate(
      events,
      event_label = dplyr::if_else(
        !is.na(.data$custom_event_label) & nzchar(.data$custom_event_label),
        .data$custom_event_label,
        .data$event_name
      )
    )
  } else if ("event_label" %in% names(events)) {
    events <- dplyr::rename(events, event_label = .data$event_label)
  } else {
    events <- dplyr::mutate(events, event_label = .data$event_name)
  }

  if ("event_number" %in% names(events)) {
    events <- dplyr::mutate(events, event_number = as.integer(.data$event_number))
  } else if ("event_num" %in% names(events)) {
    events <- dplyr::mutate(events, event_number = as.integer(.data$event_num))
  } else {
    if ("arm_num" %in% names(events)) {
      events <- dplyr::group_by(events, .data$arm_num)
    }
    events <- dplyr::arrange(events, dplyr::across(dplyr::any_of(c("day_offset", "event_name"))))
    events <- dplyr::mutate(events, event_number = dplyr::row_number())
    events <- dplyr::ungroup(events)
  }

  dplyr::select(events, dplyr::any_of(c("event_name", "event_label", "event_number", "arm_num")))
}

#' Export REDCap project info
#'
#' @param redcap_url REDCap API URL.
#' @param api_token REDCap API token.
#' @return A list with project details.
#' @export
camr_redcap_project_info <- function(redcap_url, api_token) {
  text <- camr_redcap_post(
    redcap_url,
    api_token,
    list(content = "project")
  )
  if (!nzchar(text)) {
    return(list())
  }
  jsonlite::fromJSON(text, simplifyDataFrame = TRUE)
}

#' Export REDCap codebook (metadata in CSV format)
#'
#' @param redcap_url REDCap API URL.
#' @param api_token REDCap API token.
#' @return A tibble with codebook fields.
#' @export
camr_redcap_codebook <- function(redcap_url, api_token) {
  text <- camr_redcap_post(
    redcap_url,
    api_token,
    list(content = "metadata"),
    format = "csv"
  )
  if (!nzchar(text)) {
    return(tibble::tibble())
  }
  utils::read.csv(text = text, stringsAsFactors = FALSE) |>
    tibble::as_tibble()
}

#' Build a form-to-fields map from REDCap metadata
#'
#' @param metadata REDCap metadata.
#' @return A tibble with form_name and field_name.
#' @export
camr_redcap_form_map <- function(metadata) {
  metadata <- tibble::as_tibble(metadata)
  if (!all(c("form_name", "field_name") %in% names(metadata))) {
    stop("metadata must include form_name and field_name.")
  }
  metadata %>%
    dplyr::filter(!is.na(.data$form_name), .data$form_name != "") %>%
    dplyr::select(.data$form_name, .data$field_name)
}

#' List repeating instruments from REDCap data
#'
#' @param data REDCap records.
#' @return Character vector of repeating instrument names.
#' @export
camr_repeating_instruments <- function(data) {
  data <- tibble::as_tibble(data)
  if (!"redcap_repeat_instrument" %in% names(data)) {
    return(character(0))
  }
  instruments <- unique(data$redcap_repeat_instrument)
  instruments <- instruments[!is.na(instruments) & instruments != ""]
  sort(instruments)
}

#' Export REDCap form-event mapping
#'
#' @param redcap_url REDCap API URL.
#' @param api_token REDCap API token.
#' @return A tibble with form_name and event_name.
#' @export
camr_redcap_form_event_mapping <- function(redcap_url, api_token) {
  text <- camr_redcap_post(
    redcap_url,
    api_token,
    list(content = "formEventMapping")
  )
  if (!nzchar(text)) {
    return(tibble::tibble())
  }
  json <- jsonlite::fromJSON(text, simplifyDataFrame = TRUE)
  mapping <- tibble::as_tibble(json)

  if (!all(c("form_name", "unique_event_name") %in% names(mapping))) {
    return(mapping)
  }

  mapping <- dplyr::rename(mapping, event_name = .data$unique_event_name)
  dplyr::select(mapping, .data$form_name, .data$event_name)
}
