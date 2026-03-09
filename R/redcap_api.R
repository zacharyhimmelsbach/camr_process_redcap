#' Low-level REDCap API POST request
#'
#' Sends a POST request to the REDCap API. This is an internal function used
#' by the higher-level API functions.
#'
#' @param redcap_url Character. The REDCap API endpoint URL
#'   (e.g., "https://redcap.example.edu/api/").
#' @param api_token Character. A valid REDCap API token with appropriate permissions.
#' @param body Named list of API parameters to include in the request.
#' @param format Character. Response format, either "json" (default) or "csv".
#'
#' @return A character string containing the response body.
#'
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
  resp <- httr::POST(
    redcap_url,
    body = payload,
    encode = "form",
    httr::content_type("application/x-www-form-urlencoded")
  )
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
#' Retrieves the project metadata (data dictionary) from REDCap, which includes
#' field definitions, validation rules, and choice options for all instruments.
#'
#' @param redcap_url Character. The REDCap API endpoint URL
#'   (e.g., "https://redcap.example.edu/api/").
#' @param api_token Character. A valid REDCap API token with export permissions.
#'
#' @return A tibble containing the project metadata with columns including:
#'   \describe
#'     \item{field_name}{The variable name}
#'     \item{form_name}{The instrument/form containing this field}
#'     \item{field_type}{The field type (text, dropdown, radio, checkbox, etc.)}
#'     \item{field_label}{The field label shown to users}
#'     \item{select_choices_or_calculations}{Choice options or calc formula}
#'   }
#'
#' @examples
#' \dontrun{
#' metadata <- camr_redcap_metadata(
#'   redcap_url = "https://redcap.example.edu/api/",
#'   api_token = Sys.getenv("REDCAP_API_TOKEN")
#' )
#' }
#'
#' @seealso \code{\link{camr_redcap_read}}, \code{\link{camr_redcap_codebook}}
#'
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
#' Exports records from a REDCap project with raw (coded) values. This is the
#' primary function for retrieving study data from REDCap.
#'
#' @param redcap_url Character. The REDCap API endpoint URL
#'   (e.g., "https://redcap.example.edu/api/").
#' @param api_token Character. A valid REDCap API token with export permissions.
#' @param fields Character vector. Optional subset of field names to export.
#'   If NULL (default), all fields are exported.
#' @param records Character vector. Optional subset of record IDs to export.
#'   If NULL (default), all records are exported.
#'
#' @return A tibble containing the requested records with raw coded values.
#'   For longitudinal projects, includes `redcap_event_name`. For projects
#'   with repeating instruments, includes `redcap_repeat_instrument` and
#'   `redcap_repeat_instance`.
#'
#' @examples
#' \dontrun{
#' # Export all data
#' data <- camr_redcap_read(
#'   redcap_url = "https://redcap.example.edu/api/",
#'   api_token = Sys.getenv("REDCAP_API_TOKEN")
#' )
#'
#' # Export specific fields
#' data <- camr_redcap_read(
#'   redcap_url = "https://redcap.example.edu/api/",
#'   api_token = Sys.getenv("REDCAP_API_TOKEN"),
#'   fields = c("record_id", "age", "gender")
#' )
#' }
#'
#' @seealso \code{\link{camr_redcap_metadata}}, \code{\link{camr_apply_labels}}
#'
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
#' Retrieves event definitions for longitudinal REDCap projects. Events define
#' the timepoints or visits in a study (e.g., Baseline, Week 4, Week 8).
#'
#' @param redcap_url Character. The REDCap API endpoint URL
#'   (e.g., "https://redcap.example.edu/api/").
#' @param api_token Character. A valid REDCap API token with export permissions.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{event_name}{Character. The unique event name used in data exports.}
#'     \item{event_label}{Character. Human-readable event label.}
#'     \item{event_number}{Integer. Sequential event number within each arm.}
#'     \item{arm_num}{Integer. Arm number (if multiple arms exist).}
#'   }
#'   Returns an empty tibble for non-longitudinal projects.
#'
#' @examples
#' \dontrun{
#' events <- camr_redcap_events(
#'   redcap_url = "https://redcap.example.edu/api/",
#'   api_token = Sys.getenv("REDCAP_API_TOKEN")
#' )
#' }
#'
#' @seealso \code{\link{camr_redcap_form_event_mapping}}
#'
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
  original_event_name <- if ("event_name" %in% names(events)) as.character(events$event_name) else NULL

  # Standardize to event_name column (unique_event_name is what's used in data exports)
  if ("unique_event_name" %in% names(events)) {
    # If both exist, drop the original event_name and rename unique_event_name
    if ("event_name" %in% names(events)) {
      events <- dplyr::select(events, -"event_name")
    }
    events <- dplyr::rename(events, event_name = "unique_event_name")
  }
  if (!"event_name" %in% names(events)) {
    stop("Event metadata does not include an event name column.")
  }

  if ("custom_event_label" %in% names(events)) {
    fallback_label <- if (!is.null(original_event_name) && length(original_event_name) == nrow(events)) {
      original_event_name
    } else if ("event_label" %in% names(events)) {
      as.character(events$event_label)
    } else {
      as.character(events$event_name)
    }
    events <- dplyr::mutate(
      events,
      event_label = dplyr::if_else(
        !is.na(.data$custom_event_label) & nzchar(.data$custom_event_label),
        .data$custom_event_label,
        fallback_label
      )
    )
  } else if ("event_label" %in% names(events)) {
    events <- dplyr::mutate(events, event_label = as.character(.data$event_label))
  } else if (!is.null(original_event_name) && length(original_event_name) == nrow(events)) {
    events <- dplyr::mutate(events, event_label = original_event_name)
  } else {
    events <- dplyr::mutate(events, event_label = as.character(.data$event_name))
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
#' Retrieves general information about a REDCap project including its title,
#' creation date, purpose, and settings.
#'
#' @param redcap_url Character. The REDCap API endpoint URL
#'   (e.g., "https://redcap.example.edu/api/").
#' @param api_token Character. A valid REDCap API token with export permissions.
#'
#' @return A list containing project details including:
#'   \describe{
#'     \item{project_id}{The numeric project ID}
#'     \item{project_title}{The project title}
#'     \item{creation_time}{Project creation timestamp}
#'     \item{purpose}{Project purpose code}
#'     \item{is_longitudinal}{Whether the project uses events}
#'     \item{has_repeating_instruments_or_events}{Whether repeating elements exist}
#'   }
#'
#' @examples
#' \dontrun{
#' info <- camr_redcap_project_info(
#'   redcap_url = "https://redcap.example.edu/api/",
#'   api_token = Sys.getenv("REDCAP_API_TOKEN")
#' )
#' cat("Project:", info$project_title)
#' }
#'
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
#' Exports the project data dictionary in CSV format, which can be useful
#' for documentation or importing into other systems.
#'
#' @param redcap_url Character. The REDCap API endpoint URL
#'   (e.g., "https://redcap.example.edu/api/").
#' @param api_token Character. A valid REDCap API token with export permissions.
#'
#' @return A tibble containing the codebook with the same structure as
#'   \code{\link{camr_redcap_metadata}} but parsed from CSV format.
#'
#' @examples
#' \dontrun{
#' codebook <- camr_redcap_codebook(
#'   redcap_url = "https://redcap.example.edu/api/",
#'   api_token = Sys.getenv("REDCAP_API_TOKEN")
#' )
#' }
#'
#' @seealso \code{\link{camr_redcap_metadata}}
#'
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
#' Creates a lookup table mapping each field to its containing form/instrument.
#' Useful for determining which fields belong to which REDCap instrument.
#'
#' @param metadata A tibble of REDCap metadata as returned by
#'   \code{\link{camr_redcap_metadata}}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{form_name}{Character. The instrument/form name.}
#'     \item{field_name}{Character. The field/variable name.}
#'   }
#'
#' @examples
#' \dontrun{
#' metadata <- camr_redcap_metadata(url, token)
#' form_map <- camr_redcap_form_map(metadata)
#'
#' # Find all fields in a specific form
#' demographics_fields <- form_map$field_name[form_map$form_name == "demographics"]
#' }
#'
#' @seealso \code{\link{camr_redcap_metadata}}
#'
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
#' Identifies which instruments in the exported data are configured as
#' repeating instruments by examining the `redcap_repeat_instrument` column.
#'
#' @param data A tibble of REDCap records as returned by
#'   \code{\link{camr_redcap_read}}.
#'
#' @return A sorted character vector of unique repeating instrument names.
#'   Returns an empty character vector if no repeating instruments exist.
#'
#' @examples
#' \dontrun{
#' data <- camr_redcap_read(url, token)
#' repeating <- camr_repeating_instruments(data)
#' # Returns e.g. c("adverse_events", "medications", "vitals")
#' }
#'
#' @seealso \code{\link{camr_subject_by_instance}}
#'
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
#' Retrieves the mapping between instruments and events for longitudinal
#' REDCap projects. This shows which forms are available at which timepoints.
#'
#' @param redcap_url Character. The REDCap API endpoint URL
#'   (e.g., "https://redcap.example.edu/api/").
#' @param api_token Character. A valid REDCap API token with export permissions.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{form_name}{Character. The instrument/form name.}
#'     \item{event_name}{Character. The unique event name.}
#'   }
#'   Returns an empty tibble for non-longitudinal projects.
#'
#' @details
#' This mapping is useful for understanding the study design and for
#' identifying which forms are collected at single vs. multiple events
#' (used by \code{\link{camr_subject_level}} to identify subject-level forms).
#'
#' @examples
#' \dontrun{
#' mapping <- camr_redcap_form_event_mapping(
#'   redcap_url = "https://redcap.example.edu/api/",
#'   api_token = Sys.getenv("REDCAP_API_TOKEN")
#' )
#' }
#'
#' @seealso \code{\link{camr_redcap_events}}, \code{\link{camr_subject_level}}
#'
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

  form_col <- if ("form_name" %in% names(mapping)) {
    "form_name"
  } else if ("form" %in% names(mapping)) {
    "form"
  } else {
    NA_character_
  }

  event_col <- if ("unique_event_name" %in% names(mapping)) {
    "unique_event_name"
  } else if ("event_name" %in% names(mapping)) {
    "event_name"
  } else {
    NA_character_
  }

  if (is.na(form_col) || is.na(event_col)) {
    return(mapping)
  }

  if (form_col != "form_name") {
    mapping <- dplyr::rename(mapping, form_name = !!rlang::sym(form_col))
  }
  if (event_col != "event_name") {
    mapping <- dplyr::rename(mapping, event_name = !!rlang::sym(event_col))
  }

  dplyr::select(mapping, .data$form_name, .data$event_name)
}
