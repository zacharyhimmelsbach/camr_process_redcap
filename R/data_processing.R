#' Filter out test records
#'
#' Optionally removes rows flagged as test records based on `status_field`.
#' If `exclude_test` is `FALSE` (default), data is returned unchanged.
#'
#' @param data A tibble of REDCap records.
#' @param metadata Optional REDCap metadata for label matching.
#' @param config Configuration list with `id_field`, `status_field`,
#'   `status_test_values`, and `exclude_test`.
#'
#' @return A tibble with test rows removed when configured.
#' @export
camr_filter_test_records <- function(data, metadata = NULL, config) {
  data <- tibble::as_tibble(data)
  exclude_test <- isTRUE(config$exclude_test %||% FALSE)
  if (!exclude_test) {
    return(data)
  }

  status_field <- as.character(config$status_field %||% "")
  if (!nzchar(status_field) || !status_field %in% names(data)) {
    return(data)
  }

  test_values <- as.character(config$status_test_values %||% "Test Record")
  test_values <- test_values[!is.na(test_values) & nzchar(test_values)]
  if (length(test_values) == 0) {
    return(data)
  }

  status_raw <- as.character(data[[status_field]])
  status_label <- status_raw
  if (!is.null(metadata)) {
    labeled <- camr_apply_labels(dplyr::select(data, dplyr::any_of(status_field)), metadata)
    if (status_field %in% names(labeled)) {
      status_label <- as.character(labeled[[status_field]])
    }
  }

  is_test <- (!is.na(status_raw) & status_raw %in% test_values) |
    (!is.na(status_label) & status_label %in% test_values)
  if (!any(is_test)) {
    return(data)
  }

  id_field <- as.character(config$id_field %||% "record_id")
  if (nzchar(id_field) && id_field %in% names(data)) {
    id_values <- as.character(data[[id_field]])
    test_ids <- unique(id_values[is_test & !is.na(id_values) & nzchar(id_values)])
    if (length(test_ids) > 0) {
      id_is_test <- !is.na(id_values) & id_values %in% test_ids
      return(data[!(id_is_test | is_test), , drop = FALSE])
    }
  }

  data[!is_test, , drop = FALSE]
}

#' Extract data for a specific REDCap form
#'
#' Extracts all fields belonging to a specific REDCap instrument/form,
#' along with identifier fields and event information.
#'
#' @param data A tibble of REDCap records as returned by
#'   \code{\link{camr_redcap_read}}.
#' @param metadata A tibble of REDCap metadata as returned by
#'   \code{\link{camr_redcap_metadata}}.
#' @param form_name Character. The name of the REDCap instrument to extract.
#' @param config A list containing configuration options including `id_field`,
#'   `trial_id_field`, `status_field`, and `event_name_field`.
#' @param events A tibble of event metadata from \code{\link{camr_redcap_events}}.
#'   Optional; if provided, event labels and numbers are joined to the output.
#'
#' @return A tibble containing:
#'   \itemize{
#'     \item All fields from the specified form
#'     \item Identifier fields (record ID, trial ID, status)
#'     \item Event information (if longitudinal)
#'     \item Repeat instrument/instance columns (if applicable)
#'   }
#'   Choice fields are converted to labeled factors.
#'
#' @examples
#' \dontrun{
#' demographics <- camr_form_data(
#'   data = redcap_data,
#'   metadata = redcap_metadata,
#'   form_name = "demographics",
#'   config = config,
#'   events = redcap_events
#' )
#' }
#'
#' @seealso \code{\link{camr_subject_level}}, \code{\link{camr_subject_visit}}
#'
#' @export
camr_form_data <- function(data, metadata, form_name, config, events = NULL) {
  metadata <- tibble::as_tibble(metadata)
  data <- tibble::as_tibble(data)
  form_fields <- metadata$field_name[metadata$form_name == form_name]
  form_fields <- form_fields[!is.na(form_fields) & nzchar(form_fields)]
  keep_fields <- camr_keep_fields(config)

  base_fields <- unique(c(
    keep_fields,
    config$event_name_field,
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  ))

  checkbox_stems <- metadata$field_name[
    metadata$form_name == form_name & metadata$field_type == "checkbox"
  ]
  checkbox_stems <- checkbox_stems[!is.na(checkbox_stems) & nzchar(checkbox_stems)]
  checkbox_fields <- names(data)[vapply(
    names(data),
    function(col) any(startsWith(col, paste0(checkbox_stems, "___"))),
    logical(1)
  )]

  complete_field <- paste0(form_name, "_complete")
  complete_available <- complete_field %in% names(data)

  select_fields <- unique(c(base_fields, form_fields, checkbox_fields, if (complete_available) complete_field))
  out <- dplyr::select(data, dplyr::any_of(select_fields))

  if (complete_available) {
    complete_vals <- out[[complete_field]]
    keep <- !is.na(complete_vals) & as.character(complete_vals) != ""
    out <- out[keep, , drop = FALSE]
  } else {
    payload_fields <- setdiff(unique(c(form_fields, checkbox_fields)), base_fields)
    if (length(payload_fields) > 0) {
      keep <- apply(
        out[, payload_fields, drop = FALSE],
        1,
        function(row) {
          any(!is.na(row) & as.character(row) != "")
        }
      )
      out <- out[keep, , drop = FALSE]
    }
  }

  if (!is.null(events) && nrow(events) > 0 && config$event_name_field %in% names(out)) {
    out <- dplyr::left_join(
      out,
      dplyr::select(events, dplyr::any_of(c("event_name", "event_number", "event_label"))),
      by = setNames("event_name", config$event_name_field)
    )
  }
  out <- camr_apply_labels(out, metadata)
  out
}

#' Merge processed form outputs back into REDCap data
#'
#' Combines a list of processed form datasets into a single REDCap dataset by
#' joining on study key fields. For overlapping columns, processed values take
#' precedence over raw values when non-missing.
#'
#' @param redcap_data A tibble of REDCap records as returned by
#'   \code{\link{camr_redcap_read}}.
#' @param form_data_list A list of processed form tibbles, typically generated
#'   by per-form wrapper targets.
#' @param config A list containing configuration options including `id_field`
#'   and `event_name_field`.
#'
#' @return A tibble with processed form columns merged into `redcap_data`.
#'
#' @export
camr_merge_processed_forms <- function(redcap_data, form_data_list, config) {
  merged <- tibble::as_tibble(redcap_data)
  forms <- form_data_list %||% list()
  if (!length(forms)) {
    return(merged)
  }

  key_fields <- unique(c(
    config$id_field,
    config$event_name_field,
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  ))
  keep_fields <- camr_keep_fields(config)
  protected_fields <- unique(c(
    key_fields,
    keep_fields,
    config$trial_id_field %||% character(0),
    config$status_field %||% character(0),
    "event_number",
    "event_label"
  ))

  for (form_df in forms) {
    if (is.null(form_df) || !nrow(form_df)) {
      next
    }

    form_df <- tibble::as_tibble(form_df)
    available_keys <- intersect(key_fields, names(form_df))
    if (!(config$id_field %in% available_keys)) {
      next
    }

    payload_cols <- setdiff(names(form_df), protected_fields)
    if (!length(payload_cols)) {
      next
    }

    join_df <- form_df |>
      dplyr::select(dplyr::any_of(c(available_keys, payload_cols))) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(available_keys))) |>
      dplyr::summarise(dplyr::across(dplyr::everything(), camr_first_value), .groups = "drop")

    merged <- dplyr::left_join(
      merged,
      join_df,
      by = stats::setNames(available_keys, available_keys),
      suffix = c("", "_processed")
    )

    for (col in payload_cols) {
      processed_col <- paste0(col, "_processed")
      if (!processed_col %in% names(merged)) {
        next
      }
      merged[[col]] <- camr_merge_values(merged[[processed_col]], merged[[col]])
      merged[[processed_col]] <- NULL
    }
  }

  merged
}

#' Merge subject-level data into visit or instance data
#'
#' Joins subject-level data to visit-level or repeating instance data,
#' handling overlapping columns by preferring visit values over subject values.
#'
#' @param visit_df A tibble of visit-level or instance-level data.
#' @param subject_df A tibble of subject-level data.
#' @param id_field Character. The name of the ID field to join on.
#'
#' @return A tibble with subject-level columns merged in.
#'
#' @keywords internal
camr_merge_subject <- function(visit_df, subject_df, id_field) {
  overlap <- intersect(names(visit_df), names(subject_df))
  overlap <- setdiff(overlap, id_field)

  joined <- dplyr::left_join(visit_df, subject_df, by = id_field, suffix = c("", "_subject"))
  if (length(overlap) == 0) {
    return(joined)
  }

  for (col in overlap) {
    subj_col <- paste0(col, "_subject")
    if (!subj_col %in% names(joined)) {
      next
    }
    joined[[col]] <- camr_merge_values(joined[[col]], joined[[subj_col]])
    joined[[subj_col]] <- NULL
  }

  joined
}

#' Merge visit-level feature data into subject-visit data
#'
#' Joins additional visit-level features (e.g. TLFB summaries) into an existing
#' subject-visit table using subject ID and event keys.
#'
#' @param visit_df A tibble of subject-visit data.
#' @param feature_df A tibble of visit-level features to merge.
#' @param config A list with `id_field` and `event_name_field`.
#' @param feature_id_field Optional ID column name in `feature_df`.
#' @param feature_event_field Optional event column name in `feature_df`.
#'
#' @return A tibble with feature columns merged into `visit_df`.
#' @export
camr_merge_visit_features <- function(visit_df,
                                      feature_df,
                                      config,
                                      feature_id_field = NULL,
                                      feature_event_field = NULL) {
  visit_df <- tibble::as_tibble(visit_df)
  feature_df <- tibble::as_tibble(feature_df)
  if (!nrow(feature_df)) {
    return(visit_df)
  }

  id_field <- config$id_field
  event_field <- config$event_name_field
  if (!all(c(id_field, event_field) %in% names(visit_df))) {
    return(visit_df)
  }

  fid <- feature_id_field %||% if (id_field %in% names(feature_df)) id_field else NULL
  fevent <- feature_event_field %||% if (event_field %in% names(feature_df)) event_field else NULL
  if (is.null(fid) || is.null(fevent)) {
    return(visit_df)
  }

  if (fid != id_field) {
    feature_df <- dplyr::rename(feature_df, !!id_field := !!rlang::sym(fid))
  }
  if (fevent != event_field) {
    feature_df <- dplyr::rename(feature_df, !!event_field := !!rlang::sym(fevent))
  }

  feature_payload <- setdiff(names(feature_df), c(id_field, event_field))
  if (!length(feature_payload)) {
    return(visit_df)
  }

  feature_df <- feature_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_field, event_field)))) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(feature_payload), camr_first_value), .groups = "drop")

  joined <- dplyr::left_join(visit_df, feature_df, by = c(id_field, event_field), suffix = c("", "_feature"))

  overlap <- intersect(names(visit_df), feature_payload)
  for (col in overlap) {
    feature_col <- paste0(col, "_feature")
    if (!feature_col %in% names(joined)) {
      next
    }
    joined[[col]] <- camr_merge_values(joined[[col]], joined[[feature_col]])
    joined[[feature_col]] <- NULL
  }

  joined
}

#' Create subject-level data
#'
#' Aggregates REDCap data to one row per subject, extracting baseline/enrollment
#' characteristics. For longitudinal projects, identifies subject-level forms
#' (those appearing at only one event) and extracts the first non-missing value
#' for each field.
#'
#' @param data A tibble of REDCap records as returned by
#'   \code{\link{camr_redcap_read}}.
#' @param metadata A tibble of REDCap metadata as returned by
#'   \code{\link{camr_redcap_metadata}}.
#' @param events A tibble of event metadata from \code{\link{camr_redcap_events}}.
#' @param form_event_map A tibble from \code{\link{camr_redcap_form_event_mapping}}.
#'   Optional; if provided, used to identify single-event (subject-level) forms.
#' @param config A list containing configuration options including `id_field`,
#'   `status_field`, and optionally `subject_level_event`.
#' @param apply_labels Logical. Whether to convert multiple choice fields to
#'   labeled factors. Defaults to TRUE.
#'
#' @return A tibble with one row per unique subject containing:
#'   \itemize{
#'     \item Subject identifiers
#'     \item Fields from subject-level forms (or first event)
#'     \item Latest status value across all events
#'   }
#'
#' @details
#' The function uses this logic to identify subject-level data:
#' \enumerate{
#'   \item If `form_event_map` is provided, forms appearing at only one event
#'     are treated as subject-level forms.
#'   \item Otherwise, if `config$subject_level_event` is set, that event is used.
#'   \item Otherwise, the first event (by event_number) is used.
#' }
#'
#' @examples
#' \dontrun{
#' subject_data <- camr_subject_level(
#'   data = redcap_data,
#'   metadata = redcap_metadata,
#'   events = redcap_events,
#'   form_event_map = form_event_map,
#'   config = config
#' )
#' }
#'
#' @seealso \code{\link{camr_subject_visit}}, \code{\link{camr_consort_data}}
#'
#' @export
camr_subject_level <- function(data, metadata, events, form_event_map = NULL, config, apply_labels = TRUE) {
  metadata <- tibble::as_tibble(metadata)
  data <- tibble::as_tibble(data)
  non_repeat <- dplyr::filter(data, is.na(.data$redcap_repeat_instrument) | .data$redcap_repeat_instrument == "")

  if (!is.null(events) && nrow(events) > 0 && config$event_name_field %in% names(non_repeat)) {
    non_repeat <- dplyr::left_join(
      non_repeat,
      dplyr::select(events, dplyr::any_of(c("event_name", "event_number", "event_label"))),
      by = setNames("event_name", config$event_name_field)
    )
  }

  id_field <- config$id_field
  if (!id_field %in% names(non_repeat)) {
    stop("id_field not found in data.")
  }
  keep_fields <- camr_keep_fields(config)

  if (!is.null(form_event_map) && nrow(form_event_map) > 0 &&
      all(c("form_name", "event_name") %in% names(form_event_map))) {
    form_counts <- form_event_map |>
      dplyr::group_by(.data$form_name) |>
      dplyr::summarise(event_n = dplyr::n_distinct(.data$event_name), .groups = "drop")
    subject_forms <- form_counts$form_name[form_counts$event_n == 1]

    subject_fields <- metadata$field_name[metadata$form_name %in% subject_forms]
    base_fields <- unique(c(
      keep_fields
    ))
    select_fields <- unique(c(base_fields, subject_fields))

    subject_df <- dplyr::select(non_repeat, dplyr::any_of(select_fields)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_field))) |>
      dplyr::summarise(
        dplyr::across(dplyr::everything(), camr_first_value),
        .groups = "drop"
      )
  } else {
    if (!is.null(config$subject_level_event)) {
      subject_df <- dplyr::filter(
        non_repeat,
        .data$event_label == config$subject_level_event | .data$event_number == config$subject_level_event
      )
    } else if ("event_number" %in% names(non_repeat)) {
      subject_df <- non_repeat |>
        dplyr::arrange(.data$event_number) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(id_field))) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup()
    } else {
      subject_df <- non_repeat |>
        dplyr::group_by(dplyr::across(dplyr::all_of(id_field))) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup()
    }
  }

  if (config$status_field %in% names(non_repeat) && "event_number" %in% names(non_repeat)) {
    status_latest <- non_repeat |>
      dplyr::arrange(.data$event_number) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_field))) |>
      dplyr::summarise(status_latest = camr_last_value(.data[[config$status_field]]), .groups = "drop")
    subject_df <- dplyr::left_join(
      subject_df,
      status_latest,
      by = id_field
    )
    if (config$status_field %in% names(subject_df)) {
      subject_df[[config$status_field]] <- camr_merge_values(
        subject_df[[config$status_field]],
        subject_df$status_latest
      )
    }
    subject_df$status_latest <- NULL
  }

  if (isTRUE(apply_labels)) {
    subject_df <- camr_apply_labels(subject_df, metadata)
  }
  subject_df
}

#' Create subject-visit level data
#'
#' Creates a dataset with one row per subject-visit combination, merging in
#' subject-level characteristics. This is the primary analysis dataset for
#' longitudinal studies.
#'
#' @param data A tibble of REDCap records as returned by
#'   \code{\link{camr_redcap_read}}.
#' @param metadata A tibble of REDCap metadata as returned by
#'   \code{\link{camr_redcap_metadata}}.
#' @param events A tibble of event metadata from \code{\link{camr_redcap_events}}.
#' @param subject_level A tibble of subject-level data from
#'   \code{\link{camr_subject_level}}.
#' @param config A list containing configuration options including `id_field`
#'   and `event_name_field`.
#'
#' @return A tibble with one row per subject-visit containing:
#'   \itemize{
#'     \item All non-repeating instrument fields
#'     \item Subject-level characteristics merged in
#'     \item Event labels and numbers
#'   }
#'   Choice fields are converted to labeled factors.
#'
#' @examples
#' \dontrun{
#' visit_data <- camr_subject_visit(
#'   data = redcap_data,
#'   metadata = redcap_metadata,
#'   events = redcap_events,
#'   subject_level = subject_data,
#'   config = config
#' )
#' }
#'
#' @seealso \code{\link{camr_subject_level}}, \code{\link{camr_subject_by_instance}}
#'
#' @export
camr_subject_visit <- function(data, metadata, events, subject_level, config) {
  data <- tibble::as_tibble(data)
  non_repeat <- dplyr::filter(data, is.na(.data$redcap_repeat_instrument) | .data$redcap_repeat_instrument == "")
  keep_fields <- camr_keep_fields(config)

  if (!is.null(events) && nrow(events) > 0 && config$event_name_field %in% names(non_repeat)) {
    non_repeat <- dplyr::left_join(
      non_repeat,
      dplyr::select(events, dplyr::any_of(c("event_name", "event_number", "event_label"))),
      by = setNames("event_name", config$event_name_field)
    )
  }

  visit <- camr_merge_subject(non_repeat, subject_level, config$id_field)
  visit <- camr_apply_labels(visit, metadata)
  visit <- dplyr::select(visit, -dplyr::any_of(c("redcap_repeat_instrument", "redcap_repeat_instance")))

  missing_keep <- setdiff(keep_fields, names(visit))
  if (length(missing_keep) > 0) {
    for (col in missing_keep) {
      visit[[col]] <- NA_character_
    }
  }
  visit <- dplyr::select(visit, dplyr::any_of(c(keep_fields, names(visit))))
  visit
}

#' Build analysis-focused subject-level data
#'
#' Selects a reduced set of columns from `subject_level` based on
#' `config$analysis_forms` plus configured keep fields.
#'
#' @param subject_level Subject-level tibble.
#' @param metadata REDCap metadata tibble.
#' @param config Configuration list.
#' @return A tibble with analysis-ready subject-level columns.
#' @export
camr_analysis_subject <- function(subject_level, metadata, config) {
  subject_level <- tibble::as_tibble(subject_level)
  metadata <- tibble::as_tibble(metadata)

  keep_fields <- camr_keep_fields(config)
  analysis_forms <- as.character(config$analysis_forms %||% character(0))
  analysis_forms <- unique(analysis_forms[!is.na(analysis_forms) & nzchar(analysis_forms)])

  form_fields <- character(0)
  if (length(analysis_forms) > 0 && all(c("form_name", "field_name") %in% names(metadata))) {
    form_fields <- metadata$field_name[metadata$form_name %in% analysis_forms]
    form_fields <- unique(as.character(form_fields[!is.na(form_fields) & nzchar(form_fields)]))
  }

  resolved_fields <- unique(unlist(lapply(form_fields, function(field) {
    c(field, names(subject_level)[startsWith(names(subject_level), paste0(field, "___"))])
  }), use.names = FALSE))

  select_fields <- unique(c(keep_fields, resolved_fields))
  dplyr::select(subject_level, dplyr::any_of(select_fields))
}

#' Build analysis-focused visit-level data
#'
#' Selects a reduced set of columns from `subject_visit` based on
#' `config$analysis_forms` plus all columns in `analysis_subject`.
#'
#' @param subject_visit Subject-visit tibble.
#' @param analysis_subject Analysis-subject tibble.
#' @param metadata REDCap metadata tibble.
#' @param config Configuration list.
#' @return A tibble with analysis-ready visit-level columns.
#' @export
camr_analysis_visit <- function(subject_visit, analysis_subject, metadata, config) {
  subject_visit <- tibble::as_tibble(subject_visit)
  analysis_subject <- tibble::as_tibble(analysis_subject)
  metadata <- tibble::as_tibble(metadata)

  keep_fields <- camr_keep_fields(config)
  analysis_forms <- as.character(config$analysis_forms %||% character(0))
  analysis_forms <- unique(analysis_forms[!is.na(analysis_forms) & nzchar(analysis_forms)])

  form_fields <- character(0)
  if (length(analysis_forms) > 0 && all(c("form_name", "field_name") %in% names(metadata))) {
    form_fields <- metadata$field_name[metadata$form_name %in% analysis_forms]
    form_fields <- unique(as.character(form_fields[!is.na(form_fields) & nzchar(form_fields)]))
  }

  resolved_fields <- unique(unlist(lapply(form_fields, function(field) {
    c(field, names(subject_visit)[startsWith(names(subject_visit), paste0(field, "___"))])
  }), use.names = FALSE))

  select_fields <- unique(c(keep_fields, names(analysis_subject), resolved_fields))
  dplyr::select(subject_visit, dplyr::any_of(select_fields))
}

#' Create subject-by-instance data for a repeating instrument
#'
#' Extracts data from a repeating instrument. Results in one row per
#' subject-instance combination.
#'
#' @param data A tibble of REDCap records as returned by
#'   \code{\link{camr_redcap_read}}.
#' @param metadata A tibble of REDCap metadata as returned by
#'   \code{\link{camr_redcap_metadata}}.
#' @param events A tibble of event metadata from \code{\link{camr_redcap_events}}.
#' @param instrument Character. The name of the repeating instrument to extract.
#' @param config A list containing configuration options including `id_field`
#'   and `event_name_field`.
#'
#' @return A tibble with one row per subject-instance containing:
#'   \itemize{
#'     \item All fields from the repeating instrument
#'     \item `redcap_repeat_instance` indicating the instance number
#'     \item Event information (if longitudinal)
#'   }
#'   Returns an empty tibble if the data has no repeating instruments.
#'
#' @examples
#' \dontrun{
#' adverse_events <- camr_subject_by_instance(
#'   data = redcap_data,
#'   metadata = redcap_metadata,
#'   events = redcap_events,
#'   instrument = "adverse_events",
#'   config = config
#' )
#' }
#'
#' @seealso \code{\link{camr_repeating_instruments}}
#'
#' @export
camr_subject_by_instance <- function(data, metadata, events, instrument, config) {
  data <- tibble::as_tibble(data)
  metadata <- tibble::as_tibble(metadata)
  if (!"redcap_repeat_instrument" %in% names(data)) {
    return(tibble::tibble())
  }
  repeat_df <- dplyr::filter(data, .data$redcap_repeat_instrument == instrument)
  if (!nrow(repeat_df)) {
    return(repeat_df)
  }

  base_fields <- unique(c(
    camr_keep_fields(config),
    config$event_name_field,
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  ))

  instrument_fields <- character(0)
  if (all(c("form_name", "field_name") %in% names(metadata))) {
    instrument_fields <- metadata$field_name[metadata$form_name == instrument]
    instrument_fields <- instrument_fields[!is.na(instrument_fields) & instrument_fields != ""]
  }

  checkbox_stems <- character(0)
  if (all(c("form_name", "field_name", "field_type") %in% names(metadata))) {
    checkbox_stems <- metadata$field_name[
      metadata$form_name == instrument & metadata$field_type == "checkbox"
    ]
    checkbox_stems <- checkbox_stems[!is.na(checkbox_stems) & nzchar(checkbox_stems)]
  }
  checkbox_fields <- names(data)[vapply(
    names(data),
    function(col) any(startsWith(col, paste0(checkbox_stems, "___"))),
    logical(1)
  )]

  non_repeat <- dplyr::filter(data, is.na(.data$redcap_repeat_instrument) | .data$redcap_repeat_instrument == "")
  if (nrow(non_repeat) > 0 && config$id_field %in% names(non_repeat) && config$id_field %in% names(repeat_df)) {
    fill_fields <- intersect(base_fields, names(non_repeat))
    fill_fields <- setdiff(fill_fields, config$id_field)
    if (length(fill_fields) > 0) {
      for (col in setdiff(fill_fields, names(repeat_df))) {
        repeat_df[[col]] <- NA_character_
      }
      subject_fill <- non_repeat |>
        dplyr::group_by(dplyr::across(dplyr::all_of(config$id_field))) |>
        dplyr::summarise(dplyr::across(dplyr::all_of(fill_fields), camr_first_value), .groups = "drop")
      repeat_df <- dplyr::left_join(repeat_df, subject_fill, by = config$id_field, suffix = c("", "_subject"))
      for (col in fill_fields) {
        subject_col <- paste0(col, "_subject")
        if (!subject_col %in% names(repeat_df)) {
          next
        }
        repeat_df[[col]] <- camr_merge_values(repeat_df[[col]], repeat_df[[subject_col]])
        repeat_df[[subject_col]] <- NULL
      }
    }
  }

  keep_fields <- unique(c(base_fields, instrument_fields, checkbox_fields))
  repeat_df <- dplyr::select(repeat_df, dplyr::any_of(keep_fields))

  if (!is.null(events) && nrow(events) > 0 && config$event_name_field %in% names(repeat_df)) {
    repeat_df <- dplyr::left_join(
      repeat_df,
      dplyr::select(events, dplyr::any_of(c("event_name", "event_number", "event_label"))),
      by = setNames("event_name", config$event_name_field)
    )
  }

  repeat_df <- camr_apply_labels(repeat_df, metadata)
  repeat_df
}

#' Summarize CONSORT-style counts
#'
#' Calculates participant flow counts suitable for CONSORT diagrams, including
#' number screened, consented, active/completed, and dropped out.
#'
#' @param data A tibble of REDCap records as returned by
#'   \code{\link{camr_redcap_read}}.
#' @param metadata A tibble of REDCap metadata as returned by
#'   \code{\link{camr_redcap_metadata}}. Optional; if provided, used for
#'   label matching.
#' @param config A list containing configuration options including:
#'   \describe{
#'     \item{id_field}{Field name for participant ID}
#'     \item{trial_id_field}{Field name for trial/consent ID (optional)}
#'     \item{status_field}{Field name for participant status}
#'     \item{status_active_values}{Character vector of status values indicating active/completed}
#'     \item{status_dropout_values}{Character vector of status values indicating dropout}
#'     \item{consort_screening_eligibility_field}{Optional field indicating screening eligibility outcome}
#'     \item{consort_screen_fail_values}{Optional values of consort_screening_eligibility_field that indicate screen-fail}
#'     \item{consort_population_field}{Optional field indicating population/arm before randomization (e.g., randomized cohort vs healthy controls)}
#'     \item{consort_population_randomization_eligible_values}{Optional values of consort_population_field eligible for randomization}
#'     \item{consort_randomization_field}{Optional field containing treatment arm/group assignment}
#'     \item{consort_group_values}{Optional character vector of display labels (e.g., blinded names) for randomized groups; mapped by order to observed groups and defaults to observed values}
#'     \item{consort_intervention_completed_field}{Optional status values indicating intervention completion}
#'     \item{consort_study_completed_field}{Optional status values indicating study completion}
#'     \item{consort_attrition_status_values}{Optional status values treated as attrition reasons}
#'     \item{consort_preconsent_exclusion_reason_field}{Optional field containing pre-consent exclusion reasons}
#'     \item{consort_prerandomization_exclusion_reason_field}{Optional field containing exclusion reasons after consent but before randomization}
#'     \item{consort_prerandomization_exclusion_status_values}{Optional status values used to filter pre-randomization exclusions}
#'   }
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{stage}{Character. The flow stage name.}
#'     \item{n}{Integer. Count of unique participants at that stage.}
#'     \item{metric}{Character. Machine-readable metric identifier used by report templates.}
#'     \item{group}{Character. Treatment arm/group when applicable; otherwise NA.}
#'     \item{phase}{Character. Flow phase (for example intervention or study) when applicable; otherwise NA.}
#'     \item{reason}{Character. Exclusion/attrition reason when applicable; otherwise NA.}
#'   }
#'   Always includes the four high-level stages: "Screened", "Consented",
#'   "Active/Completed", "Dropped Out". Additional rows are included when
#'   optional CONSORT configuration fields are present.
#'
#' @details
#' Counts are based on unique participant IDs:
#' \itemize{
#'   \item Screened: All unique IDs in the data
#'   \item Consented: IDs with non-empty trial ID field
#'   \item Active/Completed: IDs with status matching `status_active_values`
#'   \item Dropped Out: IDs with status matching `status_dropout_values`
#' }
#'
#' @examples
#' \dontrun{
#' consort <- camr_consort_data(
#'   data = redcap_data,
#'   metadata = redcap_metadata,
#'   config = config
#' )
#' # Use with ggplot2 or consort package for visualization
#' }
#'
#' @seealso \code{\link{camr_render_report}}
#'
#' @export
camr_consort_data <- function(data, metadata = NULL, config) {
  data <- tibble::as_tibble(data)
  id_field <- config$id_field
  trial_field <- config$trial_id_field
  status_field <- config$status_field

  if (!id_field %in% names(data)) {
    stop("id_field not found in data.")
  }

  labeled <- NULL
  if (!is.null(metadata)) {
    labeled <- camr_apply_labels(data, metadata)
  }

  value_present <- function(x) {
    x <- as.character(x)
    !is.na(x) & trimws(x) != ""
  }

  as_character <- function(x) {
    if (is.factor(x)) {
      return(as.character(x))
    }
    as.character(x)
  }

  summarize_subject <- function(df, fields, status_field = NULL) {
    present <- fields[fields %in% names(df)]
    out <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_field))) |>
      dplyr::summarise(
        dplyr::across(dplyr::any_of(present), camr_first_value),
        .groups = "drop"
      )

    if (!is.null(status_field) && status_field %in% names(df)) {
      latest_status <- df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(id_field))) |>
        dplyr::summarise(status_latest = camr_last_value(.data[[status_field]]), .groups = "drop")
      out <- dplyr::left_join(out, latest_status, by = id_field)
      if (status_field %in% names(out)) {
        out[[status_field]] <- camr_merge_values(out$status_latest, out[[status_field]])
      } else {
        out[[status_field]] <- out$status_latest
      }
      out$status_latest <- NULL
    }

    out
  }

  match_values <- function(raw_vals, labeled_vals, targets) {
    targets <- as.character(targets %||% character(0))
    if (length(targets) == 0) {
      return(rep(FALSE, length(raw_vals)))
    }
    raw_vals <- as_character(raw_vals)
    labeled_vals <- as_character(labeled_vals)
    raw_vals %in% targets | labeled_vals %in% targets
  }

  add_row <- function(stage, n, metric, group = NA_character_, phase = NA_character_, reason = NA_character_) {
    tibble::tibble(
      stage = as.character(stage),
      n = as.integer(n),
      metric = as.character(metric),
      group = as.character(group),
      phase = as.character(phase),
      reason = as.character(reason)
    )
  }

  rand_field <- config$consort_randomization_field %||% config$randomization_field
  screening_eligibility_field <- config$consort_screening_eligibility_field
  screen_fail_values <- as.character(config$consort_screen_fail_values %||% character(0))
  screen_fail_values <- screen_fail_values[!is.na(screen_fail_values) & nzchar(screen_fail_values)]
  population_field <- config$consort_population_field
  eligible_pop_values <- as.character(config$consort_population_randomization_eligible_values %||% character(0))
  eligible_pop_values <- eligible_pop_values[!is.na(eligible_pop_values) & nzchar(eligible_pop_values)]
  int_completed_values <- as.character(config$consort_intervention_completed_field %||% character(0))
  int_completed_values <- int_completed_values[!is.na(int_completed_values) & nzchar(int_completed_values)]
  study_completed_values <- as.character(config$consort_study_completed_field %||% character(0))
  study_completed_values <- study_completed_values[!is.na(study_completed_values) & nzchar(study_completed_values)]
  preconsent_reason_field <- config$consort_preconsent_exclusion_reason_field
  prerand_reason_field <- config$consort_prerandomization_exclusion_reason_field

  fields_needed <- unique(c(
    trial_field,
    status_field,
    screening_eligibility_field,
    population_field,
    rand_field,
    preconsent_reason_field,
    prerand_reason_field
  ))
  fields_needed <- fields_needed[!is.na(fields_needed) & nzchar(fields_needed)]

  status_for_summary <- if (is.character(status_field) && length(status_field) == 1 && status_field %in% fields_needed) {
    status_field
  } else {
    NULL
  }
  subject_raw <- summarize_subject(
    data,
    fields_needed,
    status_field = status_for_summary
  )
  subject_labeled <- if (!is.null(labeled)) {
    summarize_subject(
      labeled,
      fields_needed,
      status_field = status_for_summary
    )
  } else {
    subject_raw
  }

  if (!id_field %in% names(subject_raw)) {
    stop("id_field not found in data.")
  }

  status_raw <- if (status_field %in% names(subject_raw)) subject_raw[[status_field]] else rep(NA_character_, nrow(subject_raw))
  status_label <- if (status_field %in% names(subject_labeled)) subject_labeled[[status_field]] else status_raw

  screened <- nrow(subject_raw)
  consent_mask <- if (trial_field %in% names(subject_raw)) value_present(subject_raw[[trial_field]]) else rep(NA, nrow(subject_raw))
  consented <- if (trial_field %in% names(subject_raw)) sum(consent_mask) else NA_integer_
  consent_include <- if (trial_field %in% names(subject_raw)) consent_mask else rep(TRUE, nrow(subject_raw))

  active_vals <- as.character(config$status_active_values %||% character(0))
  dropout_vals <- as.character(config$status_dropout_values %||% character(0))

  active <- if (length(active_vals) > 0) sum(match_values(status_raw, status_label, active_vals), na.rm = TRUE) else NA_integer_
  dropout <- if (length(dropout_vals) > 0) sum(match_values(status_raw, status_label, dropout_vals), na.rm = TRUE) else NA_integer_
  screen_fail <- NA_integer_
  if (is.character(screening_eligibility_field) &&
      nzchar(screening_eligibility_field) &&
      screening_eligibility_field %in% names(subject_raw) &&
      length(screen_fail_values) > 0) {
    screening_raw <- as_character(subject_raw[[screening_eligibility_field]])
    screening_label <- if (screening_eligibility_field %in% names(subject_labeled)) {
      as_character(subject_labeled[[screening_eligibility_field]])
    } else {
      screening_raw
    }
    screen_fail <- sum(match_values(screening_raw, screening_label, screen_fail_values), na.rm = TRUE)
  }
  study_complete_all <- if (length(study_completed_values) > 0) {
    match_values(status_raw, status_label, study_completed_values)
  } else {
    rep(NA, nrow(subject_raw))
  }
  int_complete_all <- if (length(int_completed_values) > 0 || length(study_completed_values) > 0) {
    match_values(status_raw, status_label, int_completed_values) | study_complete_all
  } else {
    rep(NA, nrow(subject_raw))
  }

  base_rows <- list(add_row("Screened", screened, "screened"))
  if (!is.na(screen_fail)) {
    base_rows <- c(base_rows, list(add_row("Screen Fail", screen_fail, "screen_fail")))
  }
  base_rows <- c(
    base_rows,
    list(
      add_row("Consented", consented, "consented"),
      add_row("Active/Completed", active, "active_completed"),
      add_row("Dropped Out", dropout, "dropped_out")
    )
  )
  out <- dplyr::bind_rows(!!!base_rows)

  prerand_excluded_mask <- rep(FALSE, nrow(subject_raw))
  if (is.character(prerand_reason_field) && nzchar(prerand_reason_field) && prerand_reason_field %in% names(subject_raw)) {
    prerand_reason_vals <- if (prerand_reason_field %in% names(subject_labeled)) {
      subject_labeled[[prerand_reason_field]]
    } else {
      subject_raw[[prerand_reason_field]]
    }
    prerand_excluded_mask <- value_present(as_character(prerand_reason_vals))
    filter_status <- as.character(config$consort_prerandomization_exclusion_status_values %||% character(0))
    if (length(filter_status) > 0) {
      prerand_excluded_mask <- prerand_excluded_mask & match_values(status_raw, status_label, filter_status)
    }
  }

  population_map <- tibble::tibble(pop_raw = character(0), pop_display = character(0))
  pop_raw_vals <- rep(NA_character_, nrow(subject_raw))
  pop_label_vals <- pop_raw_vals
  if (is.character(population_field) && nzchar(population_field) && population_field %in% names(subject_raw)) {
    pop_raw_vals <- as_character(subject_raw[[population_field]])
    pop_label_vals <- if (population_field %in% names(subject_labeled)) as_character(subject_labeled[[population_field]]) else pop_raw_vals
    pop_present <- value_present(pop_raw_vals)
    observed_pop <- unique(pop_label_vals[pop_present])
    observed_pop <- observed_pop[!is.na(observed_pop) & nzchar(observed_pop)]
    observed_pop <- sort(as.character(observed_pop))
    if (length(observed_pop) > 0) {
      population_map <- tibble::tibble(pop_raw = observed_pop, pop_display = observed_pop)
      for (i in seq_len(nrow(population_map))) {
        pop_value_raw <- as.character(population_map$pop_raw[[i]])
        pop_value_display <- as.character(population_map$pop_display[[i]])
        in_pop <- match_values(pop_raw_vals, pop_label_vals, pop_value_raw)
        rows <- list(
          add_row(paste0(pop_value_display, " (consented)"), sum(in_pop & consent_include, na.rm = TRUE), "population_group", group = pop_value_display)
        )
        if (!all(is.na(int_complete_all))) {
          rows <- c(
            rows,
            list(add_row(paste0(pop_value_display, " completed intervention"), sum(in_pop & consent_include & int_complete_all, na.rm = TRUE), "population_completed_intervention", group = pop_value_display, phase = "intervention"))
          )
        }
        if (!all(is.na(study_complete_all))) {
          rows <- c(
            rows,
            list(add_row(paste0(pop_value_display, " completed study"), sum(in_pop & consent_include & study_complete_all, na.rm = TRUE), "population_completed_study", group = pop_value_display, phase = "study"))
          )
        }
        out <- dplyr::bind_rows(out, !!!rows)
      }
    }
  }

  if (is.character(rand_field) && nzchar(rand_field) && rand_field %in% names(subject_raw)) {
    rand_raw <- as_character(subject_raw[[rand_field]])
    rand_label <- if (rand_field %in% names(subject_labeled)) as_character(subject_labeled[[rand_field]]) else rand_raw
    rand_present_any <- value_present(rand_raw) & consent_include
    attrition_values <- as.character(config$consort_attrition_status_values %||% c("Withdrawn", "Lost to Follow-Up", "Terminated"))
    if (length(eligible_pop_values) > 0 && nrow(population_map) > 0) {
      rand_present <- rand_present_any & match_values(pop_raw_vals, pop_label_vals, eligible_pop_values)
    } else {
      rand_present <- rand_present_any
    }
    rand_present <- rand_present & !prerand_excluded_mask
    n_randomized <- sum(rand_present)
    out <- dplyr::bind_rows(out, add_row("Randomized", n_randomized, "randomized"))

    if (nrow(population_map) > 0 && is.character(population_field) && nzchar(population_field) && population_field %in% names(subject_raw)) {
      for (i in seq_len(nrow(population_map))) {
        pop_value_raw <- as.character(population_map$pop_raw[[i]])
        pop_value_display <- as.character(population_map$pop_display[[i]])
        in_pop <- match_values(pop_raw_vals, pop_label_vals, pop_value_raw)
        is_eligible <- if (length(eligible_pop_values) > 0) {
          isTRUE(match_values(pop_value_raw, pop_value_display, eligible_pop_values)[1])
        } else {
          sum(in_pop & rand_present_any, na.rm = TRUE) > 0
        }
        out <- dplyr::bind_rows(
          out,
          add_row(paste0(pop_value_display, " eligible for randomization"), as.integer(is_eligible), "population_randomization_eligible", group = pop_value_display),
          add_row(paste0(pop_value_display, " eligible for randomization"), sum(in_pop & rand_present, na.rm = TRUE), "population_randomized", group = pop_value_display)
        )
        for (reason_val in attrition_values) {
          in_reason <- match_values(status_raw, status_label, reason_val)
          n_attr_prerand <- sum(in_pop & consent_include & !rand_present & in_reason, na.rm = TRUE)
          out <- dplyr::bind_rows(
            out,
            add_row(
              paste0(pop_value_display, " prerandomization attrition: ", reason_val),
              n_attr_prerand,
              "population_prerandomization_attrition_reason",
              group = pop_value_display,
              phase = "prerandomization",
              reason = reason_val
            )
          )
        }
      }
    }

    observed_groups <- unique(rand_label[rand_present])
    observed_groups <- observed_groups[!is.na(observed_groups) & nzchar(observed_groups)]
    observed_groups <- sort(as.character(observed_groups))

    configured_groups <- as.character(config$consort_group_values %||% character(0))
    configured_groups <- configured_groups[!is.na(configured_groups) & nzchar(configured_groups)]

    if (length(observed_groups) == 0) {
      group_map <- tibble::tibble(group_raw = character(0), group_display = character(0))
    } else if (length(configured_groups) == 0) {
      group_map <- tibble::tibble(group_raw = observed_groups, group_display = observed_groups)
    } else {
      if (length(configured_groups) < length(observed_groups)) {
        warning("consort_group_values has fewer entries than observed randomized groups; unmatched groups will use observed labels.")
        group_display <- c(configured_groups, observed_groups[(length(configured_groups) + 1):length(observed_groups)])
      } else if (length(configured_groups) > length(observed_groups)) {
        warning("consort_group_values has more entries than observed randomized groups; extra labels will be ignored.")
        group_display <- configured_groups[seq_len(length(observed_groups))]
      } else {
        group_display <- configured_groups
      }
      group_map <- tibble::tibble(group_raw = observed_groups, group_display = group_display)
    }

    int_complete <- int_complete_all
    for (i in seq_len(nrow(group_map))) {
      g_raw <- as.character(group_map$group_raw[[i]])
      g <- as.character(group_map$group_display[[i]])
      in_group <- match_values(rand_raw, rand_label, g_raw) & rand_present
      n_group <- sum(in_group, na.rm = TRUE)
      out <- dplyr::bind_rows(
        out,
        add_row(paste0(g, " randomized"), n_group, "randomized_group", group = g)
      )

      if (all(is.na(int_complete))) {
        next
      }

      int_true <- !is.na(int_complete) & int_complete
      int_false <- !is.na(int_complete) & !int_complete
      n_completed_int <- sum(in_group & int_true, na.rm = TRUE)
      out <- dplyr::bind_rows(
        out,
        add_row(paste0(g, " completed intervention"), n_completed_int, "completed_intervention_group", group = g, phase = "intervention")
      )

      if (length(study_completed_values) > 0) {
        n_completed_study <- sum(in_group & study_complete_all, na.rm = TRUE)
        out <- dplyr::bind_rows(
          out,
          add_row(paste0(g, " completed study"), n_completed_study, "completed_study_group", group = g, phase = "study")
        )
      }

      for (reason_val in attrition_values) {
        in_reason <- match_values(status_raw, status_label, reason_val)
        n_attr_int <- sum(in_group & in_reason & int_false, na.rm = TRUE)
        n_attr_study <- sum(in_group & in_reason & int_true, na.rm = TRUE)
        out <- dplyr::bind_rows(
          out,
          add_row(paste0(g, " attrition (intervention): ", reason_val), n_attr_int, "attrition_group_reason", group = g, phase = "intervention", reason = reason_val),
          add_row(paste0(g, " attrition (study): ", reason_val), n_attr_study, "attrition_group_reason", group = g, phase = "study", reason = reason_val)
        )
      }
    }
  }

  if (is.character(preconsent_reason_field) && nzchar(preconsent_reason_field) && preconsent_reason_field %in% names(subject_raw)) {
    reason_vals <- if (preconsent_reason_field %in% names(subject_labeled)) subject_labeled[[preconsent_reason_field]] else subject_raw[[preconsent_reason_field]]
    reason_vals <- as_character(reason_vals)
    valid <- value_present(reason_vals)
    if (any(valid)) {
      reason_tbl <- as.data.frame(table(reason_vals[valid]), stringsAsFactors = FALSE)
      names(reason_tbl) <- c("reason", "n")
      total <- sum(reason_tbl$n)
      out <- dplyr::bind_rows(out, add_row("Excluded before consent", total, "preconsent_excluded_total"))
      out <- dplyr::bind_rows(
        out,
        tibble::tibble(
          stage = paste0("Excluded before consent: ", reason_tbl$reason),
          n = as.integer(reason_tbl$n),
          metric = "preconsent_exclusion_reason",
          group = NA_character_,
          phase = "preconsent",
          reason = as.character(reason_tbl$reason)
        )
      )
    }
  }

  if (is.character(prerand_reason_field) && nzchar(prerand_reason_field) && prerand_reason_field %in% names(subject_raw)) {
    reason_vals <- if (prerand_reason_field %in% names(subject_labeled)) subject_labeled[[prerand_reason_field]] else subject_raw[[prerand_reason_field]]
    reason_vals <- as_character(reason_vals)
    valid_reason <- prerand_excluded_mask
    if (any(valid_reason)) {
      reason_tbl <- as.data.frame(table(reason_vals[valid_reason]), stringsAsFactors = FALSE)
      names(reason_tbl) <- c("reason", "n")
      total <- sum(reason_tbl$n)
      out <- dplyr::bind_rows(out, add_row("Excluded before randomization", total, "prerandomization_excluded_total"))
      out <- dplyr::bind_rows(
        out,
        tibble::tibble(
          stage = paste0("Excluded before randomization: ", reason_tbl$reason),
          n = as.integer(reason_tbl$n),
          metric = "prerandomization_exclusion_reason",
          group = NA_character_,
          phase = "prerandomization",
          reason = as.character(reason_tbl$reason)
        )
      )
    }
  }

  out
}
