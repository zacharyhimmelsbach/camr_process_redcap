#' Extract data for a specific REDCap form
#'
#' @param data REDCap data.
#' @param metadata REDCap metadata.
#' @param form_name REDCap form name.
#' @param config Config list.
#' @param events REDCap event metadata (optional).
#' @return A tibble with form fields and identifiers.
#' @export
camr_form_data <- function(data, metadata, form_name, config, events = NULL) {
  metadata <- tibble::as_tibble(metadata)
  data <- tibble::as_tibble(data)
  form_fields <- metadata$field_name[metadata$form_name == form_name]

  base_fields <- unique(c(
    config$id_field,
    config$trial_id_field,
    config$status_field,
    config$event_name_field,
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  ))

  select_fields <- unique(c(base_fields, form_fields))
  out <- dplyr::select(data, dplyr::any_of(select_fields))
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

#' Create subject-level data
#'
#' @param data REDCap data.
#' @param metadata REDCap metadata.
#' @param events REDCap event metadata.
#' @param form_event_map REDCap form-event mapping (optional).
#' @param apply_labels Whether to convert multiple choice fields to factors.
#' @param config Config list.
#' @return A tibble with one row per subject.
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

  if (!is.null(form_event_map) && nrow(form_event_map) > 0 &&
      all(c("form_name", "event_name") %in% names(form_event_map))) {
    form_counts <- form_event_map |>
      dplyr::group_by(.data$form_name) |>
      dplyr::summarise(event_n = dplyr::n_distinct(.data$event_name), .groups = "drop")
    subject_forms <- form_counts$form_name[form_counts$event_n == 1]

    subject_fields <- metadata$field_name[metadata$form_name %in% subject_forms]
    base_fields <- unique(c(
      config$id_field,
      config$trial_id_field,
      config$status_field
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
#' @param data REDCap data.
#' @param metadata REDCap metadata.
#' @param events REDCap event metadata.
#' @param subject_level Subject-level data.
#' @param config Config list.
#' @return A tibble with one row per subject-visit.
#' @export
camr_subject_visit <- function(data, metadata, events, subject_level, config) {
  data <- tibble::as_tibble(data)
  non_repeat <- dplyr::filter(data, is.na(.data$redcap_repeat_instrument) | .data$redcap_repeat_instrument == "")

  if (!is.null(events) && nrow(events) > 0 && config$event_name_field %in% names(non_repeat)) {
    non_repeat <- dplyr::left_join(
      non_repeat,
      dplyr::select(events, dplyr::any_of(c("event_name", "event_number", "event_label"))),
      by = setNames("event_name", config$event_name_field)
    )
  }

  visit <- camr_merge_subject(non_repeat, subject_level, config$id_field)
  visit <- camr_apply_labels(visit, metadata)
  visit
}

#' Create subject-by-instance data for a repeating instrument
#'
#' @param data REDCap data.
#' @param metadata REDCap metadata.
#' @param events REDCap event metadata.
#' @param subject_level Subject-level data.
#' @param instrument Repeating instrument name.
#' @param config Config list.
#' @return A tibble with one row per subject-instance.
#' @export
camr_subject_by_instance <- function(data, metadata, events, subject_level, instrument, config) {
  data <- tibble::as_tibble(data)
  if (!"redcap_repeat_instrument" %in% names(data)) {
    return(tibble::tibble())
  }
  repeat_df <- dplyr::filter(data, .data$redcap_repeat_instrument == instrument)

  if (!is.null(events) && nrow(events) > 0 && config$event_name_field %in% names(repeat_df)) {
    repeat_df <- dplyr::left_join(
      repeat_df,
      dplyr::select(events, dplyr::any_of(c("event_name", "event_number", "event_label"))),
      by = setNames("event_name", config$event_name_field)
    )
  }

  repeat_df <- camr_merge_subject(repeat_df, subject_level, config$id_field)

  repeat_df <- camr_apply_labels(repeat_df, metadata)
  repeat_df
}

#' Summarize CONSORT-style counts
#'
#' @param data REDCap data.
#' @param metadata REDCap metadata (optional).
#' @param config Config list.
#' @return A tibble with consort counts.
#' @export
camr_consort_data <- function(data, metadata = NULL, config) {
  data <- tibble::as_tibble(data)
  id_field <- config$id_field
  trial_field <- config$trial_id_field
  status_field <- config$status_field

  if (!id_field %in% names(data)) {
    stop("id_field not found in data.")
  }

  screened <- dplyr::n_distinct(data[[id_field]])
  consented <- if (trial_field %in% names(data)) {
    dplyr::n_distinct(data[[id_field]][!is.na(data[[trial_field]]) & data[[trial_field]] != ""])
  } else {
    NA_integer_
  }

  status_vals_raw <- if (status_field %in% names(data)) {
    as.character(data[[status_field]])
  } else {
    character(0)
  }
  status_vals <- status_vals_raw
  if (!is.null(metadata)) {
    labeled <- camr_apply_labels(data, metadata)
    if (status_field %in% names(labeled)) {
      status_vals <- as.character(labeled[[status_field]])
    }
  }

  active_vals <- as.character(config$status_active_values %||% character(0))
  dropout_vals <- as.character(config$status_dropout_values %||% character(0))

  active <- if (length(status_vals_raw) > 0 && length(active_vals) > 0) {
    in_active <- status_vals %in% active_vals | status_vals_raw %in% active_vals
    dplyr::n_distinct(data[[id_field]][in_active])
  } else {
    NA_integer_
  }

  dropout <- if (length(status_vals_raw) > 0 && length(dropout_vals) > 0) {
    in_dropout <- status_vals %in% dropout_vals | status_vals_raw %in% dropout_vals
    dplyr::n_distinct(data[[id_field]][in_dropout])
  } else {
    NA_integer_
  }

  tibble::tibble(
    stage = c("Screened", "Consented", "Active/Completed", "Dropped Out"),
    n = c(screened, consented, active, dropout)
  )
}
