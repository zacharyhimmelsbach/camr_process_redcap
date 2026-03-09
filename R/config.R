#' Validate REDCap config against metadata
#'
#' Validates a configuration list against REDCap project metadata, ensuring
#' that status field values match available choices. Automatically normalizes
#' codes to labels if codes are provided instead of labels.
#'
#' @param config A list containing configuration options. Expected fields include:
#'   \describe{
#'     \item{status_field}{Character. Name of the status field in REDCap.}
#'     \item{keep_fields}{Character vector. Columns to keep on form/subject outputs.}
#'     \item{analysis_forms}{Character vector. Forms to include in analysis_* outputs.}
#'     \item{status_active_values}{Character vector. Values indicating active/completed status.}
#'     \item{status_dropout_values}{Character vector. Values indicating dropout status.}
#'     \item{status_test_values}{Character vector. Values indicating test records.}
#'     \item{exclude_test}{Logical. If TRUE, exclude test records from all outputs.}
#'     \item{consort_intervention_completed_field}{Optional status values indicating intervention completion.}
#'     \item{consort_study_completed_field}{Optional status values indicating study completion.}
#'     \item{consort_attrition_status_values}{Optional status values used for CONSORT attrition categories.}
#'     \item{consort_prerandomization_exclusion_status_values}{Optional status values used to filter exclusion reasons before randomization.}
#'     \item{consort_population_field}{Optional field used to split consented subjects into populations (e.g., randomized cohort vs healthy controls) before randomization.}
#'     \item{consort_population_randomization_eligible_values}{Optional values of consort_population_field that are eligible for randomization.}
#'     \item{consort_screening_eligibility_field}{Optional field indicating screening eligibility outcome.}
#'     \item{consort_screen_fail_values}{Optional values of consort_screening_eligibility_field that indicate screen-fail.}
#'     \item{recruitment_study_start_date}{Optional study start date for recruitment accrual plotting.}
#'     \item{recruitment_planned_end_date}{Optional planned study end date for recruitment accrual plotting.}
#'     \item{recruitment_goal_n}{Optional recruitment goal N for accrual plotting.}
#'     \item{recruitment_consent_date_field}{Optional subject-level field containing consent date for accrual plotting.}
#'   }
#' @param metadata A tibble of REDCap metadata as returned by
#'   \code{\link{camr_redcap_metadata}}.
#'
#' @return The config list, potentially with status values normalized from
#'   codes to labels. Issues warnings if values don't match available choices.
#'
#' @details
#' This function checks that the values specified in `status_active_values` and
#' `status_dropout_values` match the choices defined for the status field in
#' REDCap. If codes are provided (e.g., "1", "2") instead of labels (e.g.,
#' "Active", "Dropout"), they are automatically converted to labels.
#'
#' @seealso \code{\link{write_redcap_targets}}, \code{\link{camr_redcap_metadata}}
#'
#' @export
camr_validate_config <- function(config, metadata) {
  if (is.null(config)) {
    stop("config is required.")
  }
  config$exclude_test <- isTRUE(config$exclude_test %||% FALSE)
  status_test_values <- as.character(config$status_test_values %||% "Test Record")
  config$status_test_values <- status_test_values[!is.na(status_test_values) & nzchar(status_test_values)]
  config$keep_fields <- camr_keep_fields(config)

  normalize_date_value <- function(x, field_name) {
    if (is.null(x) || (length(x) == 1 && is.na(x))) {
      return(NULL)
    }
    if (length(x) != 1) {
      warning(sprintf("%s must be a single date value (YYYY-MM-DD).", field_name))
      return(NULL)
    }
    if (inherits(x, "Date")) {
      return(as.character(x))
    }
    d <- as.Date(substr(as.character(x), 1, 10))
    if (is.na(d)) {
      warning(sprintf("%s could not be parsed as a date (YYYY-MM-DD).", field_name))
      return(NULL)
    }
    as.character(d)
  }

  config$recruitment_study_start_date <- normalize_date_value(
    config$recruitment_study_start_date,
    "recruitment_study_start_date"
  )
  config$recruitment_planned_end_date <- normalize_date_value(
    config$recruitment_planned_end_date,
    "recruitment_planned_end_date"
  )
  if (!is.null(config$recruitment_study_start_date) && !is.null(config$recruitment_planned_end_date)) {
    start_date <- as.Date(config$recruitment_study_start_date)
    end_date <- as.Date(config$recruitment_planned_end_date)
    if (!is.na(start_date) && !is.na(end_date) && end_date < start_date) {
      stop(
        sprintf(
          "Invalid recruitment timeline in config: recruitment_planned_end_date (%s) is earlier than recruitment_study_start_date (%s).",
          as.character(end_date),
          as.character(start_date)
        )
      )
    }
  }
  if (!is.null(config$recruitment_goal_n)) {
    goal_n <- suppressWarnings(as.numeric(config$recruitment_goal_n))
    if (length(goal_n) != 1 || is.na(goal_n) || goal_n <= 0) {
      warning("recruitment_goal_n must be a single positive number.")
      config$recruitment_goal_n <- NULL
    } else {
      config$recruitment_goal_n <- goal_n
    }
  }
  consent_date_field <- as.character(config$recruitment_consent_date_field %||% "")
  config$recruitment_consent_date_field <- if (nzchar(consent_date_field)) consent_date_field else NULL

  if (is.null(metadata) || nrow(metadata) == 0) {
    config$analysis_forms <- unique(as.character(config$analysis_forms %||% character(0)))
    warning("Metadata is empty; config validation skipped.")
    return(config)
  }

  metadata <- tibble::as_tibble(metadata)
  if ("form_name" %in% names(metadata)) {
    available_forms <- unique(as.character(metadata$form_name))
    available_forms <- available_forms[!is.na(available_forms) & nzchar(available_forms)]
    analysis_forms <- as.character(config$analysis_forms %||% available_forms)
    analysis_forms <- unique(analysis_forms[!is.na(analysis_forms) & nzchar(analysis_forms)])
    unknown_forms <- setdiff(analysis_forms, available_forms)
    if (length(unknown_forms) > 0) {
      warning(
        sprintf(
          "Some analysis_forms were not found in metadata and will be ignored: %s",
          paste(unknown_forms, collapse = ", ")
        )
      )
      analysis_forms <- setdiff(analysis_forms, unknown_forms)
    }
    config$analysis_forms <- analysis_forms
  } else {
    config$analysis_forms <- unique(as.character(config$analysis_forms %||% character(0)))
  }

  if (!is.null(config$consort_population_field) && nzchar(as.character(config$consort_population_field))) {
    if (!as.character(config$consort_population_field) %in% metadata$field_name) {
      warning("consort_population_field not found in metadata; population split will be skipped.")
    }
  }

  if (!is.null(config$recruitment_consent_date_field)) {
    consent_date_field <- as.character(config$recruitment_consent_date_field)
    if ("field_name" %in% names(metadata) && !consent_date_field %in% metadata$field_name) {
      warning("recruitment_consent_date_field not found in metadata; recruitment accrual plot may be blank.")
    }
  }

  eligible_pop_values <- as.character(config$consort_population_randomization_eligible_values %||% character(0))
  eligible_pop_values <- eligible_pop_values[!is.na(eligible_pop_values) & nzchar(eligible_pop_values)]
  if (length(eligible_pop_values) > 0) {
    if (is.null(config$consort_population_field) || !nzchar(as.character(config$consort_population_field))) {
      warning("consort_population_randomization_eligible_values provided without consort_population_field; values will be ignored.")
    } else {
      pop_field <- as.character(config$consort_population_field)
      if (pop_field %in% metadata$field_name) {
        pop_choices <- camr_choices_table(metadata) |> dplyr::filter(.data$field_name == .env$pop_field)
        if (nrow(pop_choices) > 0) {
          pop_codes <- as.character(pop_choices$code)
          pop_labels <- as.character(pop_choices$label)
          pop_mapping <- stats::setNames(pop_labels, pop_codes)
          if (all(eligible_pop_values %in% pop_codes)) {
            eligible_pop_values <- unname(pop_mapping[eligible_pop_values])
          } else if (!all(eligible_pop_values %in% pop_labels)) {
            warning(
              sprintf(
                "Some consort_population_randomization_eligible_values do not match REDCap choices for %s. Valid labels: %s. Valid codes: %s.",
                pop_field,
                paste(unique(pop_labels), collapse = ", "),
                paste(unique(pop_codes), collapse = ", ")
              )
            )
          }
        }
      }
    }
  }
  config$consort_population_randomization_eligible_values <- eligible_pop_values

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

  choices <- camr_choices_table(metadata) |> dplyr::filter(.data$field_name == .env$status_field)
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

  normalize_values_for_field <- function(field_name, values, label) {
    values <- as.character(values %||% character(0))
    if (length(values) == 0) {
      return(values)
    }
    if (is.null(field_name) || !nzchar(as.character(field_name))) {
      return(values)
    }
    field_name <- as.character(field_name)
    if (!field_name %in% metadata$field_name) {
      warning(sprintf("%s field (%s) not found in metadata; values were not normalized.", label, field_name))
      return(values)
    }
    field_choices <- camr_choices_table(metadata) |> dplyr::filter(.data$field_name == .env$field_name)
    if (nrow(field_choices) == 0) {
      return(values)
    }
    field_codes <- as.character(field_choices$code)
    field_labels <- as.character(field_choices$label)
    field_mapping <- stats::setNames(field_labels, field_codes)
    if (all(values %in% field_labels)) {
      return(values)
    }
    if (all(values %in% field_codes)) {
      return(unname(field_mapping[values]))
    }
    warning(
      sprintf(
        "Some %s values do not match REDCap choices for %s. Valid labels: %s. Valid codes: %s.",
        label,
        field_name,
        paste(unique(field_labels), collapse = ", "),
        paste(unique(field_codes), collapse = ", ")
      )
    )
    values
  }

  config$status_active_values <- normalize_values(config$status_active_values, "status_active_values")
  config$status_dropout_values <- normalize_values(config$status_dropout_values, "status_dropout_values")
  config$status_test_values <- normalize_values(config$status_test_values, "status_test_values")
  config$consort_intervention_completed_field <- normalize_values(
    config$consort_intervention_completed_field,
    "consort_intervention_completed_field"
  )
  config$consort_study_completed_field <- normalize_values(
    config$consort_study_completed_field,
    "consort_study_completed_field"
  )
  config$consort_attrition_status_values <- normalize_values(
    config$consort_attrition_status_values,
    "consort_attrition_status_values"
  )
  config$consort_prerandomization_exclusion_status_values <- normalize_values(
    config$consort_prerandomization_exclusion_status_values,
    "consort_prerandomization_exclusion_status_values"
  )
  config$consort_screen_fail_values <- normalize_values_for_field(
    config$consort_screening_eligibility_field,
    config$consort_screen_fail_values,
    "consort_screen_fail_values"
  )

  config
}
