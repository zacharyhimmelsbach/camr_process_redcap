#' Plot most recent event distribution for active/completed participants
#'
#' @param subject_level Subject-level data.
#' @param subject_visit Subject-visit data.
#' @param id_field ID field name.
#' @param status_field Status field name.
#' @param event_label_field Event label field name.
#' @param event_number_field Event number field name.
#' @param status_active_values Status values that indicate active/completed.
#' @param consort_population_field Optional field for faceting by population.
#'
#' @return A `ggplot2` object.
#' @export
camr_report_plot_latest_active <- function(subject_level,
                                           subject_visit,
                                           id_field,
                                           status_field,
                                           event_label_field,
                                           event_number_field,
                                           status_active_values,
                                           consort_population_field = NULL) {
  camr_report_latest_event_plot(
    subject_level = subject_level,
    subject_visit = subject_visit,
    id_field = id_field,
    status_field = status_field,
    event_label_field = event_label_field,
    event_number_field = event_number_field,
    status_values = status_active_values,
    population_field = consort_population_field,
    fill_color = "#4c9f70",
    x_label = "Most recent event"
  )
}
