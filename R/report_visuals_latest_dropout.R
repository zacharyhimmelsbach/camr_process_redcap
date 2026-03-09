#' Plot most recent event distribution for dropped-out participants
#'
#' @param subject_level Subject-level data.
#' @param subject_visit Subject-visit data.
#' @param id_field ID field name.
#' @param status_field Status field name.
#' @param event_label_field Event label field name.
#' @param event_number_field Event number field name.
#' @param status_dropout_values Status values that indicate dropout.
#' @param consort_population_field Optional field for faceting by population.
#'
#' @return A `ggplot2` object.
#' @export
camr_report_plot_latest_dropout <- function(subject_level,
                                            subject_visit,
                                            id_field,
                                            status_field,
                                            event_label_field,
                                            event_number_field,
                                            status_dropout_values,
                                            consort_population_field = NULL) {
  camr_report_latest_event_plot(
    subject_level = subject_level,
    subject_visit = subject_visit,
    id_field = id_field,
    status_field = status_field,
    event_label_field = event_label_field,
    event_number_field = event_number_field,
    status_values = status_dropout_values,
    population_field = consort_population_field,
    fill_color = "#d95f5f",
    x_label = "Most recent event"
  )
}
