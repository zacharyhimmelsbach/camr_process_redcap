#' Render the CAMR REDCap report
#'
#' @param qmd_path Path to the report template.
#' @param output_dir Output directory.
#' @param subject_level Subject-level data.
#' @param subject_visit Subject-visit data.
#' @param consort_data CONSORT summary data.
#' @param config Config list.
#' @return Path to the rendered report.
#' @export
camr_render_report <- function(qmd_path,
                               output_dir,
                               subject_level,
                               subject_visit,
                               consort_data,
                               config) {
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("The quarto package is required to render the report.")
  }

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  data_dir <- file.path(output_dir, "_report_data")
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

  subject_level_path <- file.path(data_dir, "subject_level.rds")
  subject_visit_path <- file.path(data_dir, "subject_visit.rds")
  consort_path <- file.path(data_dir, "consort_data.rds")

  saveRDS(subject_level, subject_level_path)
  saveRDS(subject_visit, subject_visit_path)
  saveRDS(consort_data, consort_path)

  params <- list(
    subject_level_path = subject_level_path,
    subject_visit_path = subject_visit_path,
    consort_path = consort_path,
    id_field = config$id_field,
    status_field = config$status_field,
    event_label_field = config$event_label_field,
    event_number_field = config$event_number_field,
    status_active_values = config$status_active_values,
    status_dropout_values = config$status_dropout_values
  )

  out_file <- "camr_report.html"
  quarto::quarto_render(
    input = qmd_path,
    output_dir = output_dir,
    output_file = out_file,
    execute_params = params
  )

  file.path(output_dir, out_file)
}
