#' Render the CAMR REDCap report
#'
#' Renders a Quarto HTML report containing CONSORT diagrams, participant
#' flow visualizations, and data summaries for the REDCap project.
#'
#' @param qmd_path Character. Path to the Quarto report template file.
#'   The default template is copied to the pipeline directory by
#'   \code{\link{write_redcap_targets}}.
#' @param output_dir Character. Directory where the rendered HTML report
#'   will be saved.
#' @param subject_level A tibble of subject-level data from
#'   \code{\link{camr_subject_level}}.
#' @param subject_visit A tibble of subject-visit data from
#'   \code{\link{camr_subject_visit}}.
#' @param consort_data A tibble of CONSORT counts from
#'   \code{\link{camr_consort_data}}.
#' @param config A list containing configuration options used to parameterize
#'   the report, including field names and status value definitions.
#'
#' @return Character. The path to the rendered HTML report file.
#'
#' @details
#' The function saves the input datasets as RDS files in a `_report_data`
#' subdirectory of `output_dir`, then renders the Quarto template with
#' these files as parameters. The report template can be customized by
#' editing the `.qmd` file in the pipeline directory.
#'
#' Requires the `quarto` R package and a Quarto installation.
#'
#' @examples
#' \dontrun{
#' report_path <- camr_render_report(
#'   qmd_path = "camr_report.qmd",
#'   output_dir = ".",
#'   subject_level = subject_data,
#'   subject_visit = visit_data,
#'   consort_data = consort,
#'   config = config
#' )
#' browseURL(report_path)
#' }
#'
#' @seealso \code{\link{write_redcap_targets}}, \code{\link{camr_consort_data}}
#'
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

  pop_field <- config$consort_population_field %||% NULL
  if (!is.null(pop_field)) {
    pop_field <- as.character(pop_field)
    if (length(pop_field) != 1 || is.na(pop_field) || !nzchar(pop_field)) {
      pop_field <- NULL
    }
  }

  params <- list(
    subject_level_path = subject_level_path,
    subject_visit_path = subject_visit_path,
    consort_path = consort_path,
    id_field = config$id_field,
    trial_id_field = config$trial_id_field,
    status_field = config$status_field,
    event_label_field = config$event_label_field,
    event_number_field = config$event_number_field,
    consort_population_field = pop_field,
    recruitment_study_start_date = config$recruitment_study_start_date %||% NULL,
    recruitment_planned_end_date = config$recruitment_planned_end_date %||% NULL,
    recruitment_goal_n = config$recruitment_goal_n %||% NULL,
    recruitment_consent_date_field = config$recruitment_consent_date_field %||% NULL,
    status_active_values = config$status_active_values %||% character(0),
    status_dropout_values = config$status_dropout_values %||% character(0)
  )

  out_file <- "camr_report.html"
  source_qmd <- normalizePath(qmd_path, mustWork = TRUE)
  local_qmd <- file.path(output_dir, basename(source_qmd))
  local_qmd_norm <- normalizePath(local_qmd, mustWork = FALSE)
  if (!identical(source_qmd, local_qmd_norm)) {
    file.copy(source_qmd, local_qmd, overwrite = TRUE)
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(output_dir)

  quarto::quarto_render(
    input = basename(local_qmd),
    output_file = out_file,
    execute_params = params
  )

  file.path(output_dir, out_file)
}
