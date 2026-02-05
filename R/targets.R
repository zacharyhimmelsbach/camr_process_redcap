#' Write a targets pipeline for a REDCap project
#'
#' @param redcap_url REDCap API URL.
#' @param api_token REDCap API token (used only to generate the pipeline).
#' @param out_dir Output directory for pipeline files.
#' @param overwrite Whether to overwrite existing files.
#' @param write_renviron Whether to write a .Renviron file containing the API token.
#' @param init_git Whether to initialize a git repository in out_dir.
#' @param init_commit Whether to create an initial git commit.
#' @return Paths of files written.
#' @examples
#' \dontrun{
#' write_redcap_targets(
#'   redcap_url = "https://redcap.example.edu/api/",
#'   api_token = Sys.getenv("REDCAP_API_TOKEN"),
#'   out_dir = "camr_redcap_pipeline"
#' )
#' }
#' @export
write_redcap_targets <- function(redcap_url,
                                 api_token,
                                 out_dir,
                                 overwrite = FALSE,
                                 write_renviron = TRUE,
                                 init_git = TRUE,
                                 init_commit = TRUE) {
  if (missing(out_dir) || is.null(out_dir) || !nzchar(out_dir)) {
    stop("out_dir must be a non-empty path to a new folder.")
  }
  out_dir <- normalizePath(out_dir, mustWork = FALSE)
  if (file.exists(out_dir)) {
    stop("out_dir already exists. Provide a path to a new folder.")
  }
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  metadata <- camr_redcap_metadata(redcap_url, api_token)
  forms <- unique(metadata$form_name)
  forms <- forms[!is.na(forms) & forms != ""]

  events <- camr_redcap_events(redcap_url, api_token)
  data <- camr_redcap_read(redcap_url, api_token)
  repeating <- camr_repeating_instruments(data)

  config <- list(
    redcap_url = redcap_url,
    id_field = "record_id",
    trial_id_field = "record_trial_id",
    status_field = "record_status",
    event_name_field = "redcap_event_name",
    event_number_field = "event_number",
    event_label_field = "event_label",
    subject_level_event = NULL,
    status_active_values = c("Active", "Completed"),
    status_dropout_values = c("Dropout")
  )

  config_path <- file.path(out_dir, "camr_redcap_config.yml")
  targets_path <- file.path(out_dir, "_targets.R")
  report_path <- file.path(out_dir, "camr_report.qmd")
  readme_path <- file.path(out_dir, "README.md")
  renviron_path <- file.path(out_dir, ".Renviron")
  renviron_example_path <- file.path(out_dir, "renviron.example.txt")
  gitignore_path <- file.path(out_dir, ".gitignore")

  if (file.exists(config_path) && !overwrite) {
    stop("Config file already exists. Use overwrite = TRUE to replace it.")
  }
  if (file.exists(targets_path) && !overwrite) {
    stop("_targets.R already exists. Use overwrite = TRUE to replace it.")
  }
  if (file.exists(report_path) && !overwrite) {
    stop("camr_report.qmd already exists. Use overwrite = TRUE to replace it.")
  }
  if (file.exists(readme_path) && !overwrite) {
    stop("README.md already exists. Use overwrite = TRUE to replace it.")
  }

  yaml::write_yaml(config, config_path)

  safe_forms <- vapply(forms, camr_safe_name, character(1))
  safe_forms <- make.unique(safe_forms)
  form_targets <- vapply(seq_along(forms), function(idx) {
    form <- forms[[idx]]
    safe <- safe_forms[[idx]]
    paste0(
      "  tar_target(form_", safe, ", camr_form_data(redcap_data, redcap_metadata, \"", form, "\", config_validated, redcap_events)),"
    )
  }, character(1))

  safe_repeats <- vapply(repeating, camr_safe_name, character(1))
  safe_repeats <- make.unique(safe_repeats)
  repeat_targets <- vapply(seq_along(repeating), function(idx) {
    inst <- repeating[[idx]]
    safe <- safe_repeats[[idx]]
    paste0(
      "  tar_target(repeat_", safe, ", camr_subject_by_instance(redcap_data, redcap_metadata, redcap_events, subject_level_raw, \"", inst, "\", config_validated)),"
    )
  }, character(1))

  pipeline_lines <- c(
    "library(targets)",
    "library(camr_process_redcap)",
    "",
    "tar_option_set(packages = c(\"camr_process_redcap\", \"yaml\"))",
    "",
    "config <- yaml::read_yaml(\"camr_redcap_config.yml\")",
    "redcap_token <- Sys.getenv(\"REDCAP_API_TOKEN\")",
    "if (!nzchar(redcap_token)) stop(\"Set REDCAP_API_TOKEN before running targets.\")",
    "",
    "list(",
    "  tar_target(redcap_metadata, camr_redcap_metadata(config$redcap_url, redcap_token)),",
    "  tar_target(redcap_codebook, camr_redcap_codebook(config$redcap_url, redcap_token)),",
    "  tar_target(config_validated, camr_validate_config(config, redcap_metadata)),",
    "  tar_target(redcap_events, camr_redcap_events(config_validated$redcap_url, redcap_token)),",
    "  tar_target(redcap_form_event_map, camr_redcap_form_event_mapping(config_validated$redcap_url, redcap_token)),",
    "  tar_target(redcap_data, camr_redcap_read(config_validated$redcap_url, redcap_token)),",
    "  tar_target(subject_level_raw, camr_subject_level(redcap_data, redcap_metadata, redcap_events, redcap_form_event_map, config_validated, apply_labels = FALSE)),",
    "  tar_target(subject_level, camr_apply_labels(subject_level_raw, redcap_metadata)),",
    "  tar_target(subject_visit, camr_subject_visit(redcap_data, redcap_metadata, redcap_events, subject_level_raw, config_validated)),",
    "  tar_target(consort_data, camr_consort_data(redcap_data, redcap_metadata, config_validated)),",
    form_targets,
    if (length(repeat_targets) > 0) repeat_targets else NULL,
    "  tar_target(report, camr_render_report(\"camr_report.qmd\", \".\", subject_level, subject_visit, consort_data, config_validated), format = \"file\")",
    ")"
  )

  writeLines(pipeline_lines, targets_path)

  template_path <- system.file("templates", "camr_report.qmd", package = "camr_process_redcap")
  if (!nzchar(template_path)) {
    template_path <- tryCatch(
      normalizePath(file.path("inst", "templates", "camr_report.qmd"), mustWork = TRUE),
      error = function(e) ""
    )
  }
  if (nzchar(template_path)) {
    file.copy(template_path, report_path, overwrite = TRUE)
  } else {
    stop("Report template not found in package.")
  }

  project_info <- camr_redcap_project_info(redcap_url, api_token)
  project_title <- as.character(project_info$project_title %||% "REDCap Project")
  project_id <- as.character(project_info$project_id %||% "Unknown")
  project_purpose <- as.character(project_info$purpose %||% "")
  project_notes <- as.character(project_info$project_notes %||% "")
  created_on <- as.character(Sys.Date())

  events_n <- if (!is.null(events) && nrow(events) > 0) nrow(events) else NA_integer_
  forms_n <- length(forms)
  repeats_n <- length(repeating)

  readme_lines <- c(
    "# REDCap Targets Pipeline",
    "",
    paste0("Project title: ", project_title),
    paste0("Project ID: ", project_id),
    paste0("REDCap API URL: ", redcap_url),
    paste0("Generated on: ", created_on),
    "",
    "## Overview",
    "This repository was generated by camr_process_redcap and contains a targets pipeline",
    "for the REDCap project above. It includes subject-level, subject-visit, and",
    "repeating-instrument outputs plus a Quarto report.",
    "",
    "## Project Snapshot",
    paste0("Forms detected: ", forms_n),
    paste0("Events detected: ", if (is.na(events_n)) "Unknown" else events_n),
    paste0("Repeating instruments detected: ", repeats_n),
    "",
    "## Notes",
    if (nzchar(project_purpose)) paste0("Purpose: ", project_purpose) else "Purpose: (not provided)",
    if (nzchar(project_notes)) paste0("Project notes: ", project_notes) else "Project notes: (not provided)",
    "",
    "## Getting Started",
    "1. Ensure `REDCAP_API_TOKEN` is set in `.Renviron`.",
    "2. Review `camr_redcap_config.yml` for status values and other settings.",
    "3. Run `targets::tar_make()`."
  )
  writeLines(readme_lines, readme_path)

  if (isTRUE(write_renviron)) {
    if (!file.exists(renviron_path) || overwrite) {
      writeLines(paste0("REDCAP_API_TOKEN=", api_token), renviron_path)
    }
    if (!file.exists(renviron_example_path) || overwrite) {
      writeLines("REDCAP_API_TOKEN=your_token_here", renviron_example_path)
    }
    camr_append_unique_lines(gitignore_path, ".Renviron")
  }

  if (isTRUE(init_git)) {
    git_dir <- file.path(out_dir, ".git")
    if (!dir.exists(git_dir)) {
      tryCatch(
        {
          old_wd <- getwd()
          on.exit(setwd(old_wd), add = TRUE)
          setwd(out_dir)
          system2("git", "init", stdout = TRUE, stderr = TRUE)
        },
        error = function(e) {
          warning("git init failed: ", conditionMessage(e))
        }
      )
    }
  }

  if (isTRUE(init_git) && isTRUE(init_commit)) {
    tryCatch(
      {
        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE)
        setwd(out_dir)
        user_name <- system2("git", c("config", "--get", "user.name"), stdout = TRUE, stderr = TRUE)
        user_email <- system2("git", c("config", "--get", "user.email"), stdout = TRUE, stderr = TRUE)
        user_name <- paste(user_name, collapse = "\n")
        user_email <- paste(user_email, collapse = "\n")
        if (!nzchar(user_name) || !nzchar(user_email)) {
          warning("git user.name or user.email not set; skipping initial commit.")
          return(invisible(NULL))
        }
        system2("git", c("add", "-A"), stdout = TRUE, stderr = TRUE)
        res <- system2("git", c("commit", "-m", "Initialize REDCap targets pipeline"),
                       stdout = TRUE, stderr = TRUE)
        status <- attr(res, "status")
        if (!is.null(status) && status != 0) {
          warning("git commit failed; check git user.name/user.email configuration.")
        }
      },
      error = function(e) {
        warning("git commit failed: ", conditionMessage(e))
      }
    )
  }

  c(config_path, targets_path, report_path, readme_path, renviron_path, renviron_example_path, gitignore_path)
}
