#' Write a targets pipeline for a REDCap project
#'
#' The main entry point for setting up a new REDCap data processing pipeline.
#' Connects to REDCap to fetch project metadata, then generates a complete
#' targets pipeline with configuration files, report template, and documentation.
#'
#' @param redcap_url Character. The REDCap API endpoint URL
#'   (e.g., "https://redcap.example.edu/api/").
#' @param api_token Character. A valid REDCap API token. Used during setup to
#'   fetch project structure; stored in `.Renviron` for pipeline execution.
#' @param out_dir Character. Path to a new directory where pipeline files will
#'   be created. Must not already exist.
#' @param overwrite Logical. Whether to overwrite existing files if `out_dir`
#'   contains previous pipeline files. Defaults to FALSE.
#' @param write_renviron Logical. Whether to create a `.Renviron` file containing
#'   the API token. Defaults to TRUE. The file is automatically added to `.gitignore`.
#' @param init_git Logical. Whether to initialize a git repository in `out_dir`.
#'   Defaults to TRUE.
#' @param init_commit Logical. Whether to create an initial git commit.
#'   Defaults to TRUE. Requires git user.name and user.email to be configured.
#'
#' @return Character vector of paths to the files that were created:
#'   \itemize{
#'     \item `camr_redcap_config.yml` - Configuration file
#'     \item `_targets.R` - The targets pipeline definition
#'     \item `camr_report.qmd` - Quarto report template
#'     \item `README.md` - Project documentation
#'     \item `<dirname>.Rproj` - RStudio project file
#'     \item `.Renviron` - API token (if `write_renviron = TRUE`)
#'     \item `renviron.example.txt` - Example .Renviron template
#'     \item `.gitignore` - Git ignore rules
#'   }
#'
#' @details
#' After running this function:
#' \enumerate{
#'   \item Review and customize `camr_redcap_config.yml`, especially the
#'     `status_active_values` and `status_dropout_values` to match your
#'     project's status field choices.
#'   \item Run `targets::tar_make()` to execute the pipeline.
#'   \item The pipeline will generate subject-level and visit-level datasets,
#'     plus an HTML report with CONSORT diagrams.
#' }
#'
#' @examples
#' \dontrun{
#' # Create a new pipeline
#' write_redcap_targets(
#'   api_token = Sys.getenv("REDCAP_API_TOKEN"),
#'   out_dir = "my_study_pipeline"
#' )
#'
#' # Then in the new directory:
#' # 1. Edit camr_redcap_config.yml
#' # 2. Run targets::tar_make()
#' }
#'
#' @seealso \code{\link{camr_redcap_metadata}}, \code{\link{camr_validate_config}}
#'
camr_format_list_entries <- function(entries, indent = "      ") {
  if (length(entries) == 0) {
    return(NULL)
  }

  lines <- paste0(indent, entries)
  if (length(lines) > 1) {
    lines[seq_len(length(lines) - 1)] <- paste0(lines[seq_len(length(lines) - 1)], ",")
  }
  lines
}

camr_yaml_quote <- function(x) {
  x <- as.character(x)
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

camr_yaml_list_lines <- function(name, x) {
  x <- as.character(x)
  if (length(x) == 0) {
    return(paste0(name, ": []"))
  }
  c(
    paste0(name, ":"),
    paste0("  - ", vapply(x, camr_yaml_quote, character(1)))
  )
}

camr_write_config_template <- function(config, path) {
  lines <- c(
    "# CAMR REDCap pipeline configuration",
    "# Edit this file before running targets::tar_make().",
    "",
    "# REDCap API endpoint URL for this project.",
    paste0("redcap_url: ", camr_yaml_quote(config$redcap_url)),
    "",
    "# Primary record identifier field (must exist in REDCap metadata).",
    paste0("id_field: ", camr_yaml_quote(config$id_field)),
    "",
    "# Participant/trial ID field used in subject-level outputs.",
    paste0("trial_id_field: ", camr_yaml_quote(config$trial_id_field)),
    "",
    "# Status field used to classify active, dropout, and test records.",
    paste0("status_field: ", camr_yaml_quote(config$status_field)),
    "",
    "# Core fields to always keep in analysis outputs.",
    camr_yaml_list_lines("keep_fields", config$keep_fields),
    "",
    "# REDCap forms to include in analysis_subject and analysis_visit outputs.",
    camr_yaml_list_lines("analysis_forms", config$analysis_forms),
    "",
    "# Event field names in the exported REDCap data.",
    paste0("event_name_field: ", camr_yaml_quote(config$event_name_field)),
    paste0("event_number_field: ", camr_yaml_quote(config$event_number_field)),
    paste0("event_label_field: ", camr_yaml_quote(config$event_label_field)),
    "",
    "# Optional event name for subject-level summarization. Use null to disable.",
    "subject_level_event: null",
    "",
    "# Values from status_field that indicate active/completed participants.",
    camr_yaml_list_lines("status_active_values", config$status_active_values),
    "",
    "# Values from status_field that indicate dropout/attrition.",
    camr_yaml_list_lines("status_dropout_values", config$status_dropout_values),
    "",
    "# Values from status_field that indicate test records.",
    camr_yaml_list_lines("status_test_values", config$status_test_values),
    "",
    "# If true, rows with status_test_values are excluded from outputs.",
    paste0("exclude_test: ", if (isTRUE(config$exclude_test)) "true" else "false"),
    "",
    "# Optional field to split consented participants into populations before randomization.",
    "consort_population_field: null",
    "",
    "# Optional values in consort_population_field that are eligible for randomization.",
    camr_yaml_list_lines(
      "consort_population_randomization_eligible_values",
      config$consort_population_randomization_eligible_values
    ),
    "",
    "# Optional field indicating screening eligibility outcome.",
    "consort_screening_eligibility_field: null",
    "",
    "# Optional values indicating screen-fail in consort_screening_eligibility_field.",
    camr_yaml_list_lines("consort_screen_fail_values", config$consort_screen_fail_values),
    "",
    "# Field containing randomized treatment/group assignment.",
    paste0("consort_randomization_field: ", camr_yaml_quote(config$consort_randomization_field)),
    "",
    "# Group labels used for CONSORT randomization branches.",
    camr_yaml_list_lines("consort_group_values", config$consort_group_values),
    "",
    "# Status values that indicate intervention completion.",
    paste0(
      "consort_intervention_completed_field: ",
      camr_yaml_quote(config$consort_intervention_completed_field)
    ),
    "",
    "# Status values that indicate study completion.",
    paste0("consort_study_completed_field: ", camr_yaml_quote(config$consort_study_completed_field)),
    "",
    "# Status values treated as attrition in CONSORT flow.",
    camr_yaml_list_lines("consort_attrition_status_values", config$consort_attrition_status_values),
    "",
    "# Optional field with reasons for exclusion before consent.",
    "consort_preconsent_exclusion_reason_field: null",
    "",
    "# Optional field with reasons for exclusion after consent and before randomization.",
    "consort_prerandomization_exclusion_reason_field: null",
    "",
    "# Optional status values indicating exclusion before randomization.",
    camr_yaml_list_lines(
      "consort_prerandomization_exclusion_status_values",
      config$consort_prerandomization_exclusion_status_values
    ),
    "",
    "# Recruitment accrual settings (optional). Dates must be YYYY-MM-DD or null.",
    "recruitment_study_start_date: null",
    "recruitment_planned_end_date: null",
    "recruitment_goal_n: null",
    "",
    "# Subject-level date field used for recruitment accrual chart.",
    "recruitment_consent_date_field: null",
    "",
    "# Regex used to match TLFB forms/fields in metadata.",
    paste0("tlfb_field_pattern: ", camr_yaml_quote(config$tlfb_field_pattern)),
    "",
    "# Output folder for generated TLFB files (relative to project root).",
    paste0("tlfb_out_dir: ", camr_yaml_quote(config$tlfb_out_dir))
  )

  writeLines(lines, path)
}

#' @export
write_redcap_targets <- function(api_token,
                                 out_dir,
                                 redcap_url = "https://redcap.partners.org/redcap/api/",
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


  # Clean up directory on error
  success <- FALSE
  on.exit({
    if (!success && dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  }, add = TRUE)

  metadata <- camr_redcap_metadata(redcap_url, api_token)
  forms <- unique(metadata$form_name)
  forms <- forms[!is.na(forms) & forms != ""]

  events <- camr_redcap_events(redcap_url, api_token)
  data <- camr_redcap_read(redcap_url, api_token)
  repeating <- camr_repeating_instruments(data)

  config <- list(
    redcap_url = redcap_url,
    id_field = "record_id",
    trial_id_field = "record_subject_id",
    status_field = "record_status",
    keep_fields = c("record_id", "record_status", "record_subject_id", "record_cam_id"),
    analysis_forms = forms,
    event_name_field = "redcap_event_name",
    event_number_field = "event_number",
    event_label_field = "event_label",
    subject_level_event = NULL,
    status_active_values = c("Active", "Completed Intervention", "Completed Study", "Test Record"),
    status_dropout_values = c("Lost to Follow-Up", "Withdrawn", "Terminated"),
    status_test_values = c("Test Record"),
    exclude_test = FALSE,
    consort_population_field = NULL,
    consort_population_randomization_eligible_values = character(0),
    consort_screening_eligibility_field = NULL,
    consort_screen_fail_values = character(0),
    consort_randomization_field = "rand_group",
    consort_group_values = c("Group A", "Group B"),
    consort_intervention_completed_field = "Completed Intervention",
    consort_study_completed_field = "Completed Study",
    consort_attrition_status_values = c("Withdrawn", "Lost to Follow-Up", "Terminated"),
    consort_preconsent_exclusion_reason_field = NULL,
    consort_prerandomization_exclusion_reason_field = NULL,
    consort_prerandomization_exclusion_status_values = character(0),
    recruitment_study_start_date = NULL,
    recruitment_planned_end_date = NULL,
    recruitment_goal_n = NULL,
    recruitment_consent_date_field = NULL,
    tlfb_field_pattern = "(?i)tlfb",
    tlfb_out_dir = "tlfb_files"
  )

  config_path <- file.path(out_dir, "camr_redcap_config.yml")
  targets_path <- file.path(out_dir, "_targets.R")
  report_path <- file.path(out_dir, "camr_report.qmd")
  readme_path <- file.path(out_dir, "README.md")
  renviron_path <- file.path(out_dir, ".Renviron")
  renviron_example_path <- file.path(out_dir, "renviron.example.txt")
  gitignore_path <- file.path(out_dir, ".gitignore")
  rproj_path <- file.path(out_dir, paste0(basename(out_dir), ".Rproj"))

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

  camr_write_config_template(config, config_path)

  # Create R/forms/ directory for wrapper functions
  forms_dir <- file.path(out_dir, "R", "forms")
  dir.create(forms_dir, showWarnings = FALSE, recursive = TRUE)

  safe_forms <- vapply(forms, camr_safe_name, character(1))
  safe_forms <- make.unique(safe_forms)

  # Generate wrapper function files for each form
  for (idx in seq_along(forms)) {
    form <- forms[[idx]]
    safe <- safe_forms[[idx]]
    form_file <- file.path(forms_dir, paste0(safe, ".R"))

    form_lines <- c(
      paste0("process_", safe, " <- function(redcap_data, redcap_metadata, config, redcap_events) {"),
      "  data <- camr_form_data(",
      "    redcap_data,",
      "    redcap_metadata,",
      paste0("    \"", form, "\","),
      "    config,",
      "    redcap_events",
      "  )",
      "",
      "  # Add custom processing below this line",
      "",
      "  data",
      "}"
    )
    writeLines(form_lines, form_file)
  }

  # Generate form targets that call wrapper functions
  form_targets <- lapply(seq_along(forms), function(idx) {
    safe <- safe_forms[[idx]]
    c(
      "  tar_target(",
      paste0("    form_", safe, ","),
      paste0("    process_", safe, "("),
      "      redcap_data_filtered,",
      "      redcap_metadata,",
      "      config_validated,",
      "      redcap_events",
      "    )",
      "  ),"
    )
  })
  form_refs <- if (length(safe_forms) > 0) paste0("form_", safe_forms) else character(0)
  processed_form_data_target <- c(
    "  tar_target(",
    "    processed_form_data,",
    "    list(",
    camr_format_list_entries(form_refs, indent = "      "),
    "    )",
    "  ),"
  )

  safe_repeats <- vapply(repeating, camr_safe_name, character(1))
  safe_repeats <- make.unique(safe_repeats)
  repeat_targets <- lapply(seq_along(repeating), function(idx) {
    inst <- repeating[[idx]]
    safe <- safe_repeats[[idx]]
    c(
      "  tar_target(",
      paste0("    repeat_", safe, ","),
      "    camr_subject_by_instance(",
      "      redcap_data_processed,",
      "      redcap_metadata,",
      "      redcap_events,",
      paste0("      \"", inst, "\","),
      "      config_validated",
      "    )",
      "  ),"
    )
  })

  pipeline_lines <- c(
    "library(targets)",
    "library(camrProcessRedcap)",
    "",
    "tar_source(\"R/\")",
    "",
    "tar_option_set(packages = c(\"camrProcessRedcap\", \"yaml\"))",
    "",
    "config <- yaml::read_yaml(\"camr_redcap_config.yml\")",
    "redcap_token <- Sys.getenv(\"REDCAP_API_TOKEN\")",
    "if (!nzchar(redcap_token)) stop(\"Set REDCAP_API_TOKEN before running targets.\")",
    "",
    "list(",
    "  tar_target(",
    "    redcap_metadata,",
    "    camr_redcap_metadata(",
    "      config$redcap_url,",
    "      redcap_token",
    "    )",
    "  ),",
    "  tar_target(",
    "    redcap_codebook,",
    "    camr_redcap_codebook(",
    "      config$redcap_url,",
    "      redcap_token",
    "    )",
    "  ),",
    "  tar_target(",
    "    config_validated,",
    "    camr_validate_config(",
    "      config,",
    "      redcap_metadata",
    "    )",
    "  ),",
    "  tar_target(",
    "    redcap_events,",
    "    camr_redcap_events(",
    "      config_validated$redcap_url,",
    "      redcap_token",
    "    )",
    "  ),",
    "  tar_target(",
    "    redcap_form_event_map,",
    "    camr_redcap_form_event_mapping(",
    "      config_validated$redcap_url,",
    "      redcap_token",
    "    )",
    "  ),",
    "  tar_target(",
    "    redcap_data,",
    "    camr_redcap_read(",
    "      config_validated$redcap_url,",
    "      redcap_token",
    "    )",
    "  ),",
    "  tar_target(",
    "    redcap_data_filtered,",
    "    camr_filter_test_records(",
    "      redcap_data,",
    "      redcap_metadata,",
    "      config_validated",
    "    )",
    "  ),",
    unlist(form_targets),
    processed_form_data_target,
    "  tar_target(",
    "    redcap_data_processed,",
    "    camr_merge_processed_forms(",
    "      redcap_data_filtered,",
      "      processed_form_data,",
      "      config_validated",
    "    )",
    "  ),",
    "  tar_target(",
    "    subject_level_raw,",
    "    camr_subject_level(",
    "      redcap_data_processed,",
    "      redcap_metadata,",
    "      redcap_events,",
    "      redcap_form_event_map,",
    "      config_validated,",
    "      apply_labels = FALSE",
    "    )",
    "  ),",
    "  tar_target(",
    "    subject_level,",
    "    camr_apply_labels(",
    "      subject_level_raw,",
    "      redcap_metadata",
    "    )",
    "  ),",
    "  tar_target(",
    "    subject_visit_raw,",
    "    camr_subject_visit(",
    "      redcap_data_processed,",
    "      redcap_metadata,",
    "      redcap_events,",
    "      subject_level_raw,",
    "      config_validated",
    "    )",
    "  ),",
    "  tar_target(",
    "    consort_data,",
    "    camr_consort_data(",
    "      redcap_data_processed,",
    "      redcap_metadata,",
    "      config_validated",
    "    )",
    "  ),",
    "  tar_target(",
    "    tlfb_file_index,",
    "    camr_tlfb_index(",
    "      redcap_data_processed,",
    "      redcap_metadata,",
    "      config_validated,",
    "      field_pattern = config_validated$tlfb_field_pattern",
    "    )",
    "  ),",
    "  tar_target(",
    "    tlfb_files,",
    "    camr_download_tlfb_files(",
    "      tlfb_file_index,",
    "      config_validated$redcap_url,",
    "      redcap_token,",
    "      out_dir = config_validated$tlfb_out_dir",
    "    ),",
    "    format = \"file\"",
    "  ),",
    "  tar_target(",
    "    tlfb_raw,",
    "    camr_read_tlfb_files(tlfb_files)",
    "  ),",
    "  tar_target(",
    "    tlfb_ml,",
    "    camr_ml_tlfb(",
    "      tlfb_raw,",
    "      redcap_data_processed,",
    "      config_validated,",
    "      legacy_names = TRUE",
    "    )",
    "  ),",
    "  tar_target(",
    "    tlfb_vl,",
    "    camr_vl_tlfb(",
      "      tlfb_ml,",
      "      config_validated,",
      "      legacy_names = TRUE",
    "    )",
    "  ),",
    "  tar_target(",
    "    subject_visit,",
    "    camr_merge_visit_features(",
    "      subject_visit_raw,",
    "      tlfb_vl,",
    "      config_validated",
    "    )",
    "  ),",
    "  tar_target(",
    "    analysis_subject,",
    "    camr_analysis_subject(",
    "      subject_level,",
    "      redcap_metadata,",
    "      config_validated",
    "    )",
    "  ),",
    "  tar_target(",
    "    analysis_visit,",
    "    camr_analysis_visit(",
    "      subject_visit,",
    "      analysis_subject,",
    "      redcap_metadata,",
    "      config_validated",
    "    )",
    "  ),",
    if (length(repeat_targets) > 0) unlist(repeat_targets) else NULL,
    "  tar_target(",
    "    report,",
    "    camr_render_report(",
    "      \"camr_report.qmd\",",
    "      \".\",",
    "      subject_level,",
    "      subject_visit,",
    "      consort_data,",
    "      config_validated",
    "    ),",
    "    format = \"file\"",
    "  )",
    ")"
  )

  writeLines(pipeline_lines, targets_path)

  template_path <- tryCatch(
    normalizePath(file.path("inst", "templates", "camr_report.qmd"), mustWork = TRUE),
    error = function(e) ""
  )
  if (!nzchar(template_path)) {
    template_path <- system.file("templates", "camr_report.qmd", package = "camrProcessRedcap")
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
    "This repository was generated by camrProcessRedcap and contains a targets pipeline",
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

  # Write .Rproj file for RStudio
  rproj_lines <- c(
    "Version: 1.0",
    "",
    "RestoreWorkspace: No",
    "SaveWorkspace: No",
    "AlwaysSaveHistory: Default",
    "",
    "EnableCodeIndexing: Yes",
    "UseSpacesForTab: Yes",
    "NumSpacesForTab: 2",
    "Encoding: UTF-8",
    "",
    "RnwWeave: Sweave",
    "LaTeX: pdfLaTeX",
    "",
    "AutoAppendNewline: Yes",
    "StripTrailingWhitespace: Yes"
  )
  writeLines(rproj_lines, rproj_path)

  # Add common ignores to .gitignore
  camr_append_unique_lines(gitignore_path, c("_targets/", ".Rhistory", ".RData"))

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
          success <<- TRUE
          return(invisible(NULL))
        }
        system2("git", c("add", "-A"), stdout = TRUE, stderr = TRUE)
        res <- system("git commit -m 'Initialize REDCap targets pipeline'",
                      intern = TRUE, ignore.stderr = TRUE)
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

  success <- TRUE
  c(config_path, targets_path, report_path, readme_path, rproj_path, renviron_path, renviron_example_path, gitignore_path)
}
