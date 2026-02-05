library(camr_process_redcap)

test_that("subject_visit includes subject-level data", {
  metadata <- read.csv(test_path("fixtures", "metadata.csv"), stringsAsFactors = FALSE)
  data <- read.csv(test_path("fixtures", "data.csv"), stringsAsFactors = FALSE)
  events <- read.csv(test_path("fixtures", "events.csv"), stringsAsFactors = FALSE)

  config <- list(
    id_field = "record_id",
    trial_id_field = "record_trial_id",
    status_field = "record_status",
    event_name_field = "redcap_event_name",
    event_number_field = "event_number",
    event_label_field = "event_label",
    subject_level_event = NULL,
    status_active_values = c("1"),
    status_dropout_values = c("2")
  )

  form_event_map <- data.frame(
    form_name = c("demographics", "visit"),
    event_name = c("baseline_arm_1", "visit_2_arm_1"),
    stringsAsFactors = FALSE
  )

  subject_level <- camr_subject_level(data, metadata, events, form_event_map, config, apply_labels = FALSE)
  subject_visit <- camr_subject_visit(data, metadata, events, subject_level, config)

  expect_true("record_id" %in% names(subject_visit))
  expect_true("sex" %in% names(subject_visit))
  expect_true("event_label" %in% names(subject_visit))
  expect_equal(nrow(subject_visit), nrow(data))
})
