library(camrProcessRedcap)

test_that("subject_level uses forms in single event", {
  metadata <- read.csv(test_path("fixtures", "metadata.csv"), stringsAsFactors = FALSE)
  data <- read.csv(test_path("fixtures", "data.csv"), stringsAsFactors = FALSE)
  events <- read.csv(test_path("fixtures", "events.csv"), stringsAsFactors = FALSE)

  form_event_map <- data.frame(
    form_name = c("demographics", "visit"),
    event_name = c("baseline_arm_1", "visit_2_arm_1"),
    stringsAsFactors = FALSE
  )

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

  subject_level <- camr_subject_level(data, metadata, events, form_event_map, config)

  expect_true("record_id" %in% names(subject_level))
  expect_true("sex" %in% names(subject_level))
  expect_true("visit_date" %in% names(subject_level))
  expect_equal(nrow(subject_level), length(unique(data$record_id)))
})

test_that("subject_level handles formEventMapping columns form/unique_event_name", {
  metadata <- read.csv(test_path("fixtures", "metadata.csv"), stringsAsFactors = FALSE)
  data <- read.csv(test_path("fixtures", "data.csv"), stringsAsFactors = FALSE)
  events <- read.csv(test_path("fixtures", "events.csv"), stringsAsFactors = FALSE)

  form_event_map <- data.frame(
    form = c("demographics", "visit", "visit"),
    unique_event_name = c("baseline_arm_1", "baseline_arm_1", "visit_2_arm_1"),
    stringsAsFactors = FALSE
  )

  # Mirror normalization behavior in camr_redcap_form_event_mapping()
  form_event_map <- dplyr::rename(
    form_event_map,
    form_name = "form",
    event_name = "unique_event_name"
  )

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

  subject_level <- camr_subject_level(data, metadata, events, form_event_map, config)

  # demographics appears in one event, visit appears in multiple.
  expect_true("sex" %in% names(subject_level))
  expect_false("visit_date" %in% names(subject_level))
  expect_equal(nrow(subject_level), length(unique(data$record_id)))
})

test_that("subject_level includes configured keep_fields in single-event form mode", {
  metadata <- read.csv(test_path("fixtures", "metadata.csv"), stringsAsFactors = FALSE)
  data <- read.csv(test_path("fixtures", "data.csv"), stringsAsFactors = FALSE)
  events <- read.csv(test_path("fixtures", "events.csv"), stringsAsFactors = FALSE)
  data$record_cam_id <- c("CAM-001", "CAM-001", "CAM-002")

  form_event_map <- data.frame(
    form_name = c("demographics", "visit"),
    event_name = c("baseline_arm_1", "visit_2_arm_1"),
    stringsAsFactors = FALSE
  )

  config <- list(
    id_field = "record_id",
    status_field = "record_status",
    event_name_field = "redcap_event_name",
    keep_fields = c("record_id", "record_cam_id")
  )

  subject_level <- camr_subject_level(data, metadata, events, form_event_map, config, apply_labels = FALSE)

  expect_true("record_cam_id" %in% names(subject_level))
})
