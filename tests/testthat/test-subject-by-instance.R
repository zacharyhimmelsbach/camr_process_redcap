library(camrProcessRedcap)

test_that("subject_by_instance keeps base and instrument-specific columns only", {
  metadata <- tibble::tibble(
    field_name = c(
      "record_id",
      "record_trial_id",
      "record_status",
      "ae_date",
      "ae_term",
      "med_name",
      "unrelated_field"
    ),
    form_name = c(
      "record_identifiers",
      "record_identifiers",
      "record_status",
      "adverse_events",
      "adverse_events",
      "concomitant_medications",
      "other_form"
    ),
    field_type = c("text", "text", "dropdown", "text", "text", "text", "text"),
    select_choices_or_calculations = c("", "", "1, Active | 2, Dropout", "", "", "", "")
  )

  data <- tibble::tibble(
    record_id = c("1", "1", "2"),
    record_trial_id = c("A001", "A001", "A002"),
    record_status = c("1", "1", "2"),
    redcap_event_name = c("baseline_arm_1", "visit_2_arm_1", "baseline_arm_1"),
    redcap_repeat_instrument = c("adverse_events", "adverse_events", "concomitant_medications"),
    redcap_repeat_instance = c("1", "2", "1"),
    ae_date = c("2025-01-03", "2025-02-03", NA),
    ae_term = c("Headache", "Nausea", NA),
    med_name = c(NA, NA, "Ibuprofen"),
    unrelated_field = c("x", "y", "z")
  )

  events <- tibble::tibble(
    event_name = c("baseline_arm_1", "visit_2_arm_1"),
    event_number = c(1L, 2L),
    event_label = c("Baseline", "Visit 2")
  )

  config <- list(
    id_field = "record_id",
    trial_id_field = "record_trial_id",
    status_field = "record_status",
    event_name_field = "redcap_event_name"
  )

  out <- camr_subject_by_instance(data, metadata, events, "adverse_events", config)

  expected <- c(
    "record_id",
    "record_status",
    "redcap_event_name",
    "redcap_repeat_instrument",
    "redcap_repeat_instance",
    "ae_date",
    "ae_term",
    "event_number",
    "event_label"
  )

  expect_equal(sort(names(out)), sort(expected))
  expect_equal(nrow(out), 2)
  expect_false("med_name" %in% names(out))
  expect_false("unrelated_field" %in% names(out))
})

test_that("subject_by_instance includes configured keep_fields", {
  metadata <- tibble::tibble(
    field_name = c("record_id", "record_status", "ae_date"),
    form_name = c("record_identifiers", "record_status", "adverse_events"),
    field_type = c("text", "dropdown", "text"),
    select_choices_or_calculations = c("", "1, Active | 2, Dropout", "")
  )

  data <- tibble::tibble(
    record_id = c("1"),
    record_status = c("1"),
    record_cam_id = c("CAM-001"),
    redcap_event_name = c("baseline_arm_1"),
    redcap_repeat_instrument = c("adverse_events"),
    redcap_repeat_instance = c("1"),
    ae_date = c("2025-01-03")
  )

  events <- tibble::tibble(
    event_name = c("baseline_arm_1"),
    event_number = c(1L),
    event_label = c("Baseline")
  )

  config <- list(
    id_field = "record_id",
    status_field = "record_status",
    event_name_field = "redcap_event_name",
    keep_fields = c("record_id", "record_cam_id")
  )

  out <- camr_subject_by_instance(data, metadata, events, "adverse_events", config)

  expect_true("record_id" %in% names(out))
  expect_true("record_cam_id" %in% names(out))
  expect_false("record_status" %in% names(out))
})

test_that("subject_by_instance backfills missing keep fields from non-repeat rows", {
  metadata <- tibble::tibble(
    field_name = c("record_id", "record_status", "record_subject_id", "ae_date"),
    form_name = c("record_identifiers", "record_status", "record_identifiers", "adverse_events"),
    field_type = c("text", "dropdown", "text", "text"),
    select_choices_or_calculations = c("", "1, Active | 2, Dropout", "", "")
  )

  data <- tibble::tibble(
    record_id = c("1", "1"),
    record_status = c("1", ""),
    record_subject_id = c("SUBJ-001", ""),
    redcap_event_name = c("baseline_arm_1", "baseline_arm_1"),
    redcap_repeat_instrument = c("", "adverse_events"),
    redcap_repeat_instance = c("", "1"),
    ae_date = c(NA, "2025-01-03")
  )

  events <- tibble::tibble(
    event_name = c("baseline_arm_1"),
    event_number = c(1L),
    event_label = c("Baseline")
  )

  config <- list(
    id_field = "record_id",
    status_field = "record_status",
    event_name_field = "redcap_event_name"
  )

  out <- camr_subject_by_instance(data, metadata, events, "adverse_events", config)

  expect_equal(nrow(out), 1)
  expect_equal(as.character(out$record_status[[1]]), "Active")
  expect_equal(as.character(out$record_subject_id[[1]]), "SUBJ-001")
})
