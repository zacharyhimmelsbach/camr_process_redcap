library(camrProcessRedcap)

test_that("camr_form_data keeps only rows where form is collected via complete status", {
  metadata <- tibble::tibble(
    field_name = c("record_id", "record_status", "sex", "visit_date"),
    form_name = c("record_identifiers", "record_identifiers", "record_identifiers", "visit"),
    field_type = c("text", "dropdown", "radio", "text"),
    select_choices_or_calculations = c("", "1, Active | 2, Dropout", "1, Male | 2, Female", "")
  )

  data <- tibble::tibble(
    record_id = c("1", "1", "2"),
    record_status = c("1", "1", "2"),
    sex = c("1", "", "2"),
    redcap_event_name = c("baseline_arm_1", "visit_2_arm_1", "baseline_arm_1"),
    redcap_repeat_instrument = c("", "", ""),
    redcap_repeat_instance = c("", "", ""),
    record_identifiers_complete = c("2", "", "2")
  )

  config <- list(
    id_field = "record_id",
    trial_id_field = "record_trial_id",
    status_field = "record_status",
    event_name_field = "redcap_event_name"
  )

  out <- camr_form_data(data, metadata, "record_identifiers", config)

  expect_equal(nrow(out), 2)
  expect_equal(sort(out$redcap_event_name), c("baseline_arm_1", "baseline_arm_1"))
  expect_true("redcap_repeat_instrument" %in% names(out))
  expect_true("redcap_repeat_instance" %in% names(out))
})

test_that("camr_form_data filters by non-missing form fields when complete is absent", {
  metadata <- tibble::tibble(
    field_name = c("record_id", "record_status", "visit_date"),
    form_name = c("demographics", "demographics", "visit"),
    field_type = c("text", "dropdown", "text"),
    select_choices_or_calculations = c("", "1, Active | 2, Dropout", "")
  )

  data <- tibble::tibble(
    record_id = c("1", "1", "2"),
    record_status = c("1", "1", "2"),
    redcap_event_name = c("baseline_arm_1", "visit_2_arm_1", "baseline_arm_1"),
    visit_date = c("", "2025-02-01", ""),
    redcap_repeat_instrument = c("", "", ""),
    redcap_repeat_instance = c("", "", "")
  )

  config <- list(
    id_field = "record_id",
    trial_id_field = "record_trial_id",
    status_field = "record_status",
    event_name_field = "redcap_event_name"
  )

  out <- camr_form_data(data, metadata, "visit", config)

  expect_equal(nrow(out), 1)
  expect_equal(out$redcap_event_name, "visit_2_arm_1")
})

test_that("camr_form_data includes configured keep_fields", {
  metadata <- tibble::tibble(
    field_name = c("record_id", "record_status", "sex"),
    form_name = c("record_identifiers", "record_identifiers", "record_identifiers"),
    field_type = c("text", "dropdown", "radio"),
    select_choices_or_calculations = c("", "1, Active | 2, Dropout", "1, Male | 2, Female")
  )

  data <- tibble::tibble(
    record_id = "1",
    record_status = "1",
    record_cam_id = "CAM-001",
    sex = "1",
    redcap_event_name = "baseline_arm_1",
    record_identifiers_complete = "2"
  )

  config <- list(
    id_field = "record_id",
    status_field = "record_status",
    event_name_field = "redcap_event_name",
    keep_fields = c("record_id", "record_cam_id")
  )

  out <- camr_form_data(data, metadata, "record_identifiers", config)

  expect_true("record_id" %in% names(out))
  expect_true("record_cam_id" %in% names(out))
  expect_true("record_status" %in% names(out))
})
