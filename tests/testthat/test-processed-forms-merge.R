library(camrProcessRedcap)

test_that("camr_merge_processed_forms adds derived columns", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "1", "2"),
    redcap_event_name = c("baseline_arm_1", "visit_2_arm_1", "baseline_arm_1"),
    redcap_repeat_instrument = c("", "", ""),
    redcap_repeat_instance = c("", "", ""),
    visit_value = c("10", "20", "30")
  )

  form_visit <- tibble::tibble(
    record_id = c("1", "1", "2"),
    redcap_event_name = c("baseline_arm_1", "visit_2_arm_1", "baseline_arm_1"),
    z_score = c(0.1, 0.8, -0.2)
  )

  config <- list(
    id_field = "record_id",
    event_name_field = "redcap_event_name"
  )

  out <- camr_merge_processed_forms(redcap_data, list(form_visit), config)

  expect_true("z_score" %in% names(out))
  expect_equal(out$z_score, c(0.1, 0.8, -0.2))
})

test_that("camr_merge_processed_forms prefers processed values when present", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    redcap_event_name = c("baseline_arm_1", "baseline_arm_1"),
    redcap_repeat_instrument = c("", ""),
    redcap_repeat_instance = c("", ""),
    visit_date = c("2025-01-01", "2025-01-05")
  )

  form_visit <- tibble::tibble(
    record_id = c("1", "2"),
    redcap_event_name = c("baseline_arm_1", "baseline_arm_1"),
    visit_date = c("2025-01-02", NA_character_)
  )

  config <- list(
    id_field = "record_id",
    event_name_field = "redcap_event_name"
  )

  out <- camr_merge_processed_forms(redcap_data, list(form_visit), config)

  expect_equal(out$visit_date, c("2025-01-02", "2025-01-05"))
})

test_that("camr_merge_processed_forms does not overwrite protected status field", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    redcap_event_name = c("baseline_arm_1", "baseline_arm_1"),
    redcap_repeat_instrument = c("", ""),
    redcap_repeat_instance = c("", ""),
    record_status = c("1", "2")
  )

  form_status <- tibble::tibble(
    record_id = c("1", "2"),
    redcap_event_name = c("baseline_arm_1", "baseline_arm_1"),
    record_status = c("Active", "Dropout")
  )

  config <- list(
    id_field = "record_id",
    event_name_field = "redcap_event_name",
    status_field = "record_status"
  )

  out <- camr_merge_processed_forms(redcap_data, list(form_status), config)

  expect_equal(out$record_status, c("1", "2"))
})

test_that("camr_merge_processed_forms preserves repeat-instance values", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "1"),
    redcap_event_name = c("daily_arm_1", "daily_arm_1"),
    redcap_repeat_instrument = c("daily_survey", "daily_survey"),
    redcap_repeat_instance = c("1", "2"),
    daily_score = c("10", "20")
  )

  form_repeat <- tibble::tibble(
    record_id = c("1", "1"),
    redcap_event_name = c("daily_arm_1", "daily_arm_1"),
    redcap_repeat_instrument = c("daily_survey", "daily_survey"),
    redcap_repeat_instance = c("1", "2"),
    daily_score = c("11", "22")
  )

  config <- list(
    id_field = "record_id",
    event_name_field = "redcap_event_name"
  )

  out <- camr_merge_processed_forms(redcap_data, list(form_repeat), config)

  expect_equal(out$daily_score, c("11", "22"))
})
