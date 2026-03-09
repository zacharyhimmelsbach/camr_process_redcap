library(camrProcessRedcap)

test_that("camr_validate_config normalizes status values to labels", {
  metadata <- read.csv(test_path("fixtures", "metadata.csv"), stringsAsFactors = FALSE)

  config <- list(
    status_field = "record_status",
    status_active_values = c("1"),
    status_dropout_values = c("2"),
    status_test_values = c("1"),
    exclude_test = TRUE,
    consort_intervention_completed_field = c("1"),
    consort_study_completed_field = c("1"),
    consort_attrition_status_values = c("2"),
    consort_prerandomization_exclusion_status_values = c("2"),
    consort_screening_eligibility_field = "record_status",
    consort_screen_fail_values = c("2")
  )

  validated <- camr_validate_config(config, metadata)

  expect_equal(validated$status_active_values, "Active")
  expect_equal(validated$status_dropout_values, "Dropout")
  expect_equal(validated$status_test_values, "Active")
  expect_true(validated$exclude_test)
  expect_equal(validated$consort_intervention_completed_field, "Active")
  expect_equal(validated$consort_study_completed_field, "Active")
  expect_equal(validated$consort_attrition_status_values, "Dropout")
  expect_equal(validated$consort_prerandomization_exclusion_status_values, "Dropout")
  expect_equal(validated$consort_screen_fail_values, "Dropout")
})

test_that("camr_validate_config resolves keep_fields with defaults and id_field", {
  metadata <- read.csv(test_path("fixtures", "metadata.csv"), stringsAsFactors = FALSE)

  validated_default <- camr_validate_config(
    list(
      id_field = "record_id",
      status_field = "record_status",
      status_active_values = character(0),
      status_dropout_values = character(0)
    ),
    metadata
  )

  expect_equal(
    validated_default$keep_fields,
    c("record_id", "record_status", "record_subject_id", "record_cam_id")
  )

  validated_custom <- camr_validate_config(
    list(
      id_field = "record_id",
      status_field = "record_status",
      keep_fields = c("record_cam_id"),
      status_active_values = character(0),
      status_dropout_values = character(0)
    ),
    metadata
  )

  expect_equal(validated_custom$keep_fields, c("record_id", "record_cam_id"))
})

test_that("camr_validate_config normalizes analysis_forms", {
  metadata <- read.csv(test_path("fixtures", "metadata.csv"), stringsAsFactors = FALSE)

  validated_default <- camr_validate_config(
    list(
      id_field = "record_id",
      status_field = "record_status",
      status_active_values = character(0),
      status_dropout_values = character(0)
    ),
    metadata
  )

  expect_setequal(validated_default$analysis_forms, c("demographics", "visit"))

  expect_warning(
    validated_custom <- camr_validate_config(
      list(
        id_field = "record_id",
        status_field = "record_status",
        analysis_forms = c("demographics", "not_a_form"),
        status_active_values = character(0),
        status_dropout_values = character(0)
      ),
      metadata
    ),
    "analysis_forms were not found"
  )
  expect_equal(validated_custom$analysis_forms, "demographics")
})

test_that("camr_validate_config normalizes screen-fail values against the configured screening field only", {
  metadata <- tibble::tibble(
    field_name = c("record_status", "prescreen_eligible"),
    form_name = c("status", "screen"),
    field_type = c("dropdown", "dropdown"),
    select_choices_or_calculations = c(
      "1, Active | 2, Dropout",
      "1, Yes, RNT | 2, Yes, HEALTHY CONTROL | 3, No"
    )
  )

  config <- list(
    status_field = "record_status",
    status_active_values = c("1"),
    status_dropout_values = c("2"),
    consort_screening_eligibility_field = "prescreen_eligible",
    consort_screen_fail_values = c("3")
  )

  validated <- camr_validate_config(config, metadata)

  expect_equal(validated$status_active_values, "Active")
  expect_equal(validated$status_dropout_values, "Dropout")
  expect_equal(validated$consort_screen_fail_values, "No")
})

test_that("camr_validate_config normalizes recruitment accrual config fields", {
  metadata <- tibble::tibble(
    field_name = c("record_status", "consent_date"),
    form_name = c("status", "enrollment"),
    field_type = c("dropdown", "text"),
    select_choices_or_calculations = c("1, Active | 2, Dropout", "")
  )

  validated <- camr_validate_config(
    list(
      status_field = "record_status",
      status_active_values = c("1"),
      status_dropout_values = c("2"),
      recruitment_study_start_date = "2025-01-01",
      recruitment_planned_end_date = as.Date("2025-12-31"),
      recruitment_goal_n = "120",
      recruitment_consent_date_field = "consent_date"
    ),
    metadata
  )

  expect_equal(validated$recruitment_study_start_date, "2025-01-01")
  expect_equal(validated$recruitment_planned_end_date, "2025-12-31")
  expect_equal(validated$recruitment_goal_n, 120)
  expect_equal(validated$recruitment_consent_date_field, "consent_date")
})
