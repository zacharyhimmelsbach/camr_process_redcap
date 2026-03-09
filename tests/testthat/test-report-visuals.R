library(camrProcessRedcap)

test_that("camr_report_plot_consort returns a plot object", {
  consort <- tibble::tibble(
    stage = c("Screened", "Consented", "Active/Completed", "Dropped Out"),
    n = c(10L, 8L, 6L, 2L),
    metric = c("screened", "consented", "active_completed", "dropped_out"),
    group = NA_character_,
    phase = NA_character_,
    reason = NA_character_
  )

  p <- camr_report_plot_consort(consort)

  expect_true(inherits(p, "ggplot") || inherits(p, "htmlwidget"))
})

test_that("latest-event report plot functions return ggplot objects", {
  subject_level <- tibble::tibble(
    record_id = c(1, 2, 3, 4),
    record_status = c("Active", "Completed", "Dropout", "Dropout")
  )
  subject_visit <- tibble::tibble(
    record_id = c(1, 1, 2, 2, 3, 4),
    event_number = c(1, 2, 1, 3, 2, 1),
    event_label = c("Baseline", "Visit 2", "Baseline", "Visit 3", "Visit 2", "Baseline")
  )

  p_active <- camr_report_plot_latest_active(
    subject_level = subject_level,
    subject_visit = subject_visit,
    id_field = "record_id",
    status_field = "record_status",
    event_label_field = "event_label",
    event_number_field = "event_number",
    status_active_values = c("Active", "Completed")
  )
  p_dropout <- camr_report_plot_latest_dropout(
    subject_level = subject_level,
    subject_visit = subject_visit,
    id_field = "record_id",
    status_field = "record_status",
    event_label_field = "event_label",
    event_number_field = "event_number",
    status_dropout_values = c("Dropout")
  )

  expect_s3_class(p_active, "ggplot")
  expect_s3_class(p_dropout, "ggplot")
})

test_that("latest-event plots can facet by consort population field", {
  subject_level <- tibble::tibble(
    record_id = c(1, 2, 3, 4),
    record_status = c("Active", "Completed", "Dropout", "Dropout"),
    cohort_type = c("RNT", "RNT", "Healthy Control", "Healthy Control")
  )
  subject_visit <- tibble::tibble(
    record_id = c(1, 1, 2, 2, 3, 4),
    event_number = c(1, 2, 1, 3, 2, 1),
    event_label = c("Baseline", "Visit 2", "Baseline", "Visit 3", "Visit 2", "Baseline")
  )

  p_active <- camr_report_plot_latest_active(
    subject_level = subject_level,
    subject_visit = subject_visit,
    id_field = "record_id",
    status_field = "record_status",
    event_label_field = "event_label",
    event_number_field = "event_number",
    status_active_values = c("Active", "Completed"),
    consort_population_field = "cohort_type"
  )

  expect_s3_class(p_active, "ggplot")
})

test_that("recruitment accrual plot returns ggplot object", {
  subject_level <- tibble::tibble(
    record_id = c(1, 2, 3, 4, 5),
    record_subject_id = c("S001", "S002", "", "S004", "S005"),
    consent_date = c("2025-01-05", "2025-01-20", "2025-02-02", "2025-02-12", "2025-03-01")
  )

  p <- camr_report_plot_recruitment_accrual(
    subject_level = subject_level,
    id_field = "record_id",
    trial_id_field = "record_subject_id",
    consent_date_field = "consent_date",
    study_start_date = "2025-01-01",
    planned_end_date = "2025-12-31",
    recruitment_goal_n = 120
  )

  expect_s3_class(p, "ggplot")
})

test_that("recruitment accrual plot supports population-specific panels", {
  subject_level <- tibble::tibble(
    record_id = c(1, 2, 3, 4, 5),
    record_subject_id = c("S001", "S002", "S003", "S004", "S005"),
    consent_date = c("2025-01-05", "2025-01-20", "2025-02-02", "2025-02-12", "2025-03-01"),
    cohort_type = c("RNT", "RNT", "Healthy Control", "Healthy Control", "RNT")
  )

  p <- camr_report_plot_recruitment_accrual(
    subject_level = subject_level,
    id_field = "record_id",
    trial_id_field = "record_subject_id",
    consent_date_field = "consent_date",
    study_start_date = "2025-01-01",
    planned_end_date = "2025-12-31",
    recruitment_goal_n = 120,
    consort_population_field = "cohort_type",
    population_value = "RNT"
  )

  expect_s3_class(p, "ggplot")
})
