library(camrProcessRedcap)

test_that("camr_analysis_subject selects keep fields plus configured form fields", {
  metadata <- read.csv(test_path("fixtures", "metadata.csv"), stringsAsFactors = FALSE)
  subject_level <- tibble::tibble(
    record_id = c("1", "2"),
    record_status = c("1", "2"),
    record_cam_id = c("CAM-001", "CAM-002"),
    sex = c("1", "2"),
    visit_date = c("2025-01-01", "2025-01-05"),
    symptom___1 = c("1", "0")
  )

  config <- list(
    id_field = "record_id",
    keep_fields = c("record_id", "record_status", "record_cam_id"),
    analysis_forms = "visit"
  )

  out <- camr_analysis_subject(subject_level, metadata, config)

  expect_setequal(
    names(out),
    c("record_id", "record_status", "record_cam_id", "visit_date", "symptom___1")
  )
})

test_that("camr_analysis_visit includes all analysis_subject columns", {
  metadata <- read.csv(test_path("fixtures", "metadata.csv"), stringsAsFactors = FALSE)
  analysis_subject <- tibble::tibble(
    record_id = c("1", "2"),
    record_status = c("1", "2"),
    sex = c("1", "2")
  )
  subject_visit <- tibble::tibble(
    record_id = c("1", "1", "2"),
    record_status = c("1", "1", "2"),
    sex = c("1", "1", "2"),
    redcap_event_name = c("baseline_arm_1", "visit_2_arm_1", "baseline_arm_1"),
    visit_date = c("2025-01-01", "2025-02-01", "2025-01-05")
  )

  config <- list(
    id_field = "record_id",
    keep_fields = c("record_id", "record_status"),
    analysis_forms = "demographics"
  )

  out <- camr_analysis_visit(subject_visit, analysis_subject, metadata, config)

  expect_true(all(names(analysis_subject) %in% names(out)))
  expect_false("visit_date" %in% names(out))
})
