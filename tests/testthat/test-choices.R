library(camr_process_redcap)

test_that("camr_parse_choices parses code/label", {
  parsed <- camr_parse_choices("1, Yes | 2, No")
  expect_equal(parsed$code, c("1", "2"))
  expect_equal(parsed$label, c("Yes", "No"))
})

test_that("camr_apply_labels converts multiple choice to factors", {
  metadata <- read.csv(test_path("fixtures", "metadata.csv"), stringsAsFactors = FALSE)
  data <- read.csv(test_path("fixtures", "data.csv"), stringsAsFactors = FALSE)

  labeled <- camr_apply_labels(data, metadata)

  expect_true(is.factor(labeled$record_status))
  expect_equal(levels(labeled$record_status), c("Active", "Dropout"))
  expect_true(is.factor(labeled$sex))
  expect_equal(levels(labeled$sex), c("Male", "Female"))
  expect_true(is.factor(labeled$symptom___1))
  expect_true(is.factor(labeled$symptom___2))
})
