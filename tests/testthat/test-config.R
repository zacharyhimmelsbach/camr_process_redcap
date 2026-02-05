library(camr_process_redcap)

test_that("camr_validate_config normalizes status values to labels", {
  metadata <- read.csv(test_path("fixtures", "metadata.csv"), stringsAsFactors = FALSE)

  config <- list(
    status_field = "record_status",
    status_active_values = c("1"),
    status_dropout_values = c("2")
  )

  validated <- camr_validate_config(config, metadata)

  expect_equal(validated$status_active_values, "Active")
  expect_equal(validated$status_dropout_values, "Dropout")
})
