library(camrProcessRedcap)

test_that("camr_filter_test_records excludes test statuses when configured", {
  data <- tibble::tibble(
    record_id = 1:4,
    record_status = c("Active", "Test Record", "Dropout", "Test Record")
  )
  metadata <- tibble::tibble(
    field_name = "record_status",
    form_name = "record_status",
    field_type = "dropdown",
    select_choices_or_calculations = "1, Active | 2, Dropout | 3, Test Record"
  )
  config <- list(
    status_field = "record_status",
    status_test_values = "Test Record",
    exclude_test = TRUE
  )

  out <- camr_filter_test_records(data, metadata, config)
  expect_equal(out$record_id, c(1L, 3L))
})

test_that("camr_filter_test_records keeps all rows when exclude_test is FALSE", {
  data <- tibble::tibble(
    record_id = 1:3,
    record_status = c("Active", "Test Record", "Dropout")
  )
  config <- list(
    status_field = "record_status",
    status_test_values = "Test Record",
    exclude_test = FALSE
  )

  out <- camr_filter_test_records(data, metadata = NULL, config = config)
  expect_equal(out$record_id, c(1L, 2L, 3L))
})

test_that("camr_filter_test_records excludes all rows for test-marked records", {
  data <- tibble::tibble(
    record_id = c("1", "1", "2", "2"),
    record_status = c("3", "", "1", ""),
    redcap_repeat_instrument = c("", "adverse_events", "", "adverse_events")
  )
  metadata <- tibble::tibble(
    field_name = "record_status",
    form_name = "record_status",
    field_type = "dropdown",
    select_choices_or_calculations = "1, Active | 2, Dropout | 3, Test Record"
  )
  config <- list(
    id_field = "record_id",
    status_field = "record_status",
    status_test_values = "Test Record",
    exclude_test = TRUE
  )

  out <- camr_filter_test_records(data, metadata, config)
  expect_equal(unique(out$record_id), "2")
  expect_equal(nrow(out), 2)
})
