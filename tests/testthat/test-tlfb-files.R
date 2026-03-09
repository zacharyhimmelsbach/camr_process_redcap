library(camrProcessRedcap)

test_that("camr_tlfb_index finds TLFB file references", {
  metadata <- tibble::tibble(
    field_name = c("record_id", "redcap_event_name", "tlfb_files", "other_upload"),
    form_name = c("record_identifiers", "record_identifiers", "timeline_followback", "other"),
    field_type = c("text", "text", "file", "file"),
    field_label = c("Record ID", "Event", "TLFB Files", "Other Upload")
  )

  data <- tibble::tibble(
    record_id = c("1", "1", "2"),
    redcap_event_name = c("baseline_arm_1", "visit_2_arm_1", "baseline_arm_1"),
    redcap_repeat_instrument = c("", "", ""),
    redcap_repeat_instance = c("", "", ""),
    tlfb_files = c("101", "", "202"),
    other_upload = c("900", "901", "")
  )

  config <- list(
    id_field = "record_id",
    event_name_field = "redcap_event_name"
  )

  out <- camr_tlfb_index(data, metadata, config)

  expect_equal(nrow(out), 2)
  expect_true(all(out$field_name == "tlfb_files"))
  expect_equal(sort(out$doc_id), c("101", "202"))
})

test_that("camr_download_tlfb_files returns empty character when no rows", {
  out <- camr_download_tlfb_files(
    tlfb_index = tibble::tibble(),
    redcap_url = "https://example.org/api/",
    api_token = "token",
    out_dir = tempfile("tlfb_none_")
  )
  expect_equal(out, character(0))
})
