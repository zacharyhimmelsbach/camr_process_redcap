library(camrProcessRedcap)

test_that("camr_read_tlfb_files parses assessed days and events", {
  td <- tempfile("tlfb_json_")
  dir.create(td, recursive = TRUE)
  f <- file.path(td, "sample.json")
  json <- paste0(
    '{"subject":"TEST_001","event":"baseline_arm_1","record":"5","start":"2024-08-01","end":"2024-08-03",',
    '"staff":"be931","datetime":"2025-05-05T18:23:52.696Z","appversion":"3.0.1",',
    '"events":[{"_date":"2024-08-01T00:00:00.000Z","_type":"no-use"},',
    '{"_date":"2024-08-02T00:00:00.000Z","_type":"use","_category":"etoh","_substance":"Alcohol","_method":"Alcohol","_times":1,"_amount":2,"_units":"standard drinks"}]}'
  )
  writeLines(json, f)

  out <- camr_read_tlfb_files(td)

  expect_true(nrow(out) >= 3)
  expect_true(all(c("record_id", "redcap_event_name", "tlfb_day", "event_type") %in% names(out)))
  expect_true(any(is.na(out$event_type) | out$event_type == "no-use" | out$event_type == "use"))
})

test_that("camr_ml_tlfb and camr_vl_tlfb produce summaries", {
  tlfb_raw <- tibble::tibble(
    record_id = c("5", "5", "5"),
    redcap_event_name = c("baseline_arm_1", "baseline_arm_1", "baseline_arm_1"),
    tlfb_day = as.Date(c("2024-08-01", "2024-08-02", "2024-08-03")),
    event_type = c("no-use", "use", NA),
    event_category = c(NA, "etoh", NA),
    event_substance = c(NA, "Alcohol", NA),
    event_method = c(NA, "Alcohol", NA),
    event_method_type = c(NA, NA, NA),
    event_occasions = c(NA, 1, NA),
    event_amount = c(NA, 2, NA),
    event_units = c(NA, "standard drinks", NA)
  )
  cfg <- list(id_field = "record_id", event_name_field = "redcap_event_name")

  ml <- camr_ml_tlfb(tlfb_raw, tibble::tibble(), cfg)
  vl <- camr_vl_tlfb(ml, cfg)

  expect_true(all(c("tlfb_data_collected", "tlfb_etoh_use", "tlfb_etoh_dpd") %in% names(ml)))
  expect_equal(nrow(vl), 1)
  expect_true(all(c("tlfb_missing_days", "tlfb_etoh_use", "tlfb_etoh_dpw", "tlfb_etoh_dpdd") %in% names(vl)))
})

test_that("camr_ml_tlfb and camr_vl_tlfb can return legacy column names", {
  tlfb_raw <- tibble::tibble(
    record_id = c("5", "5"),
    tlfb_subject = c("TEST_001", "TEST_001"),
    redcap_event_name = c("baseline_arm_1", "baseline_arm_1"),
    tlfb_day = as.Date(c("2024-08-01", "2024-08-02")),
    event_type = c("no-use", "use"),
    event_category = c(NA, "etoh"),
    event_substance = c(NA, "Alcohol"),
    event_method = c(NA, "Alcohol"),
    event_method_type = c(NA, NA),
    event_occasions = c(NA, 1),
    event_amount = c(NA, 2),
    event_units = c(NA, "standard drinks"),
    tlfb_start = as.Date(c("2024-08-01", "2024-08-01")),
    tlfb_end = as.Date(c("2024-08-08", "2024-08-08")),
    tlfb_staff = c("be931", "be931"),
    tlfb_completed = c("2025-05-05T18:23:52.696Z", "2025-05-05T18:23:52.696Z"),
    tlfb_app_version = c("3.0.1", "3.0.1")
  )
  cfg <- list(id_field = "record_id", event_name_field = "redcap_event_name")

  ml <- camr_ml_tlfb(tlfb_raw, tibble::tibble(), cfg, legacy_names = TRUE)
  vl <- camr_vl_tlfb(ml, cfg, legacy_names = TRUE)

  expect_true(all(c("IDX.CHR.Subject", "IDX.CHR.Visit", "IDX.DAT.TLFB.Day", "QCC.LGL.TLFB.DataCollected") %in% names(ml)))
  expect_true(all(c("IDX.CHR.Subject", "IDX.CHR.Visit", "INV.LGL.TLFB.ETOH.Use", "INV.DBL.TLFB.ETOH.DPW", "QCC.INT.TLFB.MissingDays") %in% names(vl)))
})
