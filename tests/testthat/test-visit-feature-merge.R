library(camrProcessRedcap)

test_that("camr_merge_visit_features merges TLFB visit variables into subject_visit", {
  visit_df <- tibble::tibble(
    record_id = c("5", "5", "9"),
    redcap_event_name = c("baseline_arm_1", "visit_2_arm_1", "baseline_arm_1"),
    event_label = c("Baseline", "Visit 2", "Baseline"),
    age = c(18, 18, 20)
  )

  tlfb_vl <- tibble::tibble(
    record_id = c("5", "5"),
    redcap_event_name = c("baseline_arm_1", "visit_2_arm_1"),
    `INV.LGL.TLFB.ETOH.Use` = c(TRUE, FALSE),
    `INV.DBL.TLFB.ETOH.DPW` = c(2, 0),
    `QCC.INT.TLFB.MissingDays` = c(0L, 1L)
  )

  config <- list(
    id_field = "record_id",
    event_name_field = "redcap_event_name"
  )

  out <- camr_merge_visit_features(visit_df, tlfb_vl, config)

  expect_true("INV.LGL.TLFB.ETOH.Use" %in% names(out))
  expect_true("INV.DBL.TLFB.ETOH.DPW" %in% names(out))
  expect_equal(out$`INV.DBL.TLFB.ETOH.DPW`, c(2, 0, NA))
})
