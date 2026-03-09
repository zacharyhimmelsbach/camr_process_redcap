library(camrProcessRedcap)

test_that("camr_redcap_events keeps human event labels when unique_event_name is present", {
  payload <- jsonlite::toJSON(
    data.frame(
      event_name = c("Baseline", "Week 4"),
      unique_event_name = c("baseline_arm_1", "week_4_arm_1"),
      arm_num = c(1, 1),
      stringsAsFactors = FALSE
    ),
    auto_unbox = TRUE
  )

  testthat::local_mocked_bindings(
    camr_redcap_post = function(redcap_url, api_token, body) {
      payload
    },
    .package = "camrProcessRedcap"
  )

  out <- camr_redcap_events("https://example.invalid/api/", "token")

  expect_equal(out$event_name, c("baseline_arm_1", "week_4_arm_1"))
  expect_equal(out$event_label, c("Baseline", "Week 4"))
})
