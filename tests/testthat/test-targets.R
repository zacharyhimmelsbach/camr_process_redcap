library(camrProcessRedcap)

test_that("camr_format_list_entries avoids trailing comma", {
  expect_equal(
    camrProcessRedcap:::camr_format_list_entries(c("form_a", "form_b"), indent = "  "),
    c("  form_a,", "  form_b")
  )
})

test_that("camr_format_list_entries handles empty and single-element inputs", {
  expect_null(camrProcessRedcap:::camr_format_list_entries(character(0)))
  expect_equal(
    camrProcessRedcap:::camr_format_list_entries("form_a", indent = "  "),
    "  form_a"
  )
})
