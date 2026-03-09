library(camrProcessRedcap)

test_that("camr_consort_data keeps base 4-stage summary", {
  metadata <- read.csv(test_path("fixtures", "metadata.csv"), stringsAsFactors = FALSE)
  data <- read.csv(test_path("fixtures", "data.csv"), stringsAsFactors = FALSE)

  config <- list(
    id_field = "record_id",
    trial_id_field = "record_trial_id",
    status_field = "record_status",
    status_active_values = c("1"),
    status_dropout_values = c("2")
  )

  consort <- camr_consort_data(data, metadata, config)

  expect_true(all(c("stage", "n", "metric", "group", "phase", "reason") %in% names(consort)))
  expect_equal(consort$stage[1:4], c("Screened", "Consented", "Active/Completed", "Dropped Out"))
  expect_equal(consort$n[1:4], c(2L, 1L, 1L, 1L))
  expect_equal(consort$metric[1:4], c("screened", "consented", "active_completed", "dropped_out"))
})

test_that("camr_consort_data computes arm-level and exclusion metrics", {
  data <- tibble::tibble(
    record_id = 1:6,
    record_trial_id = c("T001", "T002", "T003", "T004", "", "T006"),
    record_status = c("Completed Intervention", "Withdrawn", "Completed Study", "Lost to Follow-Up", "", "Screen Fail"),
    rand_group = c("Group A", "Group A", "Group B", "Group B", "", ""),
    excluded_before_consent_reason = c(NA, NA, NA, NA, "Ineligible", NA),
    screen_fail_reason = c(NA, NA, NA, NA, NA, "Lab criteria")
  )

  config <- list(
    id_field = "record_id",
    trial_id_field = "record_trial_id",
    status_field = "record_status",
    status_active_values = c("Active", "Completed Intervention", "Completed Study"),
    status_dropout_values = c("Withdrawn", "Lost to Follow-Up", "Screen Fail"),
    consort_randomization_field = "rand_group",
    consort_group_values = c("Group A", "Group B"),
    consort_intervention_completed_field = "Completed Intervention",
    consort_study_completed_field = "Completed Study",
    consort_attrition_status_values = c("Withdrawn", "Lost to Follow-Up", "Terminated"),
    consort_preconsent_exclusion_reason_field = "excluded_before_consent_reason",
    consort_prerandomization_exclusion_reason_field = "screen_fail_reason",
    consort_prerandomization_exclusion_status_values = c("Screen Fail")
  )

  consort <- camr_consort_data(data, metadata = NULL, config = config)

  expect_equal(consort$n[consort$metric == "screened"][1], 6L)
  expect_equal(consort$n[consort$metric == "consented"][1], 5L)
  expect_equal(consort$n[consort$metric == "active_completed"][1], 2L)
  expect_equal(consort$n[consort$metric == "dropped_out"][1], 3L)
  expect_equal(consort$n[consort$metric == "randomized"][1], 4L)

  expect_equal(consort$n[consort$metric == "randomized_group" & consort$group == "Group A"][1], 2L)
  expect_equal(consort$n[consort$metric == "randomized_group" & consort$group == "Group B"][1], 2L)

  expect_equal(
    consort$n[consort$metric == "completed_intervention_group" & consort$group == "Group A" & consort$phase == "intervention"][1],
    2L
  )
  expect_equal(
    consort$n[consort$metric == "completed_intervention_group" & consort$group == "Group B" & consort$phase == "intervention"][1],
    0L
  )
  expect_equal(
    consort$n[consort$metric == "completed_study_group" & consort$group == "Group A" & consort$phase == "study"][1],
    0L
  )
  expect_equal(
    consort$n[consort$metric == "completed_study_group" & consort$group == "Group B" & consort$phase == "study"][1],
    1L
  )

  expect_equal(
    consort$n[consort$metric == "attrition_group_reason" &
      consort$group == "Group A" &
      consort$phase == "intervention" &
      consort$reason == "Withdrawn"][1],
    1L
  )
  expect_equal(
    consort$n[consort$metric == "attrition_group_reason" &
      consort$group == "Group B" &
      consort$phase == "intervention" &
      consort$reason == "Lost to Follow-Up"][1],
    1L
  )

  expect_equal(consort$n[consort$metric == "preconsent_excluded_total"][1], 1L)
  expect_equal(consort$n[consort$metric == "preconsent_exclusion_reason" & consort$reason == "Ineligible"][1], 1L)
  expect_equal(consort$n[consort$metric == "prerandomization_excluded_total"][1], 1L)
  expect_equal(consort$n[consort$metric == "prerandomization_exclusion_reason" & consort$reason == "Lab criteria"][1], 1L)
})

test_that("prerandomization-excluded participants are not counted under randomized nodes", {
  data <- tibble::tibble(
    record_id = 1:5,
    record_trial_id = c("T001", "T002", "T003", "T004", "T005"),
    record_status = c("Active", "Active", "Screen Fail", "Active", "Active"),
    rand_group = c("Group A", "Group B", "Group B", "Group A", "Group B"),
    screen_fail_reason = c(NA, NA, "Lab criteria", NA, NA)
  )

  config <- list(
    id_field = "record_id",
    trial_id_field = "record_trial_id",
    status_field = "record_status",
    status_active_values = c("Active"),
    status_dropout_values = c("Screen Fail"),
    consort_randomization_field = "rand_group",
    consort_group_values = c("Group A", "Group B"),
    consort_intervention_completed_field = "Completed Intervention",
    consort_study_completed_field = "Completed Study",
    consort_attrition_status_values = c("Withdrawn", "Lost to Follow-Up", "Terminated"),
    consort_prerandomization_exclusion_reason_field = "screen_fail_reason",
    consort_prerandomization_exclusion_status_values = c("Screen Fail")
  )

  consort <- camr_consort_data(data, metadata = NULL, config = config)

  n_randomized <- consort$n[consort$metric == "randomized"][1]
  group_total <- sum(consort$n[consort$metric == "randomized_group"], na.rm = TRUE)

  expect_equal(n_randomized, 4L)
  expect_equal(group_total, n_randomized)
  expect_equal(consort$n[consort$metric == "prerandomization_excluded_total"][1], 1L)
})

test_that("camr_consort_data computes screen-fail count from screening eligibility field", {
  data <- tibble::tibble(
    record_id = 1:6,
    record_trial_id = c("T001", "T002", "", "", "T005", "T006"),
    record_status = c("Active", "Dropout", "Dropout", "Active", "Dropout", "Active"),
    rand_group = c("Group A", "Group B", "", "", "Group A", "Group B")
  )

  config <- list(
    id_field = "record_id",
    trial_id_field = "record_trial_id",
    status_field = "record_status",
    status_active_values = c("Active"),
    status_dropout_values = c("Dropout"),
    consort_screening_eligibility_field = "record_status",
    consort_screen_fail_values = c("Dropout"),
    consort_randomization_field = "rand_group",
    consort_group_values = c("Group A", "Group B")
  )

  consort <- camr_consort_data(data, metadata = NULL, config = config)

  expect_equal(consort$n[consort$metric == "screened"][1], 6L)
  expect_equal(consort$n[consort$metric == "screen_fail"][1], 3L)
})

test_that("camr_consort_data accepts status values for intervention/study completion", {
  data <- tibble::tibble(
    record_id = 1:4,
    record_trial_id = c("T001", "T002", "T003", "T004"),
    record_status = c("Completed Intervention", "Completed Study", "Withdrawn", "Active"),
    rand_group = c("Group A", "Group A", "Group B", "Group B")
  )

  config <- list(
    id_field = "record_id",
    trial_id_field = "record_trial_id",
    status_field = "record_status",
    status_active_values = c("Active", "Completed Intervention", "Completed Study"),
    status_dropout_values = c("Withdrawn"),
    consort_randomization_field = "rand_group",
    consort_group_values = c("Group A", "Group B"),
    consort_intervention_completed_field = "Completed Intervention",
    consort_study_completed_field = "Completed Study",
    consort_attrition_status_values = c("Withdrawn")
  )

  consort <- camr_consort_data(data, metadata = NULL, config = config)

  expect_equal(
    consort$n[consort$metric == "completed_intervention_group" & consort$group == "Group A" & consort$phase == "intervention"][1],
    2L
  )
  expect_equal(
    consort$n[consort$metric == "completed_study_group" & consort$group == "Group A" & consort$phase == "study"][1],
    1L
  )
})

test_that("camr_consort_data treats consort_group_values as masked display labels", {
  data <- tibble::tibble(
    record_id = 1:4,
    record_subject_id = c("S1", "S2", "S3", "S4"),
    record_status = c("Active", "Completed Study", "Withdrawn", "Active"),
    rand_group = c("active_drug", "active_drug", "placebo", "placebo")
  )

  config <- list(
    id_field = "record_id",
    trial_id_field = "record_subject_id",
    status_field = "record_status",
    status_active_values = c("Active", "Completed Study"),
    status_dropout_values = c("Withdrawn"),
    consort_randomization_field = "rand_group",
    consort_group_values = c("Group A", "Group B"),
    consort_intervention_completed_field = "Completed Intervention",
    consort_study_completed_field = "Completed Study",
    consort_attrition_status_values = c("Withdrawn")
  )

  consort <- camr_consort_data(data, metadata = NULL, config = config)

  expect_equal(consort$n[consort$metric == "randomized_group" & consort$group == "Group A"][1], 2L)
  expect_equal(consort$n[consort$metric == "randomized_group" & consort$group == "Group B"][1], 2L)
  expect_false(any(consort$group %in% c("active_drug", "placebo"), na.rm = TRUE))
})

test_that("camr_consort_data adds optional population split before randomization", {
  data <- tibble::tibble(
    record_id = 1:8,
    record_subject_id = c(paste0("S", 1:6), "", ""),
    record_status = c("Active", "Completed Study", "Withdrawn", "Active", "Active", "Active", "Active", "Active"),
    cohort_type = c("Participant", "Participant", "Participant", "Healthy Control", "Healthy Control", "Participant", "Participant", "Healthy Control"),
    rand_group = c("tx", "tx", "ctl", "", "", "ctl", "tx", "ctl")
  )

  config <- list(
    id_field = "record_id",
    trial_id_field = "record_subject_id",
    status_field = "record_status",
    status_active_values = c("Active", "Completed Study"),
    status_dropout_values = c("Withdrawn"),
    consort_population_field = "cohort_type",
    consort_randomization_field = "rand_group",
    consort_group_values = c("Group A", "Group B"),
    consort_intervention_completed_field = "Completed Intervention",
    consort_study_completed_field = "Completed Study",
    consort_attrition_status_values = c("Withdrawn")
  )

  consort <- camr_consort_data(data, metadata = NULL, config = config)

  expect_equal(consort$n[consort$metric == "population_group" & consort$group == "Participant"][1], 4L)
  expect_equal(consort$n[consort$metric == "population_group" & consort$group == "Healthy Control"][1], 2L)
  expect_equal(consort$n[consort$metric == "population_completed_intervention" & consort$group == "Participant"][1], 1L)
  expect_equal(consort$n[consort$metric == "population_completed_intervention" & consort$group == "Healthy Control"][1], 0L)
  expect_equal(consort$n[consort$metric == "population_completed_study" & consort$group == "Participant"][1], 1L)
  expect_equal(consort$n[consort$metric == "population_completed_study" & consort$group == "Healthy Control"][1], 0L)
  expect_equal(consort$n[consort$metric == "population_randomized" & consort$group == "Participant"][1], 4L)
  expect_equal(consort$n[consort$metric == "population_randomized" & consort$group == "Healthy Control"][1], 0L)
  expect_equal(consort$n[consort$metric == "randomized"][1], 4L)
  expect_equal(consort$n[consort$metric == "population_randomization_eligible" & consort$group == "Participant"][1], 1L)
  expect_equal(consort$n[consort$metric == "population_randomization_eligible" & consort$group == "Healthy Control"][1], 0L)
})

test_that("camr_consort_data enforces configured eligible populations for randomization", {
  data <- tibble::tibble(
    record_id = 1:8,
    record_subject_id = paste0("S", 1:8),
    record_status = c("Active", "Completed Study", "Withdrawn", "Active", "Active", "Active", "Active", "Active"),
    cohort_type = c("RNT", "RNT", "RNT", "Healthy Control", "Healthy Control", "RNT", "RNT", "Healthy Control"),
    rand_group = c("tx", "tx", "ctl", "", "", "ctl", "tx", "ctl")
  )

  config <- list(
    id_field = "record_id",
    trial_id_field = "record_subject_id",
    status_field = "record_status",
    status_active_values = c("Active", "Completed Study"),
    status_dropout_values = c("Withdrawn"),
    consort_population_field = "cohort_type",
    consort_population_randomization_eligible_values = c("RNT"),
    consort_randomization_field = "rand_group",
    consort_group_values = c("Group A", "Group B"),
    consort_intervention_completed_field = "Completed Intervention",
    consort_study_completed_field = "Completed Study",
    consort_attrition_status_values = c("Withdrawn")
  )

  consort <- camr_consort_data(data, metadata = NULL, config = config)

  expect_equal(consort$n[consort$metric == "population_randomization_eligible" & consort$group == "RNT"][1], 1L)
  expect_equal(consort$n[consort$metric == "population_randomization_eligible" & consort$group == "Healthy Control"][1], 0L)
  expect_equal(consort$n[consort$metric == "population_randomized" & consort$group == "RNT"][1], 5L)
  expect_equal(consort$n[consort$metric == "population_randomized" & consort$group == "Healthy Control"][1], 0L)
  expect_equal(
    consort$n[
      consort$metric == "population_prerandomization_attrition_reason" &
        consort$group == "RNT" &
        consort$reason == "Withdrawn"
    ][1],
    1L
  )
  expect_equal(consort$n[consort$metric == "randomized"][1], 5L)
})
