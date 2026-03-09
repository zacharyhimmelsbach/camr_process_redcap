#' Plot recruitment accrual against planned goal
#'
#' @param subject_level Subject-level data.
#' @param id_field ID field name.
#' @param trial_id_field Field used to mark consented participants.
#' @param consent_date_field Field containing participant consent date.
#' @param study_start_date Study start date (Date or ISO date string).
#' @param planned_end_date Planned study end date (Date or ISO date string).
#' @param recruitment_goal_n Planned recruitment goal.
#' @param consort_population_field Optional population field in subject-level data.
#' @param population_value Optional single population value to plot.
#' @param data_cutoff_date Optional data cutoff date (defaults to `Sys.Date()`).
#'
#' @return A `ggplot2` object.
#' @export
camr_report_plot_recruitment_accrual <- function(subject_level,
                                                 id_field,
                                                 trial_id_field,
                                                 consent_date_field,
                                                 study_start_date,
                                                 planned_end_date,
                                                 recruitment_goal_n,
                                                 consort_population_field = NULL,
                                                 population_value = NULL,
                                                 data_cutoff_date = Sys.Date()) {
  subject_level <- tibble::as_tibble(subject_level)

  parse_date <- function(x) {
    if (inherits(x, "Date")) {
      return(x)
    }
    x <- as.character(x)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    # Keep only date component for datetime strings.
    x10 <- substr(x, 1, 10)
    out <- as.Date(x10, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y"))
    missing <- is.na(out) & !is.na(x)
    if (any(missing)) {
      out[missing] <- as.Date(x[missing], tryFormats = c(
        "%Y-%m-%d",
        "%m/%d/%Y",
        "%m/%d/%y",
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d %H:%M",
        "%Y-%m-%dT%H:%M:%S",
        "%Y-%m-%dT%H:%M",
        "%m/%d/%Y %H:%M:%S",
        "%m/%d/%Y %H:%M",
        "%m/%d/%y %H:%M:%S",
        "%m/%d/%y %H:%M"
      ))
    }
    out
  }

  if (!(id_field %in% names(subject_level)) || !(consent_date_field %in% names(subject_level))) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }
  # trial_id_field is retained for API compatibility, but consent status is
  # determined by presence of a parseable consent date.
  if (!(trial_id_field %in% names(subject_level))) {
    trial_id_field <- NULL
  }

  start_date <- parse_date(study_start_date)
  end_date <- parse_date(planned_end_date)
  cutoff_date <- parse_date(data_cutoff_date)
  goal_n <- suppressWarnings(as.numeric(recruitment_goal_n))
  valid_goal <- length(goal_n) == 1 && !is.na(goal_n) && goal_n > 0
  if (is.na(start_date) || is.na(end_date) || !valid_goal) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }
  if (is.na(cutoff_date)) {
    cutoff_date <- Sys.Date()
  }
  if (end_date < start_date) {
    stop(
      sprintf(
        "Invalid recruitment timeline: recruitment_planned_end_date (%s) is earlier than recruitment_study_start_date (%s).",
        as.character(end_date),
        as.character(start_date)
      )
    )
  }

  out <- subject_level |>
    dplyr::mutate(
      .camr_consent_date = parse_date(.data[[consent_date_field]])
    ) |>
    dplyr::filter(!is.na(.data$.camr_consent_date))

  pop_title <- NULL
  if (!is.null(consort_population_field)) {
    consort_population_field <- as.character(consort_population_field)
    if (length(consort_population_field) == 1 &&
      !is.na(consort_population_field) &&
      nzchar(consort_population_field) &&
      consort_population_field %in% names(out)) {
      out <- out |>
        dplyr::mutate(
          .camr_population = as.character(.data[[consort_population_field]]),
          .camr_population = dplyr::if_else(
            is.na(.data$.camr_population) | .data$.camr_population == "",
            "Unspecified",
            .data$.camr_population
          )
        )

      if (!is.null(population_value)) {
        pop_title <- as.character(population_value)[[1]]
        out <- out |> dplyr::filter(.data$.camr_population == .env$pop_title)
      }
    }
  }

  plot_end_date <- end_date
  goal_weeks <- seq.Date(from = start_date, to = plot_end_date, by = "week")
  if (length(goal_weeks) == 0) {
    goal_weeks <- start_date
  }

  observed_end_date <- min(plot_end_date, cutoff_date)
  observed_source <- out |>
    dplyr::filter(.data$.camr_consent_date <= .env$observed_end_date)
  if (!is.null(population_value) && nrow(observed_source) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }

  observed_weeks <- if (observed_end_date >= start_date) {
    seq.Date(from = start_date, to = observed_end_date, by = "week")
  } else {
    start_date
  }
  if (length(observed_weeks) == 0) {
    observed_weeks <- start_date
  }

  observed_cumulative <- vapply(
    observed_weeks,
    function(d) sum(observed_source$.camr_consent_date <= d, na.rm = TRUE),
    numeric(1)
  )

  total_days <- as.numeric(end_date - start_date)
  goal_cumulative <- if (is.na(total_days) || total_days <= 0) {
    rep(goal_n, length(goal_weeks))
  } else {
    progress_days <- as.numeric(goal_weeks - start_date)
    pmin(goal_n, pmax(0, (progress_days / total_days) * goal_n))
  }

  goal_df <- tibble::tibble(week = goal_weeks, n = goal_cumulative)
  observed_df <- tibble::tibble(week = observed_weeks, n = observed_cumulative)

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = goal_df,
      ggplot2::aes(x = .data$week, y = .data$n, color = "Goal Consented"),
      linewidth = 1
    ) +
    ggplot2::geom_step(
      data = observed_df,
      ggplot2::aes(x = .data$week, y = .data$n, color = "Cumulative Consented"),
      linewidth = 1
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Goal Consented" = "#E3B949",
        "Cumulative Consented" = "#9A7600"
      ),
      name = NULL
    ) +
    ggplot2::labs(x = "Week", y = "Number of Participants") +
    ggplot2::scale_x_date(limits = c(start_date, plot_end_date)) +
    ggplot2::theme_minimal()

  if (!is.null(pop_title) && nzchar(pop_title)) {
    p <- p + ggplot2::ggtitle(pop_title)
  }

  p
}
