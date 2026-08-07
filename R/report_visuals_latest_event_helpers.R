#' Build a most-recent-event plot for a status subset
#' @keywords internal
camr_report_latest_event_plot <- function(subject_level,
                                          subject_visit,
                                          id_field,
                                          status_field,
                                          event_label_field,
                                          event_number_field,
                                          status_values,
                                          population_field = NULL,
                                          fill_color,
                                          x_label) {
  subject_level <- tibble::as_tibble(subject_level)
  subject_visit <- tibble::as_tibble(subject_visit)
  status_values <- as.character(status_values %||% character(0))
  if (!is.null(population_field)) {
    population_field <- as.character(population_field)
    if (length(population_field) != 1 || is.na(population_field) || !nzchar(population_field)) {
      population_field <- NULL
    }
  }

  if (!(id_field %in% names(subject_level)) || !(id_field %in% names(subject_visit))) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }
  if (!(status_field %in% names(subject_level)) || !(event_number_field %in% names(subject_visit))) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }

  subject_level[[status_field]] <- as.character(subject_level[[status_field]])

  target_subjects <- subject_level |>
    dplyr::filter(.data[[status_field]] %in% status_values) |>
    dplyr::select(dplyr::any_of(c(id_field, population_field)))
  target_ids <- target_subjects |>
    dplyr::select(dplyr::all_of(id_field))

  pop_col <- ".camr_population"
  pop_map <- NULL
  if (!is.null(population_field) && population_field %in% names(target_subjects)) {
    pop_map <- target_subjects |>
      dplyr::select(dplyr::all_of(c(id_field, population_field))) |>
      dplyr::rename(!!pop_col := dplyr::all_of(population_field))
  }

  # If population isn't available at subject level, derive one value per
  # participant from subject_visit as a fallback.
  if (!is.null(population_field) &&
      nzchar(as.character(population_field)) &&
      is.null(pop_map) &&
      population_field %in% names(subject_visit)) {
    pop_map <- subject_visit |>
      dplyr::select(dplyr::all_of(id_field), dplyr::all_of(population_field)) |>
      dplyr::group_by(.data[[id_field]]) |>
      dplyr::summarise(dplyr::across(dplyr::all_of(population_field), camr_first_value), .groups = "drop") |>
      dplyr::rename(!!pop_col := dplyr::all_of(population_field))
  }

  latest <- subject_visit |>
    dplyr::semi_join(target_ids, by = id_field) |>
    dplyr::filter(!is.na(.data[[event_number_field]])) |>
    dplyr::group_by(.data[[id_field]]) |>
    dplyr::slice_max(.data[[event_number_field]], with_ties = FALSE) |>
    dplyr::ungroup()

  if (!is.null(pop_map)) {
    latest <- latest |>
      dplyr::left_join(pop_map, by = id_field)
  }

  latest_label <- if (event_label_field %in% names(latest)) {
    latest[[event_label_field]]
  } else {
    latest[[event_number_field]]
  }

  plot_data <- tibble::as_tibble(latest) |>
    dplyr::mutate(
      event_label = latest_label,
      event_label = forcats::fct_reorder(
        .data$event_label,
        .data[[event_number_field]]
      )
    )

  has_population <- !is.null(population_field) &&
    nzchar(as.character(population_field)) &&
    pop_col %in% names(plot_data)

  if (has_population) {
    plot_data <- plot_data |>
      dplyr::mutate(population = as.character(.data[[pop_col]])) |>
      dplyr::mutate(population = dplyr::if_else(is.na(.data$population) | .data$population == "", "Unspecified", .data$population)) |>
      dplyr::count(.data$population, .data$event_label)

    return(
      ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$event_label, y = .data$n)) +
        ggplot2::geom_col(fill = fill_color) +
        ggplot2::facet_wrap(~ population) +
        ggplot2::labs(x = x_label, y = "Participants") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
    )
  }

  plot_data <- plot_data |>
    dplyr::count(.data$event_label)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$event_label, y = .data$n)) +
    ggplot2::geom_col(fill = fill_color) +
    ggplot2::labs(x = x_label, y = "Participants") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
}
