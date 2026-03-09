#' Render a CONSORT visual for the report
#'
#' Builds a CONSORT diagram from `camr_consort_data()` output. If enriched
#' arm-level metrics are present and `DiagrammeR` is installed, renders a
#' flowchart. Otherwise renders a simple four-stage diagram, with a ggplot
#' fallback if `DiagrammeR` is unavailable.
#'
#' @param consort_data A data frame returned by \code{\link{camr_consort_data}}.
#' @param layout Character. Diagram orientation: `"portrait"` (default,
#'   top-to-bottom flow) or `"landscape"` (left-to-right flow).
#' @param font_size Optional numeric font size for node labels. If `NULL`,
#'   uses a layout-specific default.
#' @param node_margin Optional Graphviz node margin string (e.g., `"0.2,0.14"`).
#'   If `NULL`, uses a layout-specific default.
#' @param font_family Character font family for node labels.
#' @param node_min_height Optional numeric minimum node height in inches.
#' @param node_min_width Optional numeric minimum node width in inches.
#'
#' @return A `DiagrammeR` widget.
#' @export
camr_report_plot_consort <- function(consort_data,
                                     layout = c("portrait", "landscape"),
                                     font_size = NULL,
                                     node_margin = NULL,
                                     font_family = "Arial",
                                     node_min_height = NULL,
                                     node_min_width = NULL) {
  layout <- match.arg(layout)
  is_landscape <- identical(layout, "landscape")
  graph_rankdir <- if (is_landscape) "LR" else "TB"
  graph_nodesep <- if (is_landscape) "1.2" else "0.65"
  graph_ranksep <- if (is_landscape) "1.0" else "0.95"
  default_fontsize <- if (is_landscape) 16 else 13
  node_fontsize <- as.character(as.integer(round(font_size %||% default_fontsize)))
  simple_nodesep <- if (is_landscape) "1.3" else "0.9"
  simple_ranksep <- if (is_landscape) "0.95" else "0.9"
  simple_fontsize <- as.character(as.integer(round((font_size %||% default_fontsize) + 1)))
  line_width <- if (is_landscape) "1.8" else "1.3"
  edge_width <- if (is_landscape) "1.5" else "1.2"
  arrow_size <- if (is_landscape) "1.0" else "0.8"
  margin_value <- as.character(node_margin %||% if (is_landscape) "0.24,0.16" else "0.12,0.08")
  min_height_value <- as.character(signif(node_min_height %||% if (is_landscape) 1.05 else 0.5, digits = 3))
  min_width_value <- as.character(signif(node_min_width %||% if (is_landscape) 2.5 else 1.0, digits = 3))

  consort_data <- tibble::as_tibble(consort_data)
  consort_n <- stats::setNames(consort_data$n, consort_data$stage)
  screened_n <- as.integer(if ("Screened" %in% names(consort_n)) consort_n[["Screened"]] else NA_integer_)
  consented_n <- as.integer(if ("Consented" %in% names(consort_n)) consort_n[["Consented"]] else NA_integer_)
  active_n <- as.integer(if ("Active/Completed" %in% names(consort_n)) consort_n[["Active/Completed"]] else NA_integer_)
  dropout_n <- as.integer(if ("Dropped Out" %in% names(consort_n)) consort_n[["Dropped Out"]] else NA_integer_)

  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("camr_report_plot_consort() requires the DiagrammeR package.")
  }

  has_metrics <- all(c("metric", "group", "phase", "reason") %in% names(consort_data))
  has_randomized_metric <- has_metrics && any(consort_data$metric == "randomized_group", na.rm = TRUE)
  has_randomized_stage <- any(
    !is.na(consort_data$group) &
      consort_data$group != "" &
      grepl(" randomized$", consort_data$stage),
    na.rm = TRUE
  )
  has_arm_flow <- has_randomized_metric || has_randomized_stage

  dot_escape <- function(x) {
    x <- as.character(x)
    gsub("\"", "\\\\\"", x)
  }

  reason_block_label <- function(title, total_n, reasons_tbl) {
    if (nrow(reasons_tbl) == 0) {
      return(sprintf("%s (n=%s)\\l", title, total_n))
    }
    bullets <- paste0("- ", reasons_tbl$reason, " (n=", reasons_tbl$n, ")\\l", collapse = "")
    paste0(sprintf("%s (n=%s)\\l", title, total_n), bullets)
  }

  get_metric_n <- function(metric_name, group = NULL, phase = NULL) {
    d <- dplyr::filter(consort_data, .data$metric == .env$metric_name)
    if (!is.null(group)) {
      d <- dplyr::filter(d, .data$group == .env$group)
    }
    if (!is.null(phase)) {
      d <- dplyr::filter(d, .data$phase == .env$phase)
    }
    if (nrow(d) == 0) {
      return(NA_integer_)
    }
    as.integer(d$n[[1]])
  }
  screen_fail_n <- if (has_metrics) get_metric_n("screen_fail") else NA_integer_

  if (!has_arm_flow) {
    lbl_screened <- sprintf("Screened (n = %s)", ifelse(is.na(screened_n), "NA", screened_n))
    lbl_consented <- sprintf("Consented (n = %s)", ifelse(is.na(consented_n), "NA", consented_n))
    lbl_active <- sprintf("Active / Completed (n = %s)", ifelse(is.na(active_n), "NA", active_n))
    lbl_dropout <- sprintf("Dropped Out (n = %s)", ifelse(is.na(dropout_n), "NA", dropout_n))
    has_screen_fail_node <- !is.na(screen_fail_n)
    lbl_screen_fail <- sprintf("Screen Fail (n = %s)", screen_fail_n)

    dot <- paste(
      c(
        "digraph consort {",
        sprintf(
          "  graph [layout = dot, rankdir = %s, nodesep = %s, ranksep = %s, splines = ortho, bgcolor = \"white\"]",
          graph_rankdir, simple_nodesep, simple_ranksep
        ),
        sprintf(
          "  node [shape = box, style = \"solid\", fillcolor = \"white\", color = \"black\", penwidth = %s, margin = \"%s\", height = %s, width = %s, fontname = \"%s\", fontsize = %s]",
          line_width, margin_value, min_height_value, min_width_value, font_family, simple_fontsize
        ),
        sprintf("  edge [color = \"black\", penwidth = %s, arrowsize = %s]", edge_width, arrow_size),
        sprintf("  screened [label = \"%s\"]", dot_escape(lbl_screened)),
        if (has_screen_fail_node) sprintf("  screen_fail [label = \"%s\"]", dot_escape(lbl_screen_fail)) else NULL,
        sprintf("  consented [label = \"%s\"]", dot_escape(lbl_consented)),
        sprintf("  active [label = \"%s\"]", dot_escape(lbl_active)),
        sprintf("  dropout [label = \"%s\"]", dot_escape(lbl_dropout)),
        "  screened -> consented",
        if (has_screen_fail_node) "  screened -> screen_fail" else NULL,
        "  consented -> active",
        "  consented -> dropout",
        "}"
      ),
      collapse = "\n"
    )
    return(DiagrammeR::grViz(dot))
  }

  randomized_n <- get_metric_n("randomized")
  group_tbl <- consort_data |>
    dplyr::filter(!is.na(.data$group), .data$group != "") |>
    dplyr::filter(.data$metric == "randomized_group" | grepl(" randomized$", .data$stage)) |>
    dplyr::arrange(.data$group)

  preconsent_tbl <- consort_data |>
    dplyr::filter(.data$metric == "preconsent_exclusion_reason", !is.na(.data$reason), .data$n > 0)
  prerand_tbl <- consort_data |>
    dplyr::filter(.data$metric == "prerandomization_exclusion_reason", !is.na(.data$reason), .data$n > 0)
  population_tbl <- consort_data |>
    dplyr::filter(.data$metric == "population_group", !is.na(.data$group), .data$group != "", .data$n > 0) |>
    dplyr::arrange(dplyr::desc(.data$n), .data$group)
  population_rand_tbl <- consort_data |>
    dplyr::filter(.data$metric == "population_randomized", !is.na(.data$group), .data$group != "")
  population_eligible_tbl <- consort_data |>
    dplyr::filter(.data$metric == "population_randomization_eligible", !is.na(.data$group), .data$group != "")
  population_prerand_attr_tbl <- consort_data |>
    dplyr::filter(
      .data$metric == "population_prerandomization_attrition_reason",
      !is.na(.data$group),
      .data$group != "",
      !is.na(.data$reason),
      .data$n > 0
    )
  population_int_tbl <- consort_data |>
    dplyr::filter(.data$metric == "population_completed_intervention", !is.na(.data$group), .data$group != "")
  population_study_tbl <- consort_data |>
    dplyr::filter(.data$metric == "population_completed_study", !is.na(.data$group), .data$group != "")

  node_lines <- c(
    sprintf("  screened [label = \"%s\"]", dot_escape(sprintf("Screened (n = %s)", ifelse(is.na(screened_n), "NA", screened_n)))),
    sprintf("  consented [label = \"%s\"]", dot_escape(sprintf("Consented (n = %s)", ifelse(is.na(consented_n), "NA", consented_n)))),
    sprintf("  randomized [label = \"%s\"]", dot_escape(sprintf("Randomized (n = %s)", ifelse(is.na(randomized_n), "NA", randomized_n))))
  )
  edge_lines <- character(0)
  rank_lines <- character(0)

  if (nrow(preconsent_tbl) > 0 || !is.na(screen_fail_n)) {
    pre_total <- if (!is.na(screen_fail_n)) screen_fail_n else sum(preconsent_tbl$n)
    pre_label <- if (nrow(preconsent_tbl) > 0) {
      reason_block_label("Screen Fail", pre_total, preconsent_tbl)
    } else {
      sprintf("Screen Fail (n=%s)\\l", pre_total)
    }
    node_lines <- c(
      node_lines,
      sprintf("  excluded_pre [label = \"%s\"]", dot_escape(pre_label))
    )
    node_lines <- c(
      node_lines,
      "  screened_split [label = \"\", width = 0.01, height = 0.01, shape = point]"
    )
    edge_lines <- c(
      edge_lines,
      "  screened -> screened_split [arrowhead = none, weight = 40]",
      "  screened_split -> consented [weight = 40]",
      "  screened_split -> excluded_pre [weight = 20]"
    )
    rank_lines <- c(rank_lines, "  { rank = same; screened_split; excluded_pre }")
  } else {
    edge_lines <- c(edge_lines, "  screened -> consented")
  }

  if (nrow(prerand_tbl) > 0) {
    pre_rand_total <- sum(prerand_tbl$n)
    prerand_label <- reason_block_label("Exclude Post-Consent", pre_rand_total, prerand_tbl)
    node_lines <- c(
      node_lines,
      sprintf("  excluded_prerand [label = \"%s\"]", dot_escape(prerand_label))
    )
    edge_lines <- c(edge_lines, "  consented -> excluded_prerand")
  }

  if (nrow(population_tbl) > 0) {
    for (i in seq_len(nrow(population_tbl))) {
      p <- as.character(population_tbl$group[[i]])
      p_node <- paste0("pop_", i)
      p_n <- as.integer(population_tbl$n[[i]])
      p_rand <- population_rand_tbl |>
        dplyr::filter(.data$group == p)
      p_rand_n <- if (nrow(p_rand) > 0) as.integer(p_rand$n[[1]]) else 0L
      p_eligible <- population_eligible_tbl |>
        dplyr::filter(.data$group == p)
      p_is_eligible <- if (nrow(p_eligible) > 0) {
        as.integer(p_eligible$n[[1]]) > 0
      } else {
        p_rand_n > 0
      }
      p_int <- population_int_tbl |>
        dplyr::filter(.data$group == p)
      p_int_n <- if (nrow(p_int) > 0) as.integer(p_int$n[[1]]) else NA_integer_
      p_study <- population_study_tbl |>
        dplyr::filter(.data$group == p)
      p_study_n <- if (nrow(p_study) > 0) as.integer(p_study$n[[1]]) else NA_integer_
      p_int_node <- paste0("pop_int_", i)
      p_study_node <- paste0("pop_study_", i)
      p_attr_node <- paste0("pop_attr_", i)
      p_split_node <- paste0("pop_split_", i)
      p_attr <- population_prerand_attr_tbl |>
        dplyr::filter(.data$group == p)

      node_lines <- c(
        node_lines,
        sprintf("  %s [label = \"%s\"]", p_node, dot_escape(sprintf("%s (n = %s)", p, p_n)))
      )
      edge_lines <- c(edge_lines, sprintf("  consented -> %s", p_node))
      if (p_is_eligible) {
        node_lines <- c(
          node_lines,
          sprintf("  %s [label = \"\", width = 0.01, height = 0.01, shape = point]", p_split_node)
        )
        edge_lines <- c(
          edge_lines,
          sprintf("  %s -> %s [arrowhead = none, weight = 40]", p_node, p_split_node),
          sprintf("  %s -> randomized [weight = 40]", p_split_node)
        )
        if (nrow(p_attr) > 0) {
          attr_label <- reason_block_label("Attrition", sum(p_attr$n), p_attr)
          node_lines <- c(
            node_lines,
            sprintf("  %s [label = \"%s\"]", p_attr_node, dot_escape(attr_label))
          )
          edge_lines <- c(
            edge_lines,
            sprintf("  %s -> %s [weight = 20]", p_split_node, p_attr_node)
          )
          rank_lines <- c(rank_lines, sprintf("  { rank = same; %s; %s }", p_split_node, p_attr_node))
        }
      }
      # Show population-level completion only for non-randomized branches to avoid
      # duplicating the randomized-arm completion path.
      if (!p_is_eligible) {
        has_int <- !is.na(p_int_n)
        has_study <- !is.na(p_study_n)
        main_first <- if (has_int) p_int_node else if (has_study) p_study_node else NA_character_
        p_nonrand_split <- paste0("pop_nonrand_split_", i)

        if (has_int) {
          node_lines <- c(
            node_lines,
            sprintf("  %s [label = \"%s\"]", p_int_node, dot_escape(sprintf("Completed intervention (n = %s)", p_int_n)))
          )
        }
        if (has_study) {
          node_lines <- c(
            node_lines,
            sprintf("  %s [label = \"%s\"]", p_study_node, dot_escape(sprintf("Completed study (n = %s)", p_study_n)))
          )
        }
        if (has_int && has_study) {
          edge_lines <- c(edge_lines, sprintf("  %s -> %s", p_int_node, p_study_node))
        }

        if (nrow(p_attr) > 0) {
          p_attr_label <- reason_block_label("Attrition", sum(p_attr$n), p_attr)
          node_lines <- c(
            node_lines,
            sprintf("  %s [label = \"%s\"]", p_attr_node, dot_escape(p_attr_label))
          )
          if (!is.na(main_first)) {
            node_lines <- c(
              node_lines,
              sprintf("  %s [label = \"\", width = 0.01, height = 0.01, shape = point]", p_nonrand_split)
            )
            edge_lines <- c(
              edge_lines,
              sprintf("  %s -> %s [arrowhead = none, weight = 40]", p_node, p_nonrand_split),
              sprintf("  %s -> %s [weight = 40]", p_nonrand_split, main_first),
              sprintf("  %s -> %s [weight = 20]", p_nonrand_split, p_attr_node)
            )
            rank_lines <- c(rank_lines, sprintf("  { rank = same; %s; %s }", p_nonrand_split, p_attr_node))
          } else {
            edge_lines <- c(edge_lines, sprintf("  %s -> %s", p_node, p_attr_node))
          }
        } else if (!is.na(main_first)) {
          edge_lines <- c(edge_lines, sprintf("  %s -> %s", p_node, main_first))
        }
      }
    }
  } else {
    edge_lines <- c(edge_lines, "  consented -> randomized")
  }

  group_nodes <- character(0)
  for (i in seq_len(nrow(group_tbl))) {
    g <- as.character(group_tbl$group[[i]])
    g_node <- paste0("group_", i)
    group_nodes <- c(group_nodes, g_node)
    int_node <- paste0("int_", i)
    study_node <- paste0("study_", i)
    attr_int_node <- paste0("attr_int_", i)
    attr_study_node <- paste0("attr_study_", i)
    g_int_split <- paste0("group_int_split_", i)
    g_study_split <- paste0("group_study_split_", i)

    n_group <- as.integer(group_tbl$n[[i]])
    n_int <- get_metric_n("completed_intervention_group", group = g, phase = "intervention")
    n_study <- get_metric_n("completed_study_group", group = g, phase = "study")

    attr_int <- consort_data |>
      dplyr::filter(.data$metric == "attrition_group_reason", .data$group == g, .data$phase == "intervention", .data$n > 0)
    attr_study <- consort_data |>
      dplyr::filter(.data$metric == "attrition_group_reason", .data$group == g, .data$phase == "study", .data$n > 0)

    node_lines <- c(
      node_lines,
      sprintf("  %s [label = \"%s\"]", g_node, dot_escape(sprintf("%s (n = %s)", g, n_group)))
    )
    edge_lines <- c(edge_lines, sprintf("  randomized -> %s", g_node))

    if (!is.na(n_int)) {
      node_lines <- c(
        node_lines,
        sprintf("  %s [label = \"%s\"]", int_node, dot_escape(sprintf("Completed intervention (n = %s)", n_int)))
      )
    }

    if (nrow(attr_int) > 0) {
      attr_int_label <- reason_block_label("Ended after eligible", sum(attr_int$n), attr_int)
      node_lines <- c(
        node_lines,
        sprintf("  %s [label = \"%s\", width = 4.2]", attr_int_node, dot_escape(attr_int_label))
      )
      if (!is.na(n_int)) {
        node_lines <- c(
          node_lines,
          sprintf("  %s [label = \"\", width = 0.01, height = 0.01, shape = point]", g_int_split)
        )
        edge_lines <- c(
          edge_lines,
          sprintf("  %s -> %s [arrowhead = none, weight = 40]", g_node, g_int_split),
          sprintf("  %s -> %s [weight = 40]", g_int_split, int_node),
          sprintf("  %s -> %s [weight = 20]", g_int_split, attr_int_node)
        )
        rank_lines <- c(rank_lines, sprintf("  { rank = same; %s; %s }", g_int_split, attr_int_node))
      } else {
        edge_lines <- c(edge_lines, sprintf("  %s -> %s", g_node, attr_int_node))
      }
    } else if (!is.na(n_int)) {
      edge_lines <- c(edge_lines, sprintf("  %s -> %s", g_node, int_node))
    }

    if (!is.na(n_study)) {
      parent <- if (!is.na(n_int)) int_node else g_node
      node_lines <- c(
        node_lines,
        sprintf("  %s [label = \"%s\"]", study_node, dot_escape(sprintf("Completed study (n = %s)", n_study)))
      )
    }

    if (nrow(attr_study) > 0) {
      parent <- if (!is.na(n_int)) int_node else g_node
      attr_study_label <- reason_block_label("Active", sum(attr_study$n), attr_study)
      node_lines <- c(
        node_lines,
        sprintf("  %s [label = \"%s\", width = 3.6]", attr_study_node, dot_escape(attr_study_label))
      )
      if (!is.na(n_study)) {
        node_lines <- c(
          node_lines,
          sprintf("  %s [label = \"\", width = 0.01, height = 0.01, shape = point]", g_study_split)
        )
        edge_lines <- c(
          edge_lines,
          sprintf("  %s -> %s [arrowhead = none, weight = 40]", parent, g_study_split),
          sprintf("  %s -> %s [weight = 40]", g_study_split, study_node),
          sprintf("  %s -> %s [weight = 20]", g_study_split, attr_study_node)
        )
        rank_lines <- c(rank_lines, sprintf("  { rank = same; %s; %s }", g_study_split, attr_study_node))
      } else {
        edge_lines <- c(edge_lines, sprintf("  %s -> %s", parent, attr_study_node))
      }
    } else if (!is.na(n_study)) {
      parent <- if (!is.na(n_int)) int_node else g_node
      edge_lines <- c(edge_lines, sprintf("  %s -> %s", parent, study_node))
    }
  }

  if (length(group_nodes) > 1) {
    edge_lines <- c(edge_lines, sprintf("  { rank = same; %s }", paste(group_nodes, collapse = "; ")))
    for (i in seq_len(length(group_nodes) - 1)) {
      edge_lines <- c(
        edge_lines,
        sprintf("  %s -> %s [style = invis, weight = 100]", group_nodes[[i]], group_nodes[[i + 1]])
      )
    }
  }

  dot <- paste(
    c(
      "digraph consort {",
      sprintf(
        "  graph [layout = dot, rankdir = %s, nodesep = %s, ranksep = %s, splines = ortho, bgcolor = \"white\"]",
        graph_rankdir, graph_nodesep, graph_ranksep
      ),
      sprintf(
        "  node [shape = box, style = \"solid\", fillcolor = \"white\", color = \"black\", penwidth = %s, margin = \"%s\", height = %s, width = %s, fontname = \"%s\", fontsize = %s]",
        line_width, margin_value, min_height_value, min_width_value, font_family, node_fontsize
      ),
      sprintf("  edge [color = \"black\", penwidth = %s, arrowsize = %s]", edge_width, arrow_size),
      node_lines,
      edge_lines,
      rank_lines,
      "}"
    ),
    collapse = "\n"
  )

  DiagrammeR::grViz(dot)
}

#' Render a slide-friendly CONSORT visual
#'
#' Generates the same CONSORT diagram as \code{\link{camr_report_plot_consort}},
#' but in a landscape layout that is better suited for presentation slides.
#'
#' @param consort_data A data frame returned by \code{\link{camr_consort_data}}.
#' @param font_size Optional numeric font size override for slide output.
#' @param node_margin Optional Graphviz node margin string override.
#' @param font_family Character font family for slide output. Defaults to Arial
#'   to reduce text distortion in presentation software.
#' @param node_min_height Optional numeric minimum node height in inches.
#' @param node_min_width Optional numeric minimum node width in inches.
#'
#' @return A `DiagrammeR` widget.
#' @export
camr_report_plot_consort_slide <- function(consort_data,
                                           font_size = 20,
                                           node_margin = "0.34,0.24",
                                           font_family = "Arial",
                                           node_min_height = 1.15,
                                           node_min_width = 2.7) {
  camr_report_plot_consort(
    consort_data = consort_data,
    layout = "landscape",
    font_size = font_size,
    node_margin = node_margin,
    font_family = font_family,
    node_min_height = node_min_height,
    node_min_width = node_min_width
  )
}
