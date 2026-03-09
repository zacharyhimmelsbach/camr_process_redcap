#' Read TLFB JSON files into a long table
#'
#' @param paths Character vector of JSON file paths, or a directory path.
#' @param recursive Logical. Whether to recurse when `paths` is a directory.
#'
#' @return A tibble with one row per assessed TLFB day and event.
#' @export
camr_read_tlfb_files <- function(paths, recursive = TRUE) {
  if (is.null(paths) || !length(paths)) {
    return(tibble::tibble())
  }

  paths <- as.character(paths)
  if (length(paths) == 1 && dir.exists(paths)) {
    paths <- list.files(paths, pattern = "[.]json$", recursive = recursive, full.names = TRUE)
  }
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    return(tibble::tibble())
  }

  out <- purrr::map_dfr(paths, function(path) {
    obj <- tryCatch(jsonlite::fromJSON(path, simplifyDataFrame = TRUE), error = function(e) NULL)
    if (is.null(obj)) {
      return(tibble::tibble())
    }

    record <- as.character(obj$record %||% NA_character_)
    subject <- as.character(obj$subject %||% NA_character_)
    event_name <- as.character(obj$event %||% NA_character_)
    start_date <- as.Date(as.character(obj$start %||% NA_character_))
    end_date <- as.Date(as.character(obj$end %||% NA_character_))
    staff <- as.character(obj$staff %||% NA_character_)
    completed <- as.character(obj$datetime %||% NA_character_)
    app_version <- as.character(obj$appversion %||% NA_character_)

    days <- if (!is.na(start_date) && !is.na(end_date) && end_date >= start_date) {
      tibble::tibble(tlfb_day = seq.Date(start_date, end_date, by = "day"))
    } else {
      tibble::tibble(tlfb_day = as.Date(character(0)))
    }

    events <- tibble::as_tibble(obj$events %||% data.frame())
    if (!nrow(events)) {
      events <- tibble::tibble()
    } else {
      col_or_na <- function(df, nm, default = NA_character_) {
        if (nm %in% names(df)) {
          df[[nm]]
        } else {
          rep(default, nrow(df))
        }
      }
      names(events) <- gsub("^_", "", names(events))
      events <- tibble::tibble(
        tlfb_day = as.Date(substr(as.character(col_or_na(events, "date")), 1, 10)),
        event_type = as.character(col_or_na(events, "type")),
        event_category = as.character(col_or_na(events, "category")),
        event_substance = as.character(col_or_na(events, "substance")),
        event_method = as.character(col_or_na(events, "method")),
        event_method_type = as.character(col_or_na(events, "methodType")),
        event_occasions = as.numeric(col_or_na(events, "times")),
        event_amount = as.numeric(col_or_na(events, "amount")),
        event_units = as.character(col_or_na(events, "units"))
      )
    }

    assessed <- if (nrow(days)) {
      dplyr::left_join(days, events, by = "tlfb_day")
    } else if (nrow(events)) {
      events
    } else {
      tibble::tibble()
    }

    if (!nrow(assessed)) {
      return(tibble::tibble())
    }

    assessed |>
      dplyr::mutate(
        record_id = record,
        tlfb_subject = subject,
        redcap_event_name = event_name,
        tlfb_start = start_date,
        tlfb_end = end_date,
        tlfb_staff = staff,
        tlfb_completed = completed,
        tlfb_app_version = app_version,
        tlfb_source_file = basename(path),
        .before = 1
      )
  })

  tibble::as_tibble(out)
}

#' Build measurement-level TLFB daily summaries
#'
#' @param tlfb_raw A tibble from \code{\link{camr_read_tlfb_files}}.
#' @param redcap_data A REDCap data tibble.
#' @param config A config list with `id_field` and `event_name_field`.
#'
#' @return A tibble with one row per subject-event-day and substance summaries.
#' @export
camr_ml_tlfb <- function(tlfb_raw, redcap_data, config, legacy_names = FALSE) {
  tlfb_raw <- tibble::as_tibble(tlfb_raw)
  if (!nrow(tlfb_raw)) {
    return(tibble::tibble())
  }

  id_field <- config$id_field %||% "record_id"
  event_field <- config$event_name_field %||% "redcap_event_name"

  if (!(id_field %in% names(tlfb_raw))) {
    if ("record_id" %in% names(tlfb_raw)) {
      tlfb_raw[[id_field]] <- tlfb_raw$record_id
    } else {
      stop("TLFB raw data must include record_id.")
    }
  }
  if (!(event_field %in% names(tlfb_raw))) {
    if ("redcap_event_name" %in% names(tlfb_raw)) {
      tlfb_raw[[event_field]] <- tlfb_raw$redcap_event_name
    } else {
      stop("TLFB raw data must include redcap_event_name.")
    }
  }

  map_substance <- function(cat, sub) {
    cat <- ifelse(is.na(cat), "", as.character(cat))
    sub <- ifelse(is.na(sub), "", as.character(sub))
    cat <- tolower(cat)
    if (cat == "nic" && sub == "E-Cigarettes") return("NIC_ENDS")
    if (cat == "nic" && sub == "Smoked Tobacco") return("NIC_CTP")
    if (cat == "nic" && sub == "Smokeless Tobacco") return("NIC_SLT")
    if (cat == "nic" && grepl("Hookah|Other", sub)) return("NIC_Other")
    if (!nzchar(cat)) return(NA_character_)
    toupper(cat)
  }

  substances <- c("ETOH", "CB", "NIC_ENDS", "NIC_CTP", "NIC_SLT", "NIC_Other", "STIM", "COC", "OPI", "HALL", "DISS", "INH", "SDH", "MISC")
  keyed <- tlfb_raw |>
    dplyr::mutate(
      tlfb_subject = if ("tlfb_subject" %in% names(tlfb_raw)) .data$tlfb_subject else as.character(.data[[id_field]]),
      tlfb_start = if ("tlfb_start" %in% names(tlfb_raw)) .data$tlfb_start else as.Date(NA),
      tlfb_end = if ("tlfb_end" %in% names(tlfb_raw)) .data$tlfb_end else as.Date(NA),
      tlfb_staff = if ("tlfb_staff" %in% names(tlfb_raw)) .data$tlfb_staff else NA_character_,
      tlfb_completed = if ("tlfb_completed" %in% names(tlfb_raw)) .data$tlfb_completed else NA_character_,
      tlfb_app_version = if ("tlfb_app_version" %in% names(tlfb_raw)) .data$tlfb_app_version else NA_character_,
      tlfb_substance_category = purrr::map2_chr(.data$event_category, .data$event_substance, map_substance),
      tlfb_is_use = .data$event_type == "use",
      tlfb_visit_collected = stringr::str_to_title(stringr::str_replace_all(stringr::str_remove(.data[[event_field]], "_arm_\\d+$"), "_", " "))
    )

  base <- keyed |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_field, event_field, "tlfb_day")))) |>
    dplyr::summarise(
      tlfb_data_collected = any(!is.na(.data$event_type)),
      tlfb_subject = camr_first_value(.data$tlfb_subject),
      tlfb_visit_collected = camr_first_value(.data$tlfb_visit_collected),
      tlfb_start = camr_first_value(.data$tlfb_start),
      tlfb_end = camr_first_value(.data$tlfb_end),
      tlfb_interval_days = as.integer(camr_first_value(as.integer(.data$tlfb_end - .data$tlfb_start)) + 1L),
      tlfb_staff = camr_first_value(.data$tlfb_staff),
      tlfb_completed = camr_first_value(.data$tlfb_completed),
      tlfb_app_version = suppressWarnings(as.integer(camr_first_value(.data$tlfb_app_version))),
      .groups = "drop"
    )

  for (substance in substances) {
    sub_rows <- keyed |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(id_field, event_field, "tlfb_day")))) |>
      dplyr::summarise(
        use_any = any(.data$tlfb_is_use & .data$tlfb_substance_category == substance, na.rm = TRUE),
        amount_total = sum(dplyr::coalesce(.data$event_occasions, 0) * dplyr::coalesce(.data$event_amount, 0) * (.data$tlfb_is_use & .data$tlfb_substance_category == substance), na.rm = TRUE),
        occ_total = sum(dplyr::coalesce(.data$event_occasions, 0) * (.data$tlfb_is_use & .data$tlfb_substance_category == substance), na.rm = TRUE),
        .groups = "drop"
      )

    base <- dplyr::left_join(base, sub_rows, by = c(id_field, event_field, "tlfb_day"))
    use_col <- paste0("tlfb_", tolower(substance), "_use")
    if (substance %in% c("ETOH", "NIC_CTP")) {
      qty_col <- if (substance == "ETOH") "tlfb_etoh_dpd" else "tlfb_nic_ctp_cpd"
      base <- base |>
        dplyr::mutate(
          !!use_col := dplyr::if_else(!.data$tlfb_data_collected, NA, .data$use_any),
          !!qty_col := dplyr::if_else(!.data$tlfb_data_collected, NA_real_, dplyr::if_else(.data$use_any, .data$amount_total, 0))
        )
    } else {
      qty_col <- paste0("tlfb_", tolower(substance), "_occasions")
      base <- base |>
        dplyr::mutate(
          !!use_col := dplyr::if_else(!.data$tlfb_data_collected, NA, .data$use_any),
          !!qty_col := dplyr::if_else(!.data$tlfb_data_collected, NA_real_, dplyr::if_else(.data$use_any, .data$occ_total, 0))
        )
    }
    base$use_any <- NULL
    base$amount_total <- NULL
    base$occ_total <- NULL
  }

  base <- base |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(id_field, event_field, "tlfb_day"))))

  if (!isTRUE(legacy_names)) {
    return(base)
  }

  out <- base |>
    dplyr::mutate(
      IDX.CHR.Subject = dplyr::coalesce(.data$tlfb_subject, as.character(.data[[id_field]])),
      IDX.CHR.Visit = .data$tlfb_visit_collected,
      IDX.DAT.TLFB.Day = .data$tlfb_day,
      QCC.CHR.VisitCollected = .data$tlfb_visit_collected,
      QCC.LGL.VisitDay = NA,
      QCC.LGL.TLFB.DataCollected = .data$tlfb_data_collected,
      QCC.DAT.TLFB.Start = .data$tlfb_start,
      QCC.DAT.TLFB.End = .data$tlfb_end,
      QCC.INT.TLFB.IntervalDays = .data$tlfb_interval_days,
      QCC.CHR.TLFB.Staff = .data$tlfb_staff,
      QCC.DTM.TLFB.Completed = .data$tlfb_completed,
      QCC.INT.TLFB.AppVersion = .data$tlfb_app_version
    )

  map_cols <- c(
    ETOH = "INV.DBL.TLFB.ETOH.DPD",
    CB = "INV.DBL.TLFB.CB.Occasions",
    NIC_ENDS = "INV.DBL.TLFB.NIC_ENDS.Occasions",
    NIC_CTP = "INV.DBL.TLFB.NIC_CTP.CPD",
    NIC_SLT = "INV.DBL.TLFB.NIC_SLT.Occasions",
    NIC_Other = "INV.DBL.TLFB.NIC_Other.Occasions",
    STIM = "INV.DBL.TLFB.STIM.Occasions",
    COC = "INV.DBL.TLFB.COC.Occasions",
    OPI = "INV.DBL.TLFB.OPI.Occasions",
    HALL = "INV.DBL.TLFB.HALL.Occasions",
    DISS = "INV.DBL.TLFB.DISS.Occasions",
    INH = "INV.DBL.TLFB.INH.Occasions",
    SDH = "INV.DBL.TLFB.SDH.Occasions",
    MISC = "INV.DBL.TLFB.MISC.Occasions"
  )
  for (sub in names(map_cols)) {
    low <- tolower(sub)
    use_from <- paste0("tlfb_", low, "_use")
    qty_from <- if (sub == "ETOH") "tlfb_etoh_dpd" else if (sub == "NIC_CTP") "tlfb_nic_ctp_cpd" else paste0("tlfb_", low, "_occasions")
    use_to <- paste0("INV.LGL.TLFB.", sub, ".Use")
    qty_to <- map_cols[[sub]]
    out[[use_to]] <- out[[use_from]]
    out[[qty_to]] <- out[[qty_from]]
  }

  fixed_cols <- c(
    "IDX.CHR.Subject",
    "IDX.CHR.Visit",
    "IDX.DAT.TLFB.Day",
    "QCC.CHR.VisitCollected",
    "QCC.LGL.VisitDay",
    "QCC.LGL.TLFB.DataCollected",
    "QCC.DAT.TLFB.Start",
    "QCC.DAT.TLFB.End",
    "QCC.INT.TLFB.IntervalDays",
    "QCC.CHR.TLFB.Staff",
    "QCC.DTM.TLFB.Completed",
    "QCC.INT.TLFB.AppVersion"
  )
  out |>
    dplyr::select(
      dplyr::all_of(c(id_field, event_field)),
      dplyr::all_of(fixed_cols[1:3]),
      dplyr::starts_with("INV.LGL.TLFB."),
      dplyr::starts_with("INV.DBL.TLFB."),
      dplyr::all_of(fixed_cols[4:length(fixed_cols)])
    )
}

#' Build visit-level TLFB summaries
#'
#' @param tlfb_ml A tibble from \code{\link{camr_ml_tlfb}}.
#' @param config A config list with `id_field` and `event_name_field`.
#'
#' @return A tibble with one row per subject-event TLFB summary.
#' @export
camr_vl_tlfb <- function(tlfb_ml, config, legacy_names = FALSE) {
  tlfb_ml <- tibble::as_tibble(tlfb_ml)
  if (!nrow(tlfb_ml)) {
    return(tibble::tibble())
  }

  id_field <- config$id_field %||% "record_id"
  event_field <- config$event_name_field %||% "redcap_event_name"
  if (!(id_field %in% names(tlfb_ml)) || !(event_field %in% names(tlfb_ml))) {
    if (isTRUE(legacy_names) && all(c("IDX.CHR.Subject", "IDX.CHR.Visit") %in% names(tlfb_ml))) {
      id_field <- "IDX.CHR.Subject"
      event_field <- "IDX.CHR.Visit"
    } else {
      stop("tlfb_ml is missing ID/event fields required for aggregation.")
    }
  }
  prefix_use <- if (legacy_names) "INV.LGL.TLFB." else "tlfb_"
  prefix_qty <- if (legacy_names) "INV.DBL.TLFB." else "tlfb_"
  days_col <- if (legacy_names) "QCC.INT.TLFB.Days" else "tlfb_days"
  missing_col <- if (legacy_names) "QCC.INT.TLFB.MissingDays" else "tlfb_missing_days"

  substances <- c("etoh", "cb", "nic_ends", "nic_ctp", "nic_slt", "nic_other", "stim", "coc", "opi", "hall", "diss", "inh", "sdh", "misc")
  legacy_suffix <- c(
    etoh = "ETOH",
    cb = "CB",
    nic_ends = "NIC_ENDS",
    nic_ctp = "NIC_CTP",
    nic_slt = "NIC_SLT",
    nic_other = "NIC_Other",
    stim = "STIM",
    coc = "COC",
    opi = "OPI",
    hall = "HALL",
    diss = "DISS",
    inh = "INH",
    sdh = "SDH",
    misc = "MISC"
  )

  out <- tlfb_ml |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_field, event_field)))) |>
    dplyr::summarise(
      tlfb_days = dplyr::n(),
      tlfb_missing_days = sum(!.data[[if (legacy_names) "QCC.LGL.TLFB.DataCollected" else "tlfb_data_collected"]] %in% TRUE, na.rm = TRUE),
      IDX.CHR.Subject = if ("IDX.CHR.Subject" %in% names(tlfb_ml)) camr_first_value(.data$`IDX.CHR.Subject`) else as.character(camr_first_value(.data[[id_field]])),
      IDX.CHR.Visit = if ("IDX.CHR.Visit" %in% names(tlfb_ml)) camr_first_value(.data$`IDX.CHR.Visit`) else as.character(camr_first_value(.data[[event_field]])),
      .groups = "drop"
    )

  for (substance in substances) {
    use_col <- if (legacy_names) {
      paste0(prefix_use, legacy_suffix[[substance]], ".Use")
    } else {
      paste0(prefix_use, substance, "_use")
    }
    qty_col <- if (legacy_names) {
      if (substance == "etoh") "INV.DBL.TLFB.ETOH.DPD" else if (substance == "nic_ctp") "INV.DBL.TLFB.NIC_CTP.CPD" else paste0(prefix_qty, legacy_suffix[[substance]], ".Occasions")
    } else {
      if (substance == "etoh") "tlfb_etoh_dpd" else if (substance == "nic_ctp") "tlfb_nic_ctp_cpd" else paste0(prefix_qty, substance, "_occasions")
    }
    dpw_col <- if (legacy_names) paste0(prefix_qty, legacy_suffix[[substance]], ".DPW") else paste0(prefix_qty, substance, "_dpw")

    sub <- tlfb_ml |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(id_field, event_field)))) |>
      dplyr::summarise(
        use_days = sum(.data[[use_col]] %in% TRUE, na.rm = TRUE),
        use_any = use_days > 0,
        dpw = use_days / dplyr::n() * 7,
        qty_mean = if (substance == "etoh") mean(.data[[qty_col]][.data[[qty_col]] > 0], na.rm = TRUE) else mean(.data[[qty_col]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        qty_mean = dplyr::if_else(is.nan(.data$qty_mean), NA_real_, .data$qty_mean)
      )

    out <- dplyr::left_join(out, sub, by = c(id_field, event_field))
    out[[use_col]] <- out$use_any
    out[[dpw_col]] <- dplyr::if_else(out$tlfb_missing_days > 0, NA_real_, out$dpw)
    if (substance == "etoh") {
      out[[if (legacy_names) "INV.DBL.TLFB.ETOH.DPDD" else "tlfb_etoh_dpdd"]] <- dplyr::if_else(out$tlfb_missing_days > 0, NA_real_, dplyr::if_else(out[[use_col]], out$qty_mean, 0))
    } else if (substance == "nic_ctp") {
      out[[if (legacy_names) "INV.DBL.TLFB.NIC_CTP.CPD" else "tlfb_nic_ctp_cpd"]] <- dplyr::if_else(out$tlfb_missing_days > 0, NA_real_, dplyr::if_else(out[[use_col]], out$qty_mean, 0))
    }
    out[[use_col]] <- dplyr::if_else(out$tlfb_missing_days > 0 & !out[[use_col]], NA, out[[use_col]])
    out$use_days <- NULL
    out$use_any <- NULL
    out$dpw <- NULL
    out$qty_mean <- NULL
  }

  if (legacy_names) {
    out[[days_col]] <- out$tlfb_days
    out[[missing_col]] <- out$tlfb_missing_days
    out <- out |>
      dplyr::select(
        dplyr::all_of(c(id_field, event_field)),
        dplyr::all_of(c("IDX.CHR.Subject", "IDX.CHR.Visit")),
        dplyr::starts_with("INV.LGL.TLFB."),
        dplyr::starts_with("INV.DBL.TLFB."),
        dplyr::all_of(c(days_col, missing_col))
      )
  }

  out
}
