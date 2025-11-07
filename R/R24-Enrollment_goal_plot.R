#' Plot enrollment actuals vs. goal (consented & randomized)
#'
#' @description
#' Generates a line plot of cumulative **Actual Consented** and **Actual Randomized**
#' over time, with a straight-line **Goal Consented** trajectory between
#' `start_date_for_goals` and `goal_finish`. Optionally overlays a **no-cost
#' extension goal line** that ends `extension_years` after `goal_finish`.
#'
#' @param report_dir Directory to save the output image (created if missing).
#' @param df_sl_status Data frame of subject-level status (defaults to `tar_read_raw("df_sl_status")`).
#' @param df_vl_visits Data frame of visit-level rows (defaults to `tar_read_raw("df_vl_visits")`).
#' @param df_sl_randomization Data frame of subject-level randomization (defaults to `tar_read_raw("df_sl_randomization")`).
#' @param start_date_actuals Date for beginning the actuals timeline (used to build monthly grid).
#' @param date_var Character. Baseline/consent date variable in `df_vl_visits`.
#' @param status_var Character. Status variable in `df_sl_status`.
#' @param id_var Character. Subject ID variable across inputs.
#' @param randomization_date_var Character. Randomization date variable in `df_sl_randomization`.
#' @param goal_recruitments Integer. Target number of consented participants by `goal_finish`.
#' @param start_date_for_goals Date at which the goal line begins (x=0, y=0).
#' @param goal_finish Date at which the goal line reaches `goal_recruitments`.
#' @param show_extension_goal Logical. ▶ If TRUE, add a second goal line that ends `extension_years` after `goal_finish`. (Default FALSE)
#' @param extension_years Integer. ▶ Length of no-cost extension in years (default 1).
#' @param filename Output PNG filename (default `"XX_enroll_goal_YYYYMMDD.png"`).
#' @param show_latest_labels Logical. Add N-labels to the most recent point for each actual series.
#'
#' @returns A list with:
#' \itemize{
#'   \item \code{plot} — ggplot2 object of the enrollment goal figure.
#'   \item \code{plot_matrix} — tibble with columns: \code{date}, \code{Actual_Consented},
#'         \code{Actual_Randomized}, \code{Goal_Consented}, and (if enabled) \code{Goal_Consented_Ext}.
#'   \item \code{path} — full file path to the saved PNG.
#' }
#'
#' @import ggplot2
#' @importFrom dplyr left_join filter mutate arrange distinct select anti_join bind_rows transmute slice_tail
#' @importFrom dplyr join_by
#' @importFrom tibble tibble
#' @importFrom lubridate floor_date years
#' @importFrom rlang sym as_string
#' @importFrom stringr str_detect
#' @export
run_clear_enrollment_goal <- function(
    report_dir,
    df_sl_status          = NULL,
    df_vl_visits          = NULL,
    df_sl_randomization   = NULL,
    # actuals build
    start_date_actuals    = as.Date("2025-06-18"),
    date_var              = "SSS.DAT.Baseline",
    status_var            = "SBJ.FCT.Status",
    id_var                = "IDX.CHR.Subject",
    randomization_date_var= "SBJ.DAT.Randomized",
    # single goal line
    goal_recruitments     = 200,
    start_date_for_goals  = as.Date("2025-06-18"),
    goal_finish           = as.Date("2028-08-31"),
    # ▶ EXTENSION controls
    show_extension_goal   = FALSE,
    extension_years       = 1,
    # output
    filename              = sprintf("XX_enroll_goal_%s.png", format(Sys.Date(), "%Y%m%d")),
    show_latest_labels    = TRUE
) {
  # 0) load from {targets} if not provided
  if (is.null(df_sl_status))        df_sl_status        <- targets::tar_read_raw("df_sl_status")
  if (is.null(df_vl_visits))        df_vl_visits        <- targets::tar_read_raw("df_vl_visits")
  if (is.null(df_sl_randomization)) df_sl_randomization <- targets::tar_read_raw("df_sl_randomization")

  # --- basic column checks
  required_status_cols <- c(id_var, status_var)
  required_visits_cols <- c(id_var, date_var)
  required_rand_cols   <- c(id_var, randomization_date_var)

  stop_if_missing <- function(df, req, df_name) {
    miss <- setdiff(req, colnames(df))
    if (length(miss) > 0) stop(sprintf("Missing columns in %s: %s", df_name, paste(miss, collapse = ", ")), call. = FALSE)
  }
  stop_if_missing(df_sl_status,        required_status_cols, "df_sl_status")
  stop_if_missing(df_vl_visits,        required_visits_cols, "df_vl_visits")
  stop_if_missing(df_sl_randomization, required_rand_cols,   "df_sl_randomization")

  id_sym <- rlang::sym(id_var)

  # 1) join status + visits to get baseline/consent date
  df <- dplyr::left_join(
    df_sl_status, df_vl_visits,
    by = dplyr::join_by(!!id_sym)
  )

  # 2) enrolled/consented (baseline present, not TEST, not Screen Fail)
  df_clean <- df |>
    dplyr::mutate(.date_raw = as.Date(.data[[date_var]])) |>
    dplyr::filter(!is.na(.date_raw)) |>
    dplyr::filter(!stringr::str_detect(.data[[id_var]], "TEST")) |>
    dplyr::filter(.data[[status_var]] != "Screen Fail")

  # earliest baseline per participant
  df_subj <- df_clean |>
    dplyr::arrange(.date_raw) |>
    dplyr::distinct(!!id_sym, .keep_all = TRUE) |>
    dplyr::select(!!id_sym, .date_raw)

  # 3) randomized cohort (strict non-NA rand date; drop TEST + Screen Fail)
  df_rand_subj <- df_sl_randomization |>
    dplyr::filter(!is.na(.data[[randomization_date_var]])) |>
    dplyr::mutate(.rand_date = as.Date(.data[[randomization_date_var]])) |>
    dplyr::distinct(!!id_sym, .keep_all = TRUE) |>
    dplyr::left_join(
      df_sl_status |>
        dplyr::select(!!id_sym, .status = !!rlang::sym(status_var)),
      by = dplyr::join_by(!!id_sym)
    ) |>
    dplyr::filter(.status != "Screen Fail") |>
    dplyr::filter(!stringr::str_detect(.data[[id_var]], "TEST")) |>
    dplyr::select(!!id_sym, .rand_date)

  # 4) ensure consented ≥ randomized (union consent + rand-only-as-proxy)
  rand_no_baseline <- dplyr::anti_join(
    df_rand_subj,
    df_subj,
    by = dplyr::join_by(!!id_sym)
  ) |>
    dplyr::transmute(!!id_sym, .date_raw = .rand_date)

  df_consent_union <- dplyr::bind_rows(df_subj, rand_no_baseline) |>
    dplyr::arrange(.date_raw) |>
    dplyr::distinct(!!id_sym, .keep_all = TRUE)

  # 5) date grid (monthly ticks + today) trimmed to start_date_for_goals
  start_month <- lubridate::floor_date(start_date_actuals, "month")
  this_month  <- lubridate::floor_date(Sys.Date(), "month")
  monthly_seq <- seq(from = start_month, to = this_month, by = "month")
  grid <- tibble::tibble(date = sort(unique(c(monthly_seq, Sys.Date())))) |>
    dplyr::filter(date >= start_date_for_goals)

  # 6) cumulative actual consented/randomized
  subj_dates <- df_consent_union$.date_raw
  rand_dates <- df_rand_subj$.rand_date
  cumulative_recruitments <- vapply(grid$date, function(d) sum(subj_dates <= d), integer(1))
  randomization_counts    <- vapply(grid$date, function(d) sum(rand_dates <= d), integer(1))

  # 7) goal lines (main and optional extension)
  goal_label <- sprintf("Consented Goal (%s)", format(goal_finish, "%Y"))
  total_days <- max(1L, as.integer(goal_finish - start_date_for_goals))

  # ▶ EXTENSION dates/label
  ext_finish <- goal_finish + lubridate::years(extension_years)
  ext_label  <- sprintf("Consented Goal (%s)", format(ext_finish, "%Y"))
  total_days_ext <- max(1L, as.integer(ext_finish - start_date_for_goals))

  plot_matrix <- grid |>
    dplyr::mutate(
      Actual_Consented  = cumulative_recruitments,
      Actual_Randomized = randomization_counts,
      Goal_Consented    = pmax(0, pmin(
        goal_recruitments,
        round(goal_recruitments * as.integer(date - start_date_for_goals) / total_days)
      )),
      # ▶ optional extension series (same total target, longer runway)
      Goal_Consented_Ext = if (show_extension_goal) {
        pmax(0, pmin(
          goal_recruitments,
          round(goal_recruitments * as.integer(date - start_date_for_goals) / total_days_ext)
        ))
      } else NA_real_
    )

  # 8) plot
  g <- ggplot2::ggplot()

  # main goal line
  g <- g +
    ggplot2::geom_segment(
      ggplot2::aes(x = start_date_for_goals, y = 0,
                   xend = goal_finish, yend = goal_recruitments,
                   color = goal_label, linetype = goal_label),
      linewidth = 1.2
    )

  # ▶ extension goal line (if enabled)
  if (isTRUE(show_extension_goal)) {
    g <- g +
      ggplot2::geom_segment(
        ggplot2::aes(x = start_date_for_goals, y = 0,
                     xend = ext_finish, yend = goal_recruitments,
                     color = ext_label, linetype = ext_label),
        linewidth = 1.2
      )
  }

  # actuals
  g <- g +
    ggplot2::geom_line(
      data = plot_matrix,
      ggplot2::aes(x = date, y = Actual_Consented, color = "Actual Consented", linetype = "Actual Consented"),
      linewidth = 1.2
    ) +
    ggplot2::geom_point(
      data = plot_matrix,
      ggplot2::aes(x = date, y = Actual_Consented, color = "Actual Consented"),
      size = 2
    ) +
    ggplot2::geom_line(
      data = plot_matrix,
      ggplot2::aes(x = date, y = Actual_Randomized, color = "Actual Randomized", linetype = "Actual Randomized"),
      linewidth = 1.2
    ) +
    ggplot2::geom_point(
      data = plot_matrix,
      ggplot2::aes(x = date, y = Actual_Randomized, color = "Actual Randomized"),
      size = 2
    )

  # legends (conditionally include extension label)
  color_map <- c(
    setNames("gray40", goal_label),
    "Actual Consented"  = "#1f77b4",
    "Actual Randomized" = "#2ca02c"
  )
  lty_map <- c(
    setNames("dashed", goal_label),
    "Actual Consented"  = "solid",
    "Actual Randomized" = "solid"
  )
  if (isTRUE(show_extension_goal)) {
    color_map <- c(color_map, setNames("gray60", ext_label))
    lty_map   <- c(lty_map,   setNames("dotdash", ext_label))  # visually distinct from dashed
  }

  g <- g +
    ggplot2::scale_color_manual(name = NULL, values = color_map) +
    ggplot2::scale_linetype_manual(name = NULL, values = lty_map) +
    ggplot2::scale_x_date(
      limits = c(start_date_for_goals, NA),
      date_breaks = "3 month",
      date_labels = "%b %Y",
      expand = ggplot2::expansion(mult = c(0.01, 0.05))
    ) +
    ggplot2::labs(x = "Month", y = "Participants") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    )

  if (isTRUE(show_latest_labels) && nrow(plot_matrix) > 0) {
    latest <- dplyr::slice_tail(plot_matrix, n = 1)
    g <- g +
      ggplot2::geom_text(
        data = latest,
        ggplot2::aes(x = date, y = Actual_Consented, label = paste0("N=", Actual_Consented)),
        vjust = -0.8, size = 3.2
      ) +
      ggplot2::geom_text(
        data = latest,
        ggplot2::aes(x = date, y = Actual_Randomized, label = paste0("N=", Actual_Randomized)),
        vjust = 1.8, size = 3.2
      )
  }

  # save + return
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(report_dir, filename)
  ggplot2::ggsave(out_path, plot = g, width = 10, height = 6, dpi = 300, bg = "white")

  list(plot = g, plot_matrix = plot_matrix, path = out_path)
}
