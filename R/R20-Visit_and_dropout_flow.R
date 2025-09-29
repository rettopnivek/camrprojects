# Functions to visualize visit flow and dropout times

#' Visualize Visit flow, separately, for active participants and dropouts
#'
#' @description
#' Create a bar plot that shows the most recent completed visit for participants
#' who are active (or completed). Takes the standard `df_visits` and
#' `df_status` as inputs. See parameter descriptions for the expected variable names.
#'
#' @param df_visits A dataframe following the conventions for `df_visits` in the
#' standard pipeline. The function expects SSS.LGL.Visit.Attd to indicate
#' visit attendance, IDX.INT.VisitNumber to provide the ordered visit numbers, and
#' SSS.FCT.VisitName to provide visit names (with levels that match the visit numbers).
#'
#' @param df_status A dataframe following the conventions for `df_status` in the
#' standard pipeline. Expects dropouts to have `SBJ.FCT.Status` of
#' "LTFU", "Withdrawn", or "Terminated".
#'
#' @param output_dir (Optional) A string of the directory to save the plots as
#' svg files.
#'
#' @param visit_flow_title A string with the title of the visit flow plot.
#' Defaults to "Active/Completed Participants: Most Recent Attended Visit"
#'
#' @param dropout_flow_title A string with the title of the dropout flow plot.
#' Defaults to "Dropped Participants: Most Recent Attended Visit"
#'
#' @param show_all_visits Logical. If TRUE, shows all visits on x-axis,
#' even if it was not the final visit for any participant. Defaults to TRUE.
#'
#' @returns A list of ggplot2 objects. Optionally saves .svg to `output_path`
#'
#' @import ggplot2
#'
#' @export
#'
#' @author Zach Himmelsbach
camr_visit_and_dropout_flow <- function(df_visits, df_status, output_dir = NULL,
                                        visit_flow_title = "Active/Completed Participants: Most Recent Attended Visit",
                                        dropout_flow_title = "Dropped Participants: Most Recent Attended Visit",
                                        dropout_statuses = c("LTFU", "Withdrawn", "Terminated"),
                                        label_max = 5,
                                        show_all_visits = TRUE) {

  # Process visit and status data
  processed_dfs <- preprocess_visit_flow_data(df_visits, df_status,
                                              dropout_statuses = dropout_statuses)

  # Visualize
  visit_flow <- viz_visit_flow(processed_dfs$df_counts |> dplyr::filter(SBJ.LGL.Dropped == FALSE),
                               processed_dfs$processed_visits |> dplyr::filter(SBJ.LGL.Dropped == FALSE),
                               plot_title = visit_flow_title,
                               output_dir = output_dir,
                               label_max = label_max,
                               show_all_visits = show_all_visits)

  dropout_flow <- viz_visit_flow(processed_dfs$df_counts |> dplyr::filter(SBJ.LGL.Dropped == TRUE),
                                 processed_dfs$processed_visits |> dplyr::filter(SBJ.LGL.Dropped == TRUE),
                                 plot_title = dropout_flow_title,
                                 output_dir = output_dir,
                                 label_max = label_max,
                                 show_all_visits = show_all_visits)

  return(list(visit_flow   = visit_flow,
              dropout_flow = dropout_flow))
}

#' Pre-processing of data for visit/dropout flow visualizations
#'
#'
#' @description
#' Takes in visit and status dataframes and prepares them for visualization
#'
#' @param df_visit A dataframe following the conventions for `df_visits` in the
#' standard pipeline. The function expects SSS.LGL.Visit.Completed to indicate
#' visit completion, IDX.INT.VisitNumber to provide the ordered visit numbers, and
#' SSS.FCT.VisitName to provide visit names (with levels that match the visit numbers).
#'
#' @param df_status A dataframe following the conventions for `df_status` in the
#' standard pipeline. Expects dropouts to have `SBJ.FCT.Status` of
#' "LTFU", "Withdrawn", or "Terminated".
#'
#' @returns A list of processed dataframes for vizualization functions.
preprocess_visit_flow_data <- function(df_visits, df_status,
                                       dropout_statuses = c("LTFU", "Withdrawn", "Terminated")) {

  # Check that inputs contain required variables
  if (!"SSS.LGL.Visit.Attd" %in% colnames(df_visits)) {
    stop("The variable SSS.LGL.Visit.Attd is required in `df_visits`.")
  }
  if (!"IDX.INT.VisitNumber" %in% colnames(df_visits)) {
    stop("The variable IDX.INT.VisitNumber is required in `df_visits`.")
  }
  if (!"SSS.FCT.VisitName" %in% colnames(df_visits)) {
    stop("The variable SSS.FCT.VisitName is required in `df_visits`. This factor should have levels corresponding to visit order.")
  }
  if (!"IDX.CHR.Subject" %in% colnames(df_status)) {
    stop("`df_status` must include `IDX.CHR.Subject`.")
  }
  if (!"IDX.CHR.Subject" %in% colnames(df_visits)) {
    stop("`df_visits` must include `IDX.CHR.Subject`.")
  }

  # Get most recent completed visit
  df_visits <- df_visits |> dplyr::group_by(IDX.CHR.Subject) |>
    dplyr::filter(SSS.LGL.Visit.Attd == TRUE) |> dplyr::arrange(IDX.CHR.Subject, desc(IDX.INT.VisitNumber)) |>
    dplyr::slice(1) |> dplyr::ungroup()

  df_visits <- base::merge(df_visits, df_status, by = "IDX.CHR.Subject", all = TRUE) |>
    dplyr::mutate(VST.SUB.INT.RecordNumber = IDX.CHR.Subject) # Previously we modified this, but we're now going with the full IDs

  df_visits <- df_visits |> dplyr::mutate(SBJ.LGL.Dropped = SBJ.FCT.Status %in% dropout_statuses)
  # Add level for participants without any completed visits
  df_visits$SSS.FCT.VisitName <- addNA(df_visits$SSS.FCT.VisitName)  # Makes NA an explicit level
  levels(df_visits$SSS.FCT.VisitName)[is.na(levels(df_visits$SSS.FCT.VisitName))] <- "No Visits Completed"

  # Reorder levels so "No Visits Completed" is first
  df_visits$SSS.FCT.VisitName <- factor(df_visits$SSS.FCT.VisitName,
                                        levels = c("No Visits Attended",
                                                   setdiff(levels(df_visits$SSS.FCT.VisitName), "No Visits Attended")))


  # Aggregate record ID labels to visit level
  df_counts <- df_visits |>
    dplyr::group_by(SSS.FCT.VisitName, SBJ.LGL.Dropped) |>
    dplyr::summarise(n = n(),
              VST.SUB.INT.RecordNumber = paste(VST.SUB.INT.RecordNumber, collapse = "\n")) |>
    dplyr::ungroup()

  return(list(df_counts = df_counts,
              processed_visits = df_visits))
}

#' Visualize visit flow
#'
#' @description
#' Take in output from `preprocess_visit_flow_data` and plot most recent
#' completed visits of participants. Can be used for active participants or
#' dropouts.
#'
#' @param df_counts A dataframe with counts and record labels
#' @param df_visits A dataframe with participants most recent visit info.
#' @param plot_title A string with the title for the plot. Also used to name
#' svg files if `output_dir` is supplied.
#' @param output_dir (Optional) The directory where the plot should be saved
#' @param label_max Integer. The maximum number of record IDs that should be
#' printed above a visit's bar in the plot.
#' @param show_all_visits Logical. If TRUE, shows all visits on x-axis,
#' even if it was not the final visit for any participant. Defaults to TRUE.
#'
#' @returns A ggplot. If `output_dir` is given, the plot is also saved there
#' as a .svg file.
#'
#' @import ggplot2
viz_visit_flow <- function(df_counts, df_visits,
                           plot_title,
                           output_dir = NULL, label_max = 5,
                           show_all_visits = TRUE) {

  # Put text labels (with Subject IDs) on only those with counts <= 5
  df_labels <- df_counts |>
    dplyr::filter(n <= label_max)

  plt <- ggplot(df_visits, aes(x = SSS.FCT.VisitName)) +
    geom_bar() +
    theme_minimal() +
    ylab("Number of Participants") +
    xlab("Last Completed Visit") +
    ggtitle(plot_title) + scale_y_continuous(breaks = scales::pretty_breaks()) +
    scale_x_discrete(drop = !show_all_visits) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(data = df_labels,
              aes(x = SSS.FCT.VisitName, y = n, label = VST.SUB.INT.RecordNumber),
              vjust = -0.5,
              inherit.aes = FALSE)

  if (!is.null(output_dir)) {
    filename_safe_title <- gsub("[^[:alnum:]_]", "_", plot_title)
    plt |>
      (\(p) ggsave(sprintf("%s/%s.svg", output_dir, filename_safe_title), p, width = 8))()
  }

  return(plt)

}
