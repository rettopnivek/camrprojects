# Visualization and reporting
# Written by...
#   Luwei Liu
# Maintained by...
#   Luwei Liu
# Email:
#   lliu48@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated: 2025-07-30



#### 1.0) CONSORT Diagram Generators ####
#' Generate CONSORT Flow Diagram (No Follow-up Phase)
#'
#' A variant for projects without a separate follow‑up phase. Shows:
#' assessment → consent → randomization → group allocation → completion,
#' and consolidates attrition into an “Excluded” box (Withdrawn, Lost to Follow‑up, Terminated).
#'
#' @param total_assessed Integer. Total assessed for eligibility.
#' @param n_consented Integer. Number who provided consent.
#' @param n_randomized Integer. Number randomized.
#' @param n_GroupA Integer. Number allocated to Group A.
#' @param n_GroupB Integer. Number allocated to Group B.
#' @param n_Completed_A Integer. Number who completed in Group A.
#' @param n_Completed_B Integer. Number who completed in Group B.
#' @param n_rand_pending Integer or NULL. Pending randomization (show box if not NULL).
#' @param n_exclusion_1 Integer. Excluded during screening.
#' @param n_pending_consent Integer or NULL. Pending consent (show box if not NULL).
#' @param n_exclusion_3 Integer. Excluded post‑consent.
#' @param exclusion_reasons_1 Character vector. Screening exclusion reasons.
#' @param pending_reasons Character vector or NULL. Pending consent reasons.
#' @param exclusion_reasons_3 Character vector. Post‑consent exclusion reasons.
#' @param withdrawn_a,withdrawn_b Integers (default 0). Withdrawn.
#' @param lost_to_followup_a,lost_to_followup_b Integers (default 0). Lost to follow‑up.
#' @param terminated_a,terminated_b Integers (default 0). Terminated.
#'
#' @return A \code{DiagrammeR} graph object rendering the CONSORT diagram.
#'
#' @export
#' @importFrom glue glue
#' @importFrom DiagrammeR grViz
#'
#' @examples
#' camr_consort_diagram_no_follow_up(
#'   total_assessed = 250,
#'   n_consented = 180,
#'   n_randomized = 150,
#'   n_GroupA = 75, n_GroupB = 75,
#'   n_Completed_A = 60, n_Completed_B = 58,
#'   n_rand_pending = 5,
#'   n_exclusion_1 = 40, n_pending_consent = 8,
#'   n_exclusion_3 = 15,
#'   exclusion_reasons_1 = c("Did not meet substance use criteria", "Other Reasons"),
#'   pending_reasons = c("Waiting for consent &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", "Still Screening"),
#'   exclusion_reasons_3 = c("Screen fail (n=15)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", "Withdrew consent (n=5)"),
#'   withdrawn_a = 3, lost_to_followup_a = 2, terminated_a = 1,
#'   withdrawn_b = 2, lost_to_followup_b = 3, terminated_b = 1
#' )
camr_consort_diagram_no_follow_up <- function(
    total_assessed,
    n_consented,
    n_randomized,
    n_GroupA,
    n_GroupB,
    n_Completed_A,
    n_Completed_B,
    n_rand_pending = NULL,
    n_exclusion_1,
    n_pending_consent = NULL,
    n_exclusion_3,
    exclusion_reasons_1,
    pending_reasons = NULL,
    exclusion_reasons_3,
    withdrawn_a = 0,
    withdrawn_b = 0,
    lost_to_followup_a = 0,
    lost_to_followup_b = 0,
    terminated_a = 0,
    terminated_b = 0
) {
  # Input validation
  stopifnot(
    is.numeric(total_assessed), total_assessed >= 0,
    is.numeric(n_consented), n_consented >= 0,
    is.numeric(n_randomized), n_randomized >= 0,
    is.numeric(n_GroupA), n_GroupA >= 0,
    is.numeric(n_GroupB), n_GroupB >= 0,
    is.numeric(n_Completed_A), n_Completed_A >= 0,
    is.numeric(n_Completed_B), n_Completed_B >= 0,
    is.null(n_rand_pending) || (is.numeric(n_rand_pending) && n_rand_pending >= 0),
    is.numeric(n_exclusion_1), n_exclusion_1 >= 0,
    is.null(n_pending_consent) || (is.numeric(n_pending_consent) && n_pending_consent >= 0),
    is.numeric(n_exclusion_3), n_exclusion_3 >= 0,
    is.character(exclusion_reasons_1),
    is.null(pending_reasons) || is.character(pending_reasons),
    is.character(exclusion_reasons_3),
    is.numeric(withdrawn_a), withdrawn_a >= 0,
    is.numeric(withdrawn_b), withdrawn_b >= 0,
    is.numeric(lost_to_followup_a), lost_to_followup_a >= 0,
    is.numeric(lost_to_followup_b), lost_to_followup_b >= 0,
    is.numeric(terminated_a), terminated_a >= 0,
    is.numeric(terminated_b), terminated_b >= 0
  )

  # Derived counts
  active_a <- max(0, n_GroupA - n_Completed_A - withdrawn_a - lost_to_followup_a - terminated_a)
  active_b <- max(0, n_GroupB - n_Completed_B - withdrawn_b - lost_to_followup_b - terminated_b)

  # Helpers
  bullet_html <- function(x, n_total = NULL) {
    # return "" if n_total==0 OR x is NULL/empty/blank/NA
    if (!is.null(n_total) && n_total == 0) return("")
    if (is.null(x)) return("")
    x <- x[!is.na(x)]
    x <- trimws(x)
    x <- x[nzchar(x)]
    if (!length(x)) return("")
    paste0("• ", x, "<br align='left'/>", collapse = "\n  ")
  }

  has_bullets <- function(x, n_total = NULL) {
    if (!is.null(n_total) && n_total == 0) return(FALSE)
    if (is.null(x)) return(FALSE)
    x <- trimws(x[!is.na(x)])
    length(x) > 0
  }

  br_if <- function(flag) if (isTRUE(flag)) "<br align='left'/>" else ""
  #labels

  # Excluded (screening)
  hb1 <- has_bullets(exclusion_reasons_1, n_exclusion_1)
  lbl_excl1 <- glue::glue("<<font point-size='18' align='left'>
  Excluded (n = {n_exclusion_1}){br_if(hb1)}
  {bullet_html(exclusion_reasons_1, n_exclusion_1)}
</font>>")

  # Excluded (post-consent / pre-randomization)
  hb3 <- has_bullets(exclusion_reasons_3, n_exclusion_3)
  lbl_excl3 <- glue::glue("<<font point-size='18' align='left'>
  Excluded (n = {n_exclusion_3}){br_if(hb3)}
  {bullet_html(exclusion_reasons_3, n_exclusion_3)}
</font>>")

  # Pending logic — show box if not NULL (even if 0)
  has_pending1 <- !is.null(n_pending_consent)
  if (has_pending1) {
    hbP <- has_bullets(pending_reasons, n_pending_consent)
    lbl_pending1 <- glue::glue("<<font point-size='18' align='left'>
  Pending (n = {n_pending_consent}){br_if(hbP)}
  {bullet_html(pending_reasons, n_pending_consent)}
</font>>")
  }

  has_pending_rand <- !is.null(n_rand_pending)
  if (has_pending_rand) {
    lbl_pending_rand <- glue::glue("<<font point-size='18'>
  Pending Randomization (n = {n_rand_pending})
</font>>")
  }

  # Combined Exclusions labels (Withdrawn + Lost to Follow-up + Terminated)
  show_exclusions_a <- (withdrawn_a + lost_to_followup_a + terminated_a) > 0
  show_exclusions_b <- (withdrawn_b + lost_to_followup_b + terminated_b) > 0

  lbl_excluded_a <- glue::glue("<<font point-size='18' align='left'>
  Excluded (n = {withdrawn_a + lost_to_followup_a + terminated_a})<br align='left'/>
  • Withdrawn (n = {withdrawn_a})<br align='left'/>
  • Lost to Follow-up (n = {lost_to_followup_a})<br align='left'/>
  • Terminated (n = {terminated_a})<br align='left'/>
</font>>")

  lbl_excluded_b <- glue::glue("<<font point-size='18' align='left'>
  Excluded (n = {withdrawn_b + lost_to_followup_b + terminated_b})<br align='left'/>
  • Withdrawn (n = {withdrawn_b})<br align='left'/>
  • Lost to Follow-up (n = {lost_to_followup_b})<br align='left'/>
  • Terminated (n = {terminated_b})<br align='left'/>
</font>>")

  lbl_active_a <- glue::glue("<<font point-size='18'>Active (n = {active_a})</font>>")
  lbl_active_b <- glue::glue("<<font point-size='18'>Active (n = {active_b})</font>>")

  # Build graph
  lines <- c(
    "digraph {",
    "  graph[splines = ortho, nodesep = 2]",
    "  node[fontname = Helvetica, shape = box, width = 6, height = 1, fontsize = 20]",
    "",
    glue::glue("  node1[label = <Assessed for Eligibility (n = {total_assessed})>]"),
    glue::glue("  node2[label = <Consented (n = {n_consented})>]"),
    glue::glue("  node3[label = <Randomized (n = {n_randomized})>]"),
    glue::glue("  node3a[label = <Group A (n = {n_GroupA})>]"),
    glue::glue("  node3b[label = <Group B (n = {n_GroupB})>]"),
    glue::glue("  node4a[label = <Completed Study (n = {n_Completed_A})>]"),
    glue::glue("  node4b[label = <Completed Study (n = {n_Completed_B})>]"),
    "",
    "  blank1[label = '', width = 0.01, height = 0.01]",
    glue::glue("  excluded1[label = {lbl_excl1}, width = 8, height = 1]")
  )

  if (has_pending1) {
    lines <- c(lines,
               "  blank2[label = '', width = 0.01, height = 0.01]",
               glue::glue("  excluded2[label = {lbl_pending1}, width = 8, height = 0.5]"))
  }

  lines <- c(lines,
             "  blank3[label = '', width = 0.01, height = 0.01]",
             glue::glue("  excluded3[label = {lbl_excl3}, width = 8, height = 1]"))

  if (has_pending_rand) {
    lines <- c(lines,
               "  blank4[label = '', width = 0.01, height = 0.01]",
               glue::glue("  excluded4[label = {lbl_pending_rand}, width = 8, height = 0.5]"))
  }

  # Group A boxes
  lines <- c(lines,
             "  blank5[label = '', width = 0.01, height = 0.01]")
  if (show_exclusions_a) {
    lines <- c(lines,
               glue::glue("  excludedA[label = {lbl_excluded_a}, width = 4, height = 0.8]"))
  }
  lines <- c(lines,
             "  blank7[label = '', width = 0.01, height = 0.01]",
             glue::glue("  activeA[label = {lbl_active_a}, width = 3.5, height = 0.5]"))

  # Group B boxes
  lines <- c(lines,
             "  blank8[label = '', width = 0.01, height = 0.01]")
  if (show_exclusions_b) {
    lines <- c(lines,
               glue::glue("  excludedB[label = {lbl_excluded_b}, width = 4, height = 0.8]"))
  }
  lines <- c(lines,
             "  blank10[label = '', width = 0.01, height = 0.01]",
             glue::glue("  activeB[label = {lbl_active_b}, width = 3.5, height = 0.5]"))

  # Connections
  lines <- c(lines,
             "",
             "  // Main connections",
             "  node1 -> blank1[dir = none]",
             "  blank1 -> excluded1[minlen = 1]",
             "  { rank = same; blank1 excluded1 }")

  if (has_pending1) {
    lines <- c(lines,
               "  blank1 -> blank2[dir = none]",
               "  blank2 -> excluded2[minlen = 1]",
               "  { rank = same; blank2 excluded2 }",
               "  blank2 -> node2[dir = none]")
  } else {
    lines <- c(lines, "  blank1 -> node2[dir = none]")
  }

  lines <- c(lines,
             "  node2 -> blank3[dir = none]",
             "  blank3 -> excluded3[minlen = 1]",
             "  { rank = same; blank3 excluded3 }")

  if (has_pending_rand) {
    lines <- c(lines,
               "  blank3 -> blank4[dir = none]",
               "  blank4 -> excluded4[minlen = 1]",
               "  { rank = same; blank4 excluded4 }",
               "  blank4 -> node3[dir = none]")
  } else {
    lines <- c(lines, "  blank3 -> node3[dir = none]")
  }

  lines <- c(lines,
             "  node3 -> node3a",
             "  node3 -> node3b",

             # Group A connections
             "  node3a -> blank5[dir = none]")
  if (show_exclusions_a) {
    lines <- c(lines,
               "  blank5 -> excludedA[minlen = 1]",
               "  { rank = same; blank5 excludedA }",
               "  blank5 -> blank7[dir = none]")
  } else {
    lines <- c(lines, "  blank5 -> blank7[dir = none]")
  }

  lines <- c(lines,
             "  blank7 -> activeA[minlen = 1]",
             "  { rank = same; blank7 activeA }",
             "  blank7 -> node4a[dir = none]",

             # Group B connections
             "  node3b -> blank8[dir = none]")
  if (show_exclusions_b) {
    lines <- c(lines,
               "  blank8 -> excludedB[minlen = 1]",
               "  { rank = same; blank8 excludedB }",
               "  blank8 -> blank10[dir = none]")
  } else {
    lines <- c(lines, "  blank8 -> blank10[dir = none]")
  }

  lines <- c(lines,
             "  blank10 -> activeB[minlen = 1]",
             "  { rank = same; blank10 activeB }",
             "  blank10 -> node4b[dir = none]",
             "}")

  DiagrammeR::grViz(paste(lines, collapse = "\n"))
}

#' Generate CONSORT Flow Diagram (with Follow-up Phase) from df_consort
#'
#' @param df A dataframe where each row represents a screened participant and contains at least:
#'   assessed, exclusion_reasons_1, IDS.CHR.Subject, SBJ.FCT.Rand.Group, SBJ.FCT.Status,
#'   SBJ.FCT.Status.ScreenFailReason, SBJ.LGL.InterventionCompleted.
#'   - assessed: non-NA for all screened participants
#'   - exclusion_reasons_1: pre-consent screen-fail reasons
#'   - IDS.CHR.Subject: subject identifier (non-NA if consented)
#'   - SBJ.FCT.Rand.Group: randomization group label (e.g., "Group A", "Group B")
#'   - SBJ.FCT.Status: participant status; expected values include
#'       "Screen Fail", "Lost to Follow-Up", "Withdrawn", "Terminated", "Completed", or NA
#'   - SBJ.FCT.Status.ScreenFailReason: reason text when SBJ.FCT.Status == "Screen Fail"
#'   - SBJ.LGL.InterventionCompleted: logical flag indicating intervention completion (TRUE/FALSE)
#' @param pending_reasons Character vector of reasons to show under the Pending consent box (optional).
#' @param pending_rand_reasons Character vector of reasons to show under the Pending randomization box (optional).
#' @param show_pending_consent Logical, whether to display the pending-consent box (default TRUE).
#' @param show_pending_rand Logical, whether to display the pending-randomization box (default TRUE).
#'
#' @return A DiagrammeR htmlwidget.
#' @importFrom DiagrammeR grViz
#' @importFrom glue glue
#' @examples
#' # Minimal reproducible example ----------------------------------------------
#' library(tibble)
#'
#' df_consort_demo <- tibble::tibble(
#'   assessed = rep(1, 14),
#'   exclusion_reasons_1 = c(
#'     "Out of age range", # R01: pre-consent exclusion
#'     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
#'   ),
#'   IDS.CHR.Subject = c(
#'     NA,        # R01 excluded pre-consent
#'     "P01",     # R02
#'     "P02",     # R03
#'     "P03",     # R04
#'     "P04",     # R05
#'     "P05",     # R06
#'     "P06",     # R07
#'     "P07",     # R08
#'     NA,        # R09 pending consent
#'     "P08",     # R10 pending randomization
#'     "P09",     # R11
#'     "P10",     # R12
#'     "P11",     # R13 withdrawn after intervention A
#'     "P12"      # R14 LTFU after intervention B
#'   ),
#'   SBJ.FCT.Rand.Group = c(
#'     NA, "Group A", "Group B", "Group A", "Group B",
#'     "Group A", "Group B", "Group A", NA, NA,
#'     "Group A", "Group B", "Group A", "Group B"
#'   ),
#'   SBJ.FCT.Status = c(
#'     "Screen Fail",             # R01: pre-consent exclusion
#'     "Completed Intervention",  # R02: finished intervention A
#'     "Completed Study",         # R03: fully completed B
#'     "Lost to Follow-Up",       # R04: lost during intervention A
#'     "Completed Intervention",  # R05: finished intervention B
#'     "Lost to Follow-Up",       # R06: lost during follow-up A (after intervention)
#'     "Withdrawn",               # R07: withdrew during intervention B
#'     "Terminated",              # R08: terminated during intervention A
#'     NA,                        # R09: pending consent
#'     NA,                        # R10: pending randomization
#'     "Completed Study",         # R11: fully completed A
#'     "Completed Study",         # R12: fully completed B
#'     "Withdrawn",               # R13: withdrew AFTER intervention A
#'     "Lost to Follow-Up"        # R14: LTFU AFTER intervention B
#'   ),
#'   SBJ.FCT.Status.ScreenFailReason = c(
#'     "Did not meet criteria", # R01 reason
#'     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
#'   ),
#'   SBJ.LGL.InterventionCompleted = c(
#'     NA,   # R01 excluded pre-consent
#'     TRUE, # R02 completed intervention
#'     TRUE, # R03 completed study
#'     FALSE,# R04 dropped before intervention complete
#'     TRUE, # R05 completed intervention
#'     TRUE, # R06 completed intervention, dropped later
#'     FALSE,# R07 dropped before intervention complete
#'     FALSE,# R08 terminated before intervention complete
#'     NA,   # R09 pending consent
#'     NA,   # R10 pending randomization
#'     TRUE, # R11 completed study
#'     TRUE, # R12 completed study
#'     TRUE, # R13 withdrew AFTER intervention complete (Group A)
#'     TRUE  # R14 LTFU AFTER intervention complete (Group B)
#'   )
#' )
#'
#' # Example 1: Show both pending boxes (auto-counted), with reasons
#' camr_consort_diagram(
#'   df_consort_demo,
#'   pending_reasons = c("Awaiting consent form (n=1)", "Scheduling (n=1)"),
#'   pending_rand_reasons = c("Waiting for lab results (n=1)"),
#'   show_pending_consent = TRUE,
#'   show_pending_rand = TRUE
#' )
#'
#' # Example 2: Hide pending-consent, show pending-randomization
#' camr_consort_diagram(
#'   df_consort_demo,
#'   pending_rand_reasons = c("PI review"),
#'   show_pending_consent = FALSE,
#'   show_pending_rand = TRUE
#' )
#'
#' # Example 3: Hide both pending boxes
#' camr_consort_diagram(
#'   df_consort_demo,
#'   show_pending_consent = FALSE,
#'   show_pending_rand = FALSE
#' )

camr_consort_diagram <- function(
    df,
    pending_reasons = NULL,
    pending_rand_reasons = NULL,
    show_pending_consent = TRUE,
    show_pending_rand = TRUE
) {
  stopifnot(all(c(
    "assessed", "IDS.CHR.Subject", "SBJ.FCT.Rand.Group",
    "SBJ.FCT.Status", "SBJ.FCT.Status.ScreenFailReason",
    "exclusion_reasons_1", "SBJ.LGL.InterventionCompleted"
  ) %in% names(df)))

  # ---- Coerce intervention-completed flag ----
  int_completed <- as.logical(df$SBJ.LGL.InterventionCompleted)
  int_completed[is.na(int_completed)] <- FALSE

  # ---- Counts from df ----
  total_assessed <- sum(!is.na(df$assessed))
  n_consented    <- sum(!is.na(df$IDS.CHR.Subject))
  n_randomized   <- sum(!is.na(df$SBJ.FCT.Rand.Group))

  n_GroupA <- sum(df$SBJ.FCT.Rand.Group == "Group A", na.rm = TRUE)
  n_GroupB <- sum(df$SBJ.FCT.Rand.Group == "Group B", na.rm = TRUE)

  # Completed Intervention is now driven by SBJ.LGL.InterventionCompleted (by group)
  n_Completed_Intervention_A <- sum(df$SBJ.FCT.Rand.Group == "Group A" & int_completed, na.rm = TRUE)
  n_Completed_Intervention_B <- sum(df$SBJ.FCT.Rand.Group == "Group B" & int_completed, na.rm = TRUE)

  # Completed Study still comes from SBJ.FCT.Status == "Completed Study" (by group)
  n_Completed_Study_A <- sum(df$SBJ.FCT.Rand.Group == "Group A" &
                               df$SBJ.FCT.Status == "Completed Study", na.rm = TRUE)
  n_Completed_Study_B <- sum(df$SBJ.FCT.Rand.Group == "Group B" &
                               df$SBJ.FCT.Status == "Completed Study", na.rm = TRUE)

  # ---- Exclusions / reasons ----
  # Phase 1: pre-consent screening exclusions (ordered by count desc)
  n_exclusion_1 <- sum(!is.na(df$exclusion_reasons_1))
  if (n_exclusion_1 > 0) {
    reasons1 <- df$exclusion_reasons_1[!is.na(df$exclusion_reasons_1)]
    tb1 <- sort(table(reasons1), decreasing = TRUE)
    tb1 <- tb1[tb1 > 0]
    exclusion_reasons_1 <- if (length(tb1)) sprintf("%s (n=%d)", names(tb1), as.integer(tb1)) else character(0)
  } else {
    exclusion_reasons_1 <- character(0)
  }

  # Phase 3: post-consent/pre-randomization exclusions (Screen Fail)
  n_exclusion_3 <- sum(df$SBJ.FCT.Status == "Screen Fail", na.rm = TRUE)
  if (n_exclusion_3 > 0) {
    reasons3 <- df$SBJ.FCT.Status.ScreenFailReason[
      df$SBJ.FCT.Status == "Screen Fail" & !is.na(df$SBJ.FCT.Status.ScreenFailReason)
    ]
    if (length(reasons3)) {
      tb3 <- sort(table(reasons3), decreasing = TRUE)
      tb3 <- tb3[tb3 > 0]
      exclusion_reasons_3 <- if (length(tb3)) sprintf("%s (n=%d)", names(tb3), as.integer(tb3)) else character(0)
    } else {
      exclusion_reasons_3 <- character(0)
    }
  } else {
    exclusion_reasons_3 <- character(0)
  }

  # ---- Auto-calc pending counts ----
  # Pending consent = assessed - excluded_1 - consented
  n_pending_consent <- max(0L, total_assessed - n_exclusion_1 - n_consented)
  # Pending randomization = consented - randomized - exclusion_3
  n_rand_pending    <- max(0L, n_consented - n_randomized - n_exclusion_3)

  # ---- Split attrition by phase using SBJ.LGL.InterventionCompleted ----
  is_attrit <- df$SBJ.FCT.Status %in% c("Withdrawn", "Lost to Follow-Up", "Terminated")
  is_int_phase   <- is_attrit & !int_completed               # before intervention completion
  is_study_phase <- is_attrit &  int_completed               # after intervention completion

  tally <- function(group, status, mask) {
    sum(df$SBJ.FCT.Rand.Group == group & df$SBJ.FCT.Status == status & mask, na.rm = TRUE)
  }

  # Intervention-phase attrition (shown before "Completed Intervention")
  withdrawn_intervention_a        <- tally("Group A", "Withdrawn",         is_int_phase)
  lost_to_followup_intervention_a  <- tally("Group A", "Lost to Follow-Up", is_int_phase)
  terminated_intervention_a        <- tally("Group A", "Terminated",        is_int_phase)

  withdrawn_intervention_b        <- tally("Group B", "Withdrawn",         is_int_phase)
  lost_to_followup_intervention_b  <- tally("Group B", "Lost to Follow-Up", is_int_phase)
  terminated_intervention_b        <- tally("Group B", "Terminated",        is_int_phase)

  # Study-phase attrition (shown after "Completed Intervention", before "Completed Study")
  withdrawn_study_a               <- tally("Group A", "Withdrawn",         is_study_phase)
  lost_to_followup_study_a         <- tally("Group A", "Lost to Follow-Up", is_study_phase)
  terminated_study_a               <- tally("Group A", "Terminated",        is_study_phase)

  withdrawn_study_b               <- tally("Group B", "Withdrawn",         is_study_phase)
  lost_to_followup_study_b         <- tally("Group B", "Lost to Follow-Up", is_study_phase)
  terminated_study_b               <- tally("Group B", "Terminated",        is_study_phase)

  # ---- Derived active counts ----
  active_intervention_a <- max(0,
                               n_GroupA - n_Completed_Intervention_A -
                                 withdrawn_intervention_a - lost_to_followup_intervention_a - terminated_intervention_a)

  active_intervention_b <- max(0,
                               n_GroupB - n_Completed_Intervention_B -
                                 withdrawn_intervention_b - lost_to_followup_intervention_b - terminated_intervention_b)

  active_study_a <- max(0,
                        n_Completed_Intervention_A - n_Completed_Study_A -
                          withdrawn_study_a - lost_to_followup_study_a - terminated_study_a)

  active_study_b <- max(0,
                        n_Completed_Intervention_B - n_Completed_Study_B -
                          withdrawn_study_b - lost_to_followup_study_b - terminated_study_b)

  # -------- Helpers --------
  bullet_html <- function(x, n_total = NULL) {
    if (!is.null(n_total) && n_total == 0) return("")
    if (is.null(x)) return("")
    x <- x[!is.na(x)]
    x <- trimws(x)
    x <- x[nzchar(x)]
    if (!length(x)) return("")
    paste0("• ", x, "<br align='left'/>", collapse = "\n  ")
  }
  has_bullets <- function(x, n_total = NULL) {
    if (!is.null(n_total) && n_total == 0) return(FALSE)
    if (is.null(x)) return(FALSE)
    x <- trimws(x[!is.na(x)])
    length(x) > 0
  }
  br_if <- function(flag) if (isTRUE(flag)) "<br align='left'/>" else ""

  # -------- Labels --------
  hb1 <- has_bullets(exclusion_reasons_1, n_exclusion_1)
  lbl_excl1 <- glue::glue("<<font point-size='18' align='left'>
  Excluded (n = {n_exclusion_1}){br_if(hb1)}
  {bullet_html(exclusion_reasons_1, n_exclusion_1)}
</font>>")

  hb3 <- has_bullets(exclusion_reasons_3, n_exclusion_3)
  lbl_excl3 <- glue::glue("<<font point-size='18' align='left'>
  Excluded (n = {n_exclusion_3}){br_if(hb3)}
  {bullet_html(exclusion_reasons_3, n_exclusion_3)}
</font>>")

  # Pending consent (toggled)
  has_pending1 <- show_pending_consent && ((n_pending_consent > 0) || has_bullets(pending_reasons, NULL))
  if (has_pending1) {
    hbP <- has_bullets(pending_reasons, n_pending_consent)
    lbl_pending1 <- glue::glue("<<font point-size='18' align='left'>
  Pending consent (n = {n_pending_consent}){br_if(hbP)}
  {bullet_html(pending_reasons, n_pending_consent)}
</font>>")
  }

  # Pending randomization (toggled)
  has_pending_rand <- show_pending_rand && ((n_rand_pending > 0) || has_bullets(pending_rand_reasons, NULL))
  if (has_pending_rand) {
    hbPR <- has_bullets(pending_rand_reasons, n_rand_pending)
    lbl_pending_rand <- glue::glue("<<font point-size='18' align='left'>
  Pending randomization (n = {n_rand_pending}){br_if(hbPR)}
  {bullet_html(pending_rand_reasons, n_rand_pending)}
</font>>")
  }

  # Intervention Phase Exclusions
  show_exclusions_intervention_a <- (withdrawn_intervention_a + lost_to_followup_intervention_a + terminated_intervention_a) > 0
  show_exclusions_intervention_b <- (withdrawn_intervention_b + lost_to_followup_intervention_b + terminated_intervention_b) > 0

  lbl_excluded_intervention_a <- glue::glue("<<font point-size='18' align='left'>
  Excluded (n = {withdrawn_intervention_a + lost_to_followup_intervention_a + terminated_intervention_a})<br align='left'/>
  • Withdrawn (n = {withdrawn_intervention_a})<br align='left'/>
  • Lost to Follow-Up (n = {lost_to_followup_intervention_a})<br align='left'/>
  • Terminated (n = {terminated_intervention_a})<br align='left'/>
</font>>")

  lbl_excluded_intervention_b <- glue::glue("<<font point-size='18' align='left'>
  Excluded (n = {withdrawn_intervention_b + lost_to_followup_intervention_b + terminated_intervention_b})<br align='left'/>
  • Withdrawn (n = {withdrawn_intervention_b})<br align='left'/>
  • Lost to Follow-Up (n = {lost_to_followup_intervention_b})<br align='left'/>
  • Terminated (n = {terminated_intervention_b})<br align='left'/>
</font>>")

  # Study Phase Exclusions (after intervention)
  show_exclusions_study_a <- (withdrawn_study_a + lost_to_followup_study_a + terminated_study_a) > 0
  show_exclusions_study_b <- (withdrawn_study_b + lost_to_followup_study_b + terminated_study_b) > 0

  lbl_excluded_study_a <- glue::glue("<<font point-size='18' align='left'>
  Excluded (n = {withdrawn_study_a + lost_to_followup_study_a + terminated_study_a})<br align='left'/>
  • Withdrawn (n = {withdrawn_study_a})<br align='left'/>
  • Lost to Follow-Up (n = {lost_to_followup_study_a})<br align='left'/>
  • Terminated (n = {terminated_study_a})<br align='left'/>
</font>>")

  lbl_excluded_study_b <- glue::glue("<<font point-size='18' align='left'>
  Excluded (n = {withdrawn_study_b + lost_to_followup_study_b + terminated_study_b})<br align='left'/>
  • Withdrawn (n = {withdrawn_study_b})<br align='left'/>
  • Lost to Follow-Up (n = {lost_to_followup_study_b})<br align='left'/>
  • Terminated (n = {terminated_study_b})<br align='left'/>
</font>>")

  # Active labels
  lbl_active_intervention_a <- glue::glue("<<font point-size='18'>Active in Intervention (n = {active_intervention_a})</font>>")
  lbl_active_intervention_b <- glue::glue("<<font point-size='18'>Active in Intervention (n = {active_intervention_b})</font>>")
  lbl_active_study_a <- glue::glue("<<font point-size='18'>Active in Study (n = {active_study_a})</font>>")
  lbl_active_study_b <- glue::glue("<<font point-size='18'>Active in Study (n = {active_study_b})</font>>")

  # -------- Build graph --------
  lines <- c(
    "digraph {",
    "  graph[splines = ortho, nodesep = 2]",
    "  node[fontname = Helvetica, shape = box, width = 6, height = 1, fontsize = 20]",
    "",
    glue::glue("  node1[label = <Assessed for eligibility (n = {total_assessed})>]"),
    glue::glue("  node2[label = <Consented (n = {n_consented})>]"),
    glue::glue("  node3[label = <Randomized (n = {n_randomized})>]"),
    glue::glue("  node3a[label = <Group A (n = {n_GroupA})>]"),
    glue::glue("  node3b[label = <Group B (n = {n_GroupB})>]"),
    glue::glue("  node4a[label = <Completed Intervention (n = {n_Completed_Intervention_A})>]"),
    glue::glue("  node4b[label = <Completed Intervention (n = {n_Completed_Intervention_B})>]"),
    glue::glue("  node5a[label = <Completed Study (n = {n_Completed_Study_A})>]"),
    glue::glue("  node5b[label = <Completed Study (n = {n_Completed_Study_B})>]"),
    "",
    "  blank1[label = '', width = 0.01, height = 0.01]",
    glue::glue("  excluded1[label = {lbl_excl1}, width = 8, height = 1]")
  )

  if (has_pending1) {
    lines <- c(lines,
               "  blank2[label = '', width = 0.01, height = 0.01]",
               glue::glue("  excluded2[label = {lbl_pending1}, width = 8, height = 0.5]"))
  }

  lines <- c(lines,
             "  blank3[label = '', width = 0.01, height = 0.01]",
             glue::glue("  excluded3[label = {lbl_excl3}, width = 8, height = 1]"))

  if (has_pending_rand) {
    lines <- c(lines,
               "  blank4[label = '', width = 0.01, height = 0.01]",
               glue::glue("  excluded4[label = {lbl_pending_rand}, width = 8, height = 0.5]"))
  }

  # Group A Intervention
  lines <- c(lines, "  blank5[label = '', width = 0.01, height = 0.01]")
  if (show_exclusions_intervention_a) {
    lines <- c(lines, glue::glue("  excluded_intervention_a[label = {lbl_excluded_intervention_a}, width = 4, height = 0.8]"))
  }
  lines <- c(lines,
             "  blank7[label = '', width = 0.01, height = 0.01]",
             glue::glue("  active_intervention_a[label = {lbl_active_intervention_a}, width = 4, height = 0.5]"))

  # Group A Study
  lines <- c(lines, "  blank8[label = '', width = 0.01, height = 0.01]")
  if (show_exclusions_study_a) {
    lines <- c(lines, glue::glue("  excluded_study_a[label = {lbl_excluded_study_a}, width = 4, height = 0.8]"))
  }
  lines <- c(lines,
             "  blank9[label = '', width = 0.01, height = 0.01]",
             glue::glue("  active_study_a[label = {lbl_active_study_a}, width = 4, height = 0.5]"))

  # Group B Intervention
  lines <- c(lines, "  blank10[label = '', width = 0.01, height = 0.01]")
  if (show_exclusions_intervention_b) {
    lines <- c(lines, glue::glue("  excluded_intervention_b[label = {lbl_excluded_intervention_b}, width = 4, height = 0.8]"))
  }
  lines <- c(lines,
             "  blank11[label = '', width = 0.01, height = 0.01]",
             glue::glue("  active_intervention_b[label = {lbl_active_intervention_b}, width = 4, height = 0.5]"))

  # Group B Study
  lines <- c(lines, "  blank12[label = '', width = 0.01, height = 0.01]")
  if (show_exclusions_study_b) {
    lines <- c(lines, glue::glue("  excluded_study_b[label = {lbl_excluded_study_b}, width = 4, height = 0.8]"))
  }
  lines <- c(lines,
             "  blank13[label = '', width = 0.01, height = 0.01]",
             glue::glue("  active_study_b[label = {lbl_active_study_b}, width = 4, height = 0.5]"))

  # -------- Connections --------
  lines <- c(lines,
             "",
             "  // Main connections",
             "  node1 -> blank1[dir = none]",
             "  blank1 -> excluded1[minlen = 1]",
             "  { rank = same; blank1 excluded1 }")

  if (has_pending1) {
    lines <- c(lines,
               "  blank1 -> blank2[dir = none]",
               "  blank2 -> excluded2[minlen = 1]",
               "  { rank = same; blank2 excluded2 }",
               "  blank2 -> node2[dir = none]")
  } else {
    lines <- c(lines, "  blank1 -> node2[dir = none]")
  }

  lines <- c(lines,
             "  node2 -> blank3[dir = none]",
             "  blank3 -> excluded3[minlen = 1]",
             "  { rank = same; blank3 excluded3 }")

  if (has_pending_rand) {
    lines <- c(lines,
               "  blank3 -> blank4[dir = none]",
               "  blank4 -> excluded4[minlen = 1]",
               "  { rank = same; blank4 excluded4 }",
               "  blank4 -> node3[dir = none]")
  } else {
    lines <- c(lines, "  blank3 -> node3[dir = none]")
  }

  lines <- c(lines,
             "  node3 -> node3a",
             "  node3 -> node3b",

             # Group A Intervention
             "  node3a -> blank5[dir = none]")
  if (show_exclusions_intervention_a) {
    lines <- c(lines,
               "  blank5 -> excluded_intervention_a[minlen = 1]",
               "  { rank = same; blank5 excluded_intervention_a }",
               "  blank5 -> blank7[dir = none]")
  } else {
    lines <- c(lines, "  blank5 -> blank7[dir = none]")
  }

  lines <- c(lines,
             "  blank7 -> active_intervention_a[minlen = 1]",
             "  { rank = same; blank7 active_intervention_a }",
             "  blank7 -> node4a[dir = none]",

             # Group A Study
             "  node4a -> blank8[dir = none]")
  if (show_exclusions_study_a) {
    lines <- c(lines,
               "  blank8 -> excluded_study_a[minlen = 1]",
               "  { rank = same; blank8 excluded_study_a }",
               "  blank8 -> blank9[dir = none]")
  } else {
    lines <- c(lines, "  blank8 -> blank9[dir = none]")
  }

  lines <- c(lines,
             "  blank9 -> active_study_a[minlen = 1]",
             "  { rank = same; blank9 active_study_a }",
             "  blank9 -> node5a[dir = none]",

             # Group B Intervention
             "  node3b -> blank10[dir = none]")
  if (show_exclusions_intervention_b) {
    lines <- c(lines,
               "  blank10 -> excluded_intervention_b[minlen = 1]",
               "  { rank = same; blank10 excluded_intervention_b }",
               "  blank10 -> blank11[dir = none]")
  } else {
    lines <- c(lines, "  blank10 -> blank11[dir = none]")
  }

  lines <- c(lines,
             "  blank11 -> active_intervention_b[minlen = 1]",
             "  { rank = same; blank11 active_intervention_b }",
             "  blank11 -> node4b[dir = none]",

             # Group B Study
             "  node4b -> blank12[dir = none]")
  if (show_exclusions_study_b) {
    lines <- c(lines,
               "  blank12 -> excluded_study_b[minlen = 1]",
               "  { rank = same; blank12 excluded_study_b }",
               "  blank12 -> blank13[dir = none]")
  } else {
    lines <- c(lines, "  blank12 -> blank13[dir = none]")
  }

  lines <- c(lines,
             "  blank13 -> active_study_b[minlen = 1]",
             "  { rank = same; blank13 active_study_b }",
             "  blank13 -> node5b[dir = none]",
             "}")

  DiagrammeR::grViz(paste(lines, collapse = "\n"))
}




