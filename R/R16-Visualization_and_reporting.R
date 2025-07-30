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
  bullet_html <- function(x) {
    if (length(x) == 0 || is.null(x)) return("")
    paste0("• ", x, "<br align=\"left\"/>", collapse = "\n  ")
  }

  lbl_excl1 <- glue::glue("<<font point-size='18' align='left'>
  Excluded (n = {n_exclusion_1})<br align='left'/>
  {bullet_html(exclusion_reasons_1)}
</font>>")

  lbl_excl3 <- glue::glue("<<font point-size='18' align='left'>
  Excluded (n = {n_exclusion_3})<br align='left'/>
  {bullet_html(exclusion_reasons_3)}
</font>>")

  # Pending logic — show box if not NULL (even if 0)
  has_pending1 <- !is.null(n_pending_consent)
  if (has_pending1) {
    lbl_pending1 <- glue::glue("<<font point-size='18'>
  Pending (n = {n_pending_consent})<br align='left'/>
  {if(!is.null(pending_reasons)) bullet_html(pending_reasons) else ''}
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
    glue::glue("  node1[label = <Individuals assessed for eligibility (n = {total_assessed})>]"),
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

#' Generate CONSORT Flow Diagram (with Follow-up Phase)
#'
#' Creates a CONSORT-style flow diagram for projects that include both an
#' intervention phase and a follow-up phase. Uses Graphviz syntax via
#' \pkg{DiagrammeR}. The diagram shows the flow of participants through
#' assessment, consent, randomization, group allocation, intervention
#' completion, and follow-up completion, including boxes for pending cases and
#' attrition (withdrawn, lost to follow-up, terminated).
#'
#' @param total_assessed Integer. Total assessed for eligibility.
#' @param n_consented Integer. Number who provided consent.
#' @param n_randomized Integer. Number randomized.
#' @param n_GroupA Integer. Number allocated to Group A.
#' @param n_GroupB Integer. Number allocated to Group B.
#' @param n_Completed_Intervention_A Integer. Number who completed the intervention in Group A.
#' @param n_Completed_Intervention_B Integer. Number who completed the intervention in Group B.
#' @param n_Completed_Study_A Integer. Number who completed the study in Group A.
#' @param n_Completed_Study_B Integer. Number who completed the study in Group B.
#' @param n_rand_pending Integer or NULL. Pending randomization.
#' @param n_exclusion_1 Integer. Excluded during screening.
#' @param n_pending_consent Integer or NULL. Pending consent.
#' @param n_exclusion_3 Integer. Excluded post-consent.
#' @param exclusion_reasons_1 Character vector. Screening exclusion reasons.
#' @param pending_reasons Character vector or NULL. Reasons for pending consent.
#' @param exclusion_reasons_3 Character vector. Post-consent exclusion reasons.
#' @param withdrawn_intervention_a,b Integers (default 0). Withdrawn during intervention.
#' @param lost_to_followup_intervention_a,b Integers (default 0). Lost during intervention.
#' @param terminated_intervention_a,b Integers (default 0). Terminated during intervention.
#' @param withdrawn_study_a,b Integers (default 0). Withdrawn during follow-up.
#' @param lost_to_followup_study_a,b Integers (default 0). Lost during follow-up.
#' @param terminated_study_a,b Integers (default 0). Terminated during follow-up.
#'
#' @return A \code{DiagrammeR} graph object rendering the CONSORT diagram.
#'
#' @export
#' @importFrom glue glue
#' @importFrom DiagrammeR grViz
#'
#' @examples
#' camr_consort_diagram(
#'   total_assessed = 250, n_consented = 180, n_randomized = 150,
#'   n_GroupA = 75, n_GroupB = 75,
#'   n_Completed_Intervention_A = 60, n_Completed_Intervention_B = 58,
#'   n_Completed_Study_A = 52, n_Completed_Study_B = 50,
#'   n_rand_pending = 5, n_exclusion_1 = 40, n_pending_consent = 8, n_exclusion_3 = 15,
#'   exclusion_reasons_1 = c("Did not meet criteria (n=39)", "Other Reasons (n=1)"),
#'   pending_reasons = c("Waiting for consent (n=5)&nbsp;&nbsp;", "Still Screening (n=10)"),
#'   exclusion_reasons_3 = c("Screen fail (n=15)", "Withdrew consent (n=5)&nbsp;&nbsp;&nbsp;&nbsp;"),
#'   withdrawn_intervention_a = 3, lost_to_followup_intervention_a = 2, terminated_intervention_a = 1,
#'   withdrawn_intervention_b = 2, lost_to_followup_intervention_b = 3, terminated_intervention_b = 1,
#'   withdrawn_study_a = 2, lost_to_followup_study_a = 2, terminated_study_a = 1,
#'   withdrawn_study_b = 3, lost_to_followup_study_b = 1, terminated_study_b = 2
#' )
camr_consort_diagram <- function(
    total_assessed,
    n_consented,
    n_randomized,
    n_GroupA,
    n_GroupB,
    n_Completed_Intervention_A,
    n_Completed_Intervention_B,
    n_Completed_Study_A,
    n_Completed_Study_B,
    n_rand_pending = NULL,
    n_exclusion_1,
    n_pending_consent = NULL,
    n_exclusion_3,
    exclusion_reasons_1,
    pending_reasons = NULL,
    exclusion_reasons_3,
    withdrawn_intervention_a = 0,
    withdrawn_intervention_b = 0,
    lost_to_followup_intervention_a = 0,
    lost_to_followup_intervention_b = 0,
    terminated_intervention_a = 0,
    terminated_intervention_b = 0,
    withdrawn_study_a = 0,
    withdrawn_study_b = 0,
    lost_to_followup_study_a = 0,
    lost_to_followup_study_b = 0,
    terminated_study_a = 0,
    terminated_study_b = 0
) {
  # Input validation
  stopifnot(
    is.numeric(total_assessed), total_assessed >= 0,
    is.numeric(n_consented), n_consented >= 0,
    is.numeric(n_randomized), n_randomized >= 0,
    is.numeric(n_GroupA), n_GroupA >= 0,
    is.numeric(n_GroupB), n_GroupB >= 0,
    is.numeric(n_Completed_Intervention_A), n_Completed_Intervention_A >= 0,
    is.numeric(n_Completed_Intervention_B), n_Completed_Intervention_B >= 0,
    is.numeric(n_Completed_Study_A), n_Completed_Study_A >= 0,
    is.numeric(n_Completed_Study_B), n_Completed_Study_B >= 0,
    is.null(n_rand_pending) || (is.numeric(n_rand_pending) && n_rand_pending >= 0),
    is.numeric(n_exclusion_1), n_exclusion_1 >= 0,
    is.null(n_pending_consent) || (is.numeric(n_pending_consent) && n_pending_consent >= 0),
    is.numeric(n_exclusion_3), n_exclusion_3 >= 0,
    is.character(exclusion_reasons_1),
    is.null(pending_reasons) || is.character(pending_reasons),
    is.character(exclusion_reasons_3),
    is.numeric(withdrawn_intervention_a), withdrawn_intervention_a >= 0,
    is.numeric(withdrawn_intervention_b), withdrawn_intervention_b >= 0,
    is.numeric(lost_to_followup_intervention_a), lost_to_followup_intervention_a >= 0,
    is.numeric(lost_to_followup_intervention_b), lost_to_followup_intervention_b >= 0,
    is.numeric(terminated_intervention_a), terminated_intervention_a >= 0,
    is.numeric(terminated_intervention_b), terminated_intervention_b >= 0,
    is.numeric(withdrawn_study_a), withdrawn_study_a >= 0,
    is.numeric(withdrawn_study_b), withdrawn_study_b >= 0,
    is.numeric(lost_to_followup_study_a), lost_to_followup_study_a >= 0,
    is.numeric(lost_to_followup_study_b), lost_to_followup_study_b >= 0,
    is.numeric(terminated_study_a), terminated_study_a >= 0,
    is.numeric(terminated_study_b), terminated_study_b >= 0
  )

  # Derived counts
  active_intervention_a <- max(0, n_GroupA - n_Completed_Intervention_A -
                                 withdrawn_intervention_a - lost_to_followup_intervention_a - terminated_intervention_a)
  active_intervention_b <- max(0, n_GroupB - n_Completed_Intervention_B -
                                 withdrawn_intervention_b - lost_to_followup_intervention_b - terminated_intervention_b)

  active_study_a <- max(0, n_Completed_Intervention_A - n_Completed_Study_A -
                          withdrawn_study_a - lost_to_followup_study_a - terminated_study_a)
  active_study_b <- max(0, n_Completed_Intervention_B - n_Completed_Study_B -
                          withdrawn_study_b - lost_to_followup_study_b - terminated_study_b)

  bullet_html <- function(x) {
    if (length(x) == 0 || is.null(x)) return("")
    paste0("• ", x, "<br align='left'/>", collapse = "\n  ")
  }

  # Labels
  lbl_excl1 <- glue::glue("<<font point-size='18' align='left'>
  Excluded (n = {n_exclusion_1})<br align='left'/>
  {bullet_html(exclusion_reasons_1)}
</font>>")

  lbl_excl3 <- glue::glue("<<font point-size='18' align='left'>
  Excluded (n = {n_exclusion_3})<br align='left'/>
  {bullet_html(exclusion_reasons_3)}
</font>>")

  # Pending logic
  has_pending1 <- !is.null(n_pending_consent)
  if (has_pending1) {
    lbl_pending1 <- glue::glue("<<font point-size='18'>
  Pending (n = {n_pending_consent})<br align='left'/>
  {if(!is.null(pending_reasons)) bullet_html(pending_reasons) else ''}
</font>>")
  }

  has_pending_rand <- !is.null(n_rand_pending)
  if (has_pending_rand) {
    lbl_pending_rand <- glue::glue("<<font point-size='18'>
  Pending Randomization (n = {n_rand_pending})
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

  # Study Phase Exclusions
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

  # Build graph
  lines <- c(
    "digraph {",
    "  graph[splines = ortho, nodesep = 2]",
    "  node[fontname = Helvetica, shape = box, width = 6, height = 1, fontsize = 20]",
    "",
    glue::glue("  node1[label = <Individuals assessed for eligibility (n = {total_assessed})>]"),
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

  # Group A Intervention Phase
  lines <- c(lines,
             "  blank5[label = '', width = 0.01, height = 0.01]")
  if (show_exclusions_intervention_a) {
    lines <- c(lines,
               glue::glue("  excluded_intervention_a[label = {lbl_excluded_intervention_a}, width = 4, height = 0.8]"))
  }
  lines <- c(lines,
             "  blank7[label = '', width = 0.01, height = 0.01]",
             glue::glue("  active_intervention_a[label = {lbl_active_intervention_a}, width = 4, height = 0.5]"))

  # Group A Study Phase
  lines <- c(lines,
             "  blank8[label = '', width = 0.01, height = 0.01]")
  if (show_exclusions_study_a) {
    lines <- c(lines,
               glue::glue("  excluded_study_a[label = {lbl_excluded_study_a}, width = 4, height = 0.8]"))
  }
  lines <- c(lines,
             "  blank9[label = '', width = 0.01, height = 0.01]",
             glue::glue("  active_study_a[label = {lbl_active_study_a}, width = 4, height = 0.5]"))

  # Group B Intervention Phase
  lines <- c(lines,
             "  blank10[label = '', width = 0.01, height = 0.01]")
  if (show_exclusions_intervention_b) {
    lines <- c(lines,
               glue::glue("  excluded_intervention_b[label = {lbl_excluded_intervention_b}, width = 4, height = 0.8]"))
  }
  lines <- c(lines,
             "  blank11[label = '', width = 0.01, height = 0.01]",
             glue::glue("  active_intervention_b[label = {lbl_active_intervention_b}, width = 4, height = 0.5]"))

  # Group B Study Phase
  lines <- c(lines,
             "  blank12[label = '', width = 0.01, height = 0.01]")
  if (show_exclusions_study_b) {
    lines <- c(lines,
               glue::glue("  excluded_study_b[label = {lbl_excluded_study_b}, width = 4, height = 0.8]"))
  }
  lines <- c(lines,
             "  blank13[label = '', width = 0.01, height = 0.01]",
             glue::glue("  active_study_b[label = {lbl_active_study_b}, width = 4, height = 0.5]"))

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

             # Group A Intervention Phase
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

             # Group A Study Phase
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

             # Group B Intervention Phase
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

             # Group B Study Phase
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
