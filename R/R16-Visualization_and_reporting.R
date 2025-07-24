# Visualization and reporting
# Written by...
#   Luwei Liu
# Maintained by...
#   Luwei Liu
# Email:
#   lliu48@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated: 2025-07-24



#### 1.0) CONSORT Diagram Generator ####
#' Generate CONSORT Flow Diagram
#'
#' Creates a CONSORT-style flow diagram using Graphviz syntax via the DiagrammeR package.
#' The diagram visually represents the flow of participants through different stages of a study,
#' including assessment, consent,randomization, group allocation (Group A and Group B),  
#' and completion, with optional boxes for lost to follow-up, and pending cases.
#'
#' @param total_assessed Total number of individuals assessed for eligibility (integer)
#' @param n_consented Number of individuals who provided consent (integer)
#' @param n_randomized Number of individuals randomized (integer)
#' @param n_GroupA Number allocated to Group A (integer)
#' @param n_GroupB Number allocated to Group B (integer)
#' @param n_Completed_A Number who completed the study in Group A (integer)
#' @param n_Completed_B Number who completed the study in Group B (integer)
#' @param n_rand_pending Number pending randomization (integer)
#' @param n_exclusion_1 Number excluded during screening (integer)
#' @param n_pending_consent Number pending consent (integer)
#' @param n_exclusion_3 Number excluded post-consent (integer)
#' @param exclusion_reasons_1 Character vector of screening exclusion reasons
#' @param pending_reasons Character vector of reasons for pending cases
#' @param exclusion_reasons_3 Character vector of post-consent exclusion reasons
#' @param withdrawn_a Number withdrawn from Group A (default = 0)
#' @param withdrawn_b Number withdrawn from Group B (default = 0)
#' @param lost_to_followup_a Number lost to follow-up in Group A (default = 0)
#' @param lost_to_followup_b Number lost to follow-up in Group B (default = 0)
#'
#' @return A DiagrammeR graph object visualizing the CONSORT flow diagram
#' 
#' @details 
#' This function generates a standardized CONSORT (Consolidated Standards of Reporting Trials) 
#' diagram commonly used in DSMB reports or project updates to document participant flow.
#' Key features include:
#' \itemize{
#'   \item Automatic calculation of active participants in each group
#'   \item Dynamic display of exclusion reasons with bullet points
#'   \item Conditional display of boxes based on participant counts
#'   \item Formatting with consistent styling
#' }
#'
#' @examples
#' # 1) Basic example - no pending cases, no attrition 
#' camr_consort_diagram(
#'   total_assessed    = 200,
#'   n_consented       = 170,  
#'   n_randomized      = 150,  
#'   n_GroupA          = 75,
#'   n_GroupB          = 75,
#'   n_Completed_A     = 70,
#'   n_Completed_B     = 75,
#'   n_rand_pending    = 0,
#'   n_exclusion_1     = 30,   
#'   n_pending_consent = 0,
#'   n_exclusion_3     = 20,  
#'   exclusion_reasons_1 = c("Did not meet criteria (n=25)", "Other reasons (n=5)"),
#'   pending_reasons     = NULL,
#'   exclusion_reasons_3 = c("Screen fail (n=15)", "Withdrew consent (n=5)&nbsp;&nbsp;&nbsp;") #sometimes adding empty space helps with alignment.
#' )
#'
#' # 2) With pending cases only 
#' camr_consort_diagram(
#'   total_assessed    = 200,
#'   n_consented       = 150,  
#'   n_randomized      = 130,  
#'   n_GroupA          = 65,
#'   n_GroupB          = 65,
#'   n_Completed_A     = 65,
#'   n_Completed_B     = 65,
#'   n_rand_pending    = 5,
#'   n_exclusion_1     = 30,
#'   n_pending_consent = 20,   
#'   n_exclusion_3     = 20,
#'   exclusion_reasons_1 = c("Did not meet criteria (n=25)", "Other reasons (n=5)"),
#'   pending_reasons     = c("Still screening (n=15)", "Pending consent (n=5)&nbsp;&nbsp;&nbsp;"),
#'   exclusion_reasons_3 = c("Screen fail (n=15)", "Withdrew consent (n=5)&nbsp;&nbsp;&nbsp;")
#' )
#'
#' # 3) With attrition only 
#' camr_consort_diagram(
#'   total_assessed    = 200,
#'   n_consented       = 170,
#'   n_randomized      = 150,
#'   n_GroupA          = 75,
#'   n_GroupB          = 75,
#'   n_Completed_A     = 65,   
#'   n_Completed_B     = 70,   
#'   n_rand_pending    = 0,
#'   n_exclusion_1     = 30,
#'   n_pending_consent = 0,
#'   n_exclusion_3     = 20,
#'   exclusion_reasons_1 = c("Did not meet criteria (n=25)", "Other reasons (n=5)"),
#'   pending_reasons     = NULL,
#'   exclusion_reasons_3 = c("Screen fail (n=15)", "Withdrew consent (n=5)&nbsp;&nbsp;&nbsp;"),
#'   withdrawn_a       = 3,
#'   withdrawn_b       = 2,
#'   lost_to_followup_a = 2,
#'   lost_to_followup_b = 3
#' )
#'
#' # 4) Complete example with both pending cases and attrition 
#' camr_consort_diagram(
#'   total_assessed    = 200,
#'   n_consented       = 150,  
#'   n_randomized      = 130,  
#'   n_GroupA          = 65,
#'   n_GroupB          = 65,
#'   n_Completed_A     = 55,   
#'   n_Completed_B     = 60,   
#'   n_rand_pending    = 5,
#'   n_exclusion_1     = 30,
#'   n_pending_consent = 20,
#'   n_exclusion_3     = 20,
#'   exclusion_reasons_1 = c("Did not meet criteria (n=25)", "Other reasons (n=5)"),
#'   pending_reasons     = c("Still screening (n=15)", "Pending consent (n=5)&nbsp;&nbsp;&nbsp;"),
#'   exclusion_reasons_3 = c("Screen fail (n=15)", "Withdrew consent (n=5)&nbsp;&nbsp;&nbsp;"),
#'   withdrawn_a       = 3,
#'   withdrawn_b       = 2,
#'   lost_to_followup_a = 2,
#'   lost_to_followup_b = 3
#' )
#' @seealso \code{\link[DiagrammeR]{grViz}} for the underlying graphing function
#' @export
camr_consort_diagram <- function(
    total_assessed,
    n_consented,
    n_randomized,
    n_GroupA,
    n_GroupB,
    n_Completed_A,
    n_Completed_B,
    n_rand_pending,
    n_exclusion_1,
    n_pending_consent,
    n_exclusion_3,
    exclusion_reasons_1,
    pending_reasons,
    exclusion_reasons_3,
    withdrawn_a = 0,
    withdrawn_b = 0,
    lost_to_followup_a = 0,
    lost_to_followup_b = 0
) {
  # Derived counts
  active_a <- max(0, n_GroupA - n_Completed_A - withdrawn_a - lost_to_followup_a)
  active_b <- max(0, n_GroupB - n_Completed_B - withdrawn_b - lost_to_followup_b)
  
  bullet_html <- function(x) {
    if (length(x) == 0) return("")
    paste0("• ", x, "<br align='left'/>", collapse = "\n  ")
  }
  
  lbl_excl1 <- glue::glue("<<font point-size='18' align='left'>
  Excluded (n = {n_exclusion_1})<br align='left'/>
  {bullet_html(exclusion_reasons_1)}
</font>>")
  
  lbl_excl3 <- glue::glue("<<font point-size='18' align='left'>
  Excluded (n = {n_exclusion_3})<br align='left'/>
  {bullet_html(exclusion_reasons_3)}
</font>>")
  
  # Pending
  has_pending1 <- n_pending_consent > 0
  if (has_pending1) {
    lbl_pending1 <- glue::glue("<<font point-size='18' align='left'>
  Pending (n = {n_pending_consent})<br align='left'/>
  {bullet_html(pending_reasons)}
</font>>")
  }
  
  has_pending_rand <- n_rand_pending > 0
  if (has_pending_rand) {
    lbl_pending_rand <- glue::glue("<<font point-size='18'>
  Pending Randomization (n = {n_rand_pending})
</font>>")
  }
  
  # Withdrawn / LTFU
  show_withdrawn <- withdrawn_a > 0 | withdrawn_b > 0
  show_ltfu <- lost_to_followup_a > 0 | lost_to_followup_b > 0
  
  lbl_withdrawn_a <- glue::glue("<<font point-size='18'>Withdrawn (n = {withdrawn_a})</font>>")
  lbl_withdrawn_b <- glue::glue("<<font point-size='18'>Withdrawn (n = {withdrawn_b})</font>>")
  lbl_ltfu_a <- glue::glue("<<font point-size='18'>Lost to Follow-Up (n = {lost_to_followup_a})</font>>")
  lbl_ltfu_b <- glue::glue("<<font point-size='18'>Lost to Follow-Up (n = {lost_to_followup_b})</font>>")
  lbl_active_a <- glue::glue("<<font point-size='18'>Active (n = {active_a})</font>>")
  lbl_active_b <- glue::glue("<<font point-size='18'>Active (n = {active_b})</font>>")
  
  # Start building graph
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
               glue::glue("  excluded2[label = {lbl_pending1}, width = 8, height = 0.3]")
    )
  }
  
  lines <- c(lines,
             "  blank3[label = '', width = 0.01, height = 0.01]",
             glue::glue("  excluded3[label = {lbl_excl3}, width = 8, height = 1]")
  )
  
  if (has_pending_rand) {
    lines <- c(lines,
               "  blank4[label = '', width = 0.01, height = 0.01]",
               glue::glue("  excluded4[label = {lbl_pending_rand}, width = 8, height = 0.5]")
    )
  }
  
  # Group A boxes
  lines <- c(lines,
             "  blank5[label = '', width = 0.01, height = 0.01]",
             glue::glue("  withdrawnA[label = {lbl_withdrawn_a}, width = 3.5, height = 0.5]")
  )
  
  if (show_ltfu) {
    lines <- c(lines,
               "  blank6[label = '', width = 0.01, height = 0.01]",
               glue::glue("  ltfuA[label = {lbl_ltfu_a}, width = 3.5, height = 0.5]")
    )
  }
  
  lines <- c(lines,
             "  blank7[label = '', width = 0.01, height = 0.01]",
             glue::glue("  activeA[label = {lbl_active_a}, width = 3.5, height = 0.5]")
  )
  
  # Group B boxes
  lines <- c(lines,
             "  blank8[label = '', width = 0.01, height = 0.01]",
             glue::glue("  withdrawnB[label = {lbl_withdrawn_b}, width = 3.5, height = 0.5]")
  )
  
  if (show_ltfu) {
    lines <- c(lines,
               "  blank9[label = '', width = 0.01, height = 0.01]",
               glue::glue("  ltfuB[label = {lbl_ltfu_b}, width = 3.5, height = 0.5]")
    )
  }
  
  lines <- c(lines,
             "  blank10[label = '', width = 0.01, height = 0.01]",
             glue::glue("  activeB[label = {lbl_active_b}, width = 3.5, height = 0.5]")
  )
  
  # Connections
  lines <- c(lines,
             "",
             "  // Main connections",
             "  node1 -> blank1[dir = none]",
             "  blank1 -> excluded1[minlen = 1]",
             "  { rank = same; blank1 excluded1 }"
  )
  
  if (has_pending1) {
    lines <- c(lines,
               "  blank1 -> blank2[dir = none]",
               "  blank2 -> excluded2[minlen = 1]",
               "  { rank = same; blank2 excluded2 }",
               "  blank2 -> node2[dir = none]"
    )
  } else {
    lines <- c(lines, "  blank1 -> node2[dir = none]")
  }
  
  lines <- c(lines,
             "  node2 -> blank3[dir = none]",
             "  blank3 -> excluded3[minlen = 1]",
             "  { rank = same; blank3 excluded3 }"
  )
  
  if (has_pending_rand) {
    lines <- c(lines,
               "  blank3 -> blank4[dir = none]",
               "  blank4 -> excluded4[minlen = 1]",
               "  { rank = same; blank4 excluded4 }",
               "  blank4 -> node3[dir = none]"
    )
  } else {
    lines <- c(lines, "  blank3 -> node3[dir = none]")
  }
  
  lines <- c(lines, 
             "  node3 -> node3a", 
             "  node3 -> node3b",
             
             # Group A connections
             "  node3a -> blank5[dir = none]",
             "  blank5 -> withdrawnA[minlen = 1]",
             "  { rank = same; blank5 withdrawnA }"
  )
  
  if (show_ltfu) {
    lines <- c(lines,
               "  blank5 -> blank6[dir = none]",
               "  blank6 -> ltfuA[minlen = 1]",
               "  { rank = same; blank6 ltfuA }",
               "  blank6 -> blank7[dir = none]"
    )
  } else {
    lines <- c(lines,
               "  blank5 -> blank7[dir = none]"
    )
  }
  
  lines <- c(lines,
             "  blank7 -> activeA[minlen = 1]",
             "  { rank = same; blank7 activeA }",
             "  blank7 -> node4a[dir = none]",
             
             # Group B connections
             "  node3b -> blank8[dir = none]",
             "  blank8 -> withdrawnB[minlen = 1]",
             "  { rank = same; blank8 withdrawnB }"
  )
  
  if (show_ltfu) {
    lines <- c(lines,
               "  blank8 -> blank9[dir = none]",
               "  blank9 -> ltfuB[minlen = 1]",
               "  { rank = same; blank9 ltfuB }",
               "  blank9 -> blank10[dir = none]"
    )
  } else {
    lines <- c(lines,
               "  blank8 -> blank10[dir = none]"
    )
  }
  
  lines <- c(lines,
             "  blank10 -> activeB[minlen = 1]",
             "  { rank = same; blank10 activeB }",
             "  blank10 -> node4b[dir = none]",
             "}"
  )
  
  DiagrammeR::grViz(paste(lines, collapse = "\n"))
}