# Run beside app.R: Rscript tests_context_revision.R
# Loads only the reusable harness declarations from tests_revision.R;
# does not rerun its six tests or open the application UI.
context_harness <- parse("tests_revision.R", encoding = "UTF-8")
for (context_expr in context_harness) {
  is_test <- is.call(context_expr) && identical(context_expr[[1]], as.name("revision_test"))
  if (is_test) {
    if (grepl("^Automatic MC", as.character(context_expr[[2]]))) break
    next
  }
  eval(context_expr, envir = globalenv())
}
rm(context_harness, context_expr)

revision_test("Paired decision comparison is invalidated when the mission horizon changes", {
  shiny::testServer(revision_make_server(), {
    do.call(session$setInputs, revision_defaults)
    session$setInputs(recompute_multicriteria = 1)
    session$setInputs(decision_target = "TOPSIS:A1", scenario_parameter = "mean_corrective",
      scenario_baseline = 8, scenario_alternative = 4,
      scenario_evidence_source = "Synthetic regression scenario; not measured JLTV evidence",
      scenario_notes = "Pairing by identical seeds", save_decision_link = 1)
    links <- snapshot_state()$decision_scenario_links
    revision_check(nrow(links) == 1L && links$active[1] == 1L,
      "A real active decision link must be saved before comparing.")
    session$setInputs(compare_link_paired = 1)
    paired <- paired_comparison_state()
    revision_check(!is.null(paired) && nrow(paired$comparison) == 3L &&
      identical(paired$link_id, links$link_id[1]),
      "The comparator must produce all three indicators for the saved decision link.")
    revision_check(identical(paired$base$replications$seed, paired$alternative$replications$seed),
      "Base and alternative must retain the same per-replication seeds.")
    session$setInputs(simulation_horizon = 48)
    revision_check(is.null(paired_comparison_state()),
      "The H=24 comparison must disappear after H=48 is selected.")
  })
})

revision_test("Changing the saved decision link invalidates the paired comparison", {
  shiny::testServer(revision_make_server(), {
    do.call(session$setInputs, revision_defaults)
    session$setInputs(recompute_multicriteria = 1)
    session$setInputs(decision_target = "TOPSIS:A1", scenario_parameter = "mean_corrective",
      scenario_baseline = 8, scenario_alternative = 4,
      scenario_evidence_source = "Synthetic regression link one", scenario_notes = "",
      save_decision_link = 1)
    session$setInputs(compare_link_paired = 1)
    paired <- paired_comparison_state()
    revision_check(!is.null(paired), "The first saved link must produce a comparison.")
    first_id <- paired$link_id
    session$setInputs(decision_target = "TOPSIS:A2", scenario_alternative = 2,
      scenario_evidence_source = "Synthetic regression link two", save_decision_link = 2)
    links <- snapshot_state()$decision_scenario_links
    active <- links[links$active == 1L, , drop = FALSE]
    revision_check(nrow(active) == 1L && active$link_id[1] != first_id &&
      active$target_id[1] == "A2" && active$alternative_value[1] == 2,
      "A second distinct link must be the sole active link.")
    revision_check(is.null(paired_comparison_state()),
      "A new saved link must not retain the old link's paired indicators.")
    session$setInputs(compare_link_paired = 2)
    revised <- paired_comparison_state()
    revision_check(!is.null(revised) && identical(revised$link_id, active$link_id[1]),
      "A new comparison must identify the new active link.")
  })
})

revision_test("Adding only a multicriteria audit row preserves a completed simulation", {
  shiny::testServer(revision_make_server(), {
    do.call(session$setInputs, revision_defaults)
    session$setInputs(recompute_multicriteria = 1, run_simulation = 1)
    completed <- simulation_state()
    revision_check(!is.null(completed), "The simulation must complete before the audit-only change.")
    signature <- completed$input_signature
    audit_before <- nrow(snapshot_state()$multicriteria_runs)
    source_before <- snapshot_state()$topsis_scores
    session$setInputs(recompute_multicriteria = 2)
    revision_check(nrow(snapshot_state()$multicriteria_runs) == audit_before + 1L,
      "The action must actually add an audited MC run, not merely reuse cached output.")
    revision_check(identical(source_before, snapshot_state()$topsis_scores),
      "No scientific source records changed during the audit action.")
    retained <- simulation_state()
    revision_check(!is.null(retained) && identical(retained$input_signature, signature) &&
      identical(retained$run_id, completed$run_id) && identical(retained$summary, completed$summary),
      "Audit db_version changes must preserve the simulation's identity and indicators.")
  })
})

revision_test("Report exports the complete frozen technical context including mission tolerance", {
  shiny::testServer(revision_make_server(), {
    settings <- modifyList(revision_defaults, list(
      simulation_horizon = 48, simulation_replications = 8,
      mission_tolerance = 2, mission_utilization = .5, initial_unavailable = 1,
      pm_initial_phase = "staggered", support_pm = 72,
      support_lead_cv = .6, support_pm_part_probability = .25,
      cost_asset = 1000, cost_diag = 100, cost_repair = 200, cost_bay = 50,
      cost_part = 25, cost_corrective = 10, cost_preventive = 5, cost_downtime = 2
    ))
    do.call(session$setInputs, settings)
    session$setInputs(run_simulation = 1)
    completed <- simulation_state()
    revision_check(!is.null(completed) && completed$base_config$mission_tolerance_hours == 2,
      "The scenario must execute with an explicit two-hour mission tolerance.")
    captured <- completed$input_settings
    revision_check(is.list(captured) && length(captured) >= 30L,
      "The result must capture the technical settings needed to reproduce it.")
    # A presentation-only selector must not discard valid scientific output.
    session$setInputs(questionnaire_preview_choice = "questionnaire_respondents")
    revision_check(!is.null(simulation_state()),
      "Changing a data-preview selector is not a new scientific scenario.")
    report <- assemble_report()
    values <- stats::setNames(as.character(report$assumptions$Valor), report$assumptions$Item)
    technical_keys <- paste0("Parâmetro técnico: ", names(captured))
    revision_check(all(technical_keys %in% names(values)) && !anyDuplicated(report$assumptions$Item),
      "Every captured technical setting must appear exactly once in the report.")
    expected <- c(mission_tolerance = "2", mission_utilization = "0.5", initial_unavailable = "1",
      pm_initial_phase = "staggered", support_lead_cv = "0.6",
      support_pm_part_probability = "0.25", cost_part = "25", costs_complete = "FALSE",
      simulation_seed = "21", simulation_horizon = "48", common_random_numbers = "TRUE")
    actual <- unname(values[paste0("Parâmetro técnico: ", names(expected))])
    revision_check(identical(actual, unname(expected)),
      "Tolerance, utilization, PM, supply variability/probability, cost governance and seed must be exported faithfully.")
    revision_check(identical(values[["Assinatura SHA256 das entradas"]], completed$input_signature),
      "The exported source signature must identify this completed scenario.")
    revision_check(identical(report$simulation$summary, completed$summary),
      "Exported simulation metrics must belong to the captured technical context.")
    # Verify the actual Excel artifact preserves the technical context, too.
    xlsx <- tempfile("jltv_context_", fileext = ".xlsx")
    revision_core$write_assessment_workbook(xlsx, report)
    sheet <- openxlsx::read.xlsx(xlsx, sheet = "Premissas", startRow = 4)
    tolerance_row <- which(sheet[[1]] == "Parâmetro técnico: mission_tolerance")
    revision_check(length(tolerance_row) == 1L && as.numeric(sheet[[2]][tolerance_row]) == 2,
      "The Excel Premissas sheet must contain the executed mission tolerance.")
    unlink(xlsx)
  })
})

# Optional exact motor equivalence against the preserved pre-revision app.
# The comparison covers seeded numerical outputs and detailed event/state logs;
# it does not compare version metadata or UI text.
context_original_file <- Sys.getenv("JLTV_ORIGINAL_APP_FILE", unset = file.path("..", "original", "app.R"))
if (file.exists(context_original_file)) {
  revision_test("Original and revised simulation motors preserve seeded events and numerical outcomes", {
    previous_core <- Sys.getenv("JLTV_CORE_ONLY", unset = NA_character_)
    Sys.setenv(JLTV_CORE_ONLY = "1")
    original <- new.env(parent = globalenv())
    source(context_original_file, local = original, encoding = "UTF-8")
    if (is.na(previous_core)) Sys.unsetenv("JLTV_CORE_ONLY") else Sys.setenv(JLTV_CORE_ONLY = previous_core)
    config <- list(fleet_size = 6L, diagnostic_teams = 2L, repair_teams = 2L, bays = 2L,
      initial_stock = 3L, reorder_point = 1L, reorder_qty = 3L, pm_interval = 120,
      utilization = .5, stagger_pm = TRUE, initial_unavailable = 1L,
      mission_tolerance_hours = 2, asset_cost_horizon = NA_real_,
      diagnostic_team_cost_horizon = NA_real_, repair_team_cost_horizon = NA_real_,
      bay_cost_horizon = NA_real_, part_unit_cost = NA_real_,
      corrective_event_cost = NA_real_, preventive_event_cost = NA_real_, downtime_hour_cost = NA_real_)
    L <- revision_core$mean_cv_lognormal_model
    parameters <- list(life_model = list(type = "exponential", rate = 1/180),
      diagnosis_model = L(2, .35), corrective_model = L(8, .35),
      preventive_model = L(4, .35), test_model = L(1, .35),
      lead_time_model = L(24, .6), part_required_probability = .65,
      pm_part_required_probability = .25)
    for (seed in c(7L, 21L, 20261003L)) {
      before <- original$simulate_replication(config, parameters, 336, 4L, seed, keep_logs = TRUE)
      after <- revision_core$simulate_replication(config, parameters, 336, 4L, seed, keep_logs = TRUE)
      revision_check(isTRUE(all.equal(before, after, tolerance = 0)),
        paste("Physical motor changed its metrics or detailed events for seed", seed))
    }
    before <- original$evaluate_configuration(config, parameters, 336, 4L, 10L, 21L)
    after <- revision_core$evaluate_configuration(config, parameters, 336, 4L, 10L, 21L)
    revision_check(isTRUE(all.equal(before, after, tolerance = 0)),
      "Replications, mission Wilson interval and summary must remain exactly equivalent.")
  })
} else {
  cat("SKIP: original motor comparison (set JLTV_ORIGINAL_APP_FILE to enable).\n")
}

unlink(c(revision_temp_paths, paste0(revision_temp_paths, "-wal"), paste0(revision_temp_paths, "-shm")))
cat("CONTEXT REVISION:", revision_passed, "passed;", length(revision_failed), "failed.\n")
if (length(revision_failed)) stop(paste(revision_failed, collapse = "\n"), call. = FALSE)
