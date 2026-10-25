# Run beside app.R with: Rscript tests_quality_revision.R
# No user database or files are modified. Source the application's real core.

quality_previous_core <- Sys.getenv("JLTV_CORE_ONLY", unset = NA_character_)
Sys.setenv(JLTV_CORE_ONLY = "1")
quality_core <- new.env(parent = globalenv())
source(Sys.getenv("JLTV_APP_FILE", unset = "app.R"), local = quality_core, encoding = "UTF-8")
if (is.na(quality_previous_core)) Sys.unsetenv("JLTV_CORE_ONLY") else
  Sys.setenv(JLTV_CORE_ONLY = quality_previous_core)
quality_passed <- 0L
quality_failures <- character()
quality_assert <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}
quality_test <- function(name, expr) {
  tryCatch({
    force(expr)
    quality_passed <<- quality_passed + 1L
    cat("PASS:", name, "\n")
  }, error = function(e) {
    quality_failures <<- c(quality_failures, paste(name, conditionMessage(e), sep = ": "))
    cat("FAIL:", name, "-", conditionMessage(e), "\n")
  })
}
quality_scores <- data.frame(
  respondent_id = c("R1", "R2"), alternative_id = "A1", alternative_name = "Motor",
  subcriterion_id = "FREQUENCY", score = c(1, 9), data_source = "EMPIRICAL_TEST",
  stringsAsFactors = FALSE
)

quality_test("Distinct opinions preserve mean, median, and valid coverage", {
  quality_assert(quality_core$validate_topsis_scores_data(quality_scores)$valid,
    "One response from each of two evaluators is valid.")
  aggregated <- quality_core$aggregate_topsis_scores(quality_scores)
  quality_assert(nrow(aggregated) == 1L && aggregated$value == 5 &&
    aggregated$median == 5 && aggregated$n_valid == 2 && aggregated$coverage == 1,
    "Two distinct opinions 1 and 9 must yield 5 and 100% coverage.")
})

quality_test("P6 duplicate opinion is rejected instead of counted twice", {
  duplicated <- rbind(quality_scores, quality_scores[1, , drop = FALSE])
  validation <- quality_core$validate_topsis_scores_data(duplicated)
  quality_assert(!isTRUE(validation$valid) && grepl("duplicad", validation$message),
    "Repeated respondent/system/subcriterion must produce an explicit duplicate diagnostic.")
  aggregation <- tryCatch(quality_core$aggregate_topsis_scores(duplicated), error = identity)
  quality_assert(inherits(aggregation, "error"),
    "Duplicate records must not alter the mean from 5 to 3.667 or raise coverage to 150%.")
})

quality_test("Different scores or source labels do not make a duplicate key valid", {
  other <- quality_scores[1, , drop = FALSE]
  other$score <- 7
  other$data_source <- "OTHER_IMPORT"
  quality_assert(!isTRUE(quality_core$validate_topsis_scores_data(rbind(quality_scores, other))$valid),
    "The same evaluator's cell cannot receive multiple votes by changing score or source.")
  other$respondent_id <- " R1 "
  quality_assert(!isTRUE(quality_core$validate_topsis_scores_data(rbind(quality_scores, other))$valid),
    "Whitespace cannot bypass duplicate respondent identity.")
})

quality_test("CSV append checks duplicates against records already in the table", {
  # A fresh file is internally valid but repeats an existing observation.
  incoming <- quality_scores[1, , drop = FALSE]
  quality_assert(quality_core$validate_topsis_scores_data(incoming)$valid,
    "The one-row file is internally valid.")
  validation <- quality_core$validate_topsis_import_data(quality_scores, incoming, "append")
  quality_assert(!isTRUE(validation$valid),
    "Append of an existing key must be rejected before any database write.")
  incoming$respondent_id <- "R3"
  quality_assert(quality_core$validate_topsis_import_data(quality_scores, incoming, "append")$valid,
    "A new evaluator must remain addable.")
})

quality_test("CSV replacement permits clean revisions but rejects duplicates in the new file", {
  incoming <- quality_scores
  incoming$score <- c(3, 7)
  quality_assert(quality_core$validate_topsis_import_data(quality_scores, incoming, "replace")$valid,
    "Replacing a table may revise existing opinions with one record per key.")
  incoming <- rbind(incoming, incoming[1, , drop = FALSE])
  quality_assert(!isTRUE(quality_core$validate_topsis_import_data(quality_scores, incoming, "replace")$valid),
    "Replace must not introduce duplicate keys.")
})

quality_test("Duplicate diagnostic survives the complete MC computation", {
  raw <- quality_core$synthetic_questionnaire_demo_raw(3L, 20261003L)
  snapshot <- quality_core$transform_google_forms_questionnaire(raw, evidence_class = "SYNTHETIC")
  snapshot$topsis_scores <- rbind(snapshot$topsis_scores, snapshot$topsis_scores[1, , drop = FALSE])
  result <- quality_core$compute_multicriteria(snapshot, "mean", TRUE, "SYNTHETIC")
  quality_assert(!isTRUE(result$topsis$estimable) && grepl("duplicad", result$topsis$reason),
    "MC must explain duplicate P6 records rather than only say no evaluations are available.")
})

quality_test("Calibration and downtime share the RAM population of active vehicles", {
  snapshot <- quality_core$synthetic_demo_data()
  inactive_id <- snapshot$assets$asset_id[1]
  snapshot$assets$active[1] <- 0L
  expected_orders <- snapshot$work_orders[snapshot$work_orders$asset_id != inactive_id, , drop = FALSE]
  expected_delays <- snapshot$work_order_delays[
    snapshot$work_order_delays$work_order_id %in% expected_orders$work_order_id, , drop = FALSE]
  analysis <- quality_core$derive_analysis_inputs(snapshot, fit_models = FALSE)
  quality_assert(nrow(analysis$work_orders) == nrow(expected_orders) &&
    !any(analysis$work_orders$asset_id == inactive_id),
    "The work orders exposed to the calibration button must exclude inactive vehicles.")
  quality_assert(identical(analysis$delays$work_order_id, expected_delays$work_order_id),
    "Observed downtime must exclude delays attached to excluded work orders.")
  hours <- function(wo) as.numeric(difftime(
    quality_core$parse_datetime_utc(wo$active_repair_end_at),
    quality_core$parse_datetime_utc(wo$active_repair_start_at), units = "hours"))
  corrected_mean <- mean(hours(analysis$work_orders)[analysis$work_orders$maintenance_type == "CORRECTIVE"])
  expected_mean <- mean(hours(expected_orders)[expected_orders$maintenance_type == "CORRECTIVE"])
  quality_assert(abs(corrected_mean - expected_mean) < 1e-10,
    "Calibration means must come from the same active work orders as RAM.")
  expected_decomposition <- quality_core$observed_downtime_components(expected_orders, expected_delays)
  actual_decomposition <- quality_core$observed_downtime_components(analysis$work_orders, analysis$delays)
  quality_assert(isTRUE(all.equal(actual_decomposition, expected_decomposition)),
    "The observed stage decomposition must share the calibration population.")
  quality_assert(nrow(snapshot$work_orders) > nrow(analysis$work_orders) &&
    nrow(analysis$assets) == nrow(snapshot$assets),
    "Filtering the scientific population must not delete source data or original asset metadata.")
})

quality_test("Source quality is checked before excluding inactive vehicles", {
  snapshot <- quality_core$synthetic_demo_data()
  inactive_id <- snapshot$assets$asset_id[1]
  snapshot$assets$active[1] <- 0L
  bad_row <- which(snapshot$work_orders$asset_id == inactive_id)[1]
  snapshot$work_orders$active_repair_end_at[bad_row] <- "1900-01-01 00:00:00"
  result <- tryCatch(quality_core$derive_analysis_inputs(snapshot, fit_models = FALSE), error = identity)
  quality_assert(inherits(result, "error"),
    "An invalid inactive work order must still be diagnosed in the original source dataset.")
})

cat("QUALITY REVISION:", quality_passed, "passed;", length(quality_failures), "failed.\n")
if (length(quality_failures)) stop(paste(quality_failures, collapse = "\n"), call. = FALSE)
