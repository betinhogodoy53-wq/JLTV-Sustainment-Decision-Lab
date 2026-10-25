# Auditoria campo a campo: muda UM campo, roda e compara com a execução de referência.
# Rode: Rscript tests_field_audit.R   (usa banco temporário; não abre navegador nem Google)
Sys.setenv(JLTV_DB_PATH = file.path(tempdir(), "audit.sqlite"), JLTV_SEED_SYNTHETIC = "1")
suppressWarnings(suppressMessages(library(shiny)))
app <- suppressWarnings(source("app.R", encoding = "UTF-8")$value)
fp <- function(x) {
  v <- unlist(rapply(x, function(z) if (is.numeric(z)) round(z, 6) else NULL, how = "unlist"))
  paste(length(v), format(sum(v * seq_along(v), na.rm = TRUE), digits = 12))
}
defaults <- list(
  target_ready = 8, simulation_horizon = 168, target_probability = 0.8, simulation_replications = 40,
  simulation_seed = 20260910, mission_utilization = 1, pm_initial_phase = "fresh", initial_unavailable = 0,
  mission_tolerance = 0, common_random_numbers = TRUE, failure_distribution = "weibull",
  failure_parameter_1 = 180, failure_parameter_2 = 1.2, mean_diagnosis = 2, mean_corrective = 8,
  mean_preventive = 4, mean_test = 1, duration_cv = 0.35, support_diag = 2, support_repair = 3,
  support_bays = 2, support_pm = 250, support_stock = 6, support_reorder_point = 2,
  support_reorder_qty = 6, support_lead = 24, support_lead_cv = 0.35, support_part_probability = 0.65,
  support_pm_part_probability = NA_real_,
  costs_complete = FALSE, diag_reference = "current", diag_fleet = 12,
  cost_asset = 100000, cost_diag = 12000, cost_repair = 18000, cost_bay = 8000, cost_part = 2500,
  cost_corrective = 1500, cost_preventive = 800, cost_downtime = 500,
  grid_fleet = "10:12", grid_diag = "2", grid_repair = "2:3", grid_bays = "2", grid_stock = "2,6",
  score_aggregation = "mean", only_consistent_ahp = TRUE, min_cell_coverage = 0)
# contexto em que a preventiva realmente ocorre
pm_ctx <- list(simulation_horizon = 1000, pm_initial_phase = "staggered", support_pm = 100)
tests <- list(
  list("target_ready", 11), list("diag_fleet", 16), list("simulation_horizon", 500),
  list("simulation_replications", 70), list("simulation_seed", 777), list("mission_utilization", 0.5),
  list("initial_unavailable", 3), list("mission_tolerance", 24), list("common_random_numbers", FALSE),
  list("failure_distribution", "exponential"), list("failure_parameter_1", 90), list("failure_parameter_2", 3),
  list("mean_diagnosis", 8), list("mean_corrective", 24), list("mean_test", 6), list("duration_cv", 1.2),
  list("support_diag", 1), list("support_repair", 1), list("support_bays", 1), list("support_stock", 1),
  list("support_reorder_point", 0), list("support_reorder_qty", 1), list("support_lead", 120),
  list("support_lead_cv", 1.5), list("support_part_probability", 1), list("costs_complete", TRUE),
  list("cost_downtime", 9000), list("cost_part", 90000), list("target_probability", 0.99),
  list("pm_initial_phase", "staggered", "pm"), list("support_pm", 40, "pm"), list("mean_preventive", 30, "pm"),
  list("support_pm_part_probability", 1, "pm"), list("mission_utilization", 0.3, "pm"),
  list("grid_fleet", "20"), list("grid_repair", "9"), list("grid_stock", "40"),
  list("score_aggregation", "median", "mc"), list("only_consistent_ahp", FALSE, "mc"), list("min_cell_coverage", 80, "mc"))
res <- data.frame(campo = character(), contexto = character(), mudou = logical(), stringsAsFactors = FALSE)
shiny::testServer(app, {
  vals <- defaults
  lev <- improvement_lever_catalog()
  for (i in seq_len(nrow(lev))) {
    vals[[paste0("lever_use_", lev$lever_id[i])]] <- TRUE
    vals[[paste0("lever_mag_", lev$lever_id[i])]] <- lev$default_magnitude[i]
  }
  do.call(session$setInputs, vals)
  counter <- 0L
  run_fp <- function(ctx = list(), mc = FALSE) {
    do.call(session$setInputs, c(ctx, list(run_improvements = (counter <<- counter + 1L))))
    a <- improvement_state()
    if (is.null(a)) return("ERRO: sem resultado")
    if (mc) { m <- recompute_multicriteria(FALSE); return(fp(list(m$ranking %||% m, m$weights %||% NULL))) }
    fp(list(a$base, a$levers, a$package))
  }
  base_default <- run_fp()
  base_mc <- fp({ m <- recompute_multicriteria(FALSE); list(m$ranking %||% m, m$weights %||% NULL) })
  base_pm <- run_fp(pm_ctx)
  for (t in tests) {
    f <- t[[1]]; v <- t[[2]]; kind <- if (length(t) > 2) t[[3]] else "sim"
    ctx_reset <- defaults[c(f, names(pm_ctx))]; ctx_reset <- ctx_reset[!vapply(ctx_reset, is.null, logical(1))]
    if (kind == "pm") {
      ctx <- pm_ctx; ctx[[f]] <- v
      out <- run_fp(ctx); changed <- !identical(out, base_pm)
      do.call(session$setInputs, defaults[names(pm_ctx)])
      session$setInputs(support_pm_part_probability = NA_real_, mission_utilization = 1)
    } else if (kind == "mc") {
      args <- list(v); names(args) <- f; do.call(session$setInputs, args)
      m <- recompute_multicriteria(FALSE)
      out <- fp(list(m$ranking %||% m, m$weights %||% NULL)); changed <- !identical(out, base_mc)
      args <- defaults[f]; do.call(session$setInputs, args)
    } else {
      args <- list(v); names(args) <- f; do.call(session$setInputs, args)
      out <- run_fp(); changed <- !identical(out, base_default)
      do.call(session$setInputs, defaults[f])
    }
    res[nrow(res) + 1L, ] <<- list(f, kind, changed)
    cat(sprintf("%-32s %-4s %s\n", f, kind, if (changed) "ALTERA o resultado" else "NAO ALTEROU"))
  }
})
cat("\n=== RESUMO ===\n")
print(res[!res$mudou, ])

