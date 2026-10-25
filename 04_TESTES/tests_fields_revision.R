# v1.5.7 — os campos do Passo 3 realmente alteram o resultado? Rode: Rscript tests_fields_revision.R
# Usa um banco temporário e o servidor Shiny simulado (testServer); nunca abre navegador nem Google.
Sys.setenv(JLTV_DB_PATH = file.path(tempdir(), "fields.sqlite"), JLTV_SEED_SYNTHETIC = "1")
suppressWarnings(suppressMessages(library(shiny)))
app <- suppressWarnings(source("app.R", encoding = "UTF-8")$value)
failed <- character()
check <- function(ok, msg) { if (isTRUE(ok)) cat("PASS:", msg, "\n") else { cat("FAIL:", msg, "\n"); failed <<- c(failed, msg) } }
defaults <- list(
  target_ready = 8, simulation_horizon = 168, target_probability = 0.8, simulation_replications = 50,
  simulation_seed = 20260910, mission_utilization = 1, pm_initial_phase = "fresh", initial_unavailable = 0,
  mission_tolerance = 0, common_random_numbers = TRUE, failure_distribution = "weibull",
  failure_parameter_1 = 180, failure_parameter_2 = 1.2, mean_diagnosis = 2, mean_corrective = 8,
  mean_preventive = 4, mean_test = 1, duration_cv = 0.35, support_diag = 2, support_repair = 3,
  support_bays = 2, support_pm = 250, support_stock = 6, support_reorder_point = 2,
  support_reorder_qty = 6, support_lead = 24, support_lead_cv = 0.35, support_part_probability = 0.65,
  costs_complete = FALSE, diag_reference = "current", diag_fleet = 12,
  grid_fleet = "10:12", grid_diag = "2", grid_repair = "2:3", grid_bays = "2", grid_stock = "2,6")
shiny::testServer(app, {
  vals <- defaults
  lev <- improvement_lever_catalog()
  for (i in seq_len(nrow(lev))) {
    vals[[paste0("lever_use_", lev$lever_id[i])]] <- TRUE
    vals[[paste0("lever_mag_", lev$lever_id[i])]] <- lev$default_magnitude[i]
  }
  do.call(session$setInputs, vals)
  session$setInputs(run_improvements = 1)
  a <- improvement_state()
  check(!is.null(a) && a$target_ready == 8 && a$config$fleet_size == 12, "execução padrão: M = 8 e frota = 12")
  session$setInputs(target_ready = 10)
  check(is.null(improvement_state()), "mudar a meta M retira o resultado antigo (não deixa número desatualizado)")
  session$setInputs(run_improvements = 2)
  b <- improvement_state()
  check(b$target_ready == 10 && b$base$success_probability[[1]] < a$base$success_probability[[1]],
        "meta M = 10 reduz a chance de cumprir a missão")
  session$setInputs(diag_fleet = 14, support_repair = 1, support_bays = 1, support_stock = 3)
  session$setInputs(run_improvements = 3)
  d <- improvement_state()
  check(d$config$fleet_size == 14 && d$config$repair_teams == 1 && d$config$bays == 1 && d$config$initial_stock == 3,
        "os campos da situação atual (frota, reparo, boxes, estoque) chegam ao resultado")
  check(abs(d$base$mean_availability[[1]] - b$base$mean_availability[[1]]) > 1e-6,
        "mudar equipe/box/estoque altera a disponibilidade simulada")
  session$setInputs(grid_fleet = "20", grid_repair = "6", grid_stock = "50")
  session$setInputs(run_improvements = 4)
  e <- improvement_state()
  check(identical(e$config$fleet_size, d$config$fleet_size) && isTRUE(all.equal(e$base$mean_availability[[1]], d$base$mean_availability[[1]])),
        "a grade de dimensionamento não altera a situação atual (por desenho)")
  session$setInputs(failure_parameter_1 = 400)
  session$setInputs(run_improvements = 5)
  f <- improvement_state()
  check(f$base$mean_availability[[1]] > d$base$mean_availability[[1]], "MTBF maior (escala 400 h) aumenta a disponibilidade")
  session$setInputs(mean_corrective = 30)
  session$setInputs(run_improvements = 6)
  g <- improvement_state()
  check(g$base$mean_availability[[1]] < f$base$mean_availability[[1]], "reparo corretivo mais longo reduz a disponibilidade")
})
if (length(failed)) stop(paste(failed, collapse = "\n"), call. = FALSE)
cat("FIELDS: todos os testes passaram\n")
