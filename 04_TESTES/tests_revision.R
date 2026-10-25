# Regression tests for scientific/result consistency. Run beside app.R:
# Rscript tests_revision.R
# Uses isolated temporary SQLite databases and never opens Google or Excel.
# Failures are collected so the original version exposes more than one defect.

revision_app_file <- Sys.getenv("JLTV_APP_FILE", unset = "app.R")
revision_previous_core <- Sys.getenv("JLTV_CORE_ONLY", unset = NA_character_)
Sys.setenv(JLTV_CORE_ONLY = "1")
revision_core <- new.env(parent = globalenv())
source(revision_app_file, local = revision_core, encoding = "UTF-8")
if (is.na(revision_previous_core)) Sys.unsetenv("JLTV_CORE_ONLY") else
  Sys.setenv(JLTV_CORE_ONLY = revision_previous_core)

revision_passed <- 0L
revision_failed <- character()
revision_check <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}
revision_test <- function(name, code) {
  if (!grepl(Sys.getenv("JLTV_REVISION_FILTER", ".*"), name)) return(invisible(NULL))
  tryCatch({
    force(code)
    revision_passed <<- revision_passed + 1L
    cat("PASS:", name, "\n")
  }, error = function(e) {
    revision_failed <<- c(revision_failed, paste(name, conditionMessage(e), sep = ": "))
    cat("FAIL:", name, "-", conditionMessage(e), "\n")
  })
}

revision_test("TOPSIS identical alternatives cannot yield an estimable NaN ranking", {
  subcriteria <- unlist(lapply(revision_core$questionnaire_hierarchy_spec()[-1], names),
                       use.names = FALSE)
  ratings <- expand.grid(alternative_id = c("A1", "A2"), subcriterion_id = subcriteria,
                         stringsAsFactors = FALSE)
  ratings$alternative_name <- ratings$alternative_id
  ratings$value <- 5
  weights <- data.frame(subcriterion_id = subcriteria, global_weight = rep(.1, 10))
  result <- revision_core$topsis_rank(ratings, weights)
  revision_check(!isTRUE(result$estimable) ||
    (all(is.finite(result$ranking$coefficient)) &&
     length(unique(result$ranking$coefficient)) == 1L),
    "All-equal scores must report no discrimination or a finite explicit tie; 0/0 is invalid.")
})

revision_test("Mission plot respects tolerated mission_success and splits equal minima", {
  runs <- data.frame(min_ready = c(7L, 7L, 8L),
                     mission_success = c(TRUE, FALSE, TRUE),
                     hours_below_target = c(1, 4, 0))
  chart <- revision_core$chart_mission_outcome(runs, 8L, titles = TRUE)
  plotted <- chart$data
  success <- if ("mission_success" %in% names(plotted)) {
    as.logical(plotted$mission_success)
  } else {
    as.character(plotted$outcome) == "Meta mantida"
  }
  revision_check(abs(sum(plotted$share[success]) - 2/3) < 1e-10,
    "Two of three actual mission outcomes succeeded, despite one tolerated minimum below M.")
  at_seven <- plotted[as.integer(as.character(plotted$min_ready)) == 7L, , drop = FALSE]
  revision_check(nrow(at_seven) == 2L,
    "Equal minimum readiness can contain both a tolerated success and a failure.")
  invisible(ggplot2::ggplot_build(chart))
})

# Extract the actual server function from the parsed application rather than
# sourcing the runtime UI or reimplementing server behavior in the test.
revision_find_server <- function(node) {
  if (is.expression(node)) {
    for (i in seq_along(node)) {
      found <- revision_find_server(node[[i]])
      if (!is.null(found)) return(found)
    }
    return(NULL)
  }
  if (!is.call(node)) return(NULL)
  operator <- as.character(node[[1]])[1]
  if (operator %in% c("=", "<-") && length(node) == 3L &&
      identical(node[[2]], as.name("server")) && is.call(node[[3]]) &&
      identical(node[[3]][[1]], as.name("function"))) return(node[[3]])
  # Only visit statement containers. Avoid evaluating missing subscript
  # arguments or walking through unrelated function implementations.
  indices <- if (operator == "{") seq.int(2L, length(node)) else
    if (operator == "if") seq.int(3L, length(node)) else integer()
  for (i in indices) {
    found <- revision_find_server(node[[i]])
    if (!is.null(found)) return(found)
  }
  NULL
}
revision_server_expr <- revision_find_server(parse(revision_app_file, encoding = "UTF-8"))
if (is.null(revision_server_expr)) stop("Application server function not found.", call. = FALSE)

revision_fixture <- function() {
  respondents <- paste0("REG-", 1:3)
  source_class <- "SYNTHETIC_REVISION_TEST"
  spec <- revision_core$questionnaire_hierarchy_spec()
  ahp <- do.call(rbind, lapply(names(spec), function(group) {
    pairs <- utils::combn(names(spec[[group]]), 2)
    do.call(rbind, lapply(respondents, function(id) data.frame(
      respondent_id = id, hierarchy_group = group,
      left_item = pairs[1, ], right_item = pairs[2, ], selected_side = "EQUAL",
      intensity = 1L, data_source = source_class, stringsAsFactors = FALSE)))
  }))
  subcriteria <- unlist(lapply(spec[-1], names), use.names = FALSE)
  ratings <- expand.grid(respondent_id = respondents, alternative_id = paste0("A", 1:10),
                         subcriterion_id = subcriteria, stringsAsFactors = FALSE)
  ratings$alternative_name <- unname(revision_core$questionnaire_alternative_spec()[ratings$alternative_id])
  ratings$score <- c(1, 1, 5)[match(ratings$respondent_id, respondents)] +
    as.integer(sub("A", "", ratings$alternative_id)) / 10
  ratings$data_source <- source_class
  list(questionnaire_respondents = data.frame(
    respondent_id = respondents, age = 30, service_years = 10,
    academic_background = "Test", military_background = "Test",
    direct_jltv_experience = 0L, experience_area = "Test", data_source = source_class,
    stringsAsFactors = FALSE), ahp_pairwise = ahp, topsis_scores = ratings)
}
revision_temp_paths <- character()
revision_make_server <- function() {
  application <- new.env(parent = revision_core)
  application$database_path <- tempfile("jltv_revision_", fileext = ".sqlite")
  revision_temp_paths <<- c(revision_temp_paths, application$database_path)
  revision_core$initialize_database(application$database_path)
  revision_core$seed_synthetic_database(application$database_path)
  connection <- revision_core$open_database(application$database_path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  fixture <- revision_fixture()
  for (table in names(fixture)) DBI::dbAppendTable(connection, table, fixture[[table]])
  application$documentation_references <- list()
  eval(revision_server_expr, envir = application)
}
revision_defaults <- list(
  questionnaire_source_mode = "FILE", questionnaire_auto_refresh = FALSE,
  score_aggregation = "median", only_consistent_ahp = TRUE, min_cell_coverage = 70,
  target_ready = 1, simulation_horizon = 24, target_probability = .2,
  simulation_replications = 5, simulation_seed = 21, mission_utilization = 1,
  initial_unavailable = 0, mission_tolerance = 0,
  failure_distribution = "exponential", failure_parameter_1 = 10000,
  failure_parameter_2 = .3, mean_diagnosis = 2, mean_corrective = 8,
  mean_preventive = 4, mean_test = 1, duration_cv = .35,
  support_diag = 1, support_repair = 1, support_bays = 1, support_pm = 0,
  support_stock = 1, support_reorder_point = 0, support_reorder_qty = 1,
  support_lead = 24, support_lead_cv = .35, support_part_probability = 0,
  grid_fleet = "2", grid_diag = "1", grid_repair = "1", grid_bays = "1", grid_stock = "1",
  pm_initial_phase = "fresh", common_random_numbers = TRUE, costs_complete = FALSE
)

revision_test("Automatic MC after evidence mode change retains selected settings", {
  shiny::testServer(revision_make_server(), {
    do.call(session$setInputs, revision_defaults)
    session$setInputs(recompute_multicriteria = 1)
    manual <- multicriteria_state()
    revision_check(identical(manual$score_aggregation, "median") &&
      abs(manual$min_cell_coverage - .7) < 1e-10, "Manual fixture must use median and 70% coverage.")
    session$setInputs(mode_choice = "HYBRID", confirm_empirical = FALSE, save_mode = 1)
    current <- multicriteria_state()
    revision_check(!is.null(current) && identical(current$score_aggregation, "median") &&
      abs(current$min_cell_coverage - .7) < 1e-10,
      "Automatic recalculation changed median/70% to mean/0%.")
    revision_check(identical(current$evidence_mode, "HYBRID"), "MC must identify its actual evidence mode.")
  })
})

revision_test("Generic P6 CSV replacement updates MC and keeps selected settings", {
  shiny::testServer(revision_make_server(), {
    do.call(session$setInputs, revision_defaults)
    session$setInputs(recompute_multicriteria = 1)
    before <- multicriteria_state()
    revision_check(before$topsis$ranking$alternative_id[1] == "A10", "Fixture initial winner must be A10.")
    updated <- revision_fixture()$topsis_scores
    updated$score[updated$alternative_id == "A1"] <- 10
    csv <- tempfile("jltv_p6_", fileext = ".csv")
    utils::write.csv(updated, csv, row.names = FALSE)
    session$setInputs(upload_table = "topsis_scores", upload_operation = "replace",
      upload_csv = list(name = "revision_P6.csv", size = file.info(csv)$size,
                        type = "text/csv", datapath = csv), import_csv = 1)
    current <- multicriteria_state()
    revision_check(!is.null(current) && isTRUE(current$topsis$estimable) &&
      current$topsis$ranking$alternative_id[1] == "A1",
      "New P6 dataset makes A1 the clear priority; old cached A10 must not remain.")
    revision_check(identical(current$score_aggregation, "median") &&
      abs(current$min_cell_coverage - .7) < 1e-10, "CSV refresh must retain the selected MC settings.")
    unlink(csv)
  })
})

revision_test("Editing simulation parameters invalidates or marks the old simulation", {
  shiny::testServer(revision_make_server(), {
    do.call(session$setInputs, revision_defaults)
    session$setInputs(run_simulation = 1)
    revision_check(!is.null(simulation_state()), "Fixture simulation must complete before editing.")
    session$setInputs(mean_corrective = 80)
    old <- simulation_state()
    revision_check(is.null(old) || isTRUE(old$stale) || identical(old$is_current, FALSE),
      "Simulation with repair=8 must not remain current after repair=80 is selected.")
  })
})

revision_test("Report cannot combine old simulation metrics with newly edited assumptions", {
  shiny::testServer(revision_make_server(), {
    do.call(session$setInputs, revision_defaults)
    session$setInputs(run_simulation = 1)
    revision_check(!is.null(simulation_state()), "Fixture simulation must complete before report test.")
    session$setInputs(mean_corrective = 80, simulation_seed = 99, simulation_horizon = 720)
    report <- tryCatch(assemble_report(), error = function(e) e)
    if (!inherits(report, "error") && !is.null(report$simulation)) {
      values <- stats::setNames(report$assumptions$Valor, report$assumptions$Item)
      revision_check(as.numeric(values[["Reparo corretivo médio (h)"]]) == 8 &&
        as.numeric(values[["Semente"]]) == 21 &&
        as.numeric(values[["Horizonte da missão (h)"]]) == 24,
        "A report containing the stored simulation must freeze repair=8, seed=21 and H=24.")
    }
  })
})

revision_test("v1.5.5: unit change relabels/converts meters, logs the change and requires a reason", {
  path <- tempfile("jltv_unit_", fileext = ".sqlite")
  revision_temp_paths <<- c(revision_temp_paths, path)
  revision_core$initialize_database(path)
  assets <- data.frame(asset_id = "U-1", asset_name = "U", model = "JLTV", entry_service_date = "2025-01-01",
                       initial_meter = 0, current_meter = 600, meter_unit = "KM", initial_state = "OPERATING",
                       active = 1L, data_source = "TEST", stringsAsFactors = FALSE)
  revision_core$append_asset(path, assets)
  stop_msg <- tryCatch({ revision_core$change_asset_meter_unit(path, "U-1", "HOURS", "relabel", reason = ""); "" },
                       error = function(e) conditionMessage(e))
  revision_check(nzchar(stop_msg), "Unit change without reason must fail.")
  stop_msg <- tryCatch({ revision_core$change_asset_meter_unit(path, "U-1", "HOURS", "convert", rate = NA, reason = "x"); "" },
                       error = function(e) conditionMessage(e))
  revision_check(nzchar(stop_msg), "Conversion without rate must fail.")
  revision_core$change_asset_meter_unit(path, "U-1", "HOURS", "convert", rate = 30, reason = "velocidade média validada em teste")
  after <- revision_core$read_table_safe(path, "assets")
  revision_check(identical(after$meter_unit[[1]], "HOURS") && abs(after$current_meter[[1]] - 20) < 1e-9,
                 "600 km at 30 km/h must become 20 h.")
  log <- revision_core$read_table_safe(path, "cmms_change_log")
  revision_check(nrow(log) == 1L && grepl("^UNIT_CONVERT_HOURS$", log$action[[1]]), "Unit change must be logged.")
})

revision_test("v1.5.5: Word report follows the ESG manual (XML well-formed, abstract 150-250 words)", {
  fake <- list(
    meta = list(evidence_mode = "SYNTHETIC", version = "1.5.6", generated_at = "2026-10-05"),
    assessment = list(
      kpis = list(ao_reference = 0.824, ao_improved = 0.861, ao_gain = 0.037),
      bottlenecks = data.frame(alternative_name = "Trem de força", coefficient = 0.612, stringsAsFactors = FALSE)
    ),
    multicriteria = NULL
  )
  ab <- revision_core$word_report_resumo_local(fake)
  n_pt <- revision_core$word_report_word_count(ab$pt)
  n_en <- revision_core$word_report_word_count(ab$en)
  revision_check(n_pt >= 150L && n_pt <= 250L, paste("Resumo has", n_pt, "words."))
  revision_check(n_en >= 150L && n_en <= 250L, paste("Abstract has", n_en, "words."))
  if (requireNamespace("xml2", quietly = TRUE)) {
    xml_ok <- function(x) !inherits(try(xml2::read_xml(x), silent = TRUE), "try-error")
    revision_check(xml_ok(revision_core$wx_par(revision_core$wx_run("a & b < c", 12), sect = revision_core$wx_sect(2L))),
                   "Paragraph XML must be well-formed.")
    tb <- data.frame(Sistema = c("A", "B"), Valor = c("82,4%", "n/d"), stringsAsFactors = FALSE)
    revision_check(xml_ok(revision_core$wx_table(tb, "tabela")) && xml_ok(revision_core$wx_table(tb, "quadro")),
                   "Table XML must be well-formed.")
    revision_check(xml_ok(revision_core$wx_glossary_table(revision_core$word_report_glossary())),
                   "Glossary XML must be well-formed.")
  }
  meta <- revision_core$word_report_meta(list(author = " ", year = "2026"))
  revision_check(identical(meta$author, "[NOME DO AUTOR]") && identical(meta$year, "2026"), "Empty author must use the placeholder.")
})

revision_test("v1.5.6: Word report cites the author's sources and respects the page limit", {
  db <- revision_core$word_report_ref_db("2026", "1.5.6")
  used <- new.env()
  used$keys <- character()
  one <- revision_core$word_report_cite_resolve("Veja {c:lacey} e também {p:targett,lacey}.", db, used)
  revision_check(grepl("Lacey e Hockley (2018)", one, fixed = TRUE), "Narrative citation must use author and year.")
  revision_check(grepl("(Lacey; Hockley, 2018; Targett, 2018)", one, fixed = TRUE), "Parenthetical citation must be alphabetical.")
  stop_msg <- tryCatch({ revision_core$word_report_cite_resolve("{p:inexistente}", db, used); "" }, error = function(e) conditionMessage(e))
  revision_check(nzchar(stop_msg), "Unknown source key must fail.")

  # Todo marcador do texto fixo precisa existir no cadastro; a lista só traz as fontes citadas, em ordem alfabética.
  used <- new.env()
  used$keys <- character()
  fixed_text <- c(revision_core$word_report_text_intro(100L, "SYNTHETIC"),
                  unlist(lapply(revision_core$word_report_text_theory(), function(x) x$paragraphs)),
                  unlist(lapply(revision_core$word_report_text_method(100L, "SYNTHETIC", list(replications = "200", horizon = "720", target_ready = "8", coverage = "60%", only_consistent = "TRUE")), function(x) x$paragraphs)))
  resolved <- vapply(fixed_text, revision_core$word_report_cite_resolve, character(1), db = db, used = used, USE.NAMES = FALSE)
  revision_check(!any(grepl("\\{[cp]:", resolved)), "No citation marker may remain in the fixed text.")
  # Limite do autor: no máximo 42 referências no trabalho (o aplicativo conta como uma).
  revision_check(length(used$keys) >= 30L && length(used$keys) <= 42L, paste("The fixed text must cite between 30 and 42 sources; it cites", length(used$keys)))
  revision_check(length(db) <= 42L, paste("The source register must hold at most 42 entries; it holds", length(db)))
  # Toda fonte cadastrada (exceto o próprio aplicativo, citado no corpo) precisa aparecer no texto fixo.
  revision_check(length(setdiff(names(db), c("app", used$keys))) == 0L,
                 paste("Registered but never cited:", paste(setdiff(names(db), c("app", used$keys)), collapse = ", ")))
  refs <- revision_core$word_report_references(db, used$keys)
  revision_check(length(refs) == length(used$keys), "One reference per cited source.")
  plain_refs <- gsub("**", "", refs, fixed = TRUE)
  revision_check(grepl("^1º ESCALÃO", plain_refs[[1]]) && grepl("^ABREU", plain_refs[[2]]) && grepl("^AGUARÓN", plain_refs[[3]]) &&
                   grepl("^ANDRADE", plain_refs[[4]]) && grepl("^ZABALA", plain_refs[[length(plain_refs)]]),
                 "References must be in alphabetical order.")
  revision_check(all(vapply(refs, function(x) length(gregexpr("\\*\\*", x)[[1]]) %% 2L == 0L, logical(1))), "Bold markers must be balanced.")
  # Fontes que o orientador mandou retirar não podem aparecer.
  removed <- c("BWM-COPRAS", "Industry 4.0: ", "Diesel Engine", "RCM 3", "RCM3", "Additive", "Base Industrial de Defesa", "Machine Learning Use", "Condition-Based",
               "Transforming Maintenance", "Cooling System", "Off-Road", "Gas Turbine", "Risk Assessment Methods",
               "Complete Wargames", "DUNNIGAN", "BRANDÃO", "WILDEN")
  revision_check(!any(vapply(removed, function(w) any(grepl(w, refs, fixed = TRUE)), logical(1))), "Removed sources must not be listed.")
  # TCC, monografias, dissertações e teses não entram como referência.
  revision_check(!any(grepl("Trabalho de Conclusão|Monografia|Dissertação|Tese \\(", refs)), "Theses and TCC must not be listed as references.")

  # Estimador de páginas: figura não divide; texto flui; mais conteúdo, mais páginas.
  blocks <- list(list(type = "h1", cm = 1.4),
                 list(type = "body", cm = 20),
                 list(type = "obj", kind = "figura", cm = 12))
  pages <- revision_core$word_report_est_pages(blocks)
  revision_check(pages > 1.4 && pages < 1.6, paste("Figure that does not fit must move to the next page; got", pages))
  long <- revision_core$word_report_est_text_cm(strrep("palavra ", 400))
  revision_check(long > revision_core$word_report_est_text_cm(strrep("palavra ", 40)), "Longer text must be taller.")
  tb <- data.frame(A = c("x", "y"), B = c("curto", strrep("texto longo ", 20)), stringsAsFactors = FALSE)
  revision_check(revision_core$wx_table_height_cm(tb) > revision_core$wx_table_height_cm(tb[1, , drop = FALSE]),
                 "Table height must grow with rows and wrapped text.")
  meta <- revision_core$word_report_meta(list(max_pages = "99", sector = "Instituto Therezinha de Castro"))
  revision_check(meta$max_pages == 20 && identical(meta$sector, "Instituto Therezinha de Castro"), "Article body is capped at 20 pages by the ESG 2025 manual.")
})

unlink(c(revision_temp_paths, paste0(revision_temp_paths, "-wal"),
         paste0(revision_temp_paths, "-shm")))
revision_test("v1.5.7: simulator classification (SED + Monte Carlo) appears in the report text and table", {
  tb <- revision_core$simulation_framing_table()
  revision_check(is.data.frame(tb) && nrow(tb) >= 10L && ncol(tb) == 3L, "Framing table must have rows and three columns.")
  revision_check(all(nzchar(unlist(tb))), "Framing table cells must not be empty.")
  all_text <- paste(unlist(tb), collapse = " ")
  revision_check(grepl("eventos discretos", all_text, fixed = TRUE) && grepl("Monte Carlo", all_text, fixed = TRUE),
                 "Framing table must state discrete-event simulation and Monte Carlo.")
  titles <- vapply(revision_core$word_report_text_theory(), function(x) x$title, character(1))
  revision_check(any(startsWith(titles, "2.6")), "Theory must include section 2.6 on the classification.")
  db <- revision_core$word_report_ref_db("2026", "1.5.7")
  revision_check(all(c("prado", "taha") %in% names(db)) && !("dunnigan" %in% names(db)), "Publications supporting the classification must be registered.")
  revision_check(!any(grepl("^lauro", names(db))), "Lecture notes must not be registered as report references.")
})

revision_test("v1.5.7: questionnaire import accepts varied spreadsheets and never hides responses silently", {
  pairs <- revision_core$questionnaire_pair_spec()
  alts <- revision_core$questionnaire_alternative_spec()
  n <- 5L
  make_frame <- function(nbsp = FALSE) {
    pair_headers <- paste0(pairs$code, " \u2014 ", pairs$left_label, " \u00d7 ", pairs$right_label)
    if (nbsp) pair_headers <- sub(" ", "\u00a0", pair_headers, fixed = TRUE)
    p6_headers <- as.vector(t(outer(1:10, names(alts), function(k, a) paste0("P6.", k, " \u2014 Crit\u00e9rio \u2014 avalie [", a, " ", unname(alts[a]), "]"))))
    frame <- data.frame(Timestamp = as.character(46000 + seq_len(n)), stringsAsFactors = FALSE)
    for (j in seq_len(nrow(pairs))) {
      frame[[pair_headers[[j]]]] <- if (j %% 2L == 0L) rep("1 \u2014 import\u00e2ncia equivalente", n) else
        paste0(2 + (seq_len(n) %% 7L), " \u2014 ", pairs$left_label[[j]], " mais importante")
    }
    for (h in seq_along(p6_headers)) frame[[p6_headers[[h]]]] <- as.character(1 + ((seq_len(n) + h) %% 10L))
    frame
  }
  # 1) sem colunas de perfil/P7, com cabecalhos com espaco n\u00e3o separavel e linha em branco no fim
  frame <- make_frame(nbsp = TRUE)
  frame[n + 1L, ] <- NA_character_
  res <- revision_core$transform_google_forms_questionnaire(frame, "EMPIRICAL")
  revision_check(nrow(res$questionnaire_respondents) == n, paste("Expected", n, "respondents, got", nrow(res$questionnaire_respondents)))
  revision_check(nrow(attr(res, "skipped_rows")) == 0L, "Blank rows must not be reported as errors.")
  revision_check(nrow(res$ahp_pairwise) == n * 14L && nrow(res$topsis_scores) == n * 100L, "AHP and P6 volumes must match.")
  # 2) mensagem clara quando falta uma coluna AHP
  broken <- make_frame()
  broken[[grep("^P2\\.2", names(broken))]] <- NULL
  msg <- tryCatch({ revision_core$transform_google_forms_questionnaire(broken, "EMPIRICAL"); "" }, error = function(e) conditionMessage(e))
  revision_check(grepl("P2.2", msg, fixed = TRUE), "Missing AHP column must be named in the error.")
  # 3) Excel com titulo acima do cabecalho, aba extra e leitura da aba certa
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    path <- tempfile(fileext = ".xlsx")
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "LEIA-ME"); openxlsx::writeData(wb, "LEIA-ME", data.frame(nota = "planilha de teste"))
    openxlsx::addWorksheet(wb, "Dados")
    openxlsx::writeData(wb, "Dados", "Respostas coletadas em papel", startRow = 1, startCol = 1)
    openxlsx::writeData(wb, "Dados", make_frame(), startRow = 3, startCol = 1)
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    read <- revision_core$read_questionnaire_file(path)
    revision_check(nrow(read) == n && sum(grepl("^P6\\.", names(read))) == 100L, "Excel reader must pick the sheet and header row.")
    # 4) classificacao: gravar como SYNTHETIC e olhar em EMPIRICAL nao pode ficar mudo
    db <- tempfile(fileext = ".sqlite")
    revision_core$initialize_database(db)
    out <- revision_core$sync_questionnaire_file(db, path, "SYNTHETIC")
    revision_check(out$imported == n, "File import must store all respondents.")
    revision_check(nzchar(revision_core$questionnaire_hidden_message(db, "EMPIRICAL")), "Hidden responses must be reported in another mode.")
    revision_check(!nzchar(revision_core$questionnaire_hidden_message(db, "SYNTHETIC")), "No warning when responses are visible.")
    revision_core$reclassify_questionnaire_responses(db, "EMPIRICAL")
    revision_check(!nzchar(revision_core$questionnaire_hidden_message(db, "EMPIRICAL")), "After reclassification the responses must be visible.")
    snap <- revision_core$database_snapshot(db)
    shown <- revision_core$filter_questionnaire_snapshot_by_mode(snap, "EMPIRICAL")
    revision_check(nrow(shown$questionnaire_respondents) == n, "EMPIRICAL mode must show the reclassified respondents.")
  }
})

revision_test("v1.5.7: the evidence class of imported answers follows the user's choice", {
  raw <- revision_core$synthetic_questionnaire_demo_raw(3L, 1L)
  marker <- any(vapply(seq_len(nrow(raw)), function(i) revision_core$questionnaire_is_synthetic_row(unlist(raw[i, ])), logical(1)))
  revision_check(marker, "The demo rows must carry a fictitious marker for this test to be meaningful.")
  as_empirical <- revision_core$transform_google_forms_questionnaire(raw, "EMPIRICAL")
  revision_check(all(grepl("^EMPIRICAL", as_empirical$questionnaire_respondents$data_source)),
                 "Rows marked EMPIRICAL by the user must stay EMPIRICAL even with fictitious text markers.")
  as_synthetic <- revision_core$transform_google_forms_questionnaire(raw, "SYNTHETIC")
  revision_check(all(grepl("^SYNTHETIC", as_synthetic$questionnaire_respondents$data_source)), "Rows marked SYNTHETIC must stay SYNTHETIC.")
})

revision_test("v1.5.7: report follows the advisor's structure (Introdução, Referencial, Metodologia) and the author's rules", {
  intro <- revision_core$word_report_text_intro(100L, "SYNTHETIC")
  revision_check(length(intro) >= 7L, "Introduction must cover context, justification, question, objective, method and organisation.")
  revision_check(any(grepl("^O objetivo geral é", intro)), "Introduction must state the objective in one direct sentence.")
  revision_check(any(grepl("MD40-M-01", intro, fixed = TRUE)) && any(grepl("MD44-M-01", intro, fixed = TRUE)),
                 "Introduction must name the MD manuals with their codes.")
  theory <- revision_core$word_report_text_theory()
  tt <- vapply(theory, function(x) x$title, character(1))
  revision_check(any(grepl("^2\\.1 .*Ciclo de Vida", tt)) && any(grepl("^2\\.3 .*lead time", tt)), "Theory must define GCV/ALI and lead time.")
  all_theory <- paste(unlist(lapply(theory, function(x) x$paragraphs)), collapse = " ")
  revision_check(grepl("MTTR", all_theory, fixed = TRUE) && grepl("MTTF", all_theory, fixed = TRUE) && grepl("modo de falha", all_theory, fixed = TRUE),
                 "Theory must define MTTF, MTTR and failure mode.")
  method <- revision_core$word_report_text_method(100L, "HYBRID", list(replications = "200", horizon = "720", target_ready = "8"))
  mt <- vapply(method, function(x) x$title, character(1))
  revision_check(startsWith(mt[[1]], "3.1") && any(startsWith(mt, "3.2")) && any(startsWith(mt, "3.5")), "Methodology must have subsections 3.1 to 3.5.")
  m1 <- paste(method[[1]]$paragraphs, collapse = " ")
  revision_check(grepl("H1", m1, fixed = TRUE) && grepl("H2", m1, fixed = TRUE), "Hypotheses H1 and H2 must be stated in 3.1.")
  revision_check(grepl("200 replicações", paste(method[[4]]$paragraphs, collapse = " "), fixed = TRUE), "Replications must come from the run.")
  neutral <- c("indisponibilidade", "gargalo", "aperfeiçoamento", "melhorias")
  all_fixed <- paste(c(intro, unlist(lapply(theory, function(x) x$paragraphs)), unlist(lapply(method, function(x) x$paragraphs))), collapse = " ")
  revision_check(!any(vapply(neutral, function(w) grepl(w, all_fixed, fixed = TRUE), logical(1))), "Fixed text must keep a neutral wording.")
  revision_check(!grepl("JLTV RAM", all_fixed, fixed = TRUE), "The old application name must not appear in the report text.")
  db <- revision_core$word_report_ref_db("2026", "1.5.7")
  revision_check(grepl("JLTV Sustainment Decision Lab", db$app$ref, fixed = TRUE) || grepl("JLTV SUSTAINMENT DECISION LAB", db$app$ref, fixed = TRUE),
                 "The application reference must use the new name.")
})

revision_test("v1.5.7: AI call reads the response safely (mocked HTTP): structured output, retries and diagnostics", {
  env <- revision_core
  original_post <- env$word_report_http_post
  tryCatch({
    words <- function(n) paste(rep("palavra", n), collapse = " ")
    payload <- list(resumo = words(180), abstract = words(180))
    for (k in c("sintese", "prioridades", "pesos", "robustez", "processo", "melhorias", "simulacao", "limitacoes", "conclusao", "proximos_passos"))
      payload[[k]] <- list(paste0("Texto de ", k, " {c:taha}."), "Segundo parágrafo.")
    ok_json <- as.character(jsonlite::toJSON(payload, auto_unbox = TRUE))
    claude_resp <- function(txt, stop = "end_turn", types = "text", status = 200L) {
      blocks <- lapply(types, function(ty) if (ty == "thinking") list(type = "thinking", thinking = "...") else list(type = "text", text = txt))
      list(status = status, text = as.character(jsonlite::toJSON(list(content = blocks, stop_reason = stop), auto_unbox = TRUE)))
    }
    install_mock <- function(responses) {
      state <- new.env(); state$i <- 0L; state$bodies <- list()
      assign("word_report_http_post", function(url, headers, body, timeout = 300) {
        state$i <- state$i + 1L
        state$bodies[[state$i]] <- jsonlite::fromJSON(body, simplifyVector = FALSE)
        responses[[min(state$i, length(responses))]]
      }, envir = env)
      state
    }
    run <- function() env$word_report_call_ai(list(a = 1), "claude", "sk-teste")
    fail <- function() tryCatch({ run(); "" }, error = function(e) conditionMessage(e))

    # 1) resposta normal, com bloco de raciocínio antes do texto
    st <- install_mock(list(claude_resp(ok_json, types = c("thinking", "text"))))
    res <- run()
    b1 <- st$bodies[[1]]
    revision_check(identical(b1$output_config$format$type, "json_schema") && length(b1$output_config$format$schema$required) == 12L,
                   "Claude request must ask for the JSON schema with the 12 report keys.")
    revision_check(identical(b1$thinking$type, "adaptive") && identical(b1$output_config$effort, "high") && is.null(b1$temperature) && b1$max_tokens >= 32000,
                   "Claude request must think (adaptive, high effort) before writing and leave room for the answer.")
    revision_check(setequal(names(res), env$word_report_ai_keys) && grepl("{p:taha}", res$sintese[[1]], fixed = TRUE), "All keys and valid citations must be kept (narrative citation at the end of a sentence becomes parenthetical).")
    # 2) o modelo não aceita raciocínio adaptativo: nova tentativa sem ele; depois recusa a temperatura
    st <- install_mock(list(list(status = 400L, text = '{"error":{"message":"thinking.type adaptive is not supported on this model"}}'),
                            list(status = 400L, text = '{"error":{"message":"temperature is deprecated for this model"}}'), claude_resp(ok_json)))
    res <- run()
    revision_check(st$i == 3L && identical(st$bodies[[2]]$thinking$type, "disabled") && is.null(st$bodies[[2]]$output_config$effort) &&
                   is.null(st$bodies[[3]]$temperature) && !is.null(st$bodies[[2]]$temperature), "Thinking and temperature errors must each trigger one retry without the parameter.")
    # 3) resposta cortada pelo limite de tokens: repete com o dobro
    st <- install_mock(list(claude_resp("", stop = "max_tokens", types = "thinking"), claude_resp(ok_json)))
    res <- run()
    revision_check(st$i == 2L && st$bodies[[2]]$max_tokens == 64000, "A max_tokens stop must retry with a larger limit.")
    # 4) resposta vazia e texto que não é JSON: o aviso mostra o que veio
    st <- install_mock(list(claude_resp("")))
    msg <- fail()
    revision_check(grepl("vazia", msg) && grepl("parada: end_turn", msg, fixed = TRUE), paste("Empty answer must be explained:", msg))
    st <- install_mock(list(claude_resp("Desculpe, não consigo gerar o texto.")))
    msg <- fail()
    revision_check(grepl("JSON", msg) && grepl("Desculpe", msg, fixed = TRUE), paste("Non-JSON answer must show its beginning:", msg))
    # 5) JSON dentro de cercas de código continua aceito
    st <- install_mock(list(claude_resp(paste0("```json\n", ok_json, "\n```"))))
    revision_check(length(run()$sintese) == 2L, "A fenced JSON answer must be accepted.")
    # 6) ChatGPT: resposta cortada é identificada
    openai_resp <- function(txt, finish = "stop") list(status = 200L, text = as.character(jsonlite::toJSON(
      list(choices = list(list(message = list(content = txt), finish_reason = finish))), auto_unbox = TRUE)))
    st <- install_mock(list(openai_resp(ok_json)))
    res <- env$word_report_call_ai(list(a = 1), "chatgpt", "sk-teste")
    revision_check(length(res$conclusao) == 2L, "ChatGPT answer must be read.")
    st <- install_mock(list(openai_resp(substr(ok_json, 1, 400), finish = "length")))
    msg <- tryCatch({ env$word_report_call_ai(list(a = 1), "chatgpt", "sk-teste"); "" }, error = function(e) conditionMessage(e))
    revision_check(grepl("cortada", msg), paste("A truncated ChatGPT answer must be reported:", msg))
  }, finally = assign("word_report_http_post", original_post, envir = env))
})


# TCC result and AI regression checks, kept in the existing test file.
tcc_fixture <- function() {
r <- data.frame(priority_rank=1:3,label=c("Aumentar a confiabilidade (MTBF)","Reduzir o tempo de reparo corretivo","Ampliar o estoque de sobressalentes"),
change=c("MTBF de 180,0 h para 216,0 h (+20%)","Reparo de 8,0 h para 6,4 h (−20%)","Estoque de 6 para 9"),
base_availability=.9418,new_availability=c(.9519,.9503,.9418),delta_availability=c(.0101,.0085,0),
delta_ci_lower=c(.006,.005,0),delta_ci_upper=c(.014,.012,0),new_success=c(.5,.48,.32),
demonstrated=c(TRUE,TRUE,FALSE),conclusion=c("Ganho demonstrado","Ganho demonstrado","Ganho não demonstrado"),
criterion_label=c("Falha / Ocorrência","Manutenção e Apoio","Suprimento"),targets_bottleneck="",stringsAsFactors=FALSE)
list(meta=list(evidence_mode="EMPIRICAL",version="1.5.7",generated_at="11/10/2026"),
assessment=list(kpis=list(ao_reference=.9418,ao_improved=.959,ao_gain=.0172,ao_observed=NA_real_,
availability_target=10/12,success_reference=.32,success_improved=.64,improved_label="Pacote",
top_bottleneck="Sistema elétrico",top_bottleneck_driver="Impacto na Disponibilidade",top_process="Reparo corretivo",top_process_share=.7),
bottlenecks=data.frame(rank=1,alternative_name="Sistema elétrico",coefficient=.925,dominant_criterion_label="Impacto na Disponibilidade"),
process=data.frame(component=c("Reparo corretivo","Diagnóstico","Espera por peça"),hours=c(70,30,0),share=c(.7,.3,0)),
improvements=r,package=data.frame(base_availability=.9418,new_availability=.959,delta_availability=.0172,base_success=.32,new_success=.64),
criteria_weights=c(OCCURRENCE=.2,DOWNTIME=.4,SUPPLY=.25,SUPPORT=.15),confidence=data.frame()),
improvements=list(config=list(fleet_size=12,mission_tolerance_hours=0),horizon=168,target_ready=10,replications=50,seed=21),
simulation=list(p_target=.8,summary=data.frame(configuration_id="N12",success_probability=.32,ci_lower=.207,ci_upper=.458,mean_availability=.9418)),
multicriteria=NULL,assumptions=data.frame(Item=c("Probabilidade-alvo","Horizonte da missão (h)","Meta M (viaturas prontas)"),Valor=c(".8","168","10")))
}
tcc_report <- tcc_fixture()
revision_test("JSON preserves criterion names and labels through serialization",{
f<-revision_core$word_report_facts(tcc_report); j<-jsonlite::fromJSON(jsonlite::toJSON(f,auto_unbox=TRUE,na="null",null="null"))
revision_check(is.data.frame(j$pesos_ahp_criterios)&&all(c("criterio","peso","criterio_id")%in%names(j$pesos_ahp_criterios)),"Numeric vector loses names in JSON; use named rows.")
revision_check(j$pesos_ahp_criterios$criterio[j$pesos_ahp_criterios$criterio_id=="DOWNTIME"]=="Impacto na Disponibilidade","Criterion label must correspond to the actual named weight.")
})
revision_test("JSON explains mission success and downtime aggregation",{
f<-revision_core$word_report_facts(tcc_report)
revision_check(f$contexto_da_missao$frota==12&&f$contexto_da_missao$meta_viaturas_prontas==10&&f$contexto_da_missao$horizonte_h==168,"Mission N, M and H must be available to interpret probability.")
revision_check(f$contexto_da_missao$probabilidade_alvo==.8,"Probability target is different from M/N.")
revision_check(grepl("viatura",f$definicoes$horas_do_processo)&&grepl("replica",f$definicoes$horas_do_processo),"Hours need their aggregation basis.")
})
revision_test("Conclusion contains quantitative proposals and unmet mission target",{
n<-revision_core$word_report_narrative_local(tcc_report)
s<-paste(n$conclusao,collapse=" ")
revision_check(grepl("94,2",s)&&grepl("95,9",s)&&grepl("1,0",s)&&grepl("IC95%",s),"Conclusion must synthesize run-specific effects and uncertainty.")
revision_check(grepl("propõe|prioriza",s)&&grepl("180,0|216,0",s),"Conclusion must state a concrete tested proposal and its magnitude.")
revision_check(grepl("80,0|80%",s)&&grepl("64,0|64%",s),"Package below the mission probability target must be explicit.")
revision_check(!grepl("\\{[cp]:",s),"Conclusion cannot add citations.")
})
revision_test("Missing or nonpositive effects cannot claim full objective fulfillment",{
r<-tcc_fixture();r$assessment$improvements<-NULL;r$assessment$package<-NULL
s<-paste(revision_core$word_report_narrative_local(r)$conclusao,collapse=" ")
revision_check(grepl("parcial|não foi",s)&&!grepl("foi cumprido nos limites",s),"Incomplete run cannot claim the general objective was met.")
r<-tcc_fixture();r$assessment$improvements$demonstrated<-FALSE;r$assessment$improvements$delta_ci_lower<-NA_real_
s<-paste(revision_core$word_report_narrative_local(r)$conclusao,collapse=" ")
revision_check(!grepl("propõe priorizar",s),"No positive estimable interval means no supported intervention.")
})
revision_test("Generic AI cannot erase the numerical decision core",{
original<-revision_core$word_report_call_ai
revision_core$word_report_call_ai<-function(...)list(conclusao="Recomenda-se melhorar a logística.",melhorias="É preciso investir.",proximos_passos="Estudar mais.")
tryCatch({
 n<-revision_core$word_report_resolve_narrative(tcc_report,"claude","test-only")
 s<-paste(n$text$conclusao,collapse=" ")
 revision_check(grepl("94,2",s)&&grepl("180,0|216,0",s),"AI response must preserve factual proposal core.")
 revision_check(any(grepl("IC95%",n$text$melhorias)),"Evidence paragraph must remain in section 4 before conclusion.")
},finally={revision_core$word_report_call_ai<-original})
})

revision_test("Claude still writes a supported conclusion through the API path", {
 original <- revision_core$word_report_call_ai
 expected <- revision_core$word_report_decision_core(tcc_report)$conclusao
 revision_core$word_report_call_ai <- function(...) list(conclusao=expected,melhorias="O efeito concentra-se no processo de reparo.")
 tryCatch({
  n <- revision_core$word_report_resolve_narrative(tcc_report,"claude","test-only")
  revision_check(identical(n$source,"claude") && identical(n$text$conclusao,expected),"Supported AI conclusion must be used.")
  revision_check(any(grepl("O efeito concentra",n$text$melhorias)),"AI discussion must be retained.")
 },finally={revision_core$word_report_call_ai<-original})
})
revision_test("Proposals specify candidate actions instead of only changing a parameter", {
 s <- paste(revision_core$word_report_narrative_local(tcc_report)$conclusao,collapse=" ")
 revision_check(grepl("análise de causa raiz",s)&&grepl("ferramental",s,ignore.case=TRUE),"Operational proposals need actions already documented in the lever catalog.")
 revision_check(grepl("metas de desempenho",s),"Parameter gain must be framed as a performance target, not as proven action effectiveness.")
})


revision_test("v1.5.7: RC histogram uses wide classes with counts; system pie covers 100% of the downtime and feeds the report", {
  cr <- c(0.001, 0.004, 0.06, 0.12, 0.25, 0.26, 0.37, 0.43, 0.49, 0.63, 1.39, 1.97, 0.10)
  cls <- revision_core$mc_cr_counts(cr)
  revision_check(nrow(cls) == 7L && sum(cls$n) == length(cr), "RC classes must cover every matrix.")
  revision_check(sum(cls$n[cls$status == "RC < 0,10 (consistente)"]) == 3L, "Only RC < 0.10 counts as consistent (0.10 is not).")
  mc <- list(ahp = list(individual_diagnostics = data.frame(
    respondent_id = paste0("R", seq_along(cr)), hierarchy_group = "criteria", complete = TRUE, CR = cr,
    consistent = cr < 0.10, stringsAsFactors = FALSE)))
  hist_plot <- revision_core$chart_ahp_cr_histogram(mc)
  revision_check(inherits(hist_plot, "ggplot"), "RC histogram must be a ggplot.")
  png_file <- tempfile(fileext = ".png")
  ggplot2::ggsave(png_file, hist_plot, width = 6.3, height = 3.9, dpi = 100, bg = "white")
  revision_check(file.exists(png_file) && file.size(png_file) > 3000, "RC histogram must render.")
  hb <- ggplot2::ggplot_build(hist_plot)$data[[1]]
  revision_check(nrow(hb) >= 12L && sum(hb$y) == length(cr), "Sigma histogram must use many narrow bins covering every matrix.")
  revision_check(length(unique(hb$fill)) >= 2L, "Bars must be colored by distance to the mean (sigma bands).")
  ao_plot <- revision_core$chart_availability_histogram(data.frame(avg_availability = stats::rnorm(50, 0.94, 0.01)), 10/12)
  revision_check(inherits(ao_plot, "ggplot"), "Availability histogram over replications must be built.")
  ggplot2::ggsave(png_file, ao_plot, width = 6.3, height = 3.7, dpi = 100, bg = "white")
  revision_check(file.size(png_file) > 3000, "Availability histogram must render.")

  systems <- data.frame(system_id = c("A1", "A3", "A8", "A9"), system_name = c("Motor/comb.", "Driveline", "SISTEMA ELÉTRICO/ ELETRÔNICO", "DSDU/MUX"),
                        down_h = c(10, 40, 30, 20), failures = c(1, 4, 3, 2), stringsAsFactors = FALSE)
  shares <- revision_core$system_downtime_shares(systems)
  revision_check(isTRUE(all.equal(sum(shares$share), 1)) && identical(shares$system_id, c("A3", "A8", "A9", "A1")), "Shares must add to 100% in descending order.")
  revision_check(identical(shares$system_name[[2]], "Sistema elétrico/ eletrônico") && identical(shares$system_name[[3]], "DSDU/MUX"), "Multi-word capitals become sentence case; acronyms stay.")
  pie_plot <- revision_core$chart_system_downtime_pie(systems)
  revision_check(inherits(pie_plot, "ggplot"), "System pie must be a ggplot.")
  ggplot2::ggsave(png_file, pie_plot, width = 6.3, height = 3.8, dpi = 100, bg = "white")
  revision_check(file.size(png_file) > 3000, "System pie must render.")
  revision_check(is.null(revision_core$chart_system_downtime_pie(data.frame())) &&
                 is.null(revision_core$chart_system_downtime_pie(transform(systems, down_h = 0))), "No downtime means no pie.")

  p <- revision_core$sys_demo_profiles()
  cfg <- list(fleet_size = 12, diagnostic_teams = 2, repair_teams = 3, bays = 2, utilization = 1)
  r <- revision_core$sys_compare(p, cfg, 168, 8, 20261003 + 0:3, "system", "A3", "repair", 20)
  contributions <- data.frame(alternative_id = c("A3", "A1"), alternative_name = c("Driveline", "Motor/comb."), rank = c(1L, 2L), stringsAsFactors = FALSE)
  sd <- revision_core$word_report_system_downtime(r, contributions, "SYNTHETIC")
  revision_check(!is.null(sd) && inherits(sd$plot, "ggplot") && is.data.frame(sd$table), "The report block must be built from the system simulation.")
  revision_check(any(grepl("{fig:sys_pie}", sd$paragraphs, fixed = TRUE)) && any(grepl("{tab:sys_tab}", sd$paragraphs, fixed = TRUE)), "Text must cite the figure and the table.")
  revision_check(any(grepl("demonstrativo", sd$paragraphs, fixed = TRUE)), "Synthetic catalog must be flagged in the text.")
  revision_check(any(grepl("maior prioridade segundo os especialistas", sd$paragraphs, fixed = TRUE)), "Text must compare the downtime share with the experts' priority.")
  revision_check("Posição na prioridade (TOPSIS)" %in% names(sd$table), "Table must show the TOPSIS position.")
  revision_check(is.null(revision_core$word_report_system_downtime(NULL, contributions, "SYNTHETIC")), "Without the system simulation there is no block.")
})

revision_test("Audit20261011: pie uses downtime hours and handles missing results", {
 r <- list(mode="HYBRID",systems=data.frame(system_id=c("A8","A5","A2"),system_name=c("Elétrico","Suspensão","Transmissão"),down_h=c(75,25,0)))
 revision_check(exists("system_downtime_shares",envir=revision_core),"Pie calculation is missing from the existing application.")
 d <- revision_core$system_downtime_shares(r$systems)
 revision_check(nrow(d)==2 && abs(sum(d$share)-1)<1e-12 && d$share[d$system_id=="A8"]==.75,"Pie must use hours, sum to 100% and omit zero sectors.")
 revision_check(is.null(revision_core$system_downtime_shares(NULL)),"Missing system simulation cannot fabricate shares.")
 r$systems$down_h <- 0
 revision_check(is.null(revision_core$system_downtime_shares(r$systems)),"No downtime means no pie denominator.")
})
revision_test("Audit20261011: complete conclusion including next steps excludes citations", {
 original <- revision_core$word_report_http_post
 answer <- list(conclusao=list("Resultado {p:md4401}."),proximos_passos=list("Confirmar nos registros {p:md4401}. Relacionar com Brasil (2023b)."),processo=list("O JSON não traz a origem. O reparo concentra as horas."))
 revision_core$word_report_http_post <- function(...)list(status=200L,text=as.character(jsonlite::toJSON(list(content=list(list(type="text",text=as.character(jsonlite::toJSON(answer,auto_unbox=TRUE)))),stop_reason="end_turn"),auto_unbox=TRUE)))
 tryCatch({
  got <- revision_core$word_report_call_ai(list(a=1),"claude","test-only")
  revision_check(!grepl("\\{[cp]:",paste(c(got$conclusao,got$proximos_passos),collapse=" ")),"Next steps are part of section 5 and cannot contain citations.")
  revision_check(!grepl("2023b",paste(got$proximos_passos,collapse=" ")),"Plain author-year citations cannot bypass the conclusion rule.")
  revision_check(!grepl("JSON",paste(got$processo,collapse=" ")),"Implementation messages cannot enter academic prose.")
 },finally={revision_core$word_report_http_post<-original})
})
revision_test("Audit20261011: AI paraphrase does not duplicate the quantitative core", {
 original <- revision_core$word_report_call_ai
 core <- revision_core$word_report_decision_core(tcc_report)
 revision_core$word_report_call_ai <- function(...)list(processo=gsub("concentrou","representou",core$processo),conclusao=core$conclusao)
 tryCatch({
  got <- revision_core$word_report_resolve_narrative(tcc_report,"claude","test-only")$text$processo
  revision_check(sum(grepl("70,0%",got,fixed=TRUE))==1L,"The same result cannot be repeated as deterministic text plus AI paraphrase.")
 },finally={revision_core$word_report_call_ai<-original})
})
revision_test("Audit20261011: report contains current system result and drops it when inputs change", {
 shiny::testServer(revision_make_server(), {
  do.call(session$setInputs,revision_defaults)
  session$setInputs(recompute_multicriteria=1,run_simulation=1)
  session$setInputs(sys_mode="SYNTHETIC",sys_load_demo=1,sys_fleet=12,sys_diag=2,sys_repair=2,sys_bays=2,sys_util=1,sys_horizon=720,sys_target=10,sys_n=2,sys_seed=7,sys_scope="system",sys_target_id="A8",sys_lever="repair",sys_change=20,sys_all=FALSE)
  session$setInputs(sys_run=1)
  r <- assemble_report()
  revision_check(is.list(r$systems_simulation) && nrow(r$systems_simulation$systems)==10,"Word payload loses the completed system simulation.")
  revision_check(identical(r$systems_simulation$source_signature,revision_core$sys_source_signature(r$systems_simulation$source_snapshot)),"Report must retain the executed source snapshot.")
  session$setInputs(sys_change=30)
  revision_check(is.null(assemble_report()$systems_simulation),"An edited intervention cannot export an obsolete pie.")
 })
})
revision_test("Audit20261011: Word includes pie in analysis and gives an explicit no-data notice", {
 r <- tcc_fixture()
 r$systems_simulation <- list(mode="HYBRID",horizon=168,seeds=1:2,config=list(fleet_size=12),
   systems=data.frame(system_id=c("A8","A5"),system_name=c("Elétrico","Suspensão"),down_h=c(75,25)))
 p <- tempfile(fileext=".docx")
 tryCatch({
  ans <- revision_core$write_assessment_word(p,r,meta=list(max_pages=20))
  con <- unz(p,"word/document.xml"); xml <- paste(readLines(con,warn=FALSE,encoding="UTF-8"),collapse="");close(con)
  pie <- regexpr("Participação de cada sistema no tempo parado",xml,fixed=TRUE)[1]
  end <- regexpr("5 CONCLUSÃO",xml,fixed=TRUE)[1]
  revision_check(pie>0 && pie<end,"The pie must be present in section 4.5 of the generated Word.")
  r$systems_simulation <- NULL
  ans <- revision_core$write_assessment_word(p,r,meta=list(max_pages=20))
  revision_check(grepl("pizza",ans$note) && grepl("sistema",ans$note),"Absent system results must be explained at export.")
 },finally={unlink(p)})
})
revision_test("Audit20261011: H1 is not fully supported by a changing order", {
 stability <- list(replications=300,respondents=11,summary=data.frame(rank_ref=1:3,alternative_name=c("Elétrico","Suspensão","Transmissão"),prob_top1=c(.83,.17,0),prob_top3=c(1,1,.533),rank_p05=c(1,1,3),rank_p95=c(2,2,6)))
 txt <- paste(revision_core$mc_robustness_text(NULL,stability,NULL),collapse=" ")
 revision_check(grepl("H1",txt)&&grepl("não.*integral|parcial",txt),"A variable order cannot fully corroborate the hypothesis that each position remains unchanged.")
 revision_check(grepl("53",txt),"The unstable third priority must be described by its actual top-three frequency.")
})

revision_test("Audit20261011b: AI rewrites the analysis; paragraphs with invented numbers fall back to the app text", {
 original <- revision_core$word_report_call_ai
 core <- revision_core$word_report_decision_core(tcc_report)
 good <- "A combinação de 94,2% de disponibilidade média com a concentração do tempo parado no reparo corretivo mostra que a frota perde prontidão pelo ciclo de manutenção, e não pela espera logística, o que orienta a sustentação para a confiabilidade e para a rapidez do reparo nos sistemas priorizados pelos especialistas."
 bad <- "Com a medida, a frota ganharia 7,3 p.p. de disponibilidade e 27 viaturas prontas."
 revision_core$word_report_call_ai <- function(...) list(
   processo = c(good, bad, good),
   melhorias = c(paste(core$melhorias, collapse = " "), "O ganho combinado mostra que confiabilidade e reparo atuam sobre o mesmo tempo parado."),
   conclusao = core$conclusao)
 tryCatch({
  n <- revision_core$word_report_resolve_narrative(tcc_report, "claude", "test-only")
  revision_check(any(grepl("perde prontidão pelo ciclo", n$text$processo, fixed = TRUE)), "Validated AI paragraphs must replace the app draft.")
  revision_check(!any(grepl("7,3 p.p.", n$text$processo, fixed = TRUE)), "A paragraph with numbers absent from the results must be dropped.")
  revision_check(any(grepl("mesmo tempo parado", n$text$melhorias, fixed = TRUE)), "AI rewrite of the interventions must be used.")
  revision_check("processo" %in% n$replaced && "conclusao" %in% n$replaced, "The report must record which parts the AI rewrote.")
  revision_check(grepl("descartados", n$note %||% ""), "Dropped paragraphs must be reported in the appendix note.")
 }, finally = { revision_core$word_report_call_ai <- original })
})

revision_test("Audit20261011b: mission text follows the numbers (target met vs. not met)", {
 r <- tcc_fixture()
 r$assessment$kpis$success_reference <- .96
 r$assessment$package$base_success <- .96; r$assessment$package$new_success <- 1
 r$improvements$target_ready <- 8
 r$assumptions$Valor[r$assumptions$Item == "Meta M (viaturas prontas)"] <- "8"
 s <- paste(unlist(revision_core$word_report_narrative_local(r)[c("sintese", "conclusao")]), collapse = " ")
 revision_check(!grepl("abaixo da probabilidade-alvo", s) && !grepl("não assegura a prontidão", s), "A met target cannot be described as missed.")
 revision_check(grepl("cumpre a exigência", s), "A met target must be stated.")
 ab <- revision_core$word_report_resumo_local(r)$pt
 revision_check(!grepl("não assegura", ab), "Resumo must follow the mission result.")
 s2 <- paste(unlist(revision_core$word_report_narrative_local(tcc_fixture())[c("sintese", "conclusao")]), collapse = " ")
 revision_check(grepl("abaixo da probabilidade-alvo|não assegura", s2), "A missed target must still be stated.")
})


revision_test("Audit20261012: the AI receives the full results (not the app draft) and gets one correction round", {
 ds <- revision_core$word_report_ai_dataset(tcc_report)
 revision_check(is.null(ds$rascunho_do_aplicativo) && is.list(ds$dados$missao) && is.list(ds$enquadramento_da_pesquisa) && length(ds$comparacoes_calculadas) > 0L,
                "The AI payload must carry the research frame, the mission, the results and precomputed comparisons, without the app draft.")
 js <- as.character(jsonlite::toJSON(ds, auto_unbox = TRUE, null = "null", na = "null", dataframe = "rows"))
 revision_check(grepl("94,2%", js, fixed = TRUE) && grepl("probabilidade de missão menos a probabilidade-alvo", js, fixed = TRUE), "Values must go formatted and the mission gap precomputed.")
 sp <- revision_core$word_report_ai_system_prompt()
 revision_check(grepl("ANALISE", sp) && grepl("conceito \\(com o autor\\), dado e associação", sp) && !grepl("REESCREVA", sp), "The prompt must ask for analysis under the writing guidance, not a rewrite of the draft.")
 original <- revision_core$word_report_call_ai
 calls <- new.env(); calls$n <- 0L; calls$issues <- NULL
 core <- revision_core$word_report_decision_core(tcc_report)
 words <- function(n) paste(rep("palavra", n), collapse = " ")
 revision_core$word_report_call_ai <- function(facts, provider, api_key, model = NULL, timeout = 900, previous = NULL, issues = NULL) {
   calls$n <- calls$n + 1L; if (!is.null(issues)) calls$issues <- issues
   proc <- if (is.null(previous)) "O reparo corretivo concentra 70,0% do tempo parado, e a frota ganharia 13,7 p.p. com a medida." else
     "O reparo corretivo concentra 70,0% do tempo parado: a prontidão depende da rapidez do ciclo de manutenção, e não da espera por peça."
   list(processo = c(proc, "Por isso, a intervenção que encurta o reparo atua sobre a parcela dominante do tempo parado da viatura."),
        conclusao = core$conclusao, resumo = words(200), abstract = words(200))
 }
 tryCatch({
  n <- revision_core$word_report_resolve_narrative(tcc_report, "claude", "test-only")
  revision_check(calls$n == 2L && any(grepl("13,7", calls$issues, fixed = TRUE)), "A paragraph with an invented number must trigger one correction request naming the number.")
  revision_check(any(grepl("rapidez do ciclo", n$text$processo, fixed = TRUE)) && !any(grepl("13,7", n$text$processo, fixed = TRUE)), "The corrected AI paragraph must be used.")
  revision_check(identical(n$model, "claude-opus-5-5") && "processo" %in% n$replaced, "The report must record the model and what the AI wrote.")
 }, finally = { revision_core$word_report_call_ai <- original })
})

cat("REVISION:", revision_passed, "passed;", length(revision_failed), "failed.\n")
if (length(revision_failed)) stop(paste(revision_failed, collapse = "\n"), call. = FALSE)
