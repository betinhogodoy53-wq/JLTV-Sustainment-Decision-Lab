# JLTV SUSTAINMENT DECISION LAB — 1.5.7
# CODIGO COMPLETO DO APLICATIVO EM UM UNICO ARQUIVO R
# Consolidado em 19/10/2026 para leitura e avaliacao.
# Inclui app.R e os quatro modulos carregados por ele, na ordem original.
# Os sete arquivos originais (incluindo instalador e iniciador) estao em 02_CODIGO.
# Este arquivo preserva as funcoes dos modulos e substitui somente suas chamadas
# de carregamento pelo respectivo conteudo. Recursos graficos continuam em
# 02_CODIGO/www. Nao inclui dados pessoais nem pacotes R instalados.
# Para abrir no Windows, prefira 01_ABRIR_SIMULADOR.cmd.
# Alternativa: Rscript --vanilla JLTV_Sustainment_Decision_Lab_CODIGO_COMPLETO.R

if (!identical(Sys.getenv("JLTV_CORE_ONLY"), "1")) {
  .jltv_file_arg = grep("^--file=", commandArgs(FALSE), value = TRUE)
  .jltv_source_files = unlist(lapply(sys.frames(), function(frame) frame$ofile))
  .jltv_script = if (length(.jltv_source_files)) tail(.jltv_source_files, 1) else
    if (length(.jltv_file_arg)) sub("^--file=", "", .jltv_file_arg[[1]]) else ""
  .jltv_root = if (nzchar(.jltv_script)) dirname(normalizePath(.jltv_script, mustWork = TRUE)) else getwd()
  .jltv_code = file.path(.jltv_root, "02_CODIGO")
  if (dir.exists(.jltv_code)) setwd(.jltv_code)
  if (!nzchar(Sys.getenv("JLTV_DB_PATH", ""))) {
    .jltv_data = file.path(.jltv_root, "DADOS")
    dir.create(.jltv_data, recursive = TRUE, showWarnings = FALSE)
    Sys.setenv(JLTV_DB_PATH = file.path(.jltv_data, "jltv.sqlite"))
  }
  if (!nzchar(Sys.getenv("JLTV_SEED_SYNTHETIC", ""))) Sys.setenv(JLTV_SEED_SYNTHETIC = "0")
}

`%||%` = function(left, right) {
  if (is.null(left)) right else left
}


JLTV_MODEL_VERSION = "1.5.7"
JLTV_SCHEMA_VERSION = "4"

# ============================================================================
# v1.4.2 — ITERAÇÃO COM PURRR
# ============================================================================
# purrr::map() e variantes tipadas (map_chr, map_dbl, map_lgl, map_int)
# substituem lapply/vapply e laços de acumulação. Duas regras preservam o
# comportamento da v1.4.1:
# 1. purrr embrulha erros ocorridos dentro do map ("In index: k"). Onde a
#    mensagem é exibida ao usuário, without_purrr_index() devolve o erro
#    original, com a mesma mensagem.
# 2. A junção de linhas continua com rbind (bind_rows_base); purrr::list_rbind
#    usa as regras do vctrs, que podem alterar tipos e nomes de linha.
without_purrr_index = function(expr) {
  tryCatch(expr, purrr_error_indexed = function(error_condition) {
    while (inherits(error_condition, "purrr_error_indexed") &&
           inherits(error_condition$parent, "condition")) {
      error_condition = error_condition$parent
    }
    stop(error_condition)
  })
}

root_condition_message = function(error_condition) {
  while (inherits(error_condition, "purrr_error_indexed") &&
         inherits(error_condition$parent, "condition")) {
    error_condition = error_condition$parent
  }
  conditionMessage(error_condition)
}

bind_rows_base = function(rows, template = NULL) {
  if (length(rows) == 0L) return(template)
  do.call(rbind, rows)
}

questionnaire_hierarchy_spec = function() {
  list(
    criteria = c(
      OCCURRENCE = "Falha / Ocorrência",
      DOWNTIME = "Impacto na Disponibilidade",
      SUPPLY = "Suprimento",
      SUPPORT = "Manutenção e Apoio"
    ),
    sub_occurrence = c(
      FREQUENCY = "Frequência",
      RECURRENCE = "Recorrência",
      FLEET_SCOPE = "Abrangência na frota"
    ),
    sub_downtime = c(
      TOTAL_OUT_OF_SERVICE = "Tempo total fora de serviço",
      EMPLOYMENT_IMPACT = "Efeito na capacidade de emprego"
    ),
    sub_supply = c(
      SPARE_DEPENDENCY = "Dependência de sobressalente",
      STOCKOUT = "Falta/indisponibilidade em estoque",
      LEAD_TIME = "Lead time de reposição"
    ),
    sub_support = c(
      ACTIVE_MAINTENANCE = "Tempo de manutenção ativa",
      EXTERNAL_SUPPORT = "Dependência de recurso/apoio externo"
    )
  )
}

questionnaire_parent_map = function() {
  c(
    sub_occurrence = "OCCURRENCE",
    sub_downtime = "DOWNTIME",
    sub_supply = "SUPPLY",
    sub_support = "SUPPORT"
  )
}

questionnaire_google_defaults = function() {
  list(
    source_mode = "GOOGLE_SHEETS",
    sheet_url = "https://docs.google.com/spreadsheets/d/1Org95kv5QZFKSSt5y-OfqVy_if0_QE6-y0T3bu-TIC8/edit",
    sheet_name = "RESPOSTAS",
    auto_refresh = FALSE,
    refresh_seconds = 600,
    evidence_class = "EMPIRICAL"
  )
}

questionnaire_alternative_spec = function() {
  c(
    A1 = "Motor/comb.",
    A2 = "Transmissão/transaxle",
    A3 = "Driveline",
    A4 = "Cooling package",
    A5 = "Suspensão/direção",
    A6 = "Freios/pneumático",
    A7 = "CTIS/rodas/pneus",
    A8 = "SISTEMA ELÉTRICO/ ELETRÔNICO",
    A9 = "DSDU/MUX",
    A10 = "Auxiliares/missão"
  )
}

questionnaire_p6_subcriteria = function() {
  c(
    `1` = "FREQUENCY",
    `2` = "RECURRENCE",
    `3` = "FLEET_SCOPE",
    `4` = "TOTAL_OUT_OF_SERVICE",
    `5` = "EMPLOYMENT_IMPACT",
    `6` = "SPARE_DEPENDENCY",
    `7` = "STOCKOUT",
    `8` = "LEAD_TIME",
    `9` = "ACTIVE_MAINTENANCE",
    `10` = "EXTERNAL_SUPPORT"
  )
}

questionnaire_pair_spec = function() {
  data.frame(
    code = c(
      "P1.1","P1.2","P1.3","P1.4","P1.5","P1.6",
      "P2.1","P2.2","P2.3","P3.1","P4.1","P4.2","P4.3","P5.1"
    ),
    hierarchy_group = c(
      rep("criteria", 6), rep("sub_occurrence", 3), "sub_downtime",
      rep("sub_supply", 3), "sub_support"
    ),
    left_item = c(
      "OCCURRENCE","OCCURRENCE","OCCURRENCE","DOWNTIME","DOWNTIME","SUPPLY",
      "FREQUENCY","FREQUENCY","RECURRENCE","TOTAL_OUT_OF_SERVICE",
      "SPARE_DEPENDENCY","SPARE_DEPENDENCY","STOCKOUT","ACTIVE_MAINTENANCE"
    ),
    right_item = c(
      "DOWNTIME","SUPPLY","SUPPORT","SUPPLY","SUPPORT","SUPPORT",
      "RECURRENCE","FLEET_SCOPE","FLEET_SCOPE","EMPLOYMENT_IMPACT",
      "STOCKOUT","LEAD_TIME","LEAD_TIME","EXTERNAL_SUPPORT"
    ),
    left_label = c(
      "FALHA / OCORRÊNCIA","FALHA / OCORRÊNCIA","FALHA / OCORRÊNCIA",
      "IMPACTO NA DISPONIBILIDADE","IMPACTO NA DISPONIBILIDADE","SUPRIMENTO",
      "FREQUÊNCIA","FREQUÊNCIA","RECORRÊNCIA","TEMPO TOTAL FORA DE SERVIÇO",
      "DEPENDÊNCIA DE SOBRESSALENTE","DEPENDÊNCIA DE SOBRESSALENTE",
      "FALTA/INDISPONIBILIDADE EM ESTOQUE","TEMPO DE MANUTENÇÃO ATIVA"
    ),
    right_label = c(
      "IMPACTO NA DISPONIBILIDADE","SUPRIMENTO","MANUTENÇÃO E APOIO",
      "SUPRIMENTO","MANUTENÇÃO E APOIO","MANUTENÇÃO E APOIO",
      "RECORRÊNCIA","ABRANGÊNCIA NA FROTA","ABRANGÊNCIA NA FROTA",
      "EFEITO NA CAPACIDADE DE EMPREGO","FALTA/INDISPONIBILIDADE EM ESTOQUE",
      "LEAD TIME DE REPOSIÇÃO","LEAD TIME DE REPOSIÇÃO",
      "DEPENDÊNCIA DE RECURSO/APOIO EXTERNO"
    ),
    stringsAsFactors = FALSE
  )
}

questionnaire_scalar_text = function(value) {
  if (is.null(value) || length(value) == 0L || is.na(value[[1]])) return("")
  trimws(as.character(value[[1]]))
}

normalize_questionnaire_text = function(value) {
  x = as.character(value)
  x[is.na(x)] = ""
  x = toupper(trimws(x))
  x = iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  gsub("[[:space:]]+", " ", x)
}

questionnaire_is_synthetic_row = function(row_values) {
  text = normalize_questionnaire_text(paste(as.character(row_values), collapse = " | "))
  grepl("FICTICIO|TESTE SINTETICO|TESTE FICTICIO|@TESTE\\.INVALID", text)
}

questionnaire_find_column = function(headers, pattern, fixed = FALSE) {
  hits = if (isTRUE(fixed)) which(headers == pattern) else grep(pattern, headers, perl = TRUE)
  if (length(hits) != 1L) {
    stop(paste0("Cabeçalho do questionário não identificado de forma única: ", pattern), call. = FALSE)
  }
  hits[[1]]
}

questionnaire_find_column_optional = function(headers, pattern) {
  hits = grep(pattern, headers, perl = TRUE)
  if (length(hits) == 1L) hits[[1]] else NA_integer_
}

parse_questionnaire_ahp_answer = function(value, left_label, right_label) {
  text = questionnaire_scalar_text(value)
  if (!nzchar(text)) stop("Resposta AHP vazia.", call. = FALSE)
  if (grepl("^1\\s*[—-]", text) || normalize_questionnaire_text(text) == "1") {
    return(list(selected_side = "EQUAL", intensity = 1L))
  }
  number = suppressWarnings(as.integer(sub("^\\s*([2-9]).*$", "\\1", text)))
  if (!is.finite(number) || number < 2L || number > 9L) {
    stop(paste("Resposta AHP inválida:", text), call. = FALSE)
  }
  normalized = normalize_questionnaire_text(text)
  left_norm = normalize_questionnaire_text(left_label)
  right_norm = normalize_questionnaire_text(right_label)
  left_hit = grepl(left_norm, normalized, fixed = TRUE)
  right_hit = grepl(right_norm, normalized, fixed = TRUE)
  if (left_hit && !right_hit) return(list(selected_side = "LEFT", intensity = number))
  if (right_hit && !left_hit) return(list(selected_side = "RIGHT", intensity = number))
  stop(paste("Não foi possível identificar o lado da resposta AHP:", text), call. = FALSE)
}

questionnaire_source_value = function(path, key, default = "") {
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  result = DBI::dbGetQuery(
    connection, "SELECT value FROM meta WHERE key = ?", params = list(key)
  )
  if (nrow(result) == 0L) default else as.character(result$value[[1]])
}

set_questionnaire_source_values = function(path, values) {
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  DBI::dbWithTransaction(connection, {
    for (key in names(values)) {
      DBI::dbExecute(
        connection,
        "INSERT OR REPLACE INTO meta(key, value) VALUES(?, ?)",
        params = list(paste0("questionnaire_", key), as.character(values[[key]]))
      )
    }
  })
  invisible(TRUE)
}

get_questionnaire_source_config = function(path) {
  defaults = questionnaire_google_defaults()
  read_value = function(name, default) {
    questionnaire_source_value(path, paste0("questionnaire_", name), as.character(default))
  }
  list(
    source_mode = read_value("source_mode", defaults$source_mode),
    sheet_url = read_value("sheet_url", defaults$sheet_url),
    sheet_name = read_value("sheet_name", defaults$sheet_name),
    auto_refresh = identical(toupper(read_value("auto_refresh", defaults$auto_refresh)), "TRUE"),
    refresh_seconds = max(300, suppressWarnings(as.numeric(read_value("refresh_seconds", defaults$refresh_seconds))), na.rm = TRUE),
    evidence_class = toupper(read_value("evidence_class", defaults$evidence_class))
  )
}

resolve_questionnaire_sheet_name = function(requested, available) {
  available = as.character(available)
  requested = trimws(as.character(requested %||% ""))
  if (nzchar(requested) && requested %in% available) return(requested)

  preferred = c("RESPOSTAS", "Form Responses 1", "Form_Responses")
  preferred_hit = preferred[preferred %in% available]
  if (length(preferred_hit) > 0L) return(preferred_hit[[1]])

  data_sheets = setdiff(available, c("LINKS"))
  if (length(data_sheets) == 1L) return(data_sheets[[1]])

  stop(
    paste0(
      "Não foi possível localizar automaticamente a aba de respostas. ",
      "Abas disponíveis: ", paste(available, collapse = ", ")
    ),
    call. = FALSE
  )
}

read_google_questionnaire = function(sheet_url, sheet_name = "RESPOSTAS", authenticate = FALSE) {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop("Pacote googlesheets4 não instalado. Execute 01_INSTALAR_DEPENDENCIAS.cmd.", call. = FALSE)
  }
  cache_dir = questionnaire_google_cache_dir()
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(authenticate)) {
    stop(
      "A autenticação interativa deve ser iniciada pelo botão Conectar Google.",
      call. = FALSE
    )
  }
  available = googlesheets4::sheet_names(sheet_url)
  resolved_sheet = resolve_questionnaire_sheet_name(sheet_name, available)
  raw = googlesheets4::read_sheet(
    ss = sheet_url, sheet = resolved_sheet, col_types = "c",
    .name_repair = "minimal"
  )
  attr(raw, "resolved_sheet_name") = resolved_sheet
  raw
}

# v1.4.1 — CSV no padrão brasileiro (Excel pt-BR: ';' e vírgula decimal) ou
# internacional (',' e ponto decimal), UTF-8 com ou sem BOM.
read_csv_flexible = function(path) {
  first = readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  first = if (length(first)) first[[1]] else ""
  semicolons = nchar(gsub("[^;]", "", first))
  commas = nchar(gsub("[^,]", "", first))
  separator = if (semicolons > commas) ";" else ","
  data = utils::read.csv(
    path, sep = separator, dec = if (separator == ";") "," else ".",
    stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM"
  )
  names(data) = sub("^\ufeff", "", names(data))
  data
}

# v1.5.7 — leitura tolerante de planilhas: escolhe a aba que contém as colunas P1–P6, acha a linha de
# cabeçalho (mesmo com título acima), lê tudo como texto e descarta colunas/linhas vazias.
questionnaire_clean_headers = function(headers) {
  x = as.character(headers)
  x[is.na(x)] = ""
  x = sub("^\ufeff", "", x)
  x = gsub("[\u00a0\u2007\u202f]", " ", x, perl = TRUE)
  x = gsub("[[:space:]]+", " ", x)
  trimws(x)
}

read_questionnaire_excel = function(path) {
  sheets = readxl::excel_sheets(path)
  best = NULL
  best_score = -1
  for (sheet in sheets) {
    d = tryCatch(
      suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")),
      error = function(e) NULL
    )
    if (is.null(d) || nrow(d) < 2L || ncol(d) < 2L) next
    d = as.data.frame(d, stringsAsFactors = FALSE, check.names = FALSE)
    scan_rows = seq_len(min(20L, nrow(d)))
    row_scores = vapply(scan_rows, function(r) {
      sum(grepl("^P[1-6]\\.[0-9]", questionnaire_clean_headers(unlist(d[r, ], use.names = FALSE))))
    }, numeric(1))
    header_row = scan_rows[[which.max(row_scores)]]
    score = max(row_scores)
    if (score > best_score) best = list(sheet = sheet, data = d, header_row = header_row, score = score)
    best_score = max(best_score, score)
  }
  if (is.null(best)) stop("Nenhuma aba da planilha contém respostas.", call. = FALSE)
  d = best$data
  header = questionnaire_clean_headers(unlist(d[best$header_row, ], use.names = FALSE))
  body = d[-seq_len(best$header_row), , drop = FALSE]
  keep_cols = nzchar(header)
  body = body[, keep_cols, drop = FALSE]
  names(body) = header[keep_cols]
  blank_rows = apply(body, 1L, function(r) all(is.na(r) | !nzchar(trimws(as.character(r)))))
  body = body[!blank_rows, , drop = FALSE]
  row.names(body) = NULL
  attr(body, "resolved_sheet_name") = best$sheet
  body
}

read_questionnaire_file = function(path) {
  extension = tolower(tools::file_ext(path))
  if (extension == "csv") {
    return(read_csv_flexible(path))
  }
  if (extension %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Pacote readxl não instalado. Execute 01_INSTALAR_DEPENDENCIAS.cmd.", call. = FALSE)
    }
    return(read_questionnaire_excel(path))
  }
  stop("Use um arquivo CSV, XLSX ou XLS.", call. = FALSE)
}

questionnaire_declares_experience = function(text) {
  normalized = normalize_questionnaire_text(text)
  nzchar(normalized) &
    !grepl("^(NENHUM|NENHUMA|NAO|SEM EXPERIENCIA|SEM CONTATO|N/?A$|-$)", normalized)
}

transform_google_forms_questionnaire = function(raw, evidence_class = "EMPIRICAL", progress = NULL) {
  if (!is.data.frame(raw) || nrow(raw) == 0L) {
    stop("A planilha do questionário não possui respostas.", call. = FALSE)
  }
  raw = as.data.frame(raw, stringsAsFactors = FALSE, check.names = FALSE)
  names(raw) = questionnaire_clean_headers(names(raw))
  headers = names(raw)
  # Texto de todas as células de uma vez (acesso por matriz é muito mais rápido que raw[i, ]).
  values_mat = matrix(unlist(lapply(raw, function(col) {
    x = as.character(col)
    x[is.na(x)] = ""
    trimws(x)
  }), use.names = FALSE), nrow = nrow(raw))
  # Google Forms gera "Timestamp" (inglês) ou "Carimbo de data/hora" (português).
  # Colunas de perfil e P7 são opcionais: planilhas de outras fontes podem não tê-las.
  timestamp_col = questionnaire_find_column_optional(headers, "^(Timestamp|Carimbo de data/hora|Data/hora)$")
  contact_col = questionnaire_find_column_optional(headers, "^a\\. E-mail e celular")
  age_col = questionnaire_find_column_optional(headers, "^b\\. Idade")
  academic_col = questionnaire_find_column_optional(headers, "^c\\. Formação acadêmica")
  military_col = questionnaire_find_column_optional(headers, "^d\\. Formação militar")
  service_col = questionnaire_find_column_optional(headers, "^e\\. Qual o seu tempo de serviço")
  experience_col = questionnaire_find_column_optional(headers, "^f\\. Qual sua experiência com a JLTV")
  pair_spec = questionnaire_pair_spec()
  alt_spec = questionnaire_alternative_spec()
  p6_map = questionnaire_p6_subcriteria()
  evidence_class = toupper(trimws(as.character(evidence_class)))
  if (!evidence_class %in% c("EMPIRICAL","SYNTHETIC")) evidence_class = "EMPIRICAL"

  # Estrutura do cabeçalho verificada uma única vez (erro estrutural = aborta).
  pair_columns = without_purrr_index(purrr::map_int(purrr::set_names(pair_spec$code), function(code) {
    questionnaire_find_column_optional(headers, paste0("^", gsub("\\.", "\\\\.", code), "[\\s:.)-]"))
  }))
  if (anyNA(pair_columns)) {
    stop(paste0(
      "Colunas AHP ausentes ou repetidas na planilha: ", paste(names(pair_columns)[is.na(pair_columns)], collapse = ", "),
      ". O cabeçalho de cada comparação deve começar pelo código (ex.: 'P1.1 — ...')."
    ), call. = FALSE)
  }
  p6_headers = grep("^P6\\.", headers, value = TRUE)
  p6_headers = p6_headers[!grepl("\\[Row 1\\]$", p6_headers)]
  if (length(p6_headers) != 100L) {
    stop(paste0(
      "P6 deve possuir exatamente 100 avaliações (10 critérios × 10 sistemas, com cabeçalho 'P6.n ... [An Nome]'); encontradas ",
      length(p6_headers), "."
    ), call. = FALSE)
  }
  p6_parts = without_purrr_index(purrr::map(p6_headers, function(header) {
    parts = regmatches(header, regexec("^P6\\.([0-9]+).*\\[A([0-9]+)[\\s\\]]", header, perl = TRUE))[[1]]
    if (length(parts) != 3L || !parts[[2]] %in% names(p6_map) ||
        !paste0("A", as.integer(parts[[3]])) %in% names(alt_spec)) {
      stop(paste("Cabeçalho P6 inválido:", header), call. = FALSE)
    }
    list(subcriterion = unname(p6_map[[parts[[2]]]]), alt_code = paste0("A", as.integer(parts[[3]])))
  }))
  p6_keys = purrr::map_chr(p6_parts, function(x) paste(x$alt_code, x$subcriterion))
  # v1.4.2 — atributos de P6 calculados uma vez; cada resposta vira um único
  # bloco de 100 linhas (antes: 100 data.frames de uma linha por respondente).
  p6_alt_codes = purrr::map_chr(p6_parts, "alt_code")
  p6_subcriteria = purrr::map_chr(p6_parts, "subcriterion")
  p6_alt_names = unname(alt_spec[p6_alt_codes])
  if (anyDuplicated(p6_keys)) {
    stop("P6 possui cabeçalhos repetidos para a mesma alternativa e subcritério.", call. = FALSE)
  }
  p7_columns = without_purrr_index(purrr::map(1:4, function(block) list(
    system = questionnaire_find_column_optional(headers, paste0("^", block, "\\. Sistema/conjunto$")),
    identification = questionnaire_find_column_optional(headers, paste0("^", block, "\\. Descrição e PN/NSN")),
    occurs = questionnaire_find_column_optional(headers, paste0("^", block, "\\. O que ocorre")),
    evidence = questionnaire_find_column_optional(headers, paste0("^", block, "\\. OS, requisição"))
  )))
  p6_columns = match(p6_headers, headers)

  respondents = list()
  ahp_rows = list()
  score_rows = list()
  factor_rows = list()
  skipped = list()
  warnings = list()
  seen_ids = character()

  for (i in seq_len(nrow(raw))) {
    if (is.function(progress) && (i %% 25L == 1L)) progress(i, nrow(raw))
    if (!any(nzchar(values_mat[i, ]))) next
    cell = function(col) if (is.na(col)) "" else values_mat[i, col]
    row_warnings = character()
    parsed_row = tryCatch({
      row_values = values_mat[i, ]
      contact = cell(contact_col)
      timestamp = cell(timestamp_col)
      # A classe (EMPIRICAL ou SYNTHETIC) é sempre a escolhida pelo usuário; o aplicativo não a troca sozinho.
      # Termos como "fictício" no texto apenas geram um aviso.
      synthetic = questionnaire_is_synthetic_row(row_values)
      row_class = evidence_class
      if (synthetic) row_warnings = c(row_warnings, "contém termo de dado fictício; mantida a classe escolhida pelo usuário")
      source = paste0(row_class, "_GOOGLE_SHEETS")
      signature = paste(timestamp, contact, paste(row_values, collapse = "|"), sep = "||")
      respondent_id = paste0("Q-", substr(digest::digest(signature, algo = "sha256", serialize = FALSE), 1, 16))
      age_text = cell(age_col)
      service_text = cell(service_col)
      age = suppressWarnings(as.numeric(gsub(",", ".", sub(
        ".*?([0-9]+[\\.,]?[0-9]*).*", "\\1", age_text, perl = TRUE
      ))))
      service = suppressWarnings(as.numeric(gsub(",", ".", sub(
        ".*?([0-9]+[\\.,]?[0-9]*).*", "\\1", service_text, perl = TRUE
      ))))
      if (!is.finite(age)) age = NA_real_
      if (!is.finite(service)) service = NA_real_
      experience_text = cell(experience_col)

      respondent = data.frame(
        respondent_id = respondent_id,
        age = age,
        service_years = service,
        academic_background = cell(academic_col),
        military_background = cell(military_col),
        direct_jltv_experience = as.integer(questionnaire_declares_experience(experience_text)),
        experience_area = experience_text,
        data_source = source,
        stringsAsFactors = FALSE
      )

      ahp_parsed = purrr::map(seq_len(nrow(pair_spec)), function(j) {
        tryCatch(
          parse_questionnaire_ahp_answer(
            values_mat[i, pair_columns[[j]]], pair_spec$left_label[[j]], pair_spec$right_label[[j]]
          ),
          error = function(e) e
        )
      })
      ahp_failed = purrr::detect_index(ahp_parsed, function(x) inherits(x, "error"))
      if (ahp_failed > 0L) {
        stop(paste0(pair_spec$code[[ahp_failed]], ": ", conditionMessage(ahp_parsed[[ahp_failed]])), call. = FALSE)
      }
      ahp = data.frame(
        respondent_id = respondent_id,
        hierarchy_group = pair_spec$hierarchy_group,
        left_item = pair_spec$left_item,
        right_item = pair_spec$right_item,
        selected_side = purrr::map_chr(ahp_parsed, "selected_side"),
        intensity = purrr::map_int(ahp_parsed, function(x) as.integer(x$intensity)),
        data_source = source,
        stringsAsFactors = FALSE
      )

      p6_values = values_mat[i, p6_columns]
      p6_is_na = !nzchar(p6_values) | toupper(p6_values) == "N/A"
      p6_scores = suppressWarnings(as.numeric(gsub(",", ".", p6_values)))
      p6_scores[p6_is_na] = NA_real_
      unrecognized = which(!p6_is_na & is.na(p6_scores))
      if (length(unrecognized) > 0L) {
        row_warnings = c(row_warnings, paste0(
          p6_headers[unrecognized], ": valor não reconhecido '", p6_values[unrecognized], "' tratado como N/A."
        ))
      }
      out_of_scale = which(!is.na(p6_scores) & (!is.finite(p6_scores) | p6_scores < 1 | p6_scores > 10))
      if (length(out_of_scale) > 0L) {
        first_bad = out_of_scale[[1]]
        stop(paste0("P6 fora da escala 1–10/N/A em ", p6_headers[[first_bad]], ": ", p6_values[[first_bad]]), call. = FALSE)
      }
      scores = data.frame(
        respondent_id = respondent_id,
        alternative_id = p6_alt_codes,
        alternative_name = p6_alt_names,
        subcriterion_id = p6_subcriteria,
        score = p6_scores,
        data_source = source,
        stringsAsFactors = FALSE
      )

      factors = purrr::compact(purrr::map(1:4, function(block) {
        columns = p7_columns[[block]]
        system_value = cell(columns$system)
        identification = cell(columns$identification)
        occurs = cell(columns$occurs)
        evidence = cell(columns$evidence)
        if (!nzchar(trimws(system_value)) && !nzchar(trimws(identification)) &&
            !nzchar(trimws(occurs)) && !nzchar(trimws(evidence))) return(NULL)
        data.frame(
          respondent_id = respondent_id,
          factor_type = paste0("P7_", block),
          identification = identification,
          occurrence_detail = occurs,
          related_system = system_value,
          influence_score = NA_real_,
          evidence_reference = evidence,
          data_source = source,
          stringsAsFactors = FALSE
        )
      }))
      list(ok = TRUE, respondent = respondent, ahp = ahp, scores = scores, factors = factors)
    }, error = function(error_condition) {
      list(ok = FALSE, message = root_condition_message(error_condition))
    })

    if (!isTRUE(parsed_row$ok)) {
      skipped[[length(skipped) + 1L]] = data.frame(
        linha_planilha = i + 1L, motivo = parsed_row$message, stringsAsFactors = FALSE
      )
      next
    }
    if (parsed_row$respondent$respondent_id %in% seen_ids) {
      warnings[[length(warnings) + 1L]] = data.frame(
        linha_planilha = i + 1L,
        aviso = "Resposta idêntica a outra linha; mantida uma única cópia.",
        stringsAsFactors = FALSE
      )
      next
    }
    seen_ids = c(seen_ids, parsed_row$respondent$respondent_id)
    respondents[[length(respondents) + 1L]] = parsed_row$respondent
    ahp_rows[[length(ahp_rows) + 1L]] = parsed_row$ahp
    score_rows[[length(score_rows) + 1L]] = parsed_row$scores
    factor_rows = c(factor_rows, parsed_row$factors)
    if (length(row_warnings) > 0L) {
      warnings[[length(warnings) + 1L]] = data.frame(
        linha_planilha = i + 1L, aviso = row_warnings, stringsAsFactors = FALSE
      )
    }
  }

  skipped_data = if (length(skipped)) do.call(rbind, skipped) else
    data.frame(linha_planilha = integer(), motivo = character(), stringsAsFactors = FALSE)
  warning_data = if (length(warnings)) do.call(rbind, warnings) else
    data.frame(linha_planilha = integer(), aviso = character(), stringsAsFactors = FALSE)
  if (length(respondents) == 0L) {
    stop(paste0(
      "Nenhuma resposta válida no questionário. Primeiros problemas: ",
      paste(utils::head(paste0("linha ", skipped_data$linha_planilha, ": ", skipped_data$motivo), 3), collapse = " | ")
    ), call. = FALSE)
  }

  bind_rows = function(x, template) {
    if (length(x) == 0L) return(template)
    do.call(rbind, x)
  }
  result = list(
    questionnaire_respondents = bind_rows(respondents, data.frame(
      respondent_id=character(), age=numeric(), service_years=numeric(),
      academic_background=character(), military_background=character(),
      direct_jltv_experience=integer(), experience_area=character(),
      data_source=character(), stringsAsFactors=FALSE
    )),
    ahp_pairwise = bind_rows(ahp_rows, data.frame(
      respondent_id=character(), hierarchy_group=character(), left_item=character(),
      right_item=character(), selected_side=character(), intensity=integer(),
      data_source=character(), stringsAsFactors=FALSE
    )),
    topsis_scores = bind_rows(score_rows, data.frame(
      respondent_id=character(), alternative_id=character(), alternative_name=character(),
      subcriterion_id=character(), score=numeric(), data_source=character(),
      stringsAsFactors=FALSE
    )),
    questionnaire_factors = bind_rows(factor_rows, data.frame(
      respondent_id=character(), factor_type=character(), identification=character(),
      occurrence_detail=character(), related_system=character(), influence_score=numeric(),
      evidence_reference=character(), data_source=character(), stringsAsFactors=FALSE
    ))
  )
  attr(result, "skipped_rows") = skipped_data
  attr(result, "warnings") = warning_data
  result
}

replace_questionnaire_tables = function(path, normalized) {
  ahp_validation = validate_ahp_pairwise_data(normalized$ahp_pairwise)
  if (!isTRUE(ahp_validation$valid)) {
    stop(paste(ahp_validation$issues$message, collapse = "; "), call. = FALSE)
  }
  topsis_validation = validate_topsis_scores_data(normalized$topsis_scores)
  if (!isTRUE(topsis_validation$valid)) {
    stop(topsis_validation$message %||% "P6 inválida.", call. = FALSE)
  }
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  DBI::dbWithTransaction(connection, {
    for (table in c("questionnaire_factors","topsis_scores","ahp_pairwise","questionnaire_respondents")) {
      DBI::dbExecute(connection, paste("DELETE FROM", table))
    }
    if (nrow(normalized$questionnaire_respondents) > 0L) {
      DBI::dbAppendTable(connection, "questionnaire_respondents", normalized$questionnaire_respondents)
    }
    if (nrow(normalized$ahp_pairwise) > 0L) {
      DBI::dbAppendTable(connection, "ahp_pairwise", normalized$ahp_pairwise)
    }
    if (nrow(normalized$topsis_scores) > 0L) {
      DBI::dbAppendTable(connection, "topsis_scores", normalized$topsis_scores)
    }
    if (nrow(normalized$questionnaire_factors) > 0L) {
      DBI::dbAppendTable(connection, "questionnaire_factors", normalized$questionnaire_factors)
    }
  })
  invisible(TRUE)
}


replace_questionnaire_source_tables = function(path, normalized, source_suffix) {
  if (!is.character(source_suffix) || length(source_suffix) != 1L ||
      !source_suffix %in% c("_GOOGLE_SHEETS", "_FILE")) {
    stop("source_suffix inválido para atualização do questionário.", call. = FALSE)
  }
  # A fonte selecionada representa o questionário ativo inteiro. Ao trocar de
  # planilha/arquivo, respostas anteriores não podem permanecer somadas.
  replace_questionnaire_tables(path, normalized)
  invisible(TRUE)
}



append_questionnaire_tables = function(path, normalized) {
  ahp_validation = validate_ahp_pairwise_data(normalized$ahp_pairwise)
  if (!isTRUE(ahp_validation$valid)) {
    stop(paste(ahp_validation$issues$message, collapse = "; "), call. = FALSE)
  }
  topsis_validation = validate_topsis_scores_data(normalized$topsis_scores)
  if (!isTRUE(topsis_validation$valid)) {
    stop(topsis_validation$message %||% "P6 inválida.", call. = FALSE)
  }
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  existing = DBI::dbReadTable(connection, "questionnaire_respondents")
  duplicates = intersect(
    as.character(normalized$questionnaire_respondents$respondent_id),
    as.character(existing$respondent_id)
  )
  if (length(duplicates) > 0L) {
    stop(paste("Respondente já cadastrado:", paste(duplicates, collapse = ", ")), call. = FALSE)
  }
  DBI::dbWithTransaction(connection, {
    if (nrow(normalized$questionnaire_respondents) > 0L) {
      DBI::dbAppendTable(connection, "questionnaire_respondents", normalized$questionnaire_respondents)
    }
    if (nrow(normalized$ahp_pairwise) > 0L) {
      DBI::dbAppendTable(connection, "ahp_pairwise", normalized$ahp_pairwise)
    }
    if (nrow(normalized$topsis_scores) > 0L) {
      DBI::dbAppendTable(connection, "topsis_scores", normalized$topsis_scores)
    }
    if (nrow(normalized$questionnaire_factors) > 0L) {
      DBI::dbAppendTable(connection, "questionnaire_factors", normalized$questionnaire_factors)
    }
  })
  invisible(TRUE)
}

questionnaire_google_cache_dir = function() {
  base_dir = trimws(Sys.getenv("LOCALAPPDATA", unset = ""))
  if (nzchar(base_dir)) {
    return(file.path(base_dir, "JLTV_RAM_Decision_Lab", "oauth"))
  }
  file.path(path.expand("~"), ".jltv_ram_decision_lab", "oauth")
}

questionnaire_has_cached_google_auth = function() {
  cache_dir = questionnaire_google_cache_dir()
  if (!dir.exists(cache_dir)) return(FALSE)
  auxiliary = c("google_auth_status.txt", "google_auth_profile.R")
  candidates = setdiff(
    list.files(cache_dir, all.files = TRUE, no.. = TRUE),
    auxiliary
  )
  length(candidates) > 0L
}

questionnaire_google_auth_status_file = function() {
  file.path(questionnaire_google_cache_dir(), "google_auth_status.txt")
}

launch_questionnaire_google_auth = function() {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop("Pacote googlesheets4 não instalado. Execute 01_INSTALAR_DEPENDENCIAS.cmd.", call. = FALSE)
  }
  if (.Platform$OS.type != "windows") {
    stop("Nesta versão, a autenticação guiada automática está configurada para Windows.", call. = FALSE)
  }
  cache_dir = questionnaire_google_cache_dir()
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  status_file = questionnaire_google_auth_status_file()
  if (file.exists(status_file)) unlink(status_file)
  profile_file = file.path(cache_dir, "google_auth_profile.R")
  cache_norm = normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
  status_norm = normalizePath(status_file, winslash = "/", mustWork = FALSE)
  script = c(
    sprintf("cache_dir <- %s", dQuote(cache_norm)),
    sprintf("status_file <- %s", dQuote(status_norm)),
    "tryCatch({",
    "  suppressPackageStartupMessages(library(googlesheets4))",
    "  googlesheets4::gs4_auth(email = NA, cache = cache_dir, scopes = 'spreadsheets.readonly', use_oob = FALSE)",
    "  if (!googlesheets4::gs4_has_token()) stop('Token Google não foi criado.')",
    "  writeLines('OK', status_file)",
    "}, error = function(e) {",
    "  writeLines(paste0('ERROR|', conditionMessage(e)), status_file)",
    "})",
    "q(save = 'no')"
  )
  writeLines(script, profile_file, useBytes = TRUE)
  rgui = file.path(R.home("bin"), "Rgui.exe")
  if (!file.exists(rgui)) {
    stop("Rgui.exe não localizado para concluir a autenticação Google.", call. = FALSE)
  }
  previous_profile = Sys.getenv("R_PROFILE_USER", unset = NA_character_)
  on.exit({
    if (is.na(previous_profile)) {
      Sys.unsetenv("R_PROFILE_USER")
    } else {
      Sys.setenv(R_PROFILE_USER = previous_profile)
    }
  }, add = TRUE)
  Sys.setenv(R_PROFILE_USER = normalizePath(profile_file, winslash = "/", mustWork = TRUE))
  system2(
    rgui,
    args = c("--no-save", "--no-restore", "--no-site-file"),
    wait = FALSE,
    stdout = FALSE,
    stderr = FALSE
  )
  invisible(status_file)
}

restore_questionnaire_google_auth = function() {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) return(FALSE)
  if (googlesheets4::gs4_has_token()) return(TRUE)
  if (!questionnaire_has_cached_google_auth()) {
    googlesheets4::gs4_deauth()
    return(FALSE)
  }
  cache_dir = questionnaire_google_cache_dir()
  result = try(
    googlesheets4::gs4_auth(
      email = TRUE,
      cache = cache_dir,
      scopes = "spreadsheets.readonly",
      use_oob = FALSE
    ),
    silent = TRUE
  )
  !inherits(result, "try-error") && googlesheets4::gs4_has_token()
}

sync_google_questionnaire = function(path, config, authenticate = FALSE) {
  if (isTRUE(authenticate)) {
    stop("Use launch_questionnaire_google_auth() para a autenticação interativa.", call. = FALSE)
  }
  if (!restore_questionnaire_google_auth()) {
    stop(
      "Google ainda não autenticado neste computador. Clique em Conectar Google e conclua o login no navegador.",
      call. = FALSE
    )
  }
  raw = read_google_questionnaire(
    config$sheet_url, config$sheet_name, authenticate = FALSE
  )
  resolved_sheet = attr(raw, "resolved_sheet_name", exact = TRUE)
  if (!is.null(resolved_sheet) && nzchar(resolved_sheet) &&
      !identical(resolved_sheet, config$sheet_name)) {
    config$sheet_name = resolved_sheet
    set_questionnaire_source_values(path, list(sheet_name = resolved_sheet))
  }
  fingerprint = digest::digest(
    list(raw = raw, evidence_class = config$evidence_class),
    algo = "sha256"
  )
  previous = questionnaire_source_value(
    path, "questionnaire_last_fingerprint", ""
  )
  if (identical(fingerprint, previous)) {
    previous_notes = questionnaire_source_value(path, "questionnaire_last_notes", "")
    return(list(
      changed = FALSE, rows = nrow(raw), imported = 0L,
      sheet_name = resolved_sheet %||% config$sheet_name,
      message = paste0(
        "Sem alterações; ", nrow(raw), " resposta(s) on-line na aba ",
        resolved_sheet %||% config$sheet_name, ".",
        if (nzchar(previous_notes)) paste0(" ", previous_notes) else ""
      )
    ))
  }
  normalized = transform_google_forms_questionnaire(
    as.data.frame(raw, stringsAsFactors = FALSE, check.names = FALSE),
    evidence_class = config$evidence_class
  )
  replace_questionnaire_source_tables(path, normalized, "_GOOGLE_SHEETS")
  notes = questionnaire_transform_notes(normalized)
  set_questionnaire_source_values(path, list(
    last_fingerprint = fingerprint,
    last_sync = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    last_rows = nrow(raw),
    last_notes = notes
  ))
  list(
    changed = TRUE,
    rows = nrow(raw),
    imported = nrow(normalized$questionnaire_respondents),
    ahp = nrow(normalized$ahp_pairwise),
    p6 = nrow(normalized$topsis_scores),
    p7 = nrow(normalized$questionnaire_factors),
    sheet_name = resolved_sheet %||% config$sheet_name,
    message = paste0(
      "Sincronizado: ", nrow(normalized$questionnaire_respondents),
      " respondente(s), ", nrow(normalized$ahp_pairwise),
      " comparações AHP e ", nrow(normalized$topsis_scores),
      " avaliações P6 na aba ", resolved_sheet %||% config$sheet_name, ".",
      if (nzchar(notes)) paste0(" ", notes) else ""
    ),
    skipped_rows = attr(normalized, "skipped_rows"),
    warnings = attr(normalized, "warnings")
  )
}

questionnaire_transform_notes = function(normalized) {
  skipped = attr(normalized, "skipped_rows")
  warnings = attr(normalized, "warnings")
  parts = character()
  if (is.data.frame(skipped) && nrow(skipped) > 0L) {
    parts = c(parts, paste0(
      nrow(skipped), " linha(s) ignorada(s) por erro: ",
      paste(utils::head(paste0("linha ", skipped$linha_planilha, " (", skipped$motivo, ")"), 3), collapse = "; "),
      if (nrow(skipped) > 3L) "; ..." else ""
    ))
  }
  if (is.data.frame(warnings) && nrow(warnings) > 0L) {
    parts = c(parts, paste0(nrow(warnings), " aviso(s): ",
                            paste(utils::head(warnings$aviso, 2), collapse = "; "),
                            if (nrow(warnings) > 2L) "; ..." else ""))
  }
  paste(parts, collapse = " ")
}

sync_questionnaire_file = function(path, file_path, evidence_class = "EMPIRICAL", progress = NULL) {
  evidence_class = toupper(trimws(as.character(evidence_class)))
  if (!evidence_class %in% c("EMPIRICAL", "SYNTHETIC")) evidence_class = "EMPIRICAL"  # "AUTO" = EMPIRICAL, salvo linhas marcadas como fictícias
  raw = read_questionnaire_file(file_path)
  normalized = transform_google_forms_questionnaire(
    as.data.frame(raw, stringsAsFactors = FALSE, check.names = FALSE),
    evidence_class = evidence_class, progress = progress
  )
  # v1.5.4 — a transformação marca a origem como "_GOOGLE_SHEETS"; para arquivo, regrava como "_FILE"
  # (sem isso a substituição por origem nunca encontrava as linhas do arquivo e a reimportação falhava).
  for (table_name in c("questionnaire_respondents", "ahp_pairwise", "topsis_scores", "questionnaire_factors")) {
    table_value = normalized[[table_name]]
    if (is.data.frame(table_value) && "data_source" %in% names(table_value)) {
      normalized[[table_name]]$data_source = sub("_GOOGLE_SHEETS$", "_FILE", as.character(table_value$data_source))
    }
  }
  replace_questionnaire_source_tables(path, normalized, "_FILE")
  set_questionnaire_source_values(path, list(last_fingerprint = ""))
  list(
    changed = TRUE,
    notes = questionnaire_transform_notes(normalized),
    rows = nrow(raw),
    imported = nrow(normalized$questionnaire_respondents),
    ahp = nrow(normalized$ahp_pairwise),
    p6 = nrow(normalized$topsis_scores),
    p7 = nrow(normalized$questionnaire_factors)
  )
}

# Respostas gravadas numa classe (ex.: SYNTHETIC) ficam invisíveis em outro modo de evidência (ex.: EMPIRICAL).
# Esta função conta o que existe no banco por classe e quanto está oculto no modo atual.
questionnaire_class_counts = function(path) {
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  result = DBI::dbGetQuery(connection, "SELECT data_source, COUNT(*) AS n FROM questionnaire_respondents WHERE data_source NOT LIKE '%_DEMO' GROUP BY data_source")
  if (nrow(result) == 0L) return(data.frame(evidence_class = character(), n = integer(), stringsAsFactors = FALSE))
  result$evidence_class = sub("_.*$", "", toupper(result$data_source))
  stats::aggregate(n ~ evidence_class, data = result, FUN = sum)
}

questionnaire_hidden_message = function(path, mode) {
  counts = questionnaire_class_counts(path)
  mode = toupper(trimws(as.character(mode %||% "HYBRID")))
  if (nrow(counts) == 0L || mode == "HYBRID") return("")
  hidden = counts[counts$evidence_class != mode, , drop = FALSE]
  visible = sum(counts$n[counts$evidence_class == mode])
  if (nrow(hidden) == 0L || sum(hidden$n) == 0L || visible > 0L) return("")
  paste0(
    sum(hidden$n), " resposta(s) estão gravadas como ", paste(hidden$evidence_class, collapse = "/"),
    ", mas o modo de evidência atual é ", mode, ", por isso aparecem 0 respondentes. ",
    "Use o botão 'Reclassificar respostas como ", mode, "' abaixo de 'Aplicar modo' ou troque o modo de evidência."
  )
}

reclassify_questionnaire_responses = function(path, to_class = "EMPIRICAL") {
  to_class = toupper(trimws(as.character(to_class)))
  if (!to_class %in% c("EMPIRICAL", "SYNTHETIC")) stop("Classe inválida.", call. = FALSE)
  other = setdiff(c("EMPIRICAL", "SYNTHETIC"), to_class)
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  changed = 0L
  DBI::dbWithTransaction(connection, {
    for (table in c("questionnaire_respondents", "ahp_pairwise", "topsis_scores", "questionnaire_factors")) {
      changed = changed + DBI::dbExecute(
        connection,
        paste0("UPDATE ", table, " SET data_source = ? || substr(data_source, ?) WHERE data_source LIKE ? AND data_source NOT LIKE '%_DEMO'"),
        params = list(to_class, nchar(other) + 1L, paste0(other, "_%"))
      )
    }
  })
  changed
}

manual_questionnaire_normalized = function(values, evidence_class = "EMPIRICAL") {
  pair_spec = questionnaire_pair_spec()
  alt_spec = questionnaire_alternative_spec()
  p6_map = questionnaire_p6_subcriteria()
  evidence_class = toupper(trimws(as.character(evidence_class)))
  if (!evidence_class %in% c("EMPIRICAL","SYNTHETIC")) evidence_class = "EMPIRICAL"
  source = paste0(evidence_class, "_MANUAL")
  signature = paste(
    values$contact %||% "", Sys.time(), paste(unlist(values$ahp), collapse = "|"),
    sep = "||"
  )
  respondent_id = paste0(
    "M-", substr(digest::digest(signature, algo = "sha256", serialize = FALSE), 1, 16)
  )
  respondents = data.frame(
    respondent_id = respondent_id,
    age = suppressWarnings(as.numeric(values$age)),
    service_years = suppressWarnings(as.numeric(values$service_years)),
    academic_background = values$academic_background %||% "",
    military_background = values$military_background %||% "",
    direct_jltv_experience = as.integer(questionnaire_declares_experience(values$experience %||% "")),
    experience_area = values$experience %||% "",
    data_source = source, stringsAsFactors = FALSE
  )
  ahp = bind_rows_base(without_purrr_index(purrr::map(seq_len(nrow(pair_spec)), function(i) {
    parsed = parse_questionnaire_ahp_answer(
      values$ahp[[pair_spec$code[[i]]]],
      pair_spec$left_label[[i]], pair_spec$right_label[[i]]
    )
    data.frame(
      respondent_id = respondent_id,
      hierarchy_group = pair_spec$hierarchy_group[[i]],
      left_item = pair_spec$left_item[[i]],
      right_item = pair_spec$right_item[[i]],
      selected_side = parsed$selected_side,
      intensity = as.integer(parsed$intensity),
      data_source = source, stringsAsFactors = FALSE
    )
  })))
  scores = list()
  idx = 1L
  for (a in seq_along(alt_spec)) {
    alt_code = names(alt_spec)[[a]]
    raw_values = strsplit(values$p6[[alt_code]] %||% "", "[;,]", perl = TRUE)[[1]]
    raw_values = trimws(raw_values)
    if (length(raw_values) != 10L) {
      stop(paste0(alt_code, ": informe exatamente 10 valores P6 separados por ponto e vírgula."), call. = FALSE)
    }
    for (c in 1:10) {
      value = raw_values[[c]]
      score = if (!nzchar(value) || toupper(value) == "N/A") NA_real_ else suppressWarnings(as.numeric(value))
      if (!is.na(score) && (!is.finite(score) || score < 1 || score > 10)) {
        stop(paste0(alt_code, " P6.", c, ": use 1–10 ou N/A."), call. = FALSE)
      }
      scores[[idx]] = data.frame(
        respondent_id = respondent_id,
        alternative_id = alt_code,
        alternative_name = unname(alt_spec[[alt_code]]),
        subcriterion_id = unname(p6_map[[as.character(c)]]),
        score = score, data_source = source, stringsAsFactors = FALSE
      )
      idx = idx + 1L
    }
  }
  factors = list()
  fidx = 1L
  for (block in 1:4) {
    p = values$p7[[block]]
    if (is.null(p)) next
    if (!nzchar(trimws(p$system %||% "")) &&
        !nzchar(trimws(p$identification %||% "")) &&
        !nzchar(trimws(p$occurs %||% "")) &&
        !nzchar(trimws(p$evidence %||% ""))) next
    factors[[fidx]] = data.frame(
      respondent_id = respondent_id,
      factor_type = paste0("P7_", block),
      identification = p$identification %||% "",
      occurrence_detail = p$occurs %||% "",
      related_system = p$system %||% "",
      influence_score = NA_real_,
      evidence_reference = p$evidence %||% "",
      data_source = source, stringsAsFactors = FALSE
    )
    fidx = fidx + 1L
  }
  list(
    questionnaire_respondents = respondents,
    ahp_pairwise = ahp,
    topsis_scores = do.call(rbind, scores),
    questionnaire_factors = if (length(factors)) do.call(rbind, factors) else data.frame(
      respondent_id=character(), factor_type=character(), identification=character(),
      occurrence_detail=character(), related_system=character(), influence_score=numeric(),
      evidence_reference=character(), data_source=character(), stringsAsFactors=FALSE
    )
  )
}

manual_ahp_choices = function(left_label, right_label) {
  values = c(
    paste0(9:2, " — ", left_label, " mais importante"),
    "1 — importância equivalente",
    paste0(2:9, " — ", right_label, " mais importante")
  )
  stats::setNames(values, values)
}

filter_questionnaire_snapshot_by_mode = function(snapshot, mode = "HYBRID") {
  mode = toupper(trimws(as.character(mode %||% "HYBRID")))
  if (!mode %in% c("SYNTHETIC","HYBRID","EMPIRICAL")) mode = "HYBRID"
  filter_table = function(df) {
    if (!is.data.frame(df) || nrow(df) == 0L || !"data_source" %in% names(df) || mode == "HYBRID") return(df)
    synthetic = grepl("^SYNTHETIC", toupper(as.character(df$data_source)))
    if (mode == "SYNTHETIC") df[synthetic, , drop = FALSE] else df[!synthetic, , drop = FALSE]
  }
  for (name in c("questionnaire_respondents","ahp_pairwise","topsis_scores","questionnaire_factors")) {
    snapshot[[name]] = filter_table(snapshot[[name]] %||% data.frame())
  }
  snapshot
}

# v1.4.1 — o modo de evidência também filtra as tabelas RAM/CMMS.
# EMPIRICAL exclui registros SYNTHETIC*; SYNTHETIC mantém só SYNTHETIC*;
# HYBRID mantém todos. OS seguem as viaturas mantidas; atrasos seguem as OS.
filter_cmms_snapshot_by_mode = function(snapshot, mode = "HYBRID") {
  mode = toupper(trimws(as.character(mode %||% "HYBRID")))
  if (!mode %in% c("SYNTHETIC", "HYBRID", "EMPIRICAL") || mode == "HYBRID") return(snapshot)
  keep_rows = function(df) {
    if (!is.data.frame(df) || nrow(df) == 0L || !"data_source" %in% names(df)) {
      return(rep(TRUE, if (is.data.frame(df)) nrow(df) else 0L))
    }
    synthetic = grepl("^SYNTHETIC", toupper(trimws(as.character(df$data_source))))
    if (mode == "SYNTHETIC") synthetic else !synthetic
  }
  excluded = list()
  if (is.data.frame(snapshot$assets)) {
    before = nrow(snapshot$assets)
    snapshot$assets = snapshot$assets[keep_rows(snapshot$assets), , drop = FALSE]
    excluded$assets = before - nrow(snapshot$assets)
  }
  kept_assets = as.character(snapshot$assets$asset_id %||% character())
  if (is.data.frame(snapshot$work_orders)) {
    before = nrow(snapshot$work_orders)
    orders = snapshot$work_orders
    keep = keep_rows(orders) & as.character(orders$asset_id) %in% kept_assets
    snapshot$work_orders = orders[keep, , drop = FALSE]
    excluded$work_orders = before - nrow(snapshot$work_orders)
  }
  kept_orders = as.character(snapshot$work_orders$work_order_id %||% character())
  if (is.data.frame(snapshot$work_order_delays) && nrow(snapshot$work_order_delays) > 0L) {
    before = nrow(snapshot$work_order_delays)
    delays = snapshot$work_order_delays
    snapshot$work_order_delays = delays[as.character(delays$work_order_id) %in% kept_orders, , drop = FALSE]
    excluded$work_order_delays = before - nrow(snapshot$work_order_delays)
  }
  if (is.data.frame(snapshot$exposure_log) && nrow(snapshot$exposure_log) > 0L) {
    log = snapshot$exposure_log
    keep = keep_rows(log) & as.character(log$asset_id) %in% kept_assets
    snapshot$exposure_log = log[keep, , drop = FALSE]
  }
  attr(snapshot, "cmms_excluded") = excluded
  snapshot
}

ahp_random_index = function(n) {
  ri = c(`1` = 0, `2` = 0, `3` = 0.58, `4` = 0.90, `5` = 1.12,
         `6` = 1.24, `7` = 1.32, `8` = 1.41, `9` = 1.45, `10` = 1.49)
  key = as.character(as.integer(n))
  if (!key %in% names(ri)) return(NA_real_)
  unname(ri[[key]])
}

ahp_ratio_from_response = function(selected_side, intensity) {
  side = toupper(trimws(as.character(selected_side)))
  value = suppressWarnings(as.numeric(as.character(intensity)))
  if (length(side) != 1L || length(value) != 1L || !is.finite(value) ||
      value < 1 || value > 9 || value != floor(value)) {
    stop("AHP: intensity deve ser inteiro de 1 a 9.", call. = FALSE)
  }
  if (!side %in% c("LEFT", "RIGHT", "EQUAL")) {
    stop("AHP: selected_side deve ser LEFT, RIGHT ou EQUAL.", call. = FALSE)
  }
  if (side == "EQUAL") return(1)
  if (side == "LEFT") value else 1 / value
}

ahp_expected_pairs = function(items) length(items) * (length(items) - 1L) / 2L

canonical_pair_key = function(left, right) {
  paste(sort(c(as.character(left), as.character(right))), collapse = "||")
}

validate_ahp_pairwise_data = function(df) {
  required = c(
    "respondent_id", "hierarchy_group", "left_item", "right_item",
    "selected_side", "intensity", "data_source"
  )
  issues = list()
  if (!is.data.frame(df)) stop("ahp_pairwise must be a data frame", call. = FALSE)
  missing = setdiff(required, names(df))
  if (length(missing) > 0L) {
    return(list(valid = FALSE, issues = data.frame(
      severity = "ERROR", table = "ahp_pairwise", row = 0L,
      field = paste(missing, collapse = ","), code = "MISSING_COLUMN",
      message = paste("Colunas ausentes:", paste(missing, collapse = ", ")),
      stringsAsFactors = FALSE
    )))
  }
  spec = questionnaire_hierarchy_spec()
  for (i in seq_len(nrow(df))) {
    add = function(field, code, message) {
      issues[[length(issues) + 1L]] <<- data.frame(
        severity = "ERROR", table = "ahp_pairwise", row = i,
        field = field, code = code, message = message,
        stringsAsFactors = FALSE
      )
    }
    respondent = trimws(as.character(df$respondent_id[[i]]))
    if (is.na(respondent)) respondent = ""
    group = trimws(as.character(df$hierarchy_group[[i]]))
    left = trimws(as.character(df$left_item[[i]]))
    right = trimws(as.character(df$right_item[[i]]))
    side = toupper(trimws(as.character(df$selected_side[[i]])))
    intensity = suppressWarnings(as.numeric(as.character(df$intensity[[i]])))
    if (!nzchar(respondent)) add("respondent_id", "MISSING_RESPONDENT", "respondent_id é obrigatório.")
    if (!group %in% names(spec)) {
      add("hierarchy_group", "INVALID_GROUP", "Grupo hierárquico não reconhecido.")
      next
    }
    allowed = names(spec[[group]])
    if (!left %in% allowed) add("left_item", "INVALID_ITEM", "Item esquerdo não pertence ao grupo.")
    if (!right %in% allowed) add("right_item", "INVALID_ITEM", "Item direito não pertence ao grupo.")
    if (identical(left, right)) add("right_item", "SELF_PAIR", "Comparação deve envolver itens distintos.")
    if (!side %in% c("LEFT", "RIGHT", "EQUAL")) add("selected_side", "INVALID_SIDE", "Use LEFT, RIGHT ou EQUAL.")
    if (!is.finite(intensity) || intensity < 1 || intensity > 9 || intensity != floor(intensity)) {
      add("intensity", "INVALID_INTENSITY", "A intensidade deve ser inteiro de 1 a 9.")
    }
    if (isTRUE(side == "EQUAL") && is.finite(intensity) && intensity != 1) {
      add("intensity", "EQUAL_REQUIRES_ONE", "Quando selected_side=EQUAL, intensity deve ser 1.")
    }
  }
  if (length(issues) == 0L) {
    issue_data = data.frame(
      severity = character(), table = character(), row = integer(), field = character(),
      code = character(), message = character(), stringsAsFactors = FALSE
    )
  } else issue_data = do.call(rbind, issues)
  list(valid = nrow(issue_data) == 0L, issues = issue_data)
}

ahp_matrix_from_rows = function(rows, items) {
  n = length(items)
  matrix_value = matrix(1, nrow = n, ncol = n, dimnames = list(items, items))
  if (nrow(rows) == 0L) return(matrix_value)
  for (i in seq_len(nrow(rows))) {
    left = as.character(rows$left_item[[i]])
    right = as.character(rows$right_item[[i]])
    ratio = if ("ratio" %in% names(rows)) {
      as.numeric(rows$ratio[[i]])
    } else {
      ahp_ratio_from_response(rows$selected_side[[i]], rows$intensity[[i]])
    }
    matrix_value[left, right] = ratio
    matrix_value[right, left] = 1 / ratio
  }
  matrix_value
}

ahp_priority = function(matrix_value) {
  if (!is.matrix(matrix_value) || nrow(matrix_value) != ncol(matrix_value) || nrow(matrix_value) < 2L) {
    stop("AHP: matriz quadrada com dimensão >= 2 é obrigatória.", call. = FALSE)
  }
  if (any(!is.finite(matrix_value)) || any(matrix_value <= 0)) {
    stop("AHP: a matriz deve conter apenas valores positivos e finitos.", call. = FALSE)
  }
  geometric = exp(rowMeans(log(matrix_value)))
  weights = geometric / sum(geometric)
  weighted = as.numeric(matrix_value %*% weights)
  lambda_max = mean(weighted / weights)
  n = nrow(matrix_value)
  ci = (lambda_max - n) / (n - 1)
  ri = ahp_random_index(n)
  cr = if (is.finite(ri) && ri > 0) ci / ri else 0
  names(weights) = rownames(matrix_value)
  list(weights = weights, lambda_max = lambda_max, CI = ci, RI = ri, CR = cr)
}

ahp_individual_diagnostics = function(pairwise) {
  spec = questionnaire_hierarchy_spec()
  if (!is.data.frame(pairwise) || nrow(pairwise) == 0L) {
    return(data.frame(
      respondent_id = character(), hierarchy_group = character(), complete = logical(),
      pair_count = integer(), expected_pairs = integer(), CR = numeric(),
      consistent = logical(), stringsAsFactors = FALSE
    ))
  }
  keys = unique(pairwise[, c("respondent_id", "hierarchy_group"), drop = FALSE])
  rows = purrr::map(seq_len(nrow(keys)), function(k) {
    respondent = as.character(keys$respondent_id[[k]])
    group = as.character(keys$hierarchy_group[[k]])
    data = pairwise[pairwise$respondent_id == respondent & pairwise$hierarchy_group == group, , drop = FALSE]
    items = names(spec[[group]])
    expected = ahp_expected_pairs(items)
    pair_keys = purrr::map_chr(seq_len(nrow(data)), function(i) canonical_pair_key(data$left_item[[i]], data$right_item[[i]]))
    complete = nrow(data) == expected && length(unique(pair_keys)) == expected
    cr = NA_real_
    if (complete) {
      result = try(ahp_priority(ahp_matrix_from_rows(data, items)), silent = TRUE)
      if (!inherits(result, "try-error")) cr = result$CR
    }
    data.frame(
      respondent_id = respondent, hierarchy_group = group,
      complete = complete, pair_count = nrow(data), expected_pairs = expected,
      CR = cr, consistent = isTRUE(is.finite(cr) && cr < 0.10),
      stringsAsFactors = FALSE
    )
  })
  bind_rows_base(rows)
}

aggregate_ahp_group = function(pairwise, group, include_only_consistent = TRUE, diagnostics = NULL) {
  spec = questionnaire_hierarchy_spec()
  items = names(spec[[group]])
  # v1.4.2 — o diagnóstico individual pode vir pronto do chamador (antes era
  # recalculado em cada um dos cinco grupos e mais uma vez no resultado).
  if (is.null(diagnostics)) diagnostics = ahp_individual_diagnostics(pairwise)
  eligible = diagnostics[diagnostics$hierarchy_group == group & diagnostics$complete, , drop = FALSE]
  if (isTRUE(include_only_consistent)) eligible = eligible[eligible$consistent, , drop = FALSE]
  ids = unique(as.character(eligible$respondent_id))
  data = pairwise[pairwise$hierarchy_group == group & pairwise$respondent_id %in% ids, , drop = FALSE]
  if (length(ids) == 0L || nrow(data) == 0L) {
    return(list(estimable = FALSE, reason = "Nenhum respondente elegível para este grupo.", respondents = ids))
  }
  data$pair_key = purrr::map_chr(seq_len(nrow(data)), function(i) canonical_pair_key(data$left_item[[i]], data$right_item[[i]]))
  data$raw_ratio = purrr::map_dbl(seq_len(nrow(data)), function(i) ahp_ratio_from_response(data$selected_side[[i]], data$intensity[[i]]))
  canonical = bind_rows_base(purrr::map(split(data, data$pair_key), function(block) {
    parts = strsplit(block$pair_key[[1]], "\\|\\|")[[1]]
    a = parts[[1]]; b = parts[[2]]
    ratios = purrr::map_dbl(seq_len(nrow(block)), function(i) {
      if (as.character(block$left_item[[i]]) == a) block$raw_ratio[[i]] else 1 / block$raw_ratio[[i]]
    })
    data.frame(left_item = a, right_item = b, ratio = exp(mean(log(ratios))), stringsAsFactors = FALSE)
  }))
  matrix_value = ahp_matrix_from_rows(canonical, items)
  priority = ahp_priority(matrix_value)
  list(
    estimable = TRUE, respondents = ids, matrix = matrix_value,
    weights = priority$weights, lambda_max = priority$lambda_max,
    CI = priority$CI, RI = priority$RI, CR = priority$CR
  )
}

compute_ahp_hierarchy = function(pairwise, include_only_consistent = TRUE) {
  validation = validate_ahp_pairwise_data(pairwise)
  if (!validation$valid) {
    return(list(estimable = FALSE, error = paste(validation$issues$message, collapse = "; ")))
  }
  spec = questionnaire_hierarchy_spec()
  individual_diagnostics = ahp_individual_diagnostics(pairwise)
  group_results = purrr::map(purrr::set_names(names(spec)), function(group) {
    aggregate_ahp_group(pairwise, group, include_only_consistent, diagnostics = individual_diagnostics)
  })
  if (!isTRUE(group_results$criteria$estimable)) {
    return(list(estimable = FALSE, error = "Pesos dos critérios principais não são estimáveis.", groups = group_results))
  }
  top_weights = group_results$criteria$weights
  parent_map = questionnaire_parent_map()
  final_rows = purrr::compact(purrr::map(names(parent_map), function(group) {
    result = group_results[[group]]
    if (!isTRUE(result$estimable)) return(NULL)
    parent = parent_map[[group]]
    parent_weight = unname(top_weights[[parent]])
    items = names(result$weights)
    local_weights = purrr::map_dbl(items, function(item) unname(result$weights[[item]]))
    data.frame(
      hierarchy_group = group, parent_criterion = parent,
      subcriterion_id = items, local_weight = local_weights,
      criterion_weight = parent_weight,
      global_weight = parent_weight * local_weights,
      stringsAsFactors = FALSE
    )
  }))
  final = bind_rows_base(final_rows, data.frame())
  consistency = bind_rows_base(purrr::map(names(group_results), function(group) {
    x = group_results[[group]]
    data.frame(
      hierarchy_group = group, estimable = isTRUE(x$estimable),
      respondents = length(x$respondents %||% character()),
      CR = if (isTRUE(x$estimable)) x$CR else NA_real_,
      status = if (!isTRUE(x$estimable)) "NÃO ESTIMÁVEL" else if (x$CR < 0.10) "RC < 0,10" else "RC >= 0,10",
      stringsAsFactors = FALSE
    )
  }))
  complete_hierarchy = nrow(final) == 10L && abs(sum(final$global_weight) - 1) < 1e-6
  all_group_consistent = all(purrr::map_lgl(group_results, function(x) {
    isTRUE(x$estimable) && is.finite(x$CR) && x$CR < 0.10
  }))
  hierarchy_ok = complete_hierarchy && all_group_consistent
  list(
    estimable = hierarchy_ok,
    error = if (hierarchy_ok) NULL else if (!complete_hierarchy) {
      "Hierarquia AHP incompleta: faltam pesos válidos em um ou mais grupos."
    } else {
      "Uma ou mais matrizes AHP agregadas possuem RC >= 0,10; TOPSIS bloqueado até revisão."
    },
    groups = group_results, criteria_weights = top_weights,
    subcriteria_weights = final, consistency = consistency,
    individual_diagnostics = individual_diagnostics
  )
}

validate_topsis_scores_data = function(df) {
  required = c("respondent_id", "alternative_id", "alternative_name", "subcriterion_id", "score", "data_source")
  if (!is.data.frame(df)) stop("topsis_scores must be a data frame", call. = FALSE)
  missing = setdiff(required, names(df))
  if (length(missing) > 0L) return(list(valid = FALSE, message = paste("Colunas ausentes:", paste(missing, collapse = ", "))))
  all_sub = unlist(purrr::map(questionnaire_hierarchy_spec()[-1], names), use.names = FALSE)
  score = suppressWarnings(as.numeric(as.character(df$score)))
  invalid_score = !is.na(score) & (!is.finite(score) | score < 1 | score > 10)
  invalid_sub = !as.character(df$subcriterion_id) %in% all_sub
  invalid_id = is.na(df$respondent_id) | !nzchar(trimws(as.character(df$respondent_id))) |
    is.na(df$alternative_id) | !nzchar(trimws(as.character(df$alternative_id)))
  valid = !any(invalid_score | invalid_sub | invalid_id)
  message = if (valid) "" else "Há score fora de 1–10, subcritério inválido ou identificador ausente."
  if (!valid) return(list(valid = FALSE, message = message))
  # Uma opinião por avaliador, sistema e subcritério. Reimportações não
  # podem aumentar o peso do avaliador nem criar cobertura acima de 100%.
  cell_keys = df[, c("respondent_id", "alternative_id", "subcriterion_id"), drop = FALSE]
  cell_keys[] = lapply(cell_keys, function(x) trimws(as.character(x)))
  duplicated_cell = duplicated(cell_keys) | duplicated(cell_keys, fromLast = TRUE)
  if (any(duplicated_cell)) return(list(
    valid = FALSE,
    message = "Há avaliações P6 duplicadas para o mesmo respondente, sistema e subcritério. Revise as duplicatas ou substitua a tabela; nenhuma nota foi agregada silenciosamente.",
    duplicate_rows = which(duplicated_cell)
  ))
  list(valid = TRUE, message = "")
}

# Valida o estado resultante antes de uma importação CSV de P6.
# Em append, dados existentes e recebidos devem ter chaves distintas.
validate_topsis_import_data = function(existing, incoming, operation = "append") {
  operation = match.arg(operation, c("append", "replace"))
  validation = validate_topsis_scores_data(incoming)
  if (!isTRUE(validation$valid) || operation == "replace" || nrow(existing) == 0L) return(validation)
  columns = c("respondent_id", "alternative_id", "alternative_name", "subcriterion_id", "score", "data_source")
  combined = rbind(existing[, columns, drop = FALSE], incoming[, columns, drop = FALSE])
  validate_topsis_scores_data(combined)
}

aggregate_topsis_scores = function(scores, method = c("mean", "median"), min_coverage = 0) {
  method = match.arg(method)
  validation = validate_topsis_scores_data(scores)
  if (!validation$valid) stop(validation$message, call. = FALSE)
  if (nrow(scores) == 0L) return(data.frame())
  respondent_count = length(unique(as.character(scores$respondent_id)))
  scores$score_numeric = suppressWarnings(as.numeric(as.character(scores$score)))
  scores = scores[is.finite(scores$score_numeric), , drop = FALSE]
  if (nrow(scores) == 0L) return(data.frame())
  key = interaction(scores$alternative_id, scores$subcriterion_id, drop = TRUE, lex.order = TRUE)
  out = bind_rows_base(purrr::map(split(scores, key), function(block) {
    values = block$score_numeric
    data.frame(
      alternative_id = as.character(block$alternative_id[[1]]),
      alternative_name = as.character(block$alternative_name[[1]]),
      subcriterion_id = as.character(block$subcriterion_id[[1]]),
      value = if (method == "mean") mean(values) else stats::median(values),
      n_valid = length(values), mean = mean(values), median = stats::median(values),
      sd = if (length(values) > 1L) stats::sd(values) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
  row.names(out) = NULL
  # v1.4.1 — cobertura da célula (respostas válidas / respondentes do modo).
  out$coverage = if (respondent_count > 0L) out$n_valid / respondent_count else NA_real_
  if (is.numeric(min_coverage) && length(min_coverage) == 1L && is.finite(min_coverage) && min_coverage > 0) {
    below = out$coverage < min_coverage
    attr_below = out[below, c("alternative_id", "subcriterion_id", "n_valid", "coverage"), drop = FALSE]
    out = out[!below, , drop = FALSE]
    row.names(out) = NULL
    attr(out, "below_coverage") = attr_below
  }
  out
}

topsis_rank = function(aggregated_scores, weights) {
  if (!is.data.frame(aggregated_scores) || nrow(aggregated_scores) == 0L) {
    return(list(estimable = FALSE, reason = "Não há avaliações P6 válidas."))
  }
  if (!is.data.frame(weights) || nrow(weights) != 10L) {
    return(list(estimable = FALSE, reason = "Os 10 pesos globais AHP ainda não estão disponíveis."))
  }
  criteria = as.character(weights$subcriterion_id)
  alternatives = unique(as.character(aggregated_scores$alternative_id))
  complete_alternatives = purrr::map_lgl(purrr::set_names(alternatives), function(a) {
    block = aggregated_scores[aggregated_scores$alternative_id == a, , drop = FALSE]
    all(criteria %in% block$subcriterion_id)
  })
  # v1.4.1 — alternativas sem nota em algum subcritério (N/A em todas as
  # respostas ou abaixo da cobertura mínima) são informadas, não omitidas.
  excluded = bind_rows_base(purrr::map(alternatives[!complete_alternatives], function(a) {
    block = aggregated_scores[aggregated_scores$alternative_id == a, , drop = FALSE]
    data.frame(
      alternative_id = a,
      alternative_name = as.character(block$alternative_name[[1]]),
      subcriterios_sem_nota = paste(setdiff(criteria, block$subcriterion_id), collapse = ", "),
      stringsAsFactors = FALSE
    )
  }))
  alternatives = alternatives[complete_alternatives]
  if (length(alternatives) < 2L) {
    return(list(estimable = FALSE, excluded = excluded,
                reason = "São necessárias ao menos duas alternativas com os 10 subcritérios avaliados."))
  }
  matrix_value = matrix(NA_real_, nrow = length(alternatives), ncol = length(criteria), dimnames = list(alternatives, criteria))
  names_by_id = setNames(aggregated_scores$alternative_name, aggregated_scores$alternative_id)
  # v1.4.2 — preenchimento vetorizado: uma célula só recebe valor quando há
  # exatamente uma linha para o par alternativa × subcritério (como antes).
  cell_key = paste(aggregated_scores$alternative_id, aggregated_scores$subcriterion_id, sep = "\r")
  single_cell = !(duplicated(cell_key) | duplicated(cell_key, fromLast = TRUE))
  cell_value = stats::setNames(as.numeric(aggregated_scores$value[single_cell]), cell_key[single_cell])
  matrix_value[] = unname(cell_value[paste(
    rep(alternatives, times = length(criteria)), rep(criteria, each = length(alternatives)), sep = "\r"
  )])
  if (any(!is.finite(matrix_value))) {
    return(list(estimable = FALSE, reason = "Há células sem avaliação; N/A não é imputado."))
  }
  denom = sqrt(colSums(matrix_value^2))
  if (any(denom <= 0 | !is.finite(denom))) return(list(estimable = FALSE, reason = "Normalização TOPSIS inválida."))
  normalized = sweep(matrix_value, 2, denom, "/")
  w = weights$global_weight[match(criteria, weights$subcriterion_id)]
  if (any(!is.finite(w) | w < 0) || sum(w) <= 0) return(list(estimable = FALSE, reason = "Pesos TOPSIS inválidos."))
  w = w / sum(w)
  weighted = sweep(normalized, 2, w, "*")
  ideal = apply(weighted, 2, max)
  anti = apply(weighted, 2, min)
  d_plus = sqrt(rowSums((sweep(weighted, 2, ideal, "-"))^2))
  d_minus = sqrt(rowSums((sweep(weighted, 2, anti, "-"))^2))
  separation = d_plus + d_minus
  if (any(!is.finite(separation)) || all(separation <= .Machine$double.eps)) {
    return(list(estimable = FALSE, reason = "As alternativas têm avaliações idênticas: TOPSIS não distingue prioridades."))
  }
  closeness = ifelse(separation > .Machine$double.eps, d_minus / separation, 0.5)
  result = data.frame(
    alternative_id = alternatives,
    alternative_name = unname(names_by_id[alternatives]),
    d_plus = d_plus, d_minus = d_minus, coefficient = closeness,
    stringsAsFactors = FALSE
  )
  result = result[order(-result$coefficient, result$alternative_id), , drop = FALSE]
  result$rank = seq_len(nrow(result))
  row.names(result) = NULL
  list(estimable = TRUE, ranking = result, normalized = normalized, weighted = weighted,
       ideal = ideal, anti_ideal = anti, excluded = excluded)
}

compute_multicriteria = function(
  snapshot, score_aggregation = "mean", include_only_consistent = TRUE,
  evidence_mode = "HYBRID", min_cell_coverage = 0
) {
  filtered = filter_questionnaire_snapshot_by_mode(snapshot, evidence_mode)
  pairwise = filtered$ahp_pairwise %||% data.frame()
  scores = filtered$topsis_scores %||% data.frame()
  ahp = compute_ahp_hierarchy(pairwise, include_only_consistent)
  aggregated = tryCatch(
    aggregate_topsis_scores(scores, score_aggregation, min_cell_coverage),
    error = function(e) structure(data.frame(), aggregation_error = conditionMessage(e))
  )
  aggregation_error = attr(aggregated, "aggregation_error")
  topsis = if (!is.null(aggregation_error)) {
    list(estimable = FALSE, reason = aggregation_error)
  } else if (isTRUE(ahp$estimable)) {
    topsis_rank(aggregated, ahp$subcriteria_weights)
  } else {
    list(estimable = FALSE, reason = ahp$error %||% "AHP não estimável")
  }
  list(
    ahp = ahp, aggregated_scores = aggregated, topsis = topsis,
    raw_pairwise = pairwise, raw_scores = scores,
    score_aggregation = score_aggregation,
    include_only_consistent = include_only_consistent,
    min_cell_coverage = min_cell_coverage,
    evidence_mode = toupper(evidence_mode)
  )
}


questionnaire_response_summaries = function(snapshot, mode = "HYBRID") {
  filtered = filter_questionnaire_snapshot_by_mode(snapshot, mode)
  respondents = filtered$questionnaire_respondents %||% data.frame()
  pairwise = filtered$ahp_pairwise %||% data.frame()
  scores = filtered$topsis_scores %||% data.frame()
  factors = filtered$questionnaire_factors %||% data.frame()

  n_respondents = if (is.data.frame(respondents)) nrow(respondents) else 0L
  n_ahp = if (is.data.frame(pairwise)) nrow(pairwise) else 0L
  valid_scores = if (is.data.frame(scores) && "score" %in% names(scores)) {
    suppressWarnings(as.numeric(as.character(scores$score)))
  } else numeric()
  n_p6_valid = sum(is.finite(valid_scores))
  n_p6_missing = if (n_respondents > 0L) max(0L, n_respondents * 100L - n_p6_valid) else 0L
  expected_ahp = n_respondents * nrow(questionnaire_pair_spec())

  overview = data.frame(
    indicador = c(
      "Respondentes no modo selecionado", "Comparações AHP P1–P5 carregadas",
      "Cobertura AHP", "Avaliações P6 válidas", "P6 N/A ou vazias",
      "Cobertura P6", "Registros P7", "Sistemas distintos citados em P7"
    ),
    valor = c(
      n_respondents, n_ahp,
      if (expected_ahp > 0) round(100 * n_ahp / expected_ahp, 1) else NA_real_,
      n_p6_valid, n_p6_missing,
      if (n_respondents > 0) round(100 * n_p6_valid / (n_respondents * 100), 1) else NA_real_,
      if (is.data.frame(factors)) nrow(factors) else 0L,
      if (is.data.frame(factors) && nrow(factors) > 0L && "related_system" %in% names(factors)) {
        length(unique(trimws(as.character(factors$related_system[nzchar(trimws(as.character(factors$related_system)))]))))
      } else 0L
    ),
    unidade = c("n", "n", "%", "n", "n", "%", "n", "n"),
    stringsAsFactors = FALSE
  )

  finite_summary = function(x, label) {
    x = suppressWarnings(as.numeric(as.character(x)))
    x = x[is.finite(x)]
    data.frame(
      indicador = c(paste0(label, " — n válido"), paste0(label, " — média"),
                    paste0(label, " — mediana"), paste0(label, " — mínimo"), paste0(label, " — máximo")),
      valor = c(length(x), if (length(x)) mean(x) else NA_real_,
                if (length(x)) stats::median(x) else NA_real_,
                if (length(x)) min(x) else NA_real_, if (length(x)) max(x) else NA_real_),
      stringsAsFactors = FALSE
    )
  }
  profile = data.frame(indicador=character(), valor=numeric(), stringsAsFactors=FALSE)
  if (is.data.frame(respondents) && nrow(respondents) > 0L) {
    profile = rbind(
      finite_summary(respondents$age, "Idade"),
      finite_summary(respondents$service_years, "Tempo de serviço (anos)"),
      data.frame(
        indicador = c("Respondentes com experiência JLTV declarada"),
        valor = sum(suppressWarnings(as.integer(respondents$direct_jltv_experience)) == 1L, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    )
  }

  spec = questionnaire_pair_spec()
  ahp_rows = purrr::map(seq_len(nrow(spec)), function(i) {
    block = pairwise[pairwise$hierarchy_group == spec$hierarchy_group[[i]] &
                       pairwise$left_item == spec$left_item[[i]] &
                       pairwise$right_item == spec$right_item[[i]], , drop = FALSE]
    if (nrow(block) > 0L) {
      ratios = purrr::map_dbl(seq_len(nrow(block)), function(j) {
        ahp_ratio_from_response(block$selected_side[[j]], block$intensity[[j]])
      })
      gm = exp(mean(log(ratios)))
      tendency = if (abs(log(gm)) < 1e-10) "Equivalência" else if (gm > 1) spec$left_label[[i]] else spec$right_label[[i]]
    } else {
      gm = NA_real_; tendency = "Sem respostas"
    }
    data.frame(
      pergunta = spec$code[[i]], grupo = spec$hierarchy_group[[i]],
      comparacao = paste(spec$left_label[[i]], "×", spec$right_label[[i]]),
      n_respostas = nrow(block),
      n_esquerda = sum(toupper(as.character(block$selected_side)) == "LEFT", na.rm = TRUE),
      n_equivalente = sum(toupper(as.character(block$selected_side)) == "EQUAL", na.rm = TRUE),
      n_direita = sum(toupper(as.character(block$selected_side)) == "RIGHT", na.rm = TRUE),
      razao_geometrica_esq_dir = if (is.finite(gm)) round(gm, 4) else NA_real_,
      tendencia_agregada = tendency, stringsAsFactors = FALSE
    )
  })
  ahp = bind_rows_base(ahp_rows)

  alternatives = questionnaire_alternative_spec()
  sub_map = questionnaire_p6_subcriteria()
  # v1.4.2 — uma única passagem por P6: as notas são agrupadas por célula
  # (alternativa × subcritério) antes do laço, em vez de filtrar a tabela
  # inteira 100 vezes.
  score_values = suppressWarnings(as.numeric(as.character(scores$score)))
  score_cells = split(score_values, paste(scores$alternative_id, scores$subcriterion_id, sep = "\r"))
  p6_alternatives = rep(names(alternatives), each = length(sub_map))
  p6_subs = rep(unname(sub_map), times = length(alternatives))
  p6_rows = purrr::map2(p6_alternatives, p6_subs, function(alt, sub) {
      vals = score_cells[[paste(alt, sub, sep = "\r")]] %||% numeric()
      vals = vals[is.finite(vals)]
      total_expected = n_respondents
      data.frame(
        alternativa = alt, sistema = unname(alternatives[[alt]]), subcriterio = sub,
        n_validas = length(vals), n_na_ou_vazio = max(0L, total_expected - length(vals)),
        cobertura_pct = if (total_expected > 0) round(100 * length(vals) / total_expected, 1) else NA_real_,
        media = if (length(vals)) round(mean(vals), 3) else NA_real_,
        mediana = if (length(vals)) round(stats::median(vals), 3) else NA_real_,
        desvio_padrao = if (length(vals) > 1L) round(stats::sd(vals), 3) else NA_real_,
        minimo = if (length(vals)) min(vals) else NA_real_, maximo = if (length(vals)) max(vals) else NA_real_,
        stringsAsFactors = FALSE
      )
  })
  p6 = bind_rows_base(p6_rows)

  if (!is.data.frame(factors) || nrow(factors) == 0L) {
    p7 = data.frame(
      sistema = character(), mencoes = integer(), respondentes_distintos = integer(),
      identificacoes_tecnicas = integer(), ocorrencias_necessidades = integer(),
      fontes_para_confirmacao = integer(), stringsAsFactors = FALSE
    )
  } else {
    system = trimws(as.character(factors$related_system))
    system[is.na(system)] = ""
    # v1.4.1 — entradas manuais antigas gravavam só o código (A1); unifica com o rótulo do formulário.
    code_only = system %in% names(alternatives)
    system[code_only] = paste(system[code_only], unname(alternatives[system[code_only]]))
    system[!nzchar(system)] = "Sistema não informado"
    blocks = split(seq_len(nrow(factors)), system)
    p7 = bind_rows_base(purrr::map(names(blocks), function(name) {
      ii = blocks[[name]]; b = factors[ii, , drop = FALSE]
      filled = function(x) sum(nzchar(trimws(as.character(x))), na.rm = TRUE)
      data.frame(
        sistema = name, mencoes = nrow(b),
        respondentes_distintos = length(unique(as.character(b$respondent_id))),
        identificacoes_tecnicas = filled(b$identification),
        ocorrencias_necessidades = filled(b$occurrence_detail),
        fontes_para_confirmacao = filled(b$evidence_reference),
        stringsAsFactors = FALSE
      )
    }))
    p7 = p7[order(-p7$mencoes, p7$sistema), , drop = FALSE]
    row.names(p7) = NULL
  }
  list(overview=overview, profile=profile, ahp=ahp, p6=p6, p7=p7)
}

wilson_interval = function(successes, trials, conf_level = 0.95) {
  scalar_count = function(value) {
    is.numeric(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(value) &&
      value == floor(value)
  }

  if (!scalar_count(trials) || trials <= 0) {
    stop("trials must be a positive whole number", call. = FALSE)
  }
  if (!scalar_count(successes) || successes < 0 || successes > trials) {
    stop("successes must be a whole number between zero and trials", call. = FALSE)
  }
  if (
    !is.numeric(conf_level) ||
      length(conf_level) != 1L ||
      !is.finite(conf_level) ||
      conf_level <= 0 ||
      conf_level >= 1
  ) {
    stop("conf_level must be strictly between zero and one", call. = FALSE)
  }

  estimate = successes / trials
  z_score = qnorm(1 - (1 - conf_level) / 2)
  denominator = 1 + z_score^2 / trials
  centre = (estimate + z_score^2 / (2 * trials)) / denominator
  half_width = (
    z_score *
      sqrt(estimate * (1 - estimate) / trials + z_score^2 / (4 * trials^2)) /
      denominator
  )

  c(
    estimate = estimate,
    lower = max(0, centre - half_width),
    upper = min(1, centre + half_width)
  )
}

asset_active_mask = function(assets) {
  if (!is.data.frame(assets) || nrow(assets) == 0L) return(logical())
  if (!"active" %in% names(assets)) return(rep(TRUE, nrow(assets)))
  flags = suppressWarnings(as.integer(as.character(assets$active)))
  logical_flags = toupper(trimws(as.character(assets$active))) %in% c("TRUE", "T")
  (!is.na(flags) & flags == 1L) | logical_flags
}

is_preventive_type = function(values) {
  toupper(trimws(as.character(values))) %in% c("PREVENTIVE", "PREVENTIVA")
}

is_corrective_type = function(values) {
  toupper(trimws(as.character(values))) %in% c("CORRECTIVE", "CORRETIVA")
}

functional_failure_flags = function(values) {
  flags = suppressWarnings(as.integer(as.character(values)))
  logical_flags = toupper(trimws(as.character(values))) %in% c("TRUE", "T")
  (!is.na(flags) & flags == 1L) | logical_flags
}

common_active_meter_unit = function(assets) {
  if (!is.data.frame(assets) || !"meter_unit" %in% names(assets)) {
    stop("assets precisa conter meter_unit.", call. = FALSE)
  }
  active_mask = asset_active_mask(assets)
  units = toupper(trimws(as.character(assets$meter_unit[active_mask])))
  units = unique(units[nzchar(units) & !is.na(units)])
  if (length(units) == 0L) return(NA_character_)
  if (length(units) > 1L) {
    stop(
      "Há viaturas ativas com medidores em unidades diferentes (HOURS e KM). Separe a análise por unidade ou forneça uma conversão de utilização explicitamente validada.",
      call. = FALSE
    )
  }
  if (!units[[1]] %in% c("HOURS", "KM")) {
    stop("meter_unit deve ser HOURS ou KM.", call. = FALSE)
  }
  units[[1]]
}

# Dados de vida por viatura ativa (processo de renovação).
# v1.4.1:
#  - falhas funcionais no mesmo medidor da mesma viatura são consolidadas em um
#    único evento de indisponibilidade (antes interrompiam a análise);
#  - censor_at_preventive = TRUE encerra o intervalo em cada preventiva
#    registrada (observação censurada) e inicia um novo intervalo, coerente com
#    o simulador, que trata a preventiva como renovação. Sem isso, o benefício
#    da preventiva entraria duas vezes (nos dados e na simulação).
build_life_data = function(assets, work_orders, censor_at_preventive = TRUE) {
  if (!is.data.frame(assets) || !is.data.frame(work_orders)) {
    stop("assets and work_orders must be data frames", call. = FALSE)
  }

  required_asset_columns = c("asset_id", "initial_meter", "current_meter", "meter_unit")
  missing_asset_columns = setdiff(required_asset_columns, names(assets))
  if (length(missing_asset_columns) > 0L) {
    stop(
      sprintf(
        "assets is missing required columns: %s",
        paste(missing_asset_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  empty_result = data.frame(
    asset_id = character(),
    interval = integer(),
    start_meter = numeric(),
    end_meter = numeric(),
    time = numeric(),
    duration = numeric(),
    status = integer(),
    end_event = character(),
    stringsAsFactors = FALSE
  )
  attr(empty_result, "merged_failures") = 0L
  if (nrow(assets) == 0L) {
    return(empty_result)
  }

  asset_ids = as.character(assets$asset_id)
  if (any(is.na(asset_ids) | !nzchar(trimws(asset_ids)))) {
    stop("asset_id values must be present", call. = FALSE)
  }
  if (anyDuplicated(asset_ids)) {
    stop("asset_id values must be unique", call. = FALSE)
  }

  initial_meter = suppressWarnings(as.numeric(as.character(assets$initial_meter)))
  current_meter = suppressWarnings(as.numeric(as.character(assets$current_meter)))
  if (any(!is.finite(initial_meter)) || any(!is.finite(current_meter))) {
    stop("asset meter values must be finite numbers", call. = FALSE)
  }
  if (any(current_meter < initial_meter)) {
    stop("current_meter cannot be below initial_meter", call. = FALSE)
  }

  active_mask = asset_active_mask(assets)
  meter_unit = common_active_meter_unit(assets)

  required_order_columns = c(
    "asset_id",
    "maintenance_type",
    "is_functional_failure",
    "meter_at_event"
  )
  if (nrow(work_orders) > 0L) {
    missing_order_columns = setdiff(required_order_columns, names(work_orders))
    if (length(missing_order_columns) > 0L) {
      stop(
        sprintf(
          "work_orders is missing required columns: %s",
          paste(missing_order_columns, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    order_asset_ids = as.character(work_orders$asset_id)
    if (any(is.na(order_asset_ids) | !order_asset_ids %in% asset_ids)) {
      stop("every work order must reference a known asset", call. = FALSE)
    }
    failure_mask = functional_failure_flags(work_orders$is_functional_failure) &
      is_corrective_type(work_orders$maintenance_type)
    preventive_mask = is_preventive_type(work_orders$maintenance_type)
    event_meters = suppressWarnings(as.numeric(as.character(work_orders$meter_at_event)))
    if (any(failure_mask & !is.finite(event_meters))) {
      stop("functional failures must have a finite meter_at_event", call. = FALSE)
    }
  } else {
    order_asset_ids = character()
    failure_mask = logical()
    preventive_mask = logical()
    event_meters = numeric()
  }

  rows = list()
  row_index = 1L
  merged_failures = 0L

  for (asset_index in which(active_mask)) {
    this_asset_id = asset_ids[[asset_index]]
    this_initial = initial_meter[[asset_index]]
    this_current = current_meter[[asset_index]]
    this_failure_meters = sort(event_meters[
      failure_mask & order_asset_ids == this_asset_id
    ])

    if (length(this_failure_meters) > 0L) {
      if (any(this_failure_meters <= this_initial | this_failure_meters > this_current)) {
        stop(
          sprintf("failure meters for asset %s must be within its exposure", this_asset_id),
          call. = FALSE
        )
      }
      unique_failures = unique(this_failure_meters)
      merged_failures = merged_failures + length(this_failure_meters) - length(unique_failures)
      this_failure_meters = unique_failures
    }

    this_pm_meters = numeric()
    if (isTRUE(censor_at_preventive) && length(preventive_mask) > 0L) {
      this_pm_meters = event_meters[preventive_mask & order_asset_ids == this_asset_id]
      this_pm_meters = this_pm_meters[is.finite(this_pm_meters) &
                                        this_pm_meters > this_initial &
                                        this_pm_meters <= this_current]
      this_pm_meters = setdiff(unique(this_pm_meters), this_failure_meters)
    }

    boundaries = sort(c(this_failure_meters, this_pm_meters))
    boundary_status = as.integer(boundaries %in% this_failure_meters)
    boundary_event = ifelse(boundary_status == 1L, "FAILURE", "PREVENTIVE")

    interval_starts = c(this_initial, boundaries)
    interval_ends = c(boundaries, this_current)
    durations = interval_ends - interval_starts
    statuses = c(boundary_status, 0L)
    end_events = c(boundary_event, "CENSORED")

    rows[[row_index]] = data.frame(
      asset_id = rep(this_asset_id, length(durations)),
      interval = seq_along(durations),
      start_meter = interval_starts,
      end_meter = interval_ends,
      time = durations,
      duration = durations,
      status = statuses,
      end_event = end_events,
      stringsAsFactors = FALSE
    )
    row_index = row_index + 1L
  }

  if (length(rows) == 0L) {
    return(empty_result)
  }

  result = do.call(rbind, rows)
  row.names(result) = NULL
  attr(result, "meter_unit") = meter_unit
  attr(result, "merged_failures") = as.integer(merged_failures)
  result
}

# v1.4.1 — teste de tendência de Laplace para múltiplos sistemas (tempo truncado).
# U > 1,96: falhas se concentram no fim da exposição (deterioração);
# U < -1,96: melhoria. Em ambos os casos a hipótese de renovação i.i.d. usada no
# ajuste de vida e no simulador fica questionável.
laplace_trend_test = function(assets, work_orders) {
  empty = list(estimable = FALSE, U = NA_real_, p_value = NA_real_, failures = 0L,
               interpretation = "Não estimável: são necessárias ao menos 3 falhas funcionais.")
  if (!is.data.frame(assets) || !is.data.frame(work_orders) || nrow(assets) == 0L ||
      nrow(work_orders) == 0L) return(empty)
  needed = c("asset_id", "maintenance_type", "is_functional_failure", "meter_at_event")
  if (!all(needed %in% names(work_orders))) return(empty)
  active = asset_active_mask(assets)
  ids = as.character(assets$asset_id)
  initial = suppressWarnings(as.numeric(as.character(assets$initial_meter)))
  current = suppressWarnings(as.numeric(as.character(assets$current_meter)))
  failure_mask = functional_failure_flags(work_orders$is_functional_failure) &
    is_corrective_type(work_orders$maintenance_type)
  meters = suppressWarnings(as.numeric(as.character(work_orders$meter_at_event)))
  numerator = 0; variance = 0; total = 0L
  for (i in which(active)) {
    T_i = current[[i]] - initial[[i]]
    if (!is.finite(T_i) || T_i <= 0) next
    t = unique(meters[failure_mask & as.character(work_orders$asset_id) == ids[[i]]]) - initial[[i]]
    t = t[is.finite(t) & t > 0 & t <= T_i]
    n_i = length(t)
    if (n_i == 0L) next
    numerator = numerator + sum(t) - n_i * T_i / 2
    variance = variance + n_i * T_i^2 / 12
    total = total + n_i
  }
  if (total < 3L || variance <= 0) {
    empty$failures = total
    return(empty)
  }
  U = numerator / sqrt(variance)
  p_value = 2 * stats::pnorm(-abs(U))
  interpretation = if (p_value >= 0.05) {
    "Sem tendência significativa (5%): a hipótese de renovação é compatível com os registros."
  } else if (U > 0) {
    "Tendência de deterioração (5%): intervalos entre falhas diminuem com o uso; o ajuste i.i.d. tende a ser otimista."
  } else {
    "Tendência de melhoria (5%): intervalos entre falhas aumentam com o uso; o ajuste i.i.d. tende a ser pessimista."
  }
  list(estimable = TRUE, U = U, p_value = p_value, failures = total,
       interpretation = interpretation)
}

fit_life_models = function(time, status) {
  empty_models = data.frame(
    model = character(),
    loglik = numeric(),
    parameters = character(),
    aic = numeric(),
    parameter_count = integer(),
    converged = logical(),
    stringsAsFactors = FALSE
  )
  non_estimable = function(reason, failure_count = NA_integer_) {
    list(
      status = "NON_ESTIMABLE",
      estimable = FALSE,
      reason = reason,
      failure_count = failure_count,
      observation_count = length(time),
      models = empty_models,
      selected_model = NA_character_,
      parameters = numeric(),
      all_parameters = list()
    )
  }

  if (!is.numeric(time) || !is.numeric(status) || length(time) != length(status)) {
    stop("time and status must be numeric vectors of equal length", call. = FALSE)
  }
  if (length(time) == 0L) {
    return(non_estimable("No life-data observations were provided", 0L))
  }
  if (any(!is.finite(time)) || any(time <= 0)) {
    stop("time values must be finite and strictly positive", call. = FALSE)
  }
  if (any(is.na(status)) || any(!status %in% c(0, 1))) {
    stop("status values must be zero for censoring or one for failure", call. = FALSE)
  }

  status = as.integer(status)
  failure_count = sum(status)
  if (failure_count < 2L) {
    return(non_estimable("At least two observed failures are required", failure_count))
  }

  exponential_rate = failure_count / sum(time)
  exponential_log_likelihood = failure_count * log(exponential_rate) -
    exponential_rate * sum(time)

  weibull_objective = function(theta) {
    shape = exp(theta[[1]])
    scale = exp(theta[[2]])
    contributions = numeric(length(time))
    failed = status == 1L
    contributions[failed] = dweibull(
      time[failed],
      shape = shape,
      scale = scale,
      log = TRUE
    )
    contributions[!failed] = pweibull(
      time[!failed],
      shape = shape,
      scale = scale,
      lower.tail = FALSE,
      log.p = TRUE
    )
    if (any(!is.finite(contributions))) {
      return(.Machine$double.xmax / 100)
    }
    -sum(contributions)
  }

  lognormal_objective = function(theta) {
    meanlog = theta[[1]]
    sdlog = exp(theta[[2]])
    contributions = numeric(length(time))
    failed = status == 1L
    contributions[failed] = dlnorm(
      time[failed],
      meanlog = meanlog,
      sdlog = sdlog,
      log = TRUE
    )
    contributions[!failed] = plnorm(
      time[!failed],
      meanlog = meanlog,
      sdlog = sdlog,
      lower.tail = FALSE,
      log.p = TRUE
    )
    if (any(!is.finite(contributions))) {
      return(.Machine$double.xmax / 100)
    }
    -sum(contributions)
  }

  scale_start = max(sum(time) / failure_count, .Machine$double.eps)
  weibull_fit = try(
    optim(
      c(log(1), log(scale_start)),
      weibull_objective,
      method = "Nelder-Mead",
      control = list(maxit = 4000, reltol = 1e-10)
    ),
    silent = TRUE
  )

  failure_log_times = log(time[status == 1L])
  meanlog_start = mean(failure_log_times)
  sdlog_start = sd(failure_log_times)
  if (!is.finite(sdlog_start) || sdlog_start <= 0) {
    sdlog_start = 0.5
  }
  lognormal_fit = try(
    optim(
      c(meanlog_start, log(sdlog_start)),
      lognormal_objective,
      method = "Nelder-Mead",
      control = list(maxit = 4000, reltol = 1e-10)
    ),
    silent = TRUE
  )

  weibull_ok = !inherits(weibull_fit, "try-error") &&
    is.finite(weibull_fit$value)
  lognormal_ok = !inherits(lognormal_fit, "try-error") &&
    is.finite(lognormal_fit$value)

  weibull_log_likelihood = if (weibull_ok) -weibull_fit$value else -Inf
  lognormal_log_likelihood = if (lognormal_ok) -lognormal_fit$value else -Inf
  weibull_converged = weibull_ok && weibull_fit$convergence == 0L
  lognormal_converged = lognormal_ok && lognormal_fit$convergence == 0L

  all_parameters = list(
    exponential = c(
      rate = exponential_rate,
      mean = 1 / exponential_rate
    ),
    weibull = if (weibull_ok) {
      c(shape = exp(weibull_fit$par[[1]]), scale = exp(weibull_fit$par[[2]]))
    } else {
      numeric()
    },
    lognormal = if (lognormal_ok) {
      c(meanlog = lognormal_fit$par[[1]], sdlog = exp(lognormal_fit$par[[2]]))
    } else {
      numeric()
    }
  )

  models = data.frame(
    model = c("exponential", "weibull", "lognormal"),
    log_likelihood = c(
      exponential_log_likelihood,
      weibull_log_likelihood,
      lognormal_log_likelihood
    ),
    parameter_count = c(1L, 2L, 2L),
    converged = c(TRUE, weibull_converged, lognormal_converged),
    stringsAsFactors = FALSE
  )
  models$AIC = 2 * models$parameter_count - 2 * models$log_likelihood
  # v1.4.1 — AICc (correção para amostras pequenas). Com poucas observações,
  # modelos de 2 parâmetros só são escolhidos quando a melhora compensa.
  sample_size = length(time)
  correction_denominator = sample_size - models$parameter_count - 1
  models$AICc = ifelse(
    correction_denominator > 0,
    models$AIC + 2 * models$parameter_count * (models$parameter_count + 1) / correction_denominator,
    Inf
  )
  models = models[, c("model", "log_likelihood", "parameter_count", "AIC", "AICc", "converged")]

  selectable = models$converged & is.finite(models$AICc)
  if (!any(selectable)) {
    selectable = models$converged & is.finite(models$AIC)
  }
  if (!any(selectable)) {
    return(non_estimable("None of the candidate life models converged", failure_count))
  }
  selectable_indices = which(selectable)
  criterion = if (any(is.finite(models$AICc[selectable_indices]))) models$AICc else models$AIC
  selected_index = selectable_indices[[which.min(criterion[selectable_indices])]]
  selected_model = models$model[[selected_index]]

  list(
    status = "OK",
    estimable = TRUE,
    reason = NA_character_,
    failure_count = failure_count,
    observation_count = length(time),
    models = data.frame(
      model = models$model,
      loglik = models$log_likelihood,
      parameters = purrr::map_chr(
        purrr::set_names(models$model),
        function(model_name) {
          paste(
            names(all_parameters[[model_name]]),
            signif(all_parameters[[model_name]], 6),
            sep = "=",
            collapse = "; "
          )
        }
      ),
      aic = models$AIC,
      aicc = models$AICc,
      parameter_count = models$parameter_count,
      converged = models$converged,
      stringsAsFactors = FALSE
    ),
    selected_model = selected_model,
    parameters = all_parameters[[selected_model]],
    all_parameters = all_parameters
  )
}

calculate_ram_kpis = function(assets, work_orders, delays, exposure_log = NULL) {
  if (!is.data.frame(assets) || !is.data.frame(work_orders) || !is.data.frame(delays)) {
    stop("assets, work_orders, and delays must be data frames", call. = FALSE)
  }

  required_asset_columns = c("initial_meter", "current_meter", "meter_unit")
  missing_asset_columns = setdiff(required_asset_columns, names(assets))
  if (length(missing_asset_columns) > 0L) {
    stop(
      sprintf(
        "assets is missing required columns: %s",
        paste(missing_asset_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  active_mask = asset_active_mask(assets)
  meter_unit = common_active_meter_unit(assets)
  initial_meter = suppressWarnings(as.numeric(as.character(assets$initial_meter)))
  current_meter = suppressWarnings(as.numeric(as.character(assets$current_meter)))
  if (any(!is.finite(initial_meter)) || any(!is.finite(current_meter))) {
    stop("asset meter values must be finite numbers", call. = FALSE)
  }
  # v1.4.1 — numerador e denominador da mesma população: OS de viaturas
  # inativas não entram nas falhas nem nos tempos (a exposição já era só das ativas).
  if (nrow(work_orders) > 0L && "asset_id" %in% names(assets) &&
      "asset_id" %in% names(work_orders)) {
    active_ids = as.character(assets$asset_id[active_mask])
    work_orders = work_orders[as.character(work_orders$asset_id) %in% active_ids, , drop = FALSE]
  }
  exposure_by_asset = current_meter - initial_meter
  if (any(exposure_by_asset < 0)) {
    stop("current_meter cannot be below initial_meter", call. = FALSE)
  }
  exposure = sum(exposure_by_asset[active_mask])

  required_order_columns = c(
    "work_order_id",
    "maintenance_type",
    "is_functional_failure"
  )
  if (nrow(work_orders) > 0L) {
    missing_order_columns = setdiff(required_order_columns, names(work_orders))
    if (length(missing_order_columns) > 0L) {
      stop(
        sprintf(
          "work_orders is missing required columns: %s",
          paste(missing_order_columns, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  parse_times = function(values) parse_datetime_utc(values)
  duration_hours = function(start_values, end_values) {
    starts = parse_times(start_values)
    ends = parse_times(end_values)
    as.numeric(difftime(ends, starts, units = "hours"))
  }

  if (nrow(work_orders) > 0L) {
    maintenance_types = toupper(trimws(as.character(work_orders$maintenance_type)))
    functional_flags = functional_failure_flags(work_orders$is_functional_failure)
  } else {
    maintenance_types = character()
    functional_flags = logical()
  }
  corrective_mask = maintenance_types %in% c("CORRECTIVE", "CORRETIVA")
  preventive_mask = maintenance_types %in% c("PREVENTIVE", "PREVENTIVA")
  failure_mask = corrective_mask & !is.na(functional_flags) & functional_flags
  failures = sum(failure_mask)
  MTBF = if (failures > 0L && exposure > 0) exposure / failures else NA_real_

  active_columns_present = all(
    c("active_repair_start_at", "active_repair_end_at") %in% names(work_orders)
  )
  active_durations = rep(NA_real_, nrow(work_orders))
  if (active_columns_present && nrow(work_orders) > 0L) {
    active_durations = duration_hours(
      work_orders$active_repair_start_at,
      work_orders$active_repair_end_at
    )
  }

  corrective_durations = active_durations[corrective_mask]
  corrective_complete = length(corrective_durations) > 0L &&
    all(is.finite(corrective_durations)) &&
    all(corrective_durations >= 0)
  MTTR = if (corrective_complete) mean(corrective_durations) else NA_real_
  total_corrective_time = if (corrective_complete) {
    sum(corrective_durations)
  } else {
    NA_real_
  }

  preventive_durations = active_durations[preventive_mask]
  preventive_complete = if (any(preventive_mask)) {
    all(is.finite(preventive_durations)) && all(preventive_durations >= 0)
  } else {
    active_columns_present
  }
  active_preventive_time = if (preventive_complete) {
    sum(preventive_durations)
  } else {
    NA_real_
  }

  delay_columns_present = all(c("work_order_id", "start_at", "end_at") %in% names(delays))
  delay_records_available = delay_columns_present && nrow(delays) > 0L
  delay_durations = numeric()
  delays_complete = FALSE
  if (delay_records_available) {
    delay_durations = duration_hours(delays$start_at, delays$end_at)
    delays_complete = all(is.finite(delay_durations)) && all(delay_durations >= 0)
  }

  ALDT = NA_real_
  if (delays_complete && failures > 0L) {
    corrective_failure_ids = as.character(work_orders$work_order_id[failure_mask])
    relevant_delays = as.character(delays$work_order_id) %in% corrective_failure_ids
    ALDT = sum(delay_durations[relevant_delays]) / failures
  }

  time_based_exposure = identical(meter_unit, "HOURS")
  Ai = if (time_based_exposure && is.finite(MTBF) && is.finite(MTTR) && MTBF + MTTR > 0) {
    MTBF / (MTBF + MTTR)
  } else {
    NA_real_
  }
  Aa = if (
    time_based_exposure &&
    exposure > 0 &&
      is.finite(total_corrective_time) &&
      is.finite(active_preventive_time)
  ) {
    exposure / (exposure + total_corrective_time + active_preventive_time)
  } else {
    NA_real_
  }

  stage_columns = c(
    "detected_at",
    "diagnosis_start_at",
    "diagnosis_end_at",
    "active_repair_start_at",
    "active_repair_end_at",
    "test_start_at",
    "released_at"
  )
  stage_columns_present = all(stage_columns %in% names(work_orders))
  stages_complete = FALSE
  downtime_total = NA_real_
  if (stage_columns_present && nrow(work_orders) > 0L) {
    stage_numeric = do.call(
      cbind,
      purrr::map(
        stage_columns,
        function(column_name) as.numeric(parse_times(work_orders[[column_name]]))
      )
    )
    if (is.null(dim(stage_numeric))) {
      stage_numeric = matrix(stage_numeric, nrow = nrow(work_orders))
    }
    stages_finite = all(is.finite(stage_numeric))
    stages_ordered = stages_finite && all(
      apply(stage_numeric, 1, function(stage_row) all(diff(stage_row) >= 0))
    )
    stages_complete = stages_finite && stages_ordered
    if (stages_complete) {
      downtime_total = sum(
        duration_hours(work_orders$detected_at, work_orders$released_at)
      )
    }
  }

  Ao = if (
    time_based_exposure &&
    exposure > 0 &&
      stages_complete &&
      delays_complete &&
      is.finite(downtime_total)
  ) {
    exposure / (exposure + downtime_total)
  } else {
    NA_real_
  }

  # v1.4.1 — Ao por calendário: 1 - downtime / tempo de calendário observado
  # (exposure_log.start_at/end_at das viaturas ativas). Comparável com a
  # disponibilidade simulada, que é medida em horas de calendário.
  calendar_hours = NA_real_
  if (is.data.frame(exposure_log) && nrow(exposure_log) > 0L &&
      all(c("asset_id", "start_at", "end_at") %in% names(exposure_log)) &&
      "asset_id" %in% names(assets)) {
    active_ids = as.character(assets$asset_id[active_mask])
    log_rows = exposure_log[as.character(exposure_log$asset_id) %in% active_ids, , drop = FALSE]
    if (nrow(log_rows) > 0L) {
      spans = duration_hours(log_rows$start_at, log_rows$end_at)
      covered = unique(as.character(log_rows$asset_id))
      if (all(is.finite(spans)) && all(spans >= 0) && all(active_ids %in% covered)) {
        calendar_hours = sum(spans)
      }
    }
  }
  Ao_calendar = if (is.finite(calendar_hours) && calendar_hours > 0 && stages_complete &&
                    delays_complete && is.finite(downtime_total) && downtime_total <= calendar_hours) {
    1 - downtime_total / calendar_hours
  } else {
    NA_real_
  }
  observed_utilization = if (time_based_exposure && is.finite(calendar_hours) && calendar_hours > 0) {
    exposure / calendar_hours
  } else {
    NA_real_
  }

  availability_labels = c(
    Ao_calendar = if (is.na(Ao_calendar)) {
      "Disponibilidade operacional por calendário — não estimável: requer exposure_log (início/fim) de todas as viaturas ativas, etapas completas e registros de atrasos."
    } else {
      "Disponibilidade operacional por calendário — 1 − downtime / tempo de calendário observado; comparável à disponibilidade simulada."
    },
    Ai = if (is.na(Ai)) {
      if (!time_based_exposure) {
        "Disponibilidade inerente (Ai) — não estimável com exposição em KM sem conversão validada para tempo."
      } else {
        "Disponibilidade inerente (Ai) — não estimável: MTBF ou reparo corretivo ativo incompleto."
      }
    } else {
      "Disponibilidade inerente (Ai) — considera falhas e reparo corretivo ativo."
    },
    Aa = if (is.na(Aa)) {
      if (!time_based_exposure) {
        "Disponibilidade atingida (Aa) — não estimável com exposição em KM sem conversão validada para tempo."
      } else {
        "Disponibilidade atingida (Aa) — não estimável: tempos ativos corretivos ou preventivos incompletos."
      }
    } else {
      "Disponibilidade atingida (Aa) — inclui manutenção corretiva e preventiva ativa."
    },
    Ao = if (is.na(Ao)) {
      if (!time_based_exposure) {
        "Disponibilidade operacional (Ao) — não estimável com exposição em KM sem conversão validada para tempo."
      } else {
        "Disponibilidade operacional (Ao) — não estimável: requer etapas completas e registros de atrasos."
      }
    } else {
      "Disponibilidade operacional (Ao, base horas de uso) — exposição / (exposição + downtime). Com baixa utilização, subestima a prontidão de calendário."
    },
    snapshot_readiness = paste(
      "Razão instantânea de prontidão da frota — não é Ao;",
      "é uma medida de estado no instante observado."
    )
  )

  list(
    failures = as.integer(failures),
    exposure = exposure,
    exposure_unit = meter_unit,
    mtbf_unit = if (identical(meter_unit, "HOURS")) "h" else if (identical(meter_unit, "KM")) "km" else "",
    MTBF = MTBF,
    MTTR = MTTR,
    active_preventive_time = active_preventive_time,
    ALDT = ALDT,
    Ai = Ai,
    Aa = Aa,
    Ao = Ao,
    Ao_calendar = Ao_calendar,
    calendar_hours = calendar_hours,
    observed_utilization = observed_utilization,
    mtbf = MTBF,
    mttr = MTTR,
    ai = Ai,
    aa = Aa,
    ao = Ao,
    ao_reason = if (is.na(Ao)) {
      "Disponibilidade operacional não estimável: etapas ou atrasos estão incompletos."
    } else {
      NA_character_
    },
    availability_labels = availability_labels
  )
}

pareto_front = function(results) {
  if (!is.data.frame(results)) {
    stop("results must be a data frame", call. = FALSE)
  }
  required_columns = c("ci_lower", "expected_cost")
  missing_columns = setdiff(required_columns, names(results))
  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "results is missing required columns: %s",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (nrow(results) == 0L) {
    return(results)
  }
  if (
    !is.numeric(results$ci_lower) ||
      !is.numeric(results$expected_cost) ||
      any(!is.finite(results$ci_lower)) ||
      any(!is.finite(results$expected_cost))
  ) {
    stop("ci_lower and expected_cost must contain finite numeric values", call. = FALSE)
  }

  dominated = logical(nrow(results))
  for (candidate_index in seq_len(nrow(results))) {
    readiness_no_worse = results$ci_lower >= results$ci_lower[[candidate_index]]
    cost_no_worse = results$expected_cost <= results$expected_cost[[candidate_index]]
    strictly_better = results$ci_lower > results$ci_lower[[candidate_index]] |
      results$expected_cost < results$expected_cost[[candidate_index]]
    dominated[[candidate_index]] = any(
      readiness_no_worse & cost_no_worse & strictly_better
    )
  }

  efficient = results[!dominated, , drop = FALSE]
  row.names(efficient) = NULL
  efficient
}

select_integrated_solution = function(results, p_target, costs_complete) {
  if (!is.data.frame(results)) {
    stop("results must be a data frame", call. = FALSE)
  }
  required_columns = c("fleet_size", "ci_lower")
  missing_columns = setdiff(required_columns, names(results))
  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "results is missing required columns: %s",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (
    !is.numeric(p_target) ||
      length(p_target) != 1L ||
      !is.finite(p_target) ||
      p_target < 0 ||
      p_target > 1
  ) {
    stop("p_target must be a finite probability between zero and one", call. = FALSE)
  }
  if (!is.logical(costs_complete) || length(costs_complete) != 1L || is.na(costs_complete)) {
    stop("costs_complete must be one non-missing logical value", call. = FALSE)
  }
  if (!is.numeric(results$fleet_size) || !is.numeric(results$ci_lower)) {
    stop("fleet_size and ci_lower must be numeric", call. = FALSE)
  }

  feasible = results[
    is.finite(results$ci_lower) & results$ci_lower >= p_target,
    ,
    drop = FALSE
  ]
  row.names(feasible) = NULL
  if (nrow(feasible) == 0L) {
    return(list(
      status = "NO_FEASIBLE",
      recommended = NULL,
      candidates = feasible,
      selected = NULL,
      optimum = NULL,
      feasible = feasible,
      pareto = feasible
    ))
  }
  if (any(!is.finite(feasible$fleet_size))) {
    stop("feasible fleet_size values must be finite", call. = FALSE)
  }

  if (costs_complete) {
    if (
      !"expected_cost" %in% names(feasible) ||
        !is.numeric(feasible$expected_cost) ||
        any(!is.finite(feasible$expected_cost))
    ) {
      stop(
        "costs_complete is TRUE but feasible expected_cost values are incomplete",
        call. = FALSE
      )
    }

    minimum_fleet_size = min(feasible$fleet_size)
    fleet_candidates = feasible[
      feasible$fleet_size == minimum_fleet_size,
      ,
      drop = FALSE
    ]
    minimum_cost = min(fleet_candidates$expected_cost)
    selected = fleet_candidates[
      which(fleet_candidates$expected_cost == minimum_cost)[[1]],
      ,
      drop = FALSE
    ]
    row.names(selected) = NULL

    return(list(
      status = "OPTIMUM",
      recommended = selected,
      candidates = feasible,
      selected = selected,
      optimum = selected,
      feasible = feasible,
      pareto = pareto_front(feasible)
    ))
  }

  objective_columns = c(
    "ci_lower",
    intersect(
      c(
        "fleet_size",
        "diagnostic_teams",
        "repair_teams",
        "maintenance_teams",
        "teams",
        "bays",
        "boxes",
        "initial_stock",
        "stock",
        "logistic_time",
        "logistics_time",
        "lead_time"
      ),
      names(feasible)
    )
  )
  minimize_columns = setdiff(objective_columns, "ci_lower")
  if (any(!purrr::map_lgl(feasible[objective_columns], is.numeric))) {
    stop("Pareto objective columns must be numeric", call. = FALSE)
  }

  dominates = function(left_index, right_index) {
    left_values = as.numeric(feasible[left_index, objective_columns, drop = TRUE])
    right_values = as.numeric(feasible[right_index, objective_columns, drop = TRUE])
    if (any(!is.finite(left_values)) || any(!is.finite(right_values))) {
      return(FALSE)
    }

    readiness_no_worse = feasible$ci_lower[[left_index]] >= feasible$ci_lower[[right_index]]
    readiness_strict = feasible$ci_lower[[left_index]] > feasible$ci_lower[[right_index]]
    resources_no_worse = TRUE
    resources_strict = FALSE
    for (column_name in minimize_columns) {
      resources_no_worse = resources_no_worse &&
        feasible[[column_name]][[left_index]] <= feasible[[column_name]][[right_index]]
      resources_strict = resources_strict ||
        feasible[[column_name]][[left_index]] < feasible[[column_name]][[right_index]]
    }
    readiness_no_worse && resources_no_worse && (readiness_strict || resources_strict)
  }

  dominated = logical(nrow(feasible))
  for (candidate_index in seq_len(nrow(feasible))) {
    other_indices = setdiff(seq_len(nrow(feasible)), candidate_index)
    dominated[[candidate_index]] = any(
      purrr::map_lgl(other_indices, function(other_index) dominates(other_index, candidate_index))
    )
  }
  efficient = feasible[!dominated, , drop = FALSE]
  row.names(efficient) = NULL

  list(
    status = "PARETO",
    recommended = NULL,
    candidates = efficient,
    selected = NULL,
    optimum = NULL,
    feasible = feasible,
    pareto = efficient
  )
}

# ============================================================================
# 2. MOTOR DE EVENTOS DISCRETOS, MONTE CARLO E DECISÃO INTEGRADA
# ============================================================================

sample_model = function(model, n = 1) {
  if (!is.list(model) || is.null(model$type) || length(model$type) != 1L) {
    stop("model must be a named list with one type", call. = FALSE)
  }
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 1 || n != floor(n)) {
    stop("n must be a positive whole number", call. = FALSE)
  }
  n = as.integer(n)
  model_type = tolower(trimws(as.character(model$type)))
  positive_scalar = function(value) {
    is.numeric(value) && length(value) == 1L && !is.na(value) &&
      is.finite(value) && value > 0
  }
  nonnegative_scalar = function(value) {
    is.numeric(value) && length(value) == 1L && !is.na(value) &&
      is.finite(value) && value >= 0
  }

  if (model_type == "constant") {
    if (!positive_scalar(model$value)) {
      stop("constant model requires value > 0", call. = FALSE)
    }
    return(rep(as.numeric(model$value), n))
  }
  if (model_type == "exponential") {
    if (!positive_scalar(model$rate)) {
      stop("exponential model requires rate > 0", call. = FALSE)
    }
    return(stats::rexp(n, rate = model$rate))
  }
  if (model_type == "weibull") {
    if (!positive_scalar(model$shape) || !positive_scalar(model$scale)) {
      stop("weibull model requires shape and scale > 0", call. = FALSE)
    }
    return(stats::rweibull(n, shape = model$shape, scale = model$scale))
  }
  if (model_type == "lognormal") {
    if (!is.numeric(model$meanlog) || length(model$meanlog) != 1L ||
        !is.finite(model$meanlog) || !nonnegative_scalar(model$sdlog)) {
      stop("lognormal model requires finite meanlog and sdlog >= 0", call. = FALSE)
    }
    if (model$sdlog == 0) return(rep(exp(model$meanlog), n))
    return(stats::rlnorm(n, meanlog = model$meanlog, sdlog = model$sdlog))
  }
  if (model_type == "zero_lognormal") {
    valid_probability = is.numeric(model$p_zero) && length(model$p_zero) == 1L &&
      is.finite(model$p_zero) && model$p_zero >= 0 && model$p_zero <= 1
    if (!valid_probability || !is.numeric(model$meanlog) ||
        length(model$meanlog) != 1L || !is.finite(model$meanlog) ||
        !nonnegative_scalar(model$sdlog)) {
      stop(
        "zero_lognormal requires p_zero in [0,1], finite meanlog and sdlog >= 0",
        call. = FALSE
      )
    }
    is_zero = stats::runif(n) < model$p_zero
    values = if (model$sdlog == 0) {
      rep(exp(model$meanlog), n)
    } else {
      stats::rlnorm(n, meanlog = model$meanlog, sdlog = model$sdlog)
    }
    values[is_zero] = 0
    return(values)
  }
  if (model_type == "empirical") {
    values = suppressWarnings(as.numeric(model$values))
    if (length(values) == 0L || any(!is.finite(values)) || any(values < 0)) {
      stop("empirical model requires finite nonnegative values", call. = FALSE)
    }
    return(sample(values, size = n, replace = TRUE))
  }
  stop(
    paste0(
      "unsupported model type: ", model_type,
      ". Use constant, exponential, weibull, lognormal, zero_lognormal or empirical."
    ),
    call. = FALSE
  )
}

simulation_option = function(config, name, default) {
  value = config[[name]]
  if (is.null(value) || length(value) == 0L) return(default)
  if (length(value) == 1L && is.na(value) && !is.na(default)) return(default)
  value
}

validate_simulation_inputs = function(config, params, horizon, target_ready) {
  collector = new.env(parent = emptyenv())
  collector$errors = character()
  add_error = function(message) {
    collector$errors = c(collector$errors, message)
  }
  required_config = c(
    "fleet_size", "diagnostic_teams", "repair_teams", "bays",
    "initial_stock", "reorder_point", "reorder_qty", "pm_interval",
    "asset_cost_horizon", "diagnostic_team_cost_horizon",
    "repair_team_cost_horizon", "bay_cost_horizon", "part_unit_cost",
    "corrective_event_cost", "preventive_event_cost", "downtime_hour_cost"
  )
  required_params = c(
    "life_model", "diagnosis_model", "corrective_model", "preventive_model",
    "test_model", "lead_time_model", "part_required_probability"
  )
  if (!is.list(config)) {
    add_error("config must be a named list")
  } else {
    missing_config = setdiff(required_config, names(config))
    if (length(missing_config) > 0L) {
      add_error(paste("missing config fields:", paste(missing_config, collapse = ", ")))
    }
  }
  if (!is.list(params)) {
    add_error("params must be a named list")
  } else {
    missing_params = setdiff(required_params, names(params))
    if (length(missing_params) > 0L) {
      add_error(paste("missing parameter fields:", paste(missing_params, collapse = ", ")))
    }
  }
  if (length(collector$errors) > 0L) {
    return(list(valid = FALSE, errors = unique(collector$errors)))
  }

  whole_positive = c("fleet_size", "diagnostic_teams", "repair_teams", "bays")
  for (field in whole_positive) {
    value = config[[field]]
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value < 1 || value != floor(value)) {
      add_error(paste(field, "must be a positive whole number"))
    }
  }
  whole_nonnegative = c("initial_stock", "reorder_point", "reorder_qty")
  for (field in whole_nonnegative) {
    value = config[[field]]
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value < 0 || value != floor(value)) {
      add_error(paste(field, "must be a nonnegative whole number"))
    }
  }
  if (is.numeric(config$reorder_point) && is.numeric(config$reorder_qty) &&
      is.finite(config$reorder_point) && config$reorder_point > 0 &&
      is.finite(config$reorder_qty) && config$reorder_qty == 0) {
    add_error("reorder_qty must be positive when reorder_point is positive")
  }
  if (!is.numeric(config$pm_interval) || length(config$pm_interval) != 1L ||
      is.na(config$pm_interval)) {
    add_error("pm_interval must be numeric; non-positive or infinite disables PM")
  }
  cost_fields = c(
    "asset_cost_horizon", "diagnostic_team_cost_horizon",
    "repair_team_cost_horizon", "bay_cost_horizon", "part_unit_cost",
    "corrective_event_cost", "preventive_event_cost", "downtime_hour_cost"
  )
  for (field in cost_fields) {
    value = config[[field]]
    valid_cost = is.numeric(value) && length(value) == 1L &&
      (is.na(value) || (is.finite(value) && value >= 0))
    if (!valid_cost) add_error(paste(field, "must be nonnegative or NA"))
  }
  # v1.4.1 — opções explícitas do motor (ausentes = comportamento da v1.4.0)
  utilization = simulation_option(config, "utilization", 1)
  if (!is.numeric(utilization) || length(utilization) != 1L ||
      !is.finite(utilization) || utilization <= 0 || utilization > 1) {
    add_error("utilization must be in (0, 1]")
  }
  stagger = simulation_option(config, "stagger_pm", FALSE)
  if (!is.logical(stagger) || length(stagger) != 1L || is.na(stagger)) {
    add_error("stagger_pm must be TRUE or FALSE")
  }
  initial_unavailable = simulation_option(config, "initial_unavailable", 0)
  if (!is.numeric(initial_unavailable) || length(initial_unavailable) != 1L ||
      !is.finite(initial_unavailable) || initial_unavailable < 0 ||
      initial_unavailable != floor(initial_unavailable) ||
      (is.numeric(config$fleet_size) && length(config$fleet_size) == 1L &&
         is.finite(config$fleet_size) && initial_unavailable > config$fleet_size)) {
    add_error("initial_unavailable must be a whole number from zero through fleet_size")
  }
  tolerance = simulation_option(config, "mission_tolerance_hours", 0)
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      !is.finite(tolerance) || tolerance < 0) {
    add_error("mission_tolerance_hours must be a finite nonnegative number")
  }
  if (!is.numeric(horizon) || length(horizon) != 1L ||
      !is.finite(horizon) || horizon <= 0) {
    add_error("horizon must be a finite positive number")
  }
  if (!is.numeric(target_ready) || length(target_ready) != 1L ||
      !is.finite(target_ready) || target_ready < 0 ||
      target_ready != floor(target_ready) || target_ready > config$fleet_size) {
    add_error("target_ready must be a whole number from zero through fleet_size")
  }
  probability = params$part_required_probability
  if (!is.numeric(probability) || length(probability) != 1L ||
      !is.finite(probability) || probability < 0 || probability > 1) {
    add_error("part_required_probability must be between zero and one")
  }
  pm_probability = params$pm_part_required_probability
  if (!is.null(pm_probability) && !(length(pm_probability) == 1L && is.na(pm_probability))) {
    if (!is.numeric(pm_probability) || length(pm_probability) != 1L ||
        !is.finite(pm_probability) || pm_probability < 0 || pm_probability > 1) {
      add_error("pm_part_required_probability must be between zero and one or NA")
    }
  }
  model_fields = setdiff(required_params, "part_required_probability")
  for (field in model_fields) {
    model_error = tryCatch(
      {
        sample_model(params[[field]], 1)
        NULL
      },
      error = function(error_condition) conditionMessage(error_condition)
    )
    if (!is.null(model_error)) add_error(paste(field, model_error, sep = ": "))
  }
  list(
    valid = length(collector$errors) == 0L,
    errors = unique(collector$errors)
  )
}

# Motor de eventos discretos.
# v1.4.1: mesma lógica e mesma sequência de números aleatórios da v1.4.0 quando
# utilization = 1, stagger_pm = FALSE, initial_unavailable = 0 e
# mission_tolerance_hours = 0. A fila de eventos e os registros passaram a usar
# vetores (antes data.frame + rbind a cada evento), e os logs detalhados só são
# montados quando keep_logs = TRUE.
simulate_replication = function(
  config,
  params,
  horizon,
  target_ready,
  seed,
  keep_logs = FALSE,
  validate = TRUE
) {
  if (isTRUE(validate)) {
    validation = validate_simulation_inputs(config, params, horizon, target_ready)
    if (!validation$valid) {
      stop(paste(validation$errors, collapse = "; "), call. = FALSE)
    }
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
    stop("seed must be one finite number", call. = FALSE)
  }
  set.seed(as.integer(seed))
  tiny = 1e-10
  utilization = as.numeric(simulation_option(config, "utilization", 1))
  stagger_pm = isTRUE(simulation_option(config, "stagger_pm", FALSE))
  initial_unavailable = as.integer(simulation_option(config, "initial_unavailable", 0))
  tolerance_hours = as.numeric(simulation_option(config, "mission_tolerance_hours", 0))
  pm_part_probability = params$pm_part_required_probability
  if (is.null(pm_part_probability) || (length(pm_part_probability) == 1L && is.na(pm_part_probability))) {
    pm_part_probability = params$part_required_probability
  }
  pm_enabled = is.finite(config$pm_interval) && config$pm_interval > 0

  sim = new.env(parent = emptyenv())
  sim$n = as.integer(config$fleet_size)
  sim$time = 0
  sim$last_time = 0
  sim$event_counter = 0L
  sim$ev_time = numeric()
  sim$ev_priority = integer()
  sim$ev_serial = integer()
  sim$ev_type = character()
  sim$ev_asset = integer()
  sim$state = rep("OPERATING", sim$n)
  sim$state_start = rep(0, sim$n)
  sim$operating_since_pm = rep(0, sim$n)
  sim$job_type = rep(NA_character_, sim$n)
  sim$ready = sim$n
  sim$min_ready = sim$n
  sim$area_ready = 0
  sim$hours_below = 0
  sim$diagnosis_busy = 0L
  sim$repair_busy = 0L
  sim$diagnosis_queue = integer()
  sim$repair_queue = integer()
  sim$part_queue = integer()
  sim$stock = as.integer(config$initial_stock)
  sim$order_outstanding = FALSE
  sim$failures = 0L
  sim$preventive_events = 0L
  sim$parts_consumed = 0L
  sim$orders_placed = 0L
  state_names = c(
    "OPERATING", "QUEUE_DIAGNOSIS", "DIAGNOSIS", "WAIT_PART", "QUEUE_REPAIR",
    "ACTIVE_CORRECTIVE", "ACTIVE_PREVENTIVE", "TEST_RELEASE"
  )
  sim$state_totals = stats::setNames(numeric(length(state_names)), state_names)
  sim$total_logged = 0
  if (keep_logs) {
    sim$log_state = list()
    sim$log_event = list()
    sim$log_ready = list(c(0, sim$n))
  }

  if (stagger_pm && pm_enabled) {
    # Fase inicial do ciclo de PM distribuída uniformemente entre as viaturas,
    # evitando que toda a frota chegue à preventiva no mesmo instante.
    sim$operating_since_pm = stats::runif(sim$n, 0, config$pm_interval)
  }

  schedule_event = function(time, type, asset = NA_integer_, priority = 5L) {
    if (!is.finite(time) || time < sim$time - tiny) {
      stop("event time violates chronological order", call. = FALSE)
    }
    sim$event_counter = sim$event_counter + 1L
    sim$ev_time = c(sim$ev_time, as.numeric(time))
    sim$ev_priority = c(sim$ev_priority, as.integer(priority))
    sim$ev_serial = c(sim$ev_serial, sim$event_counter)
    sim$ev_type = c(sim$ev_type, as.character(type))
    sim$ev_asset = c(sim$ev_asset, as.integer(asset))
    invisible(NULL)
  }

  next_event_index = function() {
    candidates = which(sim$ev_time == min(sim$ev_time))
    if (length(candidates) > 1L) {
      priorities = sim$ev_priority[candidates]
      candidates = candidates[priorities == min(priorities)]
    }
    if (length(candidates) > 1L) {
      candidates = candidates[which.min(sim$ev_serial[candidates])]
    }
    candidates[[1]]
  }

  remove_event = function(index) {
    sim$ev_time = sim$ev_time[-index]
    sim$ev_priority = sim$ev_priority[-index]
    sim$ev_serial = sim$ev_serial[-index]
    sim$ev_type = sim$ev_type[-index]
    sim$ev_asset = sim$ev_asset[-index]
    invisible(NULL)
  }

  advance_clock = function(new_time) {
    elapsed = new_time - sim$last_time
    if (elapsed < -tiny) stop("simulation clock moved backwards", call. = FALSE)
    if (elapsed > 0) {
      sim$area_ready = sim$area_ready + sim$ready * elapsed
      if (sim$ready < target_ready) {
        sim$hours_below = sim$hours_below + elapsed
      }
    }
    sim$last_time = new_time
    sim$time = new_time
    invisible(NULL)
  }

  record_event = function(type, asset = NA_integer_) {
    if (!keep_logs) return(invisible(NULL))
    asset_label = if (is.na(asset)) NA_character_ else sprintf("VTR-%03d", asset)
    sim$log_event[[length(sim$log_event) + 1L]] = list(
      time = sim$time, event = type, asset_id = asset_label,
      ready = sim$ready, stock = sim$stock
    )
    invisible(NULL)
  }

  log_state = function(asset, state, start, end) {
    duration = max(0, end - start)
    sim$state_totals[[state]] = sim$state_totals[[state]] + duration
    sim$total_logged = sim$total_logged + duration
    if (keep_logs) {
      sim$log_state[[length(sim$log_state) + 1L]] = list(
        asset_id = sprintf("VTR-%03d", asset), state = state,
        start = start, end = end, duration = duration
      )
    }
    invisible(NULL)
  }

  transition = function(asset, new_state) {
    old_state = sim$state[[asset]]
    if (identical(old_state, new_state)) return(invisible(NULL))
    duration = sim$time - sim$state_start[[asset]]
    if (duration < -tiny) stop("negative state duration", call. = FALSE)
    log_state(asset, old_state, sim$state_start[[asset]], sim$time)
    if (old_state == "OPERATING" && new_state != "OPERATING") {
      sim$ready = sim$ready - 1L
    }
    if (old_state != "OPERATING" && new_state == "OPERATING") {
      sim$ready = sim$ready + 1L
    }
    if (sim$ready < 0L || sim$ready > sim$n) {
      stop("asset conservation failure: ready count is outside fleet bounds", call. = FALSE)
    }
    sim$min_ready = min(sim$min_ready, sim$ready)
    sim$state[[asset]] = new_state
    sim$state_start[[asset]] = sim$time
    if (keep_logs) sim$log_ready[[length(sim$log_ready) + 1L]] = c(sim$time, sim$ready)
    invisible(NULL)
  }

  schedule_operating_due = function(asset) {
    # Vida e intervalo de PM estão em horas de USO; o relógio da missão está em
    # horas de calendário. utilization = horas de uso por hora de calendário.
    failure_delay = sample_model(params$life_model, 1)[[1]] / utilization
    pm_delay = if (pm_enabled) {
      max(0, config$pm_interval - sim$operating_since_pm[[asset]]) / utilization
    } else {
      Inf
    }
    if (pm_delay <= failure_delay) {
      schedule_event(sim$time + pm_delay, "PM_DUE", asset, priority = 4L)
    } else {
      schedule_event(sim$time + failure_delay, "FAILURE_DUE", asset, priority = 4L)
    }
    invisible(NULL)
  }

  ensure_reorder = function() {
    if (sim$stock <= config$reorder_point && !sim$order_outstanding &&
        config$reorder_qty > 0) {
      lead_time = sample_model(params$lead_time_model, 1)[[1]]
      sim$order_outstanding = TRUE
      sim$orders_placed = sim$orders_placed + 1L
      schedule_event(sim$time + lead_time, "DELIVERY", NA_integer_, priority = 1L)
      record_event("ORDER_PLACED")
    }
    invisible(NULL)
  }

  dispatch_diagnosis = function() {
    while (length(sim$diagnosis_queue) > 0L &&
           sim$diagnosis_busy < config$diagnostic_teams) {
      asset = sim$diagnosis_queue[[1]]
      sim$diagnosis_queue = sim$diagnosis_queue[-1]
      sim$diagnosis_busy = sim$diagnosis_busy + 1L
      transition(asset, "DIAGNOSIS")
      duration = sample_model(params$diagnosis_model, 1)[[1]]
      schedule_event(sim$time + duration, "END_DIAGNOSIS", asset, priority = 2L)
    }
    invisible(NULL)
  }

  dispatch_repair = function() {
    repair_capacity = min(config$repair_teams, config$bays)
    while (length(sim$repair_queue) > 0L && sim$repair_busy < repair_capacity) {
      asset = sim$repair_queue[[1]]
      sim$repair_queue = sim$repair_queue[-1]
      sim$repair_busy = sim$repair_busy + 1L
      job_type = sim$job_type[[asset]]
      if (identical(job_type, "CORRECTIVE")) {
        transition(asset, "ACTIVE_CORRECTIVE")
        duration = sample_model(params$corrective_model, 1)[[1]]
        event_type = "END_CORRECTIVE"
      } else {
        transition(asset, "ACTIVE_PREVENTIVE")
        duration = sample_model(params$preventive_model, 1)[[1]]
        event_type = "END_PREVENTIVE"
      }
      schedule_event(sim$time + duration, event_type, asset, priority = 2L)
    }
    invisible(NULL)
  }

  route_to_repair = function(asset) {
    probability = if (identical(sim$job_type[[asset]], "PREVENTIVE")) {
      pm_part_probability
    } else {
      params$part_required_probability
    }
    requires_part = stats::runif(1) < probability
    if (requires_part && sim$stock <= 0L) {
      transition(asset, "WAIT_PART")
      sim$part_queue = c(sim$part_queue, asset)
      ensure_reorder()
      return(invisible(NULL))
    }
    if (requires_part) {
      sim$stock = sim$stock - 1L
      sim$parts_consumed = sim$parts_consumed + 1L
      ensure_reorder()
    }
    transition(asset, "QUEUE_REPAIR")
    sim$repair_queue = c(sim$repair_queue, asset)
    dispatch_repair()
    invisible(NULL)
  }

  # Viaturas indisponíveis no instante zero (estado atual da frota): entram
  # como corretivas pendentes na fila de diagnóstico; não contam como falhas
  # novas da missão.
  if (initial_unavailable > 0L) {
    for (asset in seq_len(initial_unavailable)) {
      sim$job_type[[asset]] = "CORRECTIVE"
      transition(asset, "QUEUE_DIAGNOSIS")
      sim$diagnosis_queue = c(sim$diagnosis_queue, asset)
    }
  }
  for (asset in seq_len(sim$n)) {
    if (asset > initial_unavailable) schedule_operating_due(asset)
  }
  if (initial_unavailable > 0L) dispatch_diagnosis()

  while (length(sim$ev_time) > 0L) {
    index = next_event_index()
    event_time = sim$ev_time[[index]]
    if (event_time > horizon + tiny) break
    event_type = sim$ev_type[[index]]
    asset = sim$ev_asset[[index]]
    remove_event(index)
    advance_clock(min(event_time, horizon))
    record_event(event_type, asset)

    if (event_type == "FAILURE_DUE") {
      operating_duration = sim$time - sim$state_start[[asset]]
      sim$operating_since_pm[[asset]] =
        sim$operating_since_pm[[asset]] + operating_duration * utilization
      sim$failures = sim$failures + 1L
      sim$job_type[[asset]] = "CORRECTIVE"
      transition(asset, "QUEUE_DIAGNOSIS")
      sim$diagnosis_queue = c(sim$diagnosis_queue, asset)
      dispatch_diagnosis()
    } else if (event_type == "PM_DUE") {
      operating_duration = sim$time - sim$state_start[[asset]]
      sim$operating_since_pm[[asset]] =
        sim$operating_since_pm[[asset]] + operating_duration * utilization
      sim$preventive_events = sim$preventive_events + 1L
      sim$job_type[[asset]] = "PREVENTIVE"
      transition(asset, "QUEUE_REPAIR")
      route_to_repair(asset)
    } else if (event_type == "END_DIAGNOSIS") {
      sim$diagnosis_busy = sim$diagnosis_busy - 1L
      route_to_repair(asset)
      dispatch_diagnosis()
    } else if (event_type %in% c("END_CORRECTIVE", "END_PREVENTIVE")) {
      sim$repair_busy = sim$repair_busy - 1L
      transition(asset, "TEST_RELEASE")
      test_duration = sample_model(params$test_model, 1)[[1]]
      schedule_event(sim$time + test_duration, "END_TEST", asset, priority = 1L)
      dispatch_repair()
    } else if (event_type == "END_TEST") {
      completed_type = sim$job_type[[asset]]
      if (identical(completed_type, "PREVENTIVE")) {
        sim$operating_since_pm[[asset]] = 0
      }
      sim$job_type[[asset]] = NA_character_
      transition(asset, "OPERATING")
      schedule_operating_due(asset)
    } else if (event_type == "DELIVERY") {
      sim$stock = sim$stock + as.integer(config$reorder_qty)
      sim$order_outstanding = FALSE
      while (length(sim$part_queue) > 0L && sim$stock > 0L) {
        waiting_asset = sim$part_queue[[1]]
        sim$part_queue = sim$part_queue[-1]
        sim$stock = sim$stock - 1L
        sim$parts_consumed = sim$parts_consumed + 1L
        transition(waiting_asset, "QUEUE_REPAIR")
        sim$repair_queue = c(sim$repair_queue, waiting_asset)
      }
      dispatch_repair()
      ensure_reorder()
    } else {
      stop(paste("unknown event type", event_type), call. = FALSE)
    }
  }

  advance_clock(horizon)
  for (asset in seq_len(sim$n)) {
    duration = horizon - sim$state_start[[asset]]
    if (duration < -tiny) stop("negative closing state duration", call. = FALSE)
    log_state(asset, sim$state[[asset]], sim$state_start[[asset]], horizon)
  }
  state_totals = sim$state_totals
  state_hours = function(state_names) sum(state_totals[state_names])
  diagnosis_hours = state_hours("DIAGNOSIS")
  queue_resource_hours = state_hours(c("QUEUE_DIAGNOSIS", "QUEUE_REPAIR"))
  wait_part_hours = state_hours("WAIT_PART")
  corrective_hours = state_hours("ACTIVE_CORRECTIVE")
  preventive_hours = state_hours("ACTIVE_PREVENTIVE")
  test_hours = state_hours("TEST_RELEASE")
  downtime_asset_hours = diagnosis_hours + queue_resource_hours + wait_part_hours +
    corrective_hours + preventive_hours + test_hours
  total_asset_hours = sim$n * horizon
  conservation_error = abs(sim$total_logged - total_asset_hours)
  if (conservation_error > 1e-8 * max(1, total_asset_hours)) {
    stop("asset-hour conservation check failed", call. = FALSE)
  }
  availability = 1 - downtime_asset_hours / total_asset_hours
  if (availability < -1e-8 || availability > 1 + 1e-8) {
    stop("calculated availability is outside [0,1]", call. = FALSE)
  }
  availability = min(1, max(0, availability))

  cost_values = unlist(config[c(
    "asset_cost_horizon", "diagnostic_team_cost_horizon",
    "repair_team_cost_horizon", "bay_cost_horizon", "part_unit_cost",
    "corrective_event_cost", "preventive_event_cost", "downtime_hour_cost"
  )], use.names = FALSE)
  total_cost = if (any(is.na(cost_values))) {
    NA_real_
  } else {
    config$fleet_size * config$asset_cost_horizon +
      config$diagnostic_teams * config$diagnostic_team_cost_horizon +
      config$repair_teams * config$repair_team_cost_horizon +
      config$bays * config$bay_cost_horizon +
      sim$parts_consumed * config$part_unit_cost +
      sim$failures * config$corrective_event_cost +
      sim$preventive_events * config$preventive_event_cost +
      downtime_asset_hours * config$downtime_hour_cost
  }
  # Critério de missão: sem tolerância (padrão), nenhuma queda abaixo de M;
  # com tolerância, o tempo acumulado abaixo de M não pode exceder o limite.
  mission_success = if (tolerance_hours > 0) {
    sim$hours_below <= tolerance_hours + tiny
  } else {
    sim$min_ready >= target_ready
  }
  result = list(
    seed = as.integer(seed), fleet_size = sim$n,
    target_ready = as.integer(target_ready),
    mission_success = mission_success,
    min_ready = as.integer(sim$min_ready),
    mean_ready = sim$area_ready / horizon,
    avg_availability = availability,
    hours_below_target = sim$hours_below,
    failures = sim$failures, preventive_events = sim$preventive_events,
    diagnosis_hours = diagnosis_hours,
    queue_resource_hours = queue_resource_hours,
    wait_part_hours = wait_part_hours,
    corrective_hours = corrective_hours,
    preventive_hours = preventive_hours,
    test_hours = test_hours,
    downtime_asset_hours = downtime_asset_hours,
    parts_consumed = sim$parts_consumed,
    orders_placed = sim$orders_placed,
    total_cost = total_cost
  )
  if (keep_logs) {
    to_frame = function(rows, template) {
      if (length(rows) == 0L) return(template)
      as.data.frame(do.call(rbind, purrr::map(rows, as.data.frame, stringsAsFactors = FALSE)),
                    stringsAsFactors = FALSE)
    }
    result$state_log = to_frame(sim$log_state, data.frame(
      asset_id = character(), state = character(), start = numeric(),
      end = numeric(), duration = numeric(), stringsAsFactors = FALSE
    ))
    result$event_log = to_frame(sim$log_event, data.frame(
      time = numeric(), event = character(), asset_id = character(),
      ready = integer(), stock = integer(), stringsAsFactors = FALSE
    ))
    ready_matrix = do.call(rbind, sim$log_ready)
    result$readiness_log = data.frame(
      time = ready_matrix[, 1], ready = as.integer(ready_matrix[, 2]),
      stringsAsFactors = FALSE
    )
  }
  result
}

replication_to_row = function(result) {
  fields = c(
    "seed", "fleet_size", "target_ready", "mission_success", "min_ready",
    "mean_ready", "avg_availability", "hours_below_target", "failures",
    "preventive_events", "diagnosis_hours", "queue_resource_hours",
    "wait_part_hours", "corrective_hours", "preventive_hours", "test_hours",
    "downtime_asset_hours", "parts_consumed", "orders_placed", "total_cost"
  )
  as.data.frame(result[fields], stringsAsFactors = FALSE)
}

configuration_id = function(config) {
  paste0(
    "N", config$fleet_size,
    "-D", config$diagnostic_teams,
    "-R", config$repair_teams,
    "-B", config$bays,
    "-S", config$initial_stock
  )
}

evaluate_configuration = function(
  config,
  params,
  horizon,
  target_ready,
  replications,
  seed
) {
  validation = validate_simulation_inputs(config, params, horizon, target_ready)
  if (!validation$valid) stop(paste(validation$errors, collapse = "; "), call. = FALSE)
  if (!is.numeric(replications) || length(replications) != 1L ||
      !is.finite(replications) || replications < 1 ||
      replications != floor(replications)) {
    stop("replications must be a positive whole number", call. = FALSE)
  }
  # Cada réplica usa a própria semente (seed + índice - 1): a ordem e os
  # números aleatórios são os mesmos do laço anterior.
  rows = without_purrr_index(purrr::map(seq_len(as.integer(replications)), function(replication_index) {
    replication_to_row(simulate_replication(
      config = config, params = params, horizon = horizon,
      target_ready = target_ready,
      seed = as.integer(seed) + replication_index - 1L,
      keep_logs = FALSE, validate = FALSE
    ))
  }))
  replication_data = bind_rows_base(rows)
  row.names(replication_data) = NULL
  successes = sum(replication_data$mission_success)
  interval = wilson_interval(successes, nrow(replication_data))
  mean_or_na = function(values) {
    if (all(is.na(values))) return(NA_real_)
    mean(values, na.rm = TRUE)
  }
  summary = data.frame(
    configuration_id = configuration_id(config),
    fleet_size = config$fleet_size,
    diagnostic_teams = config$diagnostic_teams,
    repair_teams = config$repair_teams,
    bays = config$bays,
    initial_stock = config$initial_stock,
    effective_repair_capacity = min(config$repair_teams, config$bays),
    replications = nrow(replication_data),
    successes = successes,
    success_probability = unname(interval["estimate"]),
    ci_lower = unname(interval["lower"]),
    ci_upper = unname(interval["upper"]),
    mean_min_ready = mean(replication_data$min_ready),
    mean_ready = mean(replication_data$mean_ready),
    mean_availability = mean(replication_data$avg_availability),
    mean_hours_below_target = mean(replication_data$hours_below_target),
    mean_failures = mean(replication_data$failures),
    mean_preventive_events = mean(replication_data$preventive_events),
    mean_diagnosis_hours = mean(replication_data$diagnosis_hours),
    mean_queue_resource_hours = mean(replication_data$queue_resource_hours),
    mean_wait_part_hours = mean(replication_data$wait_part_hours),
    mean_corrective_hours = mean(replication_data$corrective_hours),
    mean_preventive_hours = mean(replication_data$preventive_hours),
    mean_test_hours = mean(replication_data$test_hours),
    mean_downtime_asset_hours = mean(replication_data$downtime_asset_hours),
    expected_cost = mean_or_na(replication_data$total_cost),
    stringsAsFactors = FALSE
  )
  list(summary = summary, replications = replication_data)
}

build_configuration_grid = function(
  fleet_sizes,
  diagnostic_teams,
  repair_teams,
  bays,
  initial_stocks,
  base_config
) {
  if (!is.list(base_config)) stop("base_config must be a named list", call. = FALSE)
  vectors = list(
    fleet_size = fleet_sizes, diagnostic_teams = diagnostic_teams,
    repair_teams = repair_teams, bays = bays, initial_stock = initial_stocks
  )
  for (field in names(vectors)) {
    values = suppressWarnings(as.numeric(vectors[[field]]))
    if (length(values) == 0L || any(!is.finite(values)) ||
        any(values < 0) || any(values != floor(values))) {
      stop(paste(field, "grid values must be nonnegative whole numbers"), call. = FALSE)
    }
    vectors[[field]] = unique(as.integer(values))
  }
  if (any(vectors$fleet_size < 1L) || any(vectors$diagnostic_teams < 1L) ||
      any(vectors$repair_teams < 1L) || any(vectors$bays < 1L)) {
    stop("fleet and capacity grid values must be positive", call. = FALSE)
  }
  grid = expand.grid(vectors, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  varying = names(vectors)
  for (field in setdiff(names(base_config), varying)) {
    grid[[field]] = base_config[[field]]
  }
  grid$configuration_id = purrr::map_chr(
    seq_len(nrow(grid)),
    function(index) configuration_id(as.list(grid[index, , drop = FALSE]))
  )
  grid
}

evaluate_grid = function(
  grid,
  params,
  horizon,
  target_ready,
  replications,
  seed,
  p_target,
  costs_complete,
  common_random_numbers = TRUE,
  progress = NULL
) {
  if (!is.data.frame(grid) || nrow(grid) == 0L) {
    stop("grid must contain at least one configuration", call. = FALSE)
  }
  evaluations = without_purrr_index(purrr::map(seq_len(nrow(grid)), function(configuration_index) {
    config = as.list(grid[configuration_index, , drop = FALSE])
    config$configuration_id = NULL
    evaluation = evaluate_configuration(
      config = config, params = params, horizon = horizon,
      target_ready = target_ready, replications = replications,
      # v1.4.1: números aleatórios comuns — todas as configurações usam o mesmo
      # bloco de sementes, reduzindo o ruído nas comparações entre configurações.
      seed = if (isTRUE(common_random_numbers)) {
        as.integer(seed)
      } else {
        as.integer(seed) + (configuration_index - 1L) * as.integer(replications)
      }
    )
    if (is.function(progress)) progress(configuration_index, nrow(grid))
    evaluation
  }))
  summaries = purrr::map(evaluations, "summary")
  all_replications = purrr::map(evaluations, function(evaluation) {
    replication_rows = evaluation$replications
    replication_rows$configuration_id = evaluation$summary$configuration_id[[1]]
    replication_rows
  })
  summary_data = bind_rows_base(summaries)
  row.names(summary_data) = NULL
  replication_data = bind_rows_base(all_replications)
  row.names(replication_data) = NULL
  selection = select_integrated_solution(summary_data, p_target, costs_complete)
  recommended = selection$recommended
  recommended_replications = NULL
  if (!is.null(recommended) && nrow(recommended) > 0L) {
    recommended_replications = replication_data[
      replication_data$configuration_id == recommended$configuration_id[[1]],
      ,
      drop = FALSE
    ]
    row.names(recommended_replications) = NULL
  }
  list(
    summary = summary_data,
    selection = selection,
    recommended = recommended,
    replications = replication_data,
    recommended_replications = recommended_replications,
    p_target = p_target,
    horizon = horizon,
    target_ready = target_ready,
    replications_per_configuration = replications,
    seed = seed,
    common_random_numbers = isTRUE(common_random_numbers)
  )
}

# v1.4.1 — número mínimo de replicações para que a meta seja demonstrável.
# Com 100% de sucessos, o limite inferior de Wilson é n / (n + z^2); logo a
# meta p só pode ser atingida se n >= p z^2 / (1 - p).
minimum_replications_for_target = function(p_target, conf_level = 0.95) {
  if (!is.numeric(p_target) || length(p_target) != 1L || !is.finite(p_target) ||
      p_target < 0 || p_target > 1) {
    stop("p_target must be a probability", call. = FALSE)
  }
  if (p_target >= 1) return(Inf)
  z2 = stats::qnorm(1 - (1 - conf_level) / 2)^2
  n = max(1L, as.integer(ceiling(p_target * z2 / (1 - p_target) - 1e-9)))
  while (n > 1L && wilson_interval(n - 1L, n - 1L, conf_level)[["lower"]] >= p_target) n = n - 1L
  while (wilson_interval(n, n, conf_level)[["lower"]] < p_target) n = n + 1L
  n
}

# v1.4.1 — comparação pareada cenário-base × alternativo com as mesmas sementes.
compare_scenarios_paired = function(
  config_base, params_base, config_alt, params_alt,
  horizon, target_ready, replications, seed
) {
  base = evaluate_configuration(config_base, params_base, horizon, target_ready, replications, seed)
  alt = evaluate_configuration(config_alt, params_alt, horizon, target_ready, replications, seed)
  paired_ci = function(differences) {
    n = length(differences)
    m = mean(differences)
    if (n < 2L) return(c(mean = m, lower = NA_real_, upper = NA_real_))
    half = stats::qt(0.975, df = n - 1L) * stats::sd(differences) / sqrt(n)
    c(mean = m, lower = m - half, upper = m + half)
  }
  success_diff = paired_ci(as.numeric(alt$replications$mission_success) -
                             as.numeric(base$replications$mission_success))
  availability_diff = paired_ci(alt$replications$avg_availability -
                                  base$replications$avg_availability)
  below_diff = paired_ci(alt$replications$hours_below_target -
                           base$replications$hours_below_target)
  comparison = data.frame(
    indicador = c("Probabilidade de sucesso", "Disponibilidade média", "Horas abaixo de M"),
    base = c(base$summary$success_probability, base$summary$mean_availability,
             base$summary$mean_hours_below_target),
    alternativo = c(alt$summary$success_probability, alt$summary$mean_availability,
                    alt$summary$mean_hours_below_target),
    diferenca = c(success_diff[["mean"]], availability_diff[["mean"]], below_diff[["mean"]]),
    ic95_inferior = c(success_diff[["lower"]], availability_diff[["lower"]], below_diff[["lower"]]),
    ic95_superior = c(success_diff[["upper"]], availability_diff[["upper"]], below_diff[["upper"]]),
    stringsAsFactors = FALSE
  )
  comparison$conclusao = ifelse(
    !is.finite(comparison$ic95_inferior), "IC não estimável",
    ifelse(comparison$ic95_inferior > 0, "Alternativo maior",
           ifelse(comparison$ic95_superior < 0, "Alternativo menor", "Sem diferença demonstrada"))
  )
  list(base = base, alternative = alt, comparison = comparison,
       replications = replications, seed = seed)
}

apply_scenario_value = function(config, params, parameter, value, duration_cv = 0.35, lead_cv = duration_cv) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
    stop("Valor de cenário inválido.", call. = FALSE)
  }
  integer_fields = c("fleet_size", "diagnostic_teams", "repair_teams", "bays",
                     "initial_stock", "reorder_point", "reorder_qty")
  if (parameter %in% integer_fields) {
    config[[parameter]] = as.integer(round(value))
  } else if (parameter == "pm_interval") {
    config$pm_interval = if (value > 0) value else Inf
  } else if (parameter == "lead_time_hours") {
    params$lead_time_model = mean_cv_lognormal_model(value, lead_cv)
  } else if (parameter == "mean_diagnosis") {
    params$diagnosis_model = mean_cv_lognormal_model(value, duration_cv)
  } else if (parameter == "mean_corrective") {
    params$corrective_model = mean_cv_lognormal_model(value, duration_cv)
  } else if (parameter == "mean_preventive") {
    params$preventive_model = mean_cv_lognormal_model(value, duration_cv)
  } else if (parameter == "mean_test") {
    params$test_model = mean_cv_lognormal_model(value, duration_cv)
  } else if (parameter == "part_required_probability") {
    params$part_required_probability = value
  } else {
    stop(paste("Parâmetro de cenário não reconhecido:", parameter), call. = FALSE)
  }
  list(config = config, params = params)
}

simulation_narrative = function(
  result,
  target_ready,
  horizon,
  p_target,
  evidence_mode
) {
  mode = toupper(as.character(evidence_mode))
  if (!mode %in% c("SYNTHETIC", "HYBRID", "EMPIRICAL")) {
    stop("evidence_mode must be SYNTHETIC, HYBRID or EMPIRICAL", call. = FALSE)
  }
  if (!is.null(result$ci_lower)) {
    approved = is.finite(result$ci_lower[[1]]) && result$ci_lower[[1]] >= p_target
    probability_text = paste0(
      "A probabilidade estimada de sucesso foi ",
      format(round(100 * result$success_probability[[1]], 1), decimal.mark = ","),
      "% (IC95% ",
      format(round(100 * result$ci_lower[[1]], 1), decimal.mark = ","), "–",
      format(round(100 * result$ci_upper[[1]], 1), decimal.mark = ","), "%)."
    )
  } else {
    approved = isTRUE(result$mission_success)
    probability_text = paste0(
      "Nesta replicação, o mínimo observado foi ", result$min_ready,
      " viaturas prontas."
    )
  }
  title = if (approved) "CONFIGURAÇÃO ATENDEU AO CRITÉRIO" else "CONFIGURAÇÃO NÃO ATENDEU AO CRITÉRIO"
  evidence_sentence = switch(
    mode,
    SYNTHETIC = paste(
      "Modo SYNTHETIC: o resultado é demonstrativo e não constitui evidência",
      "sobre o desempenho real da JLTV."
    ),
    HYBRID = paste(
      "Modo HYBRID: o resultado é uma projeção condicional que combina dados",
      "observados e hipóteses explícitas."
    ),
    EMPIRICAL = paste(
      "Modo EMPIRICAL: a interpretação permanece condicional ao período,",
      "à amostra, à qualidade dos registros e às hipóteses do modelo."
    )
  )
  text = paste0(
    "Para manter pelo menos ", target_ready, " viaturas durante ", horizon,
    " horas, com meta probabilística de ",
    format(round(100 * p_target, 1), decimal.mark = ","), "%, ",
    tolower(probability_text), " ", evidence_sentence,
    " O modelo mostra associação condicional, não prova causal."
  )
  list(status = if (approved) "APPROVED" else "NOT_APPROVED",
       title = title, text = text, evidence_mode = mode)
}

# ============================================================================
# 3. QUALIDADE DE DADOS, CMMS E PERSISTÊNCIA SQLITE
# ============================================================================

required_runtime_packages = function() {
  c(
    "shiny", "bslib", "DBI", "RSQLite", "DT", "ggplot2", "plotly",
    "jsonlite", "scales", "purrr", "googlesheets4", "readxl", "digest",
    "openxlsx", "shinyWidgets", "reactable", "data.table", "dplyr",
    "tidyr", "viridisLite", "patchwork", "svglite", "gganimate",
    "gifski", "survival", "readr", "shinycssloaders", "shinyvalidate",
    "pointblank", "ggdist", "ggrepel", "rsvg", "rsample", "binom",
    "thematic", "ggtext"
  )
}

resolve_database_path = function() {
  configured_path = trimws(Sys.getenv("JLTV_DB_PATH", unset = ""))
  if (nzchar(configured_path)) return(configured_path)
  file.path(getwd(), "jltv_cmms_ram.sqlite")
}

parse_datetime_utc = function(x) {
  values = as.character(x)
  values[is.na(x) | !nzchar(trimws(values))] = NA_character_
  suppressWarnings(as.POSIXct(values, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
}

empty_issues = function() {
  data.frame(
    severity = character(), table = character(), row = integer(),
    field = character(), code = character(), message = character(),
    stringsAsFactors = FALSE
  )
}

issue_row = function(severity, table, row, field, code, message) {
  data.frame(
    severity = severity, table = table, row = as.integer(row),
    field = field, code = code, message = message,
    stringsAsFactors = FALSE
  )
}

# v1.4.2 — um bloco por verificação, em vez de um data.frame por linha com
# problema; a ordem final das ocorrências é a mesma.
issue_rows_at = function(indices, severity, table, field, code, message) {
  if (length(indices) == 0L) return(list())
  list(issue_row(severity, table, indices, field, code, message))
}

finish_validation = function(issues) {
  if (length(issues) == 0L) {
    issue_data = empty_issues()
  } else {
    issue_data = do.call(rbind, issues)
    row.names(issue_data) = NULL
  }
  list(
    valid = !any(issue_data$severity == "ERROR"),
    issues = issue_data
  )
}

validate_assets_data = function(df) {
  if (!is.data.frame(df)) stop("assets must be a data frame", call. = FALSE)
  required = c(
    "asset_id", "asset_name", "model", "entry_service_date",
    "initial_meter", "current_meter", "meter_unit", "initial_state",
    "active", "data_source"
  )
  issues = list()
  missing = setdiff(required, names(df))
  if (length(missing) > 0L) {
    issues = c(issues, issue_rows_at(
      rep(0L, length(missing)), "ERROR", "assets", missing, "MISSING_COLUMN",
      paste("Coluna obrigatória ausente:", missing)
    ))
    return(finish_validation(issues))
  }
  ids = trimws(as.character(df$asset_id))
  blank_ids = which(is.na(df$asset_id) | !nzchar(ids))
  issues = c(issues, issue_rows_at(
    blank_ids, "ERROR", "assets", "asset_id", "MISSING_ID",
    "Identificador da viatura ausente."
  ))
  duplicate_ids = which(duplicated(ids) | duplicated(ids, fromLast = TRUE))
  issues = c(issues, issue_rows_at(
    duplicate_ids, "ERROR", "assets", "asset_id", "DUPLICATE_ID",
    "Identificador da viatura duplicado."
  ))
  initial = suppressWarnings(as.numeric(as.character(df$initial_meter)))
  current = suppressWarnings(as.numeric(as.character(df$current_meter)))
  bad_meter = which(!is.finite(initial) | !is.finite(current) |
                      initial < 0 | current < 0 | current < initial)
  issues = c(issues, issue_rows_at(
    bad_meter, "ERROR", "assets", "current_meter", "METER_REGRESSION",
    "Medidores devem ser não negativos e o atual não pode ser inferior ao inicial."
  ))
  bad_unit = which(!toupper(as.character(df$meter_unit)) %in% c("HOURS", "KM"))
  issues = c(issues, issue_rows_at(
    bad_unit, "ERROR", "assets", "meter_unit", "INVALID_UNIT",
    "Unidade deve ser HOURS ou KM."
  ))
  active_numeric = suppressWarnings(as.integer(as.character(df$active)))
  valid_active = !is.na(active_numeric) & active_numeric == 1L
  active_units = unique(toupper(trimws(as.character(df$meter_unit[valid_active]))))
  active_units = active_units[active_units %in% c("HOURS", "KM")]
  if (length(active_units) > 1L) {
    issues[[length(issues) + 1L]] = issue_row(
      "ERROR", "assets", 0L, "meter_unit", "MIXED_METER_UNITS",
      "A análise RAM não combina HOURS e KM. Separe a base por unidade ou forneça conversão de utilização explicitamente validada."
    )
  }
  bad_active = which(is.na(active_numeric) | !active_numeric %in% c(0L, 1L))
  issues = c(issues, issue_rows_at(
    bad_active, "ERROR", "assets", "active", "INVALID_FLAG",
    "Indicador active deve ser 0 ou 1."
  ))
  missing_source = which(is.na(df$data_source) | !nzchar(trimws(as.character(df$data_source))))
  issues = c(issues, issue_rows_at(
    missing_source, "ERROR", "assets", "data_source", "MISSING_SOURCE",
    "A proveniência do registro é obrigatória."
  ))
  finish_validation(issues)
}

validate_work_orders_data = function(df) {
  if (!is.data.frame(df)) stop("work_orders must be a data frame", call. = FALSE)
  required = c(
    "work_order_id", "asset_id", "maintenance_type", "is_functional_failure",
    "failure_mode", "failure_class", "detected_at", "diagnosis_start_at",
    "diagnosis_end_at", "active_repair_start_at", "active_repair_end_at",
    "test_start_at", "released_at", "meter_at_event", "status",
    "description", "data_source"
  )
  issues = list()
  missing = setdiff(required, names(df))
  if (length(missing) > 0L) {
    issues = c(issues, issue_rows_at(
      rep(0L, length(missing)), "ERROR", "work_orders", missing, "MISSING_COLUMN",
      paste("Coluna obrigatória ausente:", missing)
    ))
    return(finish_validation(issues))
  }
  ids = trimws(as.character(df$work_order_id))
  bad_id = which(is.na(df$work_order_id) | !nzchar(ids))
  duplicated_id = which(duplicated(ids) | duplicated(ids, fromLast = TRUE))
  issues = c(issues, issue_rows_at(
    bad_id, "ERROR", "work_orders", "work_order_id", "MISSING_ID",
    "Identificador de OS ausente."
  ))
  issues = c(issues, issue_rows_at(
    duplicated_id, "ERROR", "work_orders", "work_order_id", "DUPLICATE_ID",
    "Identificador de OS duplicado."
  ))
  missing_asset = which(is.na(df$asset_id) | !nzchar(trimws(as.character(df$asset_id))))
  issues = c(issues, issue_rows_at(
    missing_asset, "ERROR", "work_orders", "asset_id", "MISSING_ASSET",
    "A viatura vinculada à OS é obrigatória."
  ))
  allowed_types = c("CORRECTIVE", "PREVENTIVE", "CONDITION_BASED")
  bad_type = which(!toupper(as.character(df$maintenance_type)) %in% allowed_types)
  issues = c(issues, issue_rows_at(
    bad_type, "ERROR", "work_orders", "maintenance_type", "INVALID_TYPE",
    "Tipo deve ser CORRECTIVE, PREVENTIVE ou CONDITION_BASED."
  ))
  failure_flag = suppressWarnings(as.integer(as.character(df$is_functional_failure)))
  bad_flag = which(is.na(failure_flag) | !failure_flag %in% c(0L, 1L))
  issues = c(issues, issue_rows_at(
    bad_flag, "ERROR", "work_orders", "is_functional_failure", "INVALID_FLAG",
    "Indicador de falha funcional deve ser 0 ou 1."
  ))
  stage_columns = c(
    "detected_at", "diagnosis_start_at", "diagnosis_end_at",
    "active_repair_start_at", "active_repair_end_at", "test_start_at",
    "released_at"
  )
  for (row_index in seq_len(nrow(df))) {
    raw_values = as.character(df[row_index, stage_columns, drop = TRUE])
    parsed = parse_datetime_utc(raw_values)
    supplied = !is.na(raw_values) & nzchar(trimws(raw_values))
    invalid_format = which(supplied & is.na(parsed))
    for (column_index in invalid_format) {
      field = stage_columns[[column_index]]
      issues[[length(issues) + 1L]] = issue_row(
        "ERROR", "work_orders", row_index, field, "DATE_FORMAT",
        "Data/hora deve usar YYYY-MM-DD HH:MM:SS."
      )
    }
    ordered_values = as.numeric(parsed[!is.na(parsed)])
    if (length(ordered_values) > 1L && any(diff(ordered_values) < 0)) {
      issues[[length(issues) + 1L]] = issue_row(
        "ERROR", "work_orders", row_index, "released_at", "TIME_ORDER",
        "As etapas da OS estão fora da ordem cronológica."
      )
    }
    completed = toupper(trimws(as.character(df$status[[row_index]]))) == "COMPLETED"
    missing_stages = which(is.na(raw_values) | !nzchar(trimws(raw_values)))
    if (completed && length(missing_stages) > 0L) {
      issues[[length(issues) + 1L]] = issue_row(
        "ERROR", "work_orders", row_index,
        paste(stage_columns[missing_stages], collapse = ","),
        "MISSING_STAGE",
        "Uma OS concluída precisa de todas as etapas temporais para decompor o downtime."
      )
    }
  }
  meter = suppressWarnings(as.numeric(as.character(df$meter_at_event)))
  bad_meter = which(!is.na(df$meter_at_event) & (!is.finite(meter) | meter < 0))
  issues = c(issues, issue_rows_at(
    bad_meter, "ERROR", "work_orders", "meter_at_event", "INVALID_METER",
    "Medidor do evento deve ser não negativo."
  ))
  corrective_failure = toupper(as.character(df$maintenance_type)) == "CORRECTIVE" &
    failure_flag == 1L
  missing_failure_meter = which(corrective_failure & !is.finite(meter))
  issues = c(issues, issue_rows_at(
    missing_failure_meter, "ERROR", "work_orders", "meter_at_event", "MISSING_FAILURE_METER",
    "Falha funcional corretiva requer medidor no evento para análise de vida."
  ))
  missing_source = which(is.na(df$data_source) | !nzchar(trimws(as.character(df$data_source))))
  issues = c(issues, issue_rows_at(
    missing_source, "ERROR", "work_orders", "data_source", "MISSING_SOURCE",
    "A proveniência do registro é obrigatória."
  ))
  finish_validation(issues)
}

validate_delays_data = function(df) {
  if (!is.data.frame(df)) stop("delays must be a data frame", call. = FALSE)
  required = c(
    "work_order_id", "delay_type", "start_at", "end_at",
    "responsibility_class", "notes"
  )
  issues = list()
  missing = setdiff(required, names(df))
  if (length(missing) > 0L) {
    issues = c(issues, issue_rows_at(
      rep(0L, length(missing)), "ERROR", "work_order_delays", missing, "MISSING_COLUMN",
      paste("Coluna obrigatória ausente:", missing)
    ))
    return(finish_validation(issues))
  }
  for (row_index in seq_len(nrow(df))) {
    start = parse_datetime_utc(df$start_at[[row_index]])
    end = parse_datetime_utc(df$end_at[[row_index]])
    if (is.na(start) || is.na(end)) {
      issues[[length(issues) + 1L]] = issue_row(
        "ERROR", "work_order_delays", row_index, "start_at", "DATE_FORMAT",
        "Início e fim do atraso devem usar YYYY-MM-DD HH:MM:SS."
      )
    } else if (as.numeric(end) < as.numeric(start)) {
      issues[[length(issues) + 1L]] = issue_row(
        "ERROR", "work_order_delays", row_index, "end_at", "TIME_ORDER",
        "O fim do atraso não pode anteceder o início."
      )
    }
  }
  finish_validation(issues)
}

data_quality_report = function(snapshot) {
  if (!is.list(snapshot)) stop("snapshot must be a named list", call. = FALSE)
  issues = list()
  if (!"assets" %in% names(snapshot)) {
    issues[[length(issues) + 1L]] = issue_row(
      "ERROR", "assets", 0L, "", "MISSING_TABLE", "Tabela assets ausente."
    )
  } else {
    asset_validation = validate_assets_data(snapshot$assets)
    if (nrow(asset_validation$issues) > 0L) {
      issues[[length(issues) + 1L]] = asset_validation$issues
    }
  }
  if (!"work_orders" %in% names(snapshot)) {
    issues[[length(issues) + 1L]] = issue_row(
      "ERROR", "work_orders", 0L, "", "MISSING_TABLE", "Tabela work_orders ausente."
    )
  } else {
    order_validation = validate_work_orders_data(snapshot$work_orders)
    if (nrow(order_validation$issues) > 0L) {
      issues[[length(issues) + 1L]] = order_validation$issues
    }
  }
  if (!"work_order_delays" %in% names(snapshot)) {
    issues[[length(issues) + 1L]] = issue_row(
      "ERROR", "work_order_delays", 0L, "", "MISSING_TABLE",
      "Tabela work_order_delays ausente."
    )
  } else {
    delay_validation = validate_delays_data(snapshot$work_order_delays)
    if (nrow(delay_validation$issues) > 0L) {
      issues[[length(issues) + 1L]] = delay_validation$issues
    }
  }
  if (all(c("assets", "work_orders") %in% names(snapshot)) &&
      all(c("asset_id") %in% names(snapshot$assets)) &&
      all(c("asset_id") %in% names(snapshot$work_orders))) {
    unknown_assets = which(
      !as.character(snapshot$work_orders$asset_id) %in%
        as.character(snapshot$assets$asset_id)
    )
    issues = c(issues, issue_rows_at(
      unknown_assets, "ERROR", "work_orders", "asset_id", "UNKNOWN_ASSET",
      "A OS referencia uma viatura inexistente."
    ))
  }
  if (all(c("work_orders", "work_order_delays") %in% names(snapshot)) &&
      "work_order_id" %in% names(snapshot$work_orders) &&
      "work_order_id" %in% names(snapshot$work_order_delays)) {
    unknown_orders = which(
      !as.character(snapshot$work_order_delays$work_order_id) %in%
        as.character(snapshot$work_orders$work_order_id)
    )
    issues = c(issues, issue_rows_at(
      unknown_orders, "ERROR", "work_order_delays", "work_order_id", "UNKNOWN_ORDER",
      "O atraso referencia uma OS inexistente."
    ))
  }
  # v1.4.1 — inconsistências que antes só apareciam (como erro interno) na
  # construção dos dados de vida passam a ser apontadas aqui, por linha.
  if (all(c("assets", "work_orders") %in% names(snapshot)) &&
      is.data.frame(snapshot$assets) && is.data.frame(snapshot$work_orders) &&
      nrow(snapshot$work_orders) > 0L &&
      all(c("asset_id", "initial_meter", "current_meter") %in% names(snapshot$assets)) &&
      all(c("asset_id", "maintenance_type", "is_functional_failure", "meter_at_event") %in%
            names(snapshot$work_orders))) {
    assets = snapshot$assets
    orders = snapshot$work_orders
    active_ids = as.character(assets$asset_id[asset_active_mask(assets)])
    position = match(as.character(orders$asset_id), as.character(assets$asset_id))
    initial = suppressWarnings(as.numeric(as.character(assets$initial_meter)))[position]
    current = suppressWarnings(as.numeric(as.character(assets$current_meter)))[position]
    meter = suppressWarnings(as.numeric(as.character(orders$meter_at_event)))
    in_scope = as.character(orders$asset_id) %in% active_ids
    failure = functional_failure_flags(orders$is_functional_failure) &
      is_corrective_type(orders$maintenance_type) & in_scope
    outside = which(failure & is.finite(meter) & is.finite(initial) & is.finite(current) &
                      (meter <= initial | meter > current))
    issues = c(issues, issue_rows_at(
      outside, "ERROR", "work_orders", "meter_at_event", "FAILURE_METER_OUTSIDE_EXPOSURE",
      paste0(
        "Medidor da falha (", meter[outside], ") fora da exposição da viatura ",
        orders$asset_id[outside], " (", initial[outside], " < medidor <= ", current[outside], ")."
      )
    ))
    key = paste(orders$asset_id, meter, sep = "@")
    duplicated_failure = which(failure & is.finite(meter) & duplicated(key) &
                                 !seq_along(key) %in% outside)
    issues = c(issues, issue_rows_at(
      duplicated_failure, "WARNING", "work_orders", "meter_at_event", "SIMULTANEOUS_FAILURES",
      "Falha funcional no mesmo medidor de outra falha da mesma viatura; na análise de vida, ambas formam um único evento de indisponibilidade."
    ))
    preventive_outside = which(is_preventive_type(orders$maintenance_type) & in_scope &
                                 is.finite(meter) & is.finite(initial) & is.finite(current) &
                                 (meter <= initial | meter > current))
    issues = c(issues, issue_rows_at(
      preventive_outside, "WARNING", "work_orders", "meter_at_event", "PREVENTIVE_METER_OUTSIDE_EXPOSURE",
      "Preventiva com medidor fora da exposição da viatura; ela não será usada como ponto de censura na análise de vida."
    ))
    previous_errors = any(purrr::map_lgl(issues, function(x) any(x$severity == "ERROR")))
    if (!previous_errors) {
      life_check = tryCatch(
        build_life_data(assets, orders),
        error = function(error_condition) conditionMessage(error_condition)
      )
      if (is.character(life_check)) {
        issues[[length(issues) + 1L]] = issue_row(
          "ERROR", "work_orders", 0L, "meter_at_event", "LIFE_DATA",
          paste("Dados de vida inconsistentes:", life_check)
        )
      }
    }
  }
  if (length(issues) == 0L) issue_data = empty_issues() else issue_data = do.call(rbind, issues)
  row.names(issue_data) = NULL
  summary = data.frame(
    severity = c("ERROR", "WARNING", "INFO"),
    count = c(
      sum(issue_data$severity == "ERROR"),
      sum(issue_data$severity == "WARNING"),
      sum(issue_data$severity == "INFO")
    ),
    stringsAsFactors = FALSE
  )
  list(
    issues = issue_data,
    summary = summary,
    analysis_ready = !any(issue_data$severity == "ERROR")
  )
}

# População científica comum para RAM, calibração e decomposição.
# Mantém as tabelas originais no banco; só seleciona OS de viaturas ativas
# e atrasos ligados a essas OS. A qualidade é verificada antes do filtro.
active_work_order_population = function(assets, work_orders, delays) {
  active_ids = as.character(assets$asset_id[asset_active_mask(assets)])
  orders = work_orders[as.character(work_orders$asset_id) %in% active_ids, , drop = FALSE]
  selected_delays = delays[
    as.character(delays$work_order_id) %in% as.character(orders$work_order_id), , drop = FALSE
  ]
  list(work_orders = orders, delays = selected_delays)
}

derive_analysis_inputs = function(
  snapshot,
  fit_models = TRUE,
  model_fitter = fit_life_models
) {
  quality = data_quality_report(snapshot)
  if (!quality$analysis_ready) {
    stop("database snapshot contains blocking quality errors", call. = FALSE)
  }
  active_population = active_work_order_population(
    snapshot$assets, snapshot$work_orders, snapshot$work_order_delays
  )
  snapshot$work_orders = active_population$work_orders
  snapshot$work_order_delays = active_population$delays
  evidence_mode = "SYNTHETIC"
  if ("meta" %in% names(snapshot) && all(c("key", "value") %in% names(snapshot$meta))) {
    mode_rows = which(snapshot$meta$key == "evidence_mode")
    if (length(mode_rows) > 0L) evidence_mode = snapshot$meta$value[[mode_rows[[1]]]]
  }
  life_data = build_life_data(snapshot$assets, snapshot$work_orders)
  delays = snapshot$work_order_delays
  if (nrow(delays) == 0L) {
    delays = data.frame(
      work_order_id = character(), start_at = character(), end_at = character(),
      responsibility_class = character(), stringsAsFactors = FALSE
    )
  }
  ram = calculate_ram_kpis(snapshot$assets, snapshot$work_orders, delays, snapshot$exposure_log)
  trend = laplace_trend_test(snapshot$assets, snapshot$work_orders)
  positive_life = life_data$duration > 0
  fitted = if (!isTRUE(fit_models)) {
    list(
      status = "DEFERRED", estimable = FALSE,
      reason = "Ajuste dos modelos adiado até a abertura da análise RAM.",
      failure_count = sum(life_data$status[positive_life]),
      observation_count = sum(positive_life), models = data.frame(),
      selected_model = NA_character_, parameters = numeric(),
      all_parameters = list()
    )
  } else if (sum(life_data$status[positive_life]) >= 2L) {
    model_fitter(
      life_data$duration[positive_life],
      life_data$status[positive_life]
    )
  } else {
    list(
      status = "NON_ESTIMABLE", estimable = FALSE,
      reason = "São necessárias ao menos duas falhas observadas.",
      failure_count = sum(life_data$status[positive_life]),
      observation_count = sum(positive_life), models = data.frame(),
      selected_model = NA_character_, parameters = numeric(),
      all_parameters = list()
    )
  }
  list(
    evidence_mode = toupper(as.character(evidence_mode)),
    quality = quality,
    assets = snapshot$assets,
    work_orders = snapshot$work_orders,
    delays = delays,
    life_data = life_data,
    ram = ram,
    trend = trend,
    fitted_models = fitted
  )
}

open_database = function(path = resolve_database_path()) {
  connection = DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(connection, "PRAGMA foreign_keys = ON")
  # v1.4.1 — espera até 10 s por bloqueios transitórios (ex.: sincronização do
  # Google Drive lendo o arquivo) em vez de falhar imediatamente.
  try(DBI::dbExecute(connection, "PRAGMA busy_timeout = 10000"), silent = TRUE)
  try(DBI::dbExecute(connection, "PRAGMA journal_mode = WAL"), silent = TRUE)
  connection
}

initialize_database = function(path = resolve_database_path()) {
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  statements = c(
    "CREATE TABLE IF NOT EXISTS meta(key TEXT PRIMARY KEY, value TEXT NOT NULL)",
    paste0(
      "CREATE TABLE IF NOT EXISTS assets(",
      "asset_id TEXT PRIMARY KEY, asset_name TEXT NOT NULL, model TEXT NOT NULL, ",
      "entry_service_date TEXT, initial_meter REAL NOT NULL, current_meter REAL NOT NULL, ",
      "meter_unit TEXT NOT NULL CHECK(meter_unit IN ('HOURS','KM')), ",
      "initial_state TEXT NOT NULL, active INTEGER NOT NULL CHECK(active IN (0,1)), ",
      "data_source TEXT NOT NULL)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS exposure_log(",
      "exposure_id INTEGER PRIMARY KEY AUTOINCREMENT, asset_id TEXT NOT NULL ",
      "REFERENCES assets(asset_id), start_at TEXT, end_at TEXT, meter_start REAL, ",
      "meter_end REAL, operating_exposure REAL NOT NULL CHECK(operating_exposure >= 0), ",
      "mission_id TEXT, data_source TEXT NOT NULL)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS work_orders(",
      "work_order_id TEXT PRIMARY KEY, asset_id TEXT NOT NULL REFERENCES assets(asset_id), ",
      "maintenance_type TEXT NOT NULL CHECK(maintenance_type IN ",
      "('CORRECTIVE','PREVENTIVE','CONDITION_BASED')), ",
      "is_functional_failure INTEGER NOT NULL CHECK(is_functional_failure IN (0,1)), ",
      "failure_mode TEXT, failure_class TEXT, detected_at TEXT NOT NULL, ",
      "diagnosis_start_at TEXT, diagnosis_end_at TEXT, active_repair_start_at TEXT, ",
      "active_repair_end_at TEXT, test_start_at TEXT, released_at TEXT, ",
      "meter_at_event REAL, status TEXT NOT NULL, description TEXT, data_source TEXT NOT NULL)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS work_order_delays(",
      "delay_id INTEGER PRIMARY KEY AUTOINCREMENT, work_order_id TEXT NOT NULL ",
      "REFERENCES work_orders(work_order_id) ON DELETE CASCADE, delay_type TEXT NOT NULL, ",
      "start_at TEXT NOT NULL, end_at TEXT NOT NULL, responsibility_class TEXT NOT NULL, ",
      "notes TEXT)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS resources(",
      "resource_id TEXT PRIMARY KEY, resource_type TEXT NOT NULL, quantity INTEGER NOT NULL ",
      "CHECK(quantity >= 0), capacity INTEGER NOT NULL CHECK(capacity >= 0), ",
      "cost_horizon REAL, data_source TEXT NOT NULL)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS parts(",
      "part_id TEXT PRIMARY KEY, part_name TEXT NOT NULL, criticality TEXT, ",
      "initial_stock INTEGER NOT NULL CHECK(initial_stock >= 0), ",
      "reorder_point INTEGER NOT NULL CHECK(reorder_point >= 0), ",
      "reorder_qty INTEGER NOT NULL CHECK(reorder_qty >= 0), ",
      "lead_time_hours REAL NOT NULL CHECK(lead_time_hours >= 0), ",
      "unit_cost REAL, data_source TEXT NOT NULL)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS inventory_transactions(",
      "transaction_id INTEGER PRIMARY KEY AUTOINCREMENT, part_id TEXT NOT NULL ",
      "REFERENCES parts(part_id), work_order_id TEXT REFERENCES work_orders(work_order_id), ",
      "occurred_at TEXT NOT NULL, quantity INTEGER NOT NULL, transaction_type TEXT NOT NULL, ",
      "data_source TEXT NOT NULL)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS maintenance_plans(",
      "plan_id TEXT PRIMARY KEY, plan_name TEXT NOT NULL, trigger_unit TEXT NOT NULL ",
      "CHECK(trigger_unit IN ('HOURS','KM','DAYS')), interval_value REAL NOT NULL ",
      "CHECK(interval_value > 0), active_duration_hours REAL NOT NULL ",
      "CHECK(active_duration_hours >= 0), active INTEGER NOT NULL CHECK(active IN (0,1)), ",
      "data_source TEXT NOT NULL)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS questionnaire_respondents(",
      "respondent_id TEXT PRIMARY KEY, age REAL, service_years REAL, academic_background TEXT, ",
      "military_background TEXT, direct_jltv_experience INTEGER, experience_area TEXT, ",
      "data_source TEXT NOT NULL)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS ahp_pairwise(",
      "comparison_id INTEGER PRIMARY KEY AUTOINCREMENT, respondent_id TEXT NOT NULL, ",
      "hierarchy_group TEXT NOT NULL, left_item TEXT NOT NULL, right_item TEXT NOT NULL, ",
      "selected_side TEXT NOT NULL, intensity INTEGER NOT NULL, data_source TEXT NOT NULL)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS topsis_scores(",
      "score_id INTEGER PRIMARY KEY AUTOINCREMENT, respondent_id TEXT NOT NULL, ",
      "alternative_id TEXT NOT NULL, alternative_name TEXT NOT NULL, ",
      "subcriterion_id TEXT NOT NULL, score REAL, data_source TEXT NOT NULL)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS questionnaire_factors(",
      "factor_id INTEGER PRIMARY KEY AUTOINCREMENT, respondent_id TEXT NOT NULL, ",
      "factor_type TEXT NOT NULL, identification TEXT NOT NULL, occurrence_detail TEXT, ",
      "related_system TEXT, influence_score REAL, evidence_reference TEXT, data_source TEXT NOT NULL)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS decision_scenario_links(",
      "link_id TEXT PRIMARY KEY, created_at TEXT NOT NULL, decision_source TEXT NOT NULL, ",
      "target_id TEXT NOT NULL, target_label TEXT NOT NULL, simulation_parameter TEXT NOT NULL, ",
      "baseline_value REAL NOT NULL, alternative_value REAL NOT NULL, evidence_source TEXT NOT NULL, ",
      "multicriteria_run_id TEXT, notes TEXT, active INTEGER NOT NULL CHECK(active IN (0,1)))"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS multicriteria_runs(",
      "mc_run_id TEXT PRIMARY KEY, created_at TEXT NOT NULL, model_version TEXT NOT NULL, ",
      "score_aggregation TEXT NOT NULL, only_consistent INTEGER NOT NULL CHECK(only_consistent IN (0,1)), ",
      "evidence_mode TEXT NOT NULL DEFAULT 'HYBRID', status TEXT NOT NULL, notes TEXT)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS ahp_weight_results(",
      "result_id INTEGER PRIMARY KEY AUTOINCREMENT, mc_run_id TEXT NOT NULL REFERENCES multicriteria_runs(mc_run_id), ",
      "hierarchy_group TEXT NOT NULL, parent_criterion TEXT NOT NULL, subcriterion_id TEXT NOT NULL, ",
      "local_weight REAL NOT NULL, criterion_weight REAL NOT NULL, global_weight REAL NOT NULL)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS ahp_consistency_results(",
      "result_id INTEGER PRIMARY KEY AUTOINCREMENT, mc_run_id TEXT NOT NULL REFERENCES multicriteria_runs(mc_run_id), ",
      "hierarchy_group TEXT NOT NULL, estimable INTEGER NOT NULL, respondents INTEGER NOT NULL, ",
      "CR REAL, status TEXT NOT NULL)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS ahp_individual_results(",
      "result_id INTEGER PRIMARY KEY AUTOINCREMENT, mc_run_id TEXT NOT NULL REFERENCES multicriteria_runs(mc_run_id), ",
      "respondent_id TEXT NOT NULL, hierarchy_group TEXT NOT NULL, complete INTEGER NOT NULL, ",
      "pair_count INTEGER NOT NULL, expected_pairs INTEGER NOT NULL, CR REAL, consistent INTEGER NOT NULL)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS topsis_results(",
      "result_id INTEGER PRIMARY KEY AUTOINCREMENT, mc_run_id TEXT NOT NULL REFERENCES multicriteria_runs(mc_run_id), ",
      "alternative_id TEXT NOT NULL, alternative_name TEXT NOT NULL, d_plus REAL NOT NULL, d_minus REAL NOT NULL, ",
      "coefficient REAL NOT NULL, rank INTEGER NOT NULL)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS cmms_change_log(",
      "change_id INTEGER PRIMARY KEY AUTOINCREMENT, changed_at TEXT NOT NULL, ",
      "table_name TEXT NOT NULL, record_id TEXT NOT NULL, action TEXT NOT NULL, ",
      "reason TEXT NOT NULL, previous_json TEXT)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS simulation_scenarios(",
      "scenario_id TEXT PRIMARY KEY, scenario_name TEXT NOT NULL, evidence_mode TEXT NOT NULL, ",
      "parameters_json TEXT NOT NULL, created_at TEXT NOT NULL)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS simulation_runs(",
      "run_id TEXT PRIMARY KEY, scenario_id TEXT REFERENCES simulation_scenarios(scenario_id), ",
      "seed INTEGER NOT NULL, replications INTEGER NOT NULL, model_version TEXT NOT NULL, ",
      "started_at TEXT NOT NULL, completed_at TEXT, status TEXT NOT NULL)"
    ),
    paste0(
      "CREATE TABLE IF NOT EXISTS simulation_results(",
      "result_id INTEGER PRIMARY KEY AUTOINCREMENT, run_id TEXT NOT NULL ",
      "REFERENCES simulation_runs(run_id), configuration_json TEXT NOT NULL, ",
      "metrics_json TEXT NOT NULL)"
    ),
    "CREATE INDEX IF NOT EXISTS idx_work_orders_asset_meter ON work_orders(asset_id, meter_at_event)",
    "CREATE INDEX IF NOT EXISTS idx_work_orders_detected ON work_orders(detected_at)",
    "CREATE INDEX IF NOT EXISTS idx_delays_order ON work_order_delays(work_order_id)",
    "CREATE INDEX IF NOT EXISTS idx_exposure_asset ON exposure_log(asset_id)",
    "CREATE INDEX IF NOT EXISTS idx_ahp_respondent_group ON ahp_pairwise(respondent_id, hierarchy_group)",
    "CREATE INDEX IF NOT EXISTS idx_topsis_alt_criterion ON topsis_scores(alternative_id, subcriterion_id)",
    "CREATE INDEX IF NOT EXISTS idx_factor_respondent ON questionnaire_factors(respondent_id)",
    "CREATE INDEX IF NOT EXISTS idx_mc_runs_created ON multicriteria_runs(created_at)",
    "CREATE INDEX IF NOT EXISTS idx_topsis_results_run ON topsis_results(mc_run_id, rank)"
  )
  DBI::dbWithTransaction(connection, {
    for (statement in statements) DBI::dbExecute(connection, statement)
    link_columns = DBI::dbGetQuery(connection, "PRAGMA table_info(decision_scenario_links)")$name
    if (!"multicriteria_run_id" %in% link_columns) {
      DBI::dbExecute(connection, "ALTER TABLE decision_scenario_links ADD COLUMN multicriteria_run_id TEXT")
    }
    factor_columns = DBI::dbGetQuery(connection, "PRAGMA table_info(questionnaire_factors)")$name
    if (!"occurrence_detail" %in% factor_columns) {
      DBI::dbExecute(connection, "ALTER TABLE questionnaire_factors ADD COLUMN occurrence_detail TEXT")
    }
    mc_columns = DBI::dbGetQuery(connection, "PRAGMA table_info(multicriteria_runs)")$name
    if (!"evidence_mode" %in% mc_columns) {
      DBI::dbExecute(
        connection,
        "ALTER TABLE multicriteria_runs ADD COLUMN evidence_mode TEXT NOT NULL DEFAULT 'HYBRID'"
      )
    }
    DBI::dbExecute(
      connection,
      "INSERT OR REPLACE INTO meta(key, value) VALUES(?, ?)",
      params = list("schema_version", JLTV_SCHEMA_VERSION)
    )
    DBI::dbExecute(
      connection,
      "INSERT OR REPLACE INTO meta(key, value) VALUES(?, ?)",
      params = list("model_version", JLTV_MODEL_VERSION)
    )
  })
  invisible(path)
}

synthetic_demo_data = function(seed = 20260910) {
  set.seed(seed)
  asset_ids = sprintf("JLTV-DEMO-%02d", 1:12)
  assets = data.frame(
    asset_id = asset_ids,
    asset_name = paste("Viatura demonstrativa", sprintf("%02d", 1:12)),
    model = "JLTV-DEMO",
    entry_service_date = "2025-01-01",
    initial_meter = 0,
    current_meter = 680 + 1:12 * 8,
    meter_unit = "HOURS",
    initial_state = "OPERATING",
    active = 1L,
    data_source = "SYNTHETIC_DEMO",
    stringsAsFactors = FALSE
  )
  orders = list()
  delays = list()
  order_index = 1L
  delay_index = 1L
  origin = as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  for (asset_index in seq_along(asset_ids)) {
    event_meters = c(120, 290, 470) + asset_index * 3
    for (failure_index in seq_along(event_meters)) {
      detected = origin + (asset_index * 22 + failure_index * 55) * 24 * 3600
      diagnosis_start = detected + 1 * 3600
      diagnosis_end = diagnosis_start + (1 + (asset_index + failure_index) %% 3) * 3600
      repair_start = diagnosis_end + (4 + (asset_index * failure_index) %% 10) * 3600
      repair_end = repair_start + (3 + (asset_index + failure_index) %% 5) * 3600
      test_start = repair_end
      released = test_start + 1 * 3600
      order_id = sprintf("DEMO-C-%02d-%02d", asset_index, failure_index)
      orders[[order_index]] = data.frame(
        work_order_id = order_id, asset_id = asset_ids[[asset_index]],
        maintenance_type = "CORRECTIVE", is_functional_failure = 1L,
        failure_mode = "FALHA_FUNCIONAL_GENERICA",
        failure_class = "DEMONSTRACAO",
        detected_at = format(detected, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        diagnosis_start_at = format(diagnosis_start, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        diagnosis_end_at = format(diagnosis_end, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        active_repair_start_at = format(repair_start, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        active_repair_end_at = format(repair_end, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        test_start_at = format(test_start, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        released_at = format(released, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        meter_at_event = event_meters[[failure_index]], status = "COMPLETED",
        description = "Ocorrência sintética para teste do fluxo CMMS.",
        data_source = "SYNTHETIC_DEMO", stringsAsFactors = FALSE
      )
      delays[[delay_index]] = data.frame(
        work_order_id = order_id, delay_type = "PART",
        start_at = format(diagnosis_end, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        end_at = format(repair_start, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        responsibility_class = "LOGISTIC",
        notes = "Atraso sintético de suprimento.", stringsAsFactors = FALSE
      )
      order_index = order_index + 1L
      delay_index = delay_index + 1L
    }
    detected = origin + (asset_index * 22 + 200) * 24 * 3600
    diagnosis_start = detected
    diagnosis_end = detected
    repair_start = detected + 2 * 3600
    repair_end = repair_start + 4 * 3600
    test_start = repair_end
    released = test_start + 1 * 3600
    orders[[order_index]] = data.frame(
      work_order_id = sprintf("DEMO-P-%02d", asset_index),
      asset_id = asset_ids[[asset_index]], maintenance_type = "PREVENTIVE",
      is_functional_failure = 0L, failure_mode = NA_character_,
      failure_class = "DEMONSTRACAO",
      detected_at = format(detected, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      diagnosis_start_at = format(diagnosis_start, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      diagnosis_end_at = format(diagnosis_end, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      active_repair_start_at = format(repair_start, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      active_repair_end_at = format(repair_end, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      test_start_at = format(test_start, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      released_at = format(released, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      meter_at_event = 560 + asset_index * 3, status = "COMPLETED",
      description = "Intervenção preventiva sintética de demonstração.",
      data_source = "SYNTHETIC_DEMO", stringsAsFactors = FALSE
    )
    delays[[delay_index]] = data.frame(
      work_order_id = sprintf("DEMO-P-%02d", asset_index), delay_type = "SCHEDULE",
      start_at = format(diagnosis_end, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      end_at = format(repair_start, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      responsibility_class = "ADMINISTRATIVE",
      notes = "Espera sintética de programação.", stringsAsFactors = FALSE
    )
    order_index = order_index + 1L
    delay_index = delay_index + 1L
  }
  exposure_log = data.frame(
    asset_id = asset_ids, start_at = "2025-01-01 00:00:00",
    end_at = "2026-01-01 00:00:00", meter_start = 0,
    meter_end = assets$current_meter,
    operating_exposure = assets$current_meter,
    mission_id = "DEMO-PERIODO-01", data_source = "SYNTHETIC_DEMO",
    stringsAsFactors = FALSE
  )
  resources = data.frame(
    resource_id = c("DEMO-DIAG", "DEMO-REPAIR", "DEMO-BAY"),
    resource_type = c("DIAGNOSTIC_TEAM", "REPAIR_TEAM", "BAY"),
    quantity = c(2L, 3L, 2L), capacity = c(2L, 3L, 2L),
    cost_horizon = c(NA_real_, NA_real_, NA_real_),
    data_source = "SYNTHETIC_DEMO", stringsAsFactors = FALSE
  )
  parts = data.frame(
    part_id = "DEMO-PART-CRITICAL", part_name = "Sobressalente crítico genérico",
    criticality = "HIGH", initial_stock = 6L, reorder_point = 2L,
    reorder_qty = 6L, lead_time_hours = 24, unit_cost = NA_real_,
    data_source = "SYNTHETIC_DEMO", stringsAsFactors = FALSE
  )
  plans = data.frame(
    plan_id = "DEMO-PM-HOURS", plan_name = "Plano preventivo sintético",
    trigger_unit = "HOURS", interval_value = 250,
    active_duration_hours = 4, active = 1L,
    data_source = "SYNTHETIC_DEMO", stringsAsFactors = FALSE
  )
  list(
    assets = assets, work_orders = do.call(rbind, orders),
    work_order_delays = do.call(rbind, delays), exposure_log = exposure_log,
    resources = resources, parts = parts, maintenance_plans = plans
  )
}

seed_synthetic_database = function(
  path = resolve_database_path(),
  force = FALSE,
  seed = 20260910
) {
  initialize_database(path)
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  asset_count = DBI::dbGetQuery(connection, "SELECT COUNT(*) AS n FROM assets")$n[[1]]
  if (asset_count > 0L && !isTRUE(force)) return(invisible(FALSE))
  demo = synthetic_demo_data(seed)
  DBI::dbWithTransaction(connection, {
    if (isTRUE(force)) {
      for (table in c(
        "cmms_change_log", "simulation_results", "simulation_runs", "simulation_scenarios",
        "topsis_results", "ahp_individual_results", "ahp_consistency_results",
        "ahp_weight_results", "multicriteria_runs", "decision_scenario_links",
        "questionnaire_factors", "topsis_scores", "ahp_pairwise", "questionnaire_respondents",
        "inventory_transactions", "work_order_delays", "work_orders",
        "exposure_log", "maintenance_plans", "parts", "resources", "assets"
      )) {
        DBI::dbExecute(connection, paste("DELETE FROM", table))
      }
    }
    DBI::dbAppendTable(connection, "assets", demo$assets)
    DBI::dbAppendTable(connection, "exposure_log", demo$exposure_log)
    DBI::dbAppendTable(connection, "work_orders", demo$work_orders)
    DBI::dbAppendTable(connection, "work_order_delays", demo$work_order_delays)
    DBI::dbAppendTable(connection, "resources", demo$resources)
    DBI::dbAppendTable(connection, "parts", demo$parts)
    DBI::dbAppendTable(connection, "maintenance_plans", demo$maintenance_plans)
    metadata = list(
      evidence_mode = "SYNTHETIC",
      model_version = JLTV_MODEL_VERSION,
      warning = paste(
        "DADOS SINTÉTICOS — NÃO UTILIZAR COMO EVIDÊNCIA SOBRE A JLTV;",
        "substitua e valide os registros antes de interpretar resultados reais."
      )
    )
    for (key in names(metadata)) {
      DBI::dbExecute(
        connection,
        "INSERT OR REPLACE INTO meta(key, value) VALUES(?, ?)",
        params = list(key, metadata[[key]])
      )
    }
  })
  invisible(TRUE)
}

allowed_database_tables = function() {
  c(
    "meta", "assets", "exposure_log", "work_orders", "work_order_delays",
    "resources", "parts", "inventory_transactions", "maintenance_plans",
    "questionnaire_respondents", "ahp_pairwise", "topsis_scores",
    "questionnaire_factors", "decision_scenario_links",
    "multicriteria_runs", "ahp_weight_results", "ahp_consistency_results",
    "ahp_individual_results", "topsis_results",
    "simulation_scenarios", "simulation_runs", "simulation_results",
    "cmms_change_log"
  )
}

# v1.4.1 — correção de registros com trilha de auditoria.
toggle_asset_active = function(path, asset_id, reason) {
  reason = trimws(as.character(reason %||% ""))
  if (!nzchar(reason)) stop("Informe o motivo da alteração.", call. = FALSE)
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  current = DBI::dbGetQuery(connection, "SELECT * FROM assets WHERE asset_id = ?", params = list(asset_id))
  if (nrow(current) != 1L) stop("Viatura não encontrada.", call. = FALSE)
  new_value = if (identical(as.integer(current$active[[1]]), 1L)) 0L else 1L
  DBI::dbWithTransaction(connection, {
    DBI::dbExecute(connection, "UPDATE assets SET active = ? WHERE asset_id = ?",
                   params = list(new_value, asset_id))
    DBI::dbExecute(
      connection,
      "INSERT INTO cmms_change_log(changed_at, table_name, record_id, action, reason, previous_json) VALUES(?, ?, ?, ?, ?, ?)",
      params = list(format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"), "assets", asset_id,
                    if (new_value == 1L) "REACTIVATE" else "DEACTIVATE", reason,
                    as.character(jsonlite::toJSON(current, dataframe = "rows", na = "null")))
    )
  })
  invisible(new_value)
}

delete_work_order = function(path, work_order_id, reason) {
  reason = trimws(as.character(reason %||% ""))
  if (!nzchar(reason)) stop("Informe o motivo da exclusão.", call. = FALSE)
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  order = DBI::dbGetQuery(connection, "SELECT * FROM work_orders WHERE work_order_id = ?", params = list(work_order_id))
  if (nrow(order) != 1L) stop("OS não encontrada.", call. = FALSE)
  delays = DBI::dbGetQuery(connection, "SELECT * FROM work_order_delays WHERE work_order_id = ?", params = list(work_order_id))
  previous = as.character(jsonlite::toJSON(list(work_order = order, delays = delays), dataframe = "rows", na = "null"))
  DBI::dbWithTransaction(connection, {
    DBI::dbExecute(connection, "DELETE FROM work_order_delays WHERE work_order_id = ?", params = list(work_order_id))
    DBI::dbExecute(connection, "UPDATE inventory_transactions SET work_order_id = NULL WHERE work_order_id = ?", params = list(work_order_id))
    DBI::dbExecute(connection, "DELETE FROM work_orders WHERE work_order_id = ?", params = list(work_order_id))
    DBI::dbExecute(
      connection,
      "INSERT INTO cmms_change_log(changed_at, table_name, record_id, action, reason, previous_json) VALUES(?, ?, ?, ?, ?, ?)",
      params = list(format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"), "work_orders", work_order_id,
                    "DELETE", reason, previous)
    )
  })
  invisible(TRUE)
}

# v1.5.5 — corrige a unidade do medidor (HOURS/KM) com justificativa e trilha de auditoria.
# method = "relabel": a leitura já está na unidade de destino (erro de cadastro); só o rótulo muda.
# method = "convert": converte as leituras com uma taxa EXPLICITAMENTE informada:
#   rate = km por hora de uso (KM -> HOURS: horas = km / rate; HOURS -> KM: km = horas * rate).
change_asset_meter_unit = function(path, asset_ids, target_unit = "HOURS", method = "relabel",
                                   rate = NA_real_, reason = "") {
  reason = trimws(as.character(reason %||% ""))
  if (!nzchar(reason)) stop("Informe o motivo da alteração.", call. = FALSE)
  target_unit = toupper(trimws(as.character(target_unit)))
  if (!target_unit %in% c("HOURS", "KM")) stop("Unidade de destino deve ser HOURS ou KM.", call. = FALSE)
  if (!method %in% c("relabel", "convert")) stop("Método inválido.", call. = FALSE)
  asset_ids = unique(as.character(asset_ids))
  asset_ids = asset_ids[!is.na(asset_ids) & nzchar(asset_ids)]
  if (length(asset_ids) == 0L) stop("Nenhuma viatura selecionada.", call. = FALSE)
  rate = suppressWarnings(as.numeric(rate))
  if (identical(method, "convert") && (!is.finite(rate) || rate <= 0)) {
    stop("Informe a velocidade média validada (km por hora de uso), maior que zero.", call. = FALSE)
  }
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  stamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  changed = character()
  DBI::dbWithTransaction(connection, {
    for (asset_id in asset_ids) {
      current = DBI::dbGetQuery(connection, "SELECT * FROM assets WHERE asset_id = ?", params = list(asset_id))
      if (nrow(current) != 1L) stop(paste("Viatura não encontrada:", asset_id), call. = FALSE)
      if (identical(toupper(as.character(current$meter_unit[[1]])), target_unit)) next
      orders = DBI::dbGetQuery(
        connection, "SELECT work_order_id, meter_at_event FROM work_orders WHERE asset_id = ?",
        params = list(asset_id)
      )
      factor = if (identical(method, "relabel")) 1 else if (identical(target_unit, "HOURS")) 1 / rate else rate
      DBI::dbExecute(
        connection,
        "UPDATE assets SET meter_unit = ?, initial_meter = initial_meter * ?, current_meter = current_meter * ? WHERE asset_id = ?",
        params = list(target_unit, factor, factor, asset_id)
      )
      if (nrow(orders) > 0L && !identical(factor, 1)) {
        DBI::dbExecute(
          connection,
          "UPDATE work_orders SET meter_at_event = meter_at_event * ? WHERE asset_id = ? AND meter_at_event IS NOT NULL",
          params = list(factor, asset_id)
        )
      }
      detail = list(asset = current, work_orders_meter = orders, method = method,
                    target_unit = target_unit, rate_km_per_hour = if (identical(method, "convert")) rate else NA_real_)
      DBI::dbExecute(
        connection,
        "INSERT INTO cmms_change_log(changed_at, table_name, record_id, action, reason, previous_json) VALUES(?, ?, ?, ?, ?, ?)",
        params = list(stamp, "assets", asset_id, paste0("UNIT_", toupper(method), "_", target_unit), reason,
                      as.character(jsonlite::toJSON(detail, dataframe = "rows", na = "null", auto_unbox = TRUE)))
      )
      changed = c(changed, asset_id)
    }
  })
  invisible(changed)
}

# v1.5.5 — modelo de CSV (apenas cabeçalho) gerado pelo próprio aplicativo.
csv_template_columns = function(table) {
  switch(
    table,
    assets = c("asset_id", "asset_name", "model", "entry_service_date", "initial_meter", "current_meter",
               "meter_unit", "initial_state", "active", "data_source"),
    work_orders = c("work_order_id", "asset_id", "maintenance_type", "is_functional_failure", "failure_mode",
                    "failure_class", "detected_at", "diagnosis_start_at", "diagnosis_end_at",
                    "active_repair_start_at", "active_repair_end_at", "test_start_at", "released_at",
                    "meter_at_event", "status", "description", "data_source"),
    work_order_delays = c("work_order_id", "delay_type", "start_at", "end_at", "responsibility_class", "notes"),
    resources = c("resource_id", "resource_type", "quantity", "capacity", "cost_horizon", "data_source"),
    parts = c("part_id", "part_name", "criticality", "initial_stock", "reorder_point", "reorder_qty",
              "lead_time_hours", "unit_cost", "data_source"),
    inventory_transactions = c("part_id", "work_order_id", "occurred_at", "quantity", "transaction_type", "data_source"),
    maintenance_plans = c("plan_id", "plan_name", "trigger_unit", "interval_value", "active_duration_hours",
                          "active", "data_source"),
    NULL
  )
}

read_table_safe = function(path, table) {
  if (!is.character(table) || length(table) != 1L ||
      !table %in% allowed_database_tables()) {
    stop("table is not in the read allowlist", call. = FALSE)
  }
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  DBI::dbReadTable(connection, table)
}

append_asset = function(path, row) {
  validation = validate_assets_data(row)
  if (!validation$valid || nrow(row) != 1L) {
    stop("asset row failed validation", call. = FALSE)
  }
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  DBI::dbExecute(
    connection,
    paste0(
      "INSERT INTO assets(asset_id, asset_name, model, entry_service_date, ",
      "initial_meter, current_meter, meter_unit, initial_state, active, data_source) ",
      "VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    ),
    params = unname(as.list(row[1, c(
      "asset_id", "asset_name", "model", "entry_service_date",
      "initial_meter", "current_meter", "meter_unit", "initial_state",
      "active", "data_source"
    )]))
  )
  invisible(TRUE)
}

append_completed_work_order = function(path, order_row, delays_df = NULL) {
  order_validation = validate_work_orders_data(order_row)
  if (!order_validation$valid || nrow(order_row) != 1L) {
    stop("work-order row failed validation", call. = FALSE)
  }
  if (!is.null(delays_df)) {
    delay_validation = validate_delays_data(delays_df)
    if (!delay_validation$valid) stop("delay rows failed validation", call. = FALSE)
    if (any(delays_df$work_order_id != order_row$work_order_id[[1]])) {
      stop("every delay must reference the inserted work order", call. = FALSE)
    }
  }
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  order_columns = c(
    "work_order_id", "asset_id", "maintenance_type", "is_functional_failure",
    "failure_mode", "failure_class", "detected_at", "diagnosis_start_at",
    "diagnosis_end_at", "active_repair_start_at", "active_repair_end_at",
    "test_start_at", "released_at", "meter_at_event", "status",
    "description", "data_source"
  )
  DBI::dbWithTransaction(connection, {
    placeholders = paste(rep("?", length(order_columns)), collapse = ",")
    DBI::dbExecute(
      connection,
      paste0(
        "INSERT INTO work_orders(", paste(order_columns, collapse = ","),
        ") VALUES(", placeholders, ")"
      ),
      params = unname(as.list(order_row[1, order_columns]))
    )
    if (!is.null(delays_df) && nrow(delays_df) > 0L) {
      for (index in seq_len(nrow(delays_df))) {
        DBI::dbExecute(
          connection,
          paste0(
            "INSERT INTO work_order_delays(work_order_id, delay_type, start_at, ",
            "end_at, responsibility_class, notes) VALUES(?, ?, ?, ?, ?, ?)"
          ),
          params = unname(as.list(delays_df[index, c(
            "work_order_id", "delay_type", "start_at", "end_at",
            "responsibility_class", "notes"
          )]))
        )
      }
    }
  })
  invisible(TRUE)
}

replace_support_configuration = function(path, resources_df, parts_df, plans_df) {
  required_resources = c(
    "resource_id", "resource_type", "quantity", "capacity",
    "cost_horizon", "data_source"
  )
  required_parts = c(
    "part_id", "part_name", "criticality", "initial_stock", "reorder_point",
    "reorder_qty", "lead_time_hours", "unit_cost", "data_source"
  )
  required_plans = c(
    "plan_id", "plan_name", "trigger_unit", "interval_value",
    "active_duration_hours", "active", "data_source"
  )
  if (!all(required_resources %in% names(resources_df)) ||
      !all(required_parts %in% names(parts_df)) ||
      !all(required_plans %in% names(plans_df))) {
    stop("support configuration is missing required columns", call. = FALSE)
  }
  if (any(resources_df$quantity < 0) || any(resources_df$capacity < 0) ||
      any(parts_df$initial_stock < 0) || any(parts_df$reorder_point < 0) ||
      any(parts_df$reorder_qty < 0) || any(parts_df$lead_time_hours < 0) ||
      any(plans_df$interval_value <= 0) || any(plans_df$active_duration_hours < 0)) {
    stop("support values violate nonnegative constraints", call. = FALSE)
  }
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  DBI::dbWithTransaction(connection, {
    DBI::dbExecute(connection, "DELETE FROM resources")
    DBI::dbExecute(connection, "DELETE FROM parts")
    DBI::dbExecute(connection, "DELETE FROM maintenance_plans")
    if (nrow(resources_df) > 0L) DBI::dbAppendTable(connection, "resources", resources_df)
    if (nrow(parts_df) > 0L) DBI::dbAppendTable(connection, "parts", parts_df)
    if (nrow(plans_df) > 0L) DBI::dbAppendTable(connection, "maintenance_plans", plans_df)
  })
  invisible(TRUE)
}

get_evidence_mode = function(path = resolve_database_path()) {
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  mode = DBI::dbGetQuery(
    connection, "SELECT value FROM meta WHERE key = ?",
    params = list("evidence_mode")
  )
  value = if (nrow(mode) == 0L) "SYNTHETIC" else toupper(mode$value[[1]])
  explanation = switch(
    value,
    SYNTHETIC = "Dados fictícios para demonstração; não constituem evidência sobre a JLTV.",
    HYBRID = "Combina registros observados e hipóteses explicitamente informadas.",
    EMPIRICAL = "Usa registros declarados reais; a validade depende da qualidade e da amostra.",
    "Modo de evidência desconhecido."
  )
  list(mode = value, explanation = explanation)
}

set_evidence_mode = function(
  path = resolve_database_path(),
  mode,
  confirmed = FALSE
) {
  mode = toupper(trimws(as.character(mode)))
  if (length(mode) != 1L || !mode %in% c("SYNTHETIC", "HYBRID", "EMPIRICAL")) {
    stop("mode must be SYNTHETIC, HYBRID or EMPIRICAL", call. = FALSE)
  }
  current = get_evidence_mode(path)$mode
  if (current == "SYNTHETIC" && mode == "EMPIRICAL" && !isTRUE(confirmed)) {
    stop("changing SYNTHETIC directly to EMPIRICAL requires explicit confirmation", call. = FALSE)
  }
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  DBI::dbExecute(
    connection,
    "INSERT OR REPLACE INTO meta(key, value) VALUES(?, ?)",
    params = list("evidence_mode", mode)
  )
  invisible(mode)
}

database_snapshot = function(
  path = resolve_database_path(),
  mapper = purrr::map,
  connector = open_database,
  reader = NULL,
  disconnector = NULL
) {
  if (is.null(reader)) {
    reader = function(connection, table) DBI::dbReadTable(connection, table)
  }
  if (is.null(disconnector)) {
    disconnector = function(connection) DBI::dbDisconnect(connection)
  }
  connection = connector(path)
  on.exit(disconnector(connection), add = TRUE)
  tables = allowed_database_tables()
  result = mapper(tables, function(table) reader(connection, table))
  names(result) = tables
  result
}

persist_multicriteria_result = function(
  path, result, score_aggregation = "mean", only_consistent = TRUE
) {
  if (!is.list(result) || is.null(result$ahp) || is.null(result$topsis)) {
    stop("Resultado multicritério inválido para auditoria.", call. = FALSE)
  }
  mc_run_id = paste0(
    "MC-", format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC"),
    "-", sprintf("%04d", sample.int(9999, 1))
  )
  status = if (isTRUE(result$ahp$estimable) && isTRUE(result$topsis$estimable)) {
    "AHP_TOPSIS"
  } else if (isTRUE(result$ahp$estimable)) {
    "AHP_ONLY"
  } else {
    "NOT_ESTIMABLE"
  }
  notes = paste(na.omit(c(
    result$ahp$error %||% NA_character_,
    result$topsis$reason %||% NA_character_
  )), collapse = " | ")
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  DBI::dbWithTransaction(connection, {
    DBI::dbExecute(
      connection,
      paste0(
        "INSERT INTO multicriteria_runs(mc_run_id, created_at, model_version, score_aggregation, ",
        "only_consistent, evidence_mode, status, notes) VALUES(?, ?, ?, ?, ?, ?, ?, ?)"
      ),
      params = list(
        mc_run_id, format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        JLTV_MODEL_VERSION, as.character(score_aggregation), as.integer(isTRUE(only_consistent)),
        toupper(as.character(result$evidence_mode %||% "HYBRID")),
        status, if (nzchar(notes)) notes else NA_character_
      )
    )
    weights = result$ahp$subcriteria_weights %||% data.frame()
    if (is.data.frame(weights) && nrow(weights) > 0L) {
      out = weights[, c(
        "hierarchy_group", "parent_criterion", "subcriterion_id",
        "local_weight", "criterion_weight", "global_weight"
      ), drop = FALSE]
      out$mc_run_id = mc_run_id
      out = out[, c("mc_run_id", setdiff(names(out), "mc_run_id")), drop = FALSE]
      DBI::dbAppendTable(connection, "ahp_weight_results", out)
    }
    consistency = result$ahp$consistency %||% data.frame()
    if (is.data.frame(consistency) && nrow(consistency) > 0L) {
      out = consistency[, c("hierarchy_group", "estimable", "respondents", "CR", "status"), drop = FALSE]
      out$estimable = as.integer(out$estimable)
      out$mc_run_id = mc_run_id
      out = out[, c("mc_run_id", setdiff(names(out), "mc_run_id")), drop = FALSE]
      DBI::dbAppendTable(connection, "ahp_consistency_results", out)
    }
    individual = result$ahp$individual_diagnostics %||% data.frame()
    if (is.data.frame(individual) && nrow(individual) > 0L) {
      out = individual[, c(
        "respondent_id", "hierarchy_group", "complete", "pair_count",
        "expected_pairs", "CR", "consistent"
      ), drop = FALSE]
      out$complete = as.integer(out$complete)
      out$consistent = as.integer(out$consistent)
      out$mc_run_id = mc_run_id
      out = out[, c("mc_run_id", setdiff(names(out), "mc_run_id")), drop = FALSE]
      DBI::dbAppendTable(connection, "ahp_individual_results", out)
    }
    if (isTRUE(result$topsis$estimable)) {
      out = result$topsis$ranking[, c(
        "alternative_id", "alternative_name", "d_plus", "d_minus", "coefficient", "rank"
      ), drop = FALSE]
      out$mc_run_id = mc_run_id
      out = out[, c("mc_run_id", setdiff(names(out), "mc_run_id")), drop = FALSE]
      DBI::dbAppendTable(connection, "topsis_results", out)
    }
  })
  mc_run_id
}

# ============================================================================
# 4. FUNÇÕES PURAS DE APRESENTAÇÃO E DECISÃO
# ============================================================================

parse_integer_sequence = function(text) {
  if (!is.character(text) || length(text) != 1L || !nzchar(trimws(text))) {
    stop("Informe ao menos um inteiro ou intervalo, por exemplo 8,10:12.", call. = FALSE)
  }
  tokens = trimws(strsplit(text, ",", fixed = TRUE)[[1]])
  values = integer()
  for (token in tokens) {
    if (grepl("^[0-9]+$", token)) {
      values = c(values, as.integer(token))
    } else if (grepl("^[0-9]+:[0-9]+$", token)) {
      endpoints = as.integer(strsplit(token, ":", fixed = TRUE)[[1]])
      values = c(values, seq(endpoints[[1]], endpoints[[2]]))
    } else {
      stop(paste("Sequência inválida:", token), call. = FALSE)
    }
  }
  sort(unique(values))
}

format_percent_br = function(value, digits = 1) {
  if (length(value) != 1L || is.na(value)) return("não estimável")
  paste0(
    format(round(100 * value, digits), nsmall = digits, decimal.mark = ",", trim = TRUE),
    "%"
  )
}

format_currency_br = function(value) {
  if (length(value) != 1L || is.na(value)) return("custo não informado")
  paste0(
    "R$ ",
    format(
      round(value, 2), nsmall = 2, big.mark = ".", decimal.mark = ",",
      scientific = FALSE, trim = TRUE
    )
  )
}

make_decision_text = function(
  selection,
  target_ready,
  horizon,
  p_target,
  evidence_mode
) {
  mode = toupper(as.character(evidence_mode))
  warning = if (mode == "SYNTHETIC") {
    "DADOS SINTÉTICOS — NÃO UTILIZAR COMO EVIDÊNCIA SOBRE A JLTV."
  } else if (mode == "HYBRID") {
    "PROJEÇÃO CONDICIONAL — resultados dependem de hipóteses não empíricas declaradas."
  } else {
    "RESULTADO EMPÍRICO CONDICIONAL — verificar amostra, período, qualidade e validação."
  }
  status = selection$status %||% "NO_RESULT"
  recommended = selection$recommended %||% selection$selected
  if (status == "NO_FEASIBLE") {
    return(list(
      status = status,
      headline = "NÃO APROVADO — META NÃO DEMONSTRADA",
      body = paste0(
        "Nenhuma configuração testada atingiu, pelo limite inferior do IC95%, ",
        "a probabilidade-alvo de ", format_percent_br(p_target),
        " para manter pelo menos ", target_ready, " viaturas durante ", horizon,
        " horas. Amplie a grade, a capacidade de apoio ou revise hipóteses justificadas."
      ),
      evidence_mode = mode, warning = warning
    ))
  }
  if (status == "PARETO") {
    candidates = selection$candidates %||% selection$pareto
    return(list(
      status = status,
      headline = "APROVADO — DIMENSIONAMENTO DE FROTA + APOIO EM PARETO",
      body = paste0(
        "Há ", nrow(candidates), " configuração(ões) não dominada(s) que satisfaz(em) ",
        "a meta de prontidão. Como os custos estão incompletos, o sistema não inventa ",
        "um ótimo monetário; a escolha do apoio deve ser feita na fronteira de Pareto."
      ),
      evidence_mode = mode, warning = warning
    ))
  }
  if (!is.null(recommended) && nrow(recommended) > 0L) {
    return(list(
      status = status,
      headline = "APROVADO — DIMENSIONAMENTO DE FROTA + OTIMIZAÇÃO DO APOIO",
      body = paste0(
        "A menor frota capaz na grade possui ", recommended$fleet_size[[1]],
        " viaturas. Para esse tamanho, o apoio de menor custo esperado utiliza ",
        recommended$diagnostic_teams[[1]], " equipe(s) de diagnóstico, ",
        recommended$repair_teams[[1]], " equipe(s) de reparo, ",
        recommended$bays[[1]], " box(es) e estoque inicial de ",
        recommended$initial_stock[[1]], ". A probabilidade estimada de manter ",
        target_ready, " prontas por ", horizon, " horas é ",
        format_percent_br(recommended$success_probability[[1]]),
        ", com limite inferior IC95% de ",
        format_percent_br(recommended$ci_lower[[1]]),
        ", acima da meta de ", format_percent_br(p_target), "."
      ),
      evidence_mode = mode, warning = warning
    ))
  }
  list(
    status = "NO_RESULT", headline = "SIMULAÇÃO AINDA NÃO EXECUTADA",
    body = "Abra a aba 3. Cenário e simulação, revise os parâmetros e clique em Executar avaliação agregada completa (ou Simular grade sem melhorias).",
    evidence_mode = mode, warning = warning
  )
}

mean_cv_lognormal_model = function(mean_value, coefficient_variation = 0.35) {
  if (!is.numeric(mean_value) || length(mean_value) != 1L ||
      !is.finite(mean_value) || mean_value <= 0) {
    stop("mean_value must be positive", call. = FALSE)
  }
  if (!is.numeric(coefficient_variation) || length(coefficient_variation) != 1L ||
      !is.finite(coefficient_variation) || coefficient_variation < 0) {
    stop("coefficient_variation must be nonnegative", call. = FALSE)
  }
  if (coefficient_variation == 0) {
    return(list(type = "constant", value = mean_value))
  }
  sdlog = sqrt(log(1 + coefficient_variation^2))
  meanlog = log(mean_value) - sdlog^2 / 2
  list(type = "lognormal", meanlog = meanlog, sdlog = sdlog)
}

selected_reliability_curve = function(fitted_models, maximum_time = NULL, points = 200) {
  if (!is.list(fitted_models) || !isTRUE(fitted_models$estimable)) {
    return(data.frame(time = numeric(), reliability = numeric(), model = character()))
  }
  parameters = fitted_models$parameters
  selected = fitted_models$selected_model
  characteristic = if (selected == "exponential") {
    parameters[["mean"]]
  } else if (selected == "weibull") {
    parameters[["scale"]]
  } else {
    exp(parameters[["meanlog"]] + parameters[["sdlog"]]^2 / 2)
  }
  if (is.null(maximum_time) || !is.finite(maximum_time) || maximum_time <= 0) {
    maximum_time = max(1, characteristic * 2.5)
  }
  time = seq(0, maximum_time, length.out = max(20L, as.integer(points)))
  reliability = if (selected == "exponential") {
    exp(-parameters[["rate"]] * time)
  } else if (selected == "weibull") {
    stats::pweibull(
      time, shape = parameters[["shape"]], scale = parameters[["scale"]],
      lower.tail = FALSE
    )
  } else {
    stats::plnorm(
      time, meanlog = parameters[["meanlog"]], sdlog = parameters[["sdlog"]],
      lower.tail = FALSE
    )
  }
  data.frame(time = time, reliability = reliability, model = selected)
}

# v1.5.7 — sem registros observados, a aba RAM mostra a premissa do simulador e os resultados simulados,
# sempre rotulados como hipótese/simulação (nunca como dado observado).
assumed_reliability_curve = function(model, points = 200) {
  if (!is.list(model) || is.null(model$type)) return(NULL)
  characteristic = switch(
    model$type,
    exponential = 1 / model$rate,
    weibull = model$scale,
    lognormal = exp(model$meanlog + model$sdlog^2 / 2),
    constant = model$value,
    NA_real_
  )
  if (!is.finite(characteristic) || characteristic <= 0) return(NULL)
  time = seq(0, characteristic * 2.5, length.out = max(20L, as.integer(points)))
  reliability = switch(
    model$type,
    exponential = exp(-model$rate * time),
    weibull = stats::pweibull(time, shape = model$shape, scale = model$scale, lower.tail = FALSE),
    lognormal = stats::plnorm(time, meanlog = model$meanlog, sdlog = model$sdlog, lower.tail = FALSE),
    constant = as.numeric(time < model$value)
  )
  label = switch(
    model$type, exponential = "exponencial", weibull = "Weibull", lognormal = "lognormal", constant = "constante"
  )
  data.frame(time = time, reliability = reliability, model = label, stringsAsFactors = FALSE)
}

simulated_downtime_components = function(simulation) {
  if (is.null(simulation) || is.null(simulation$summary) || nrow(simulation$summary) == 0L) return(NULL)
  selected = simulation$recommended
  if (is.null(selected) || nrow(selected) == 0L) {
    selected = simulation$summary[which.max(simulation$summary$ci_lower), , drop = FALSE]
  }
  data.frame(
    component = c("Diagnóstico", "Fila de recurso", "Espera por peça", "Corretiva", "Preventiva", "Teste"),
    hours = c(
      selected$mean_diagnosis_hours, selected$mean_queue_resource_hours,
      selected$mean_wait_part_hours, selected$mean_corrective_hours,
      selected$mean_preventive_hours, selected$mean_test_hours
    ),
    stringsAsFactors = FALSE
  )
}

observed_downtime_components = function(work_orders, delays) {
  hours_between = function(start_values, end_values) {
    starts = parse_datetime_utc(start_values)
    ends = parse_datetime_utc(end_values)
    values = as.numeric(difftime(ends, starts, units = "hours"))
    values[!is.finite(values) | values < 0] = NA_real_
    values
  }
  if (nrow(work_orders) == 0L) {
    return(data.frame(component = character(), hours = numeric()))
  }
  maintenance_type = toupper(as.character(work_orders$maintenance_type))
  diagnosis = hours_between(work_orders$diagnosis_start_at, work_orders$diagnosis_end_at)
  active = hours_between(work_orders$active_repair_start_at, work_orders$active_repair_end_at)
  test = hours_between(work_orders$test_start_at, work_orders$released_at)
  delay = if (nrow(delays) > 0L) {
    hours_between(delays$start_at, delays$end_at)
  } else {
    numeric()
  }
  data.frame(
    component = c(
      "Diagnóstico ativo", "Reparo corretivo ativo", "Preventiva ativa",
      "Teste e liberação", "Atrasos registrados"
    ),
    hours = c(
      sum(diagnosis, na.rm = TRUE),
      sum(active[maintenance_type == "CORRECTIVE"], na.rm = TRUE),
      sum(active[maintenance_type != "CORRECTIVE"], na.rm = TRUE),
      sum(test, na.rm = TRUE), sum(delay, na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
}

spearman_sensitivity = function(results) {
  if (!is.data.frame(results) || nrow(results) < 3L) {
    return(data.frame(input = character(), outcome = character(), rho = numeric()))
  }
  input_columns = intersect(
    c("fleet_size", "diagnostic_teams", "repair_teams", "bays", "initial_stock"),
    names(results)
  )
  outcome_columns = intersect(
    c("ci_lower", "mean_availability", "mean_hours_below_target", "expected_cost"),
    names(results)
  )
  rows = list()
  row_index = 1L
  for (input_name in input_columns) {
    for (outcome_name in outcome_columns) {
      x = results[[input_name]]
      y = results[[outcome_name]]
      valid = is.finite(x) & is.finite(y)
      rho = if (sum(valid) >= 3L && length(unique(x[valid])) > 1L &&
                length(unique(y[valid])) > 1L) {
        suppressWarnings(stats::cor(x[valid], y[valid], method = "spearman"))
      } else {
        NA_real_
      }
      rows[[row_index]] = data.frame(
        input = input_name, outcome = outcome_name, rho = rho,
        stringsAsFactors = FALSE
      )
      row_index = row_index + 1L
    }
  }
  do.call(rbind, rows)
}

escape_html_text = function(x) {
  value = as.character(x)
  value = gsub("&", "&amp;", value, fixed = TRUE)
  value = gsub("<", "&lt;", value, fixed = TRUE)
  value = gsub(">", "&gt;", value, fixed = TRUE)
  value = gsub('"', "&quot;", value, fixed = TRUE)
  value
}

data_frame_html = function(df, maximum_rows = 200) {
  if (!is.data.frame(df) || nrow(df) == 0L) return("<p>Sem registros.</p>")
  display = utils::head(df, maximum_rows)
  header = paste0("<th>", escape_html_text(names(display)), "</th>", collapse = "")
  rows = apply(display, 1, function(row) {
    paste0(
      "<tr>", paste0("<td>", escape_html_text(row), "</td>", collapse = ""),
      "</tr>"
    )
  })
  paste0("<table><thead><tr>", header, "</tr></thead><tbody>",
         paste(rows, collapse = ""), "</tbody></table>")
}


# ============================================================================
# 5. v1.5.0 — RESULTADO DA AVALIAÇÃO: GARGALOS, MELHORIAS E RELATÓRIO EXCEL
# ============================================================================
# Esta seção responde à pergunta do TCC: onde estão os gargalos da
# disponibilidade da frota JLTV e quais melhorias aumentam a disponibilidade.
# Ela apenas LÊ os resultados das seções anteriores (AHP, TOPSIS, P7, RAM e o
# motor de simulação da v1.4.2) e não altera nenhum desses cálculos.
#   - especialista prioriza: AHP (pesos) e TOPSIS (ranking dos sistemas);
#   - registro parametriza: indicadores RAM observados;
#   - simulação testa: cada melhoria é simulada contra a situação de
#     referência com as mesmas sementes (comparação pareada, IC95%).

format_number_br = function(value, digits = 1) {
  if (length(value) != 1L || is.na(value) || !is.finite(value)) return("—")
  format(
    round(value, digits), nsmall = digits, decimal.mark = ",", big.mark = ".",
    trim = TRUE, scientific = FALSE
  )
}

# Diferença entre duas proporções, em pontos percentuais.
format_pp_br = function(value, digits = 1) {
  if (length(value) != 1L || is.na(value) || !is.finite(value)) return("—")
  x = 100 * value
  sign = if (x > 0.05 * 10^(-digits + 1)) "+" else if (x < -0.05 * 10^(-digits + 1)) "−" else ""
  paste0(sign, format_number_br(abs(x), digits), " p.p.")
}

lower_first = function(text) {
  text = as.character(text)
  ifelse(grepl("^\\p{Lu}\\p{Ll}", text, perl = TRUE), paste0(tolower(substr(text, 1, 1)), substr(text, 2, nchar(text))), text)
}

assessment_criterion_labels = function() {
  questionnaire_hierarchy_spec()$criteria
}

assessment_criterion_colors = function() {
  c(OCCURRENCE = "#B23A48", DOWNTIME = "#D99A2B", SUPPLY = "#1F5A94", SUPPORT = "#198F8C")
}

subcriterion_label_map = function() {
  spec = questionnaire_hierarchy_spec()
  unlist(unname(spec[setdiff(names(spec), "criteria")]))
}

subcriterion_parent_map = function() {
  spec = questionnaire_hierarchy_spec()
  parents = questionnaire_parent_map()
  out = purrr::map(names(parents), function(group) {
    stats::setNames(rep(unname(parents[[group]]), length(spec[[group]])), names(spec[[group]]))
  })
  unlist(out)
}

# Por que cada sistema ficou no topo do TOPSIS? Para cada critério c:
#   posição_c = proximidade TOPSIS calculada só com os subcritérios de c
#               (0 = melhor sistema da lista nesse critério; 1 = pior);
#   peso_c    = peso AHP global do critério;
#   impulso_c = peso_c × posição_c.
# A parcela de cada critério na barra do gráfico é impulso_c / soma(impulsos).
# O subcritério dominante é o de maior peso_global × posição normalizada.
topsis_criterion_contributions = function(multicriteria) {
  criteria = assessment_criterion_labels()
  empty = data.frame(
    rank = integer(), alternative_id = character(), alternative_name = character(),
    coefficient = numeric(),
    share_OCCURRENCE = numeric(), share_DOWNTIME = numeric(),
    share_SUPPLY = numeric(), share_SUPPORT = numeric(),
    position_OCCURRENCE = numeric(), position_DOWNTIME = numeric(),
    position_SUPPLY = numeric(), position_SUPPORT = numeric(),
    dominant_criterion = character(), dominant_criterion_label = character(),
    dominant_subcriterion = character(), dominant_subcriterion_label = character(),
    stringsAsFactors = FALSE
  )
  topsis = if (is.list(multicriteria)) multicriteria$topsis else NULL
  if (!is.list(topsis) || !isTRUE(topsis$estimable) || !is.matrix(topsis$weighted)) return(empty)
  weighted = topsis$weighted
  ranking = topsis$ranking
  ideal = topsis$ideal[colnames(weighted)]
  anti = topsis$anti_ideal[colnames(weighted)]
  parent = unname(subcriterion_parent_map()[colnames(weighted)])
  sub_labels = subcriterion_label_map()
  column_weight = colSums(weighted) # proporcional ao peso; usado só para normalizar
  global_weight = multicriteria$ahp$subcriteria_weights
  w_sub = if (is.data.frame(global_weight) && nrow(global_weight) > 0L) {
    stats::setNames(global_weight$global_weight, global_weight$subcriterion_id)[colnames(weighted)]
  } else {
    column_weight / sum(column_weight)
  }
  w_sub = w_sub / sum(w_sub)
  w_criterion = purrr::map_dbl(stats::setNames(names(criteria), names(criteria)), function(cc) sum(w_sub[parent == cc]))
  range_sub = ideal - anti
  rows = purrr::map(seq_len(nrow(ranking)), function(i) {
    alt = as.character(ranking$alternative_id[[i]])
    v = weighted[alt, ]
    position = purrr::map_dbl(stats::setNames(names(criteria), names(criteria)), function(cc) {
      cols = parent == cc
      d_plus = sqrt(sum((v[cols] - ideal[cols])^2))
      d_minus = sqrt(sum((v[cols] - anti[cols])^2))
      if (!is.finite(d_plus + d_minus) || (d_plus + d_minus) <= 0) return(0)
      d_minus / (d_plus + d_minus)
    })
    impulse = w_criterion * position
    share = if (sum(impulse) > 0) impulse / sum(impulse) else w_criterion / sum(w_criterion)
    dominant = names(impulse)[which.max(impulse)]
    sub_position = ifelse(range_sub > 0, (v - anti) / range_sub, 0)
    in_dominant = parent == dominant
    dominant_sub = names(v)[in_dominant][which.max((w_sub * sub_position)[in_dominant])]
    data.frame(
      rank = as.integer(ranking$rank[[i]]), alternative_id = alt,
      alternative_name = as.character(ranking$alternative_name[[i]]),
      coefficient = as.numeric(ranking$coefficient[[i]]),
      share_OCCURRENCE = unname(share[["OCCURRENCE"]]), share_DOWNTIME = unname(share[["DOWNTIME"]]),
      share_SUPPLY = unname(share[["SUPPLY"]]), share_SUPPORT = unname(share[["SUPPORT"]]),
      position_OCCURRENCE = unname(position[["OCCURRENCE"]]), position_DOWNTIME = unname(position[["DOWNTIME"]]),
      position_SUPPLY = unname(position[["SUPPLY"]]), position_SUPPORT = unname(position[["SUPPORT"]]),
      dominant_criterion = dominant, dominant_criterion_label = unname(criteria[[dominant]]),
      dominant_subcriterion = dominant_sub,
      dominant_subcriterion_label = unname(sub_labels[[dominant_sub]]),
      stringsAsFactors = FALSE
    )
  })
  out = bind_rows_base(rows, empty)
  row.names(out) = NULL
  out
}

# Quantas vezes cada sistema foi citado na P7 (onde buscar a evidência).
p7_mentions_by_alternative = function(factors) {
  alternatives = questionnaire_alternative_spec()
  empty = data.frame(
    alternative_id = names(alternatives), alternative_name = unname(alternatives),
    p7_mentions = 0L, p7_respondents = 0L, p7_examples = "",
    stringsAsFactors = FALSE
  )
  if (!is.data.frame(factors) || nrow(factors) == 0L || !"related_system" %in% names(factors)) {
    return(empty)
  }
  system = normalize_questionnaire_text(factors$related_system)
  identification = trimws(as.character(factors$identification %||% rep("", nrow(factors))))
  identification[is.na(identification)] = ""
  respondent = as.character(factors$respondent_id %||% rep("", nrow(factors)))
  rows = purrr::map(names(alternatives), function(code) {
    name_norm = normalize_questionnaire_text(alternatives[[code]])
    hit = grepl(paste0("^", code, "\\b"), system, perl = TRUE) |
      (nzchar(name_norm) & grepl(name_norm, system, fixed = TRUE))
    examples = unique(identification[hit & nzchar(identification)])
    data.frame(
      alternative_id = code, alternative_name = unname(alternatives[[code]]),
      p7_mentions = as.integer(sum(hit)),
      p7_respondents = length(unique(respondent[hit])),
      p7_examples = paste(utils::head(examples, 3L), collapse = "; "),
      stringsAsFactors = FALSE
    )
  })
  bind_rows_base(rows, empty)
}

# Catálogo de melhorias testáveis no simulador. Cada melhoria ataca um
# critério do AHP e um componente do tempo parado. As magnitudes padrão são
# hipóteses de cenário editáveis na interface, não valores medidos da JLTV.
improvement_lever_catalog = function() {
  data.frame(
    lever_id = c(
      "CONFIABILIDADE", "ESTOQUE", "PRAZO_REPOSICAO",
      "EQUIPE_REPARO", "EQUIPE_DIAGNOSTICO", "TEMPO_REPARO", "TEMPO_DIAGNOSTICO"
    ),
    label = c(
      "Aumentar a confiabilidade (MTBF)",
      "Ampliar o estoque de sobressalentes",
      "Reduzir o prazo de reposição de peças",
      "Acrescentar equipe de reparo (com box)",
      "Acrescentar equipe de diagnóstico",
      "Reduzir o tempo de reparo corretivo",
      "Reduzir o tempo de diagnóstico"
    ),
    criterion = c("OCCURRENCE", "SUPPLY", "SUPPLY", "SUPPORT", "SUPPORT", "SUPPORT", "SUPPORT"),
    subcriteria = c(
      "FREQUENCY;RECURRENCE;FLEET_SCOPE", "SPARE_DEPENDENCY;STOCKOUT", "LEAD_TIME",
      "EXTERNAL_SUPPORT", "EXTERNAL_SUPPORT", "ACTIVE_MAINTENANCE", "ACTIVE_MAINTENANCE"
    ),
    downtime_component = c(
      "Falhas (frequência)", "Espera por peça", "Espera por peça",
      "Espera por equipe/box", "Espera por equipe/box", "Reparo corretivo", "Diagnóstico"
    ),
    magnitude_kind = c("percent_up", "percent_up", "percent_down", "increment", "increment", "percent_down", "percent_down"),
    magnitude_unit = c("% no MTBF", "% no estoque", "% no prazo", "equipe(s)", "equipe(s)", "% no tempo", "% no tempo"),
    default_magnitude = c(20, 50, 30, 1, 1, 20, 25),
    example_actions = c(
      "Manutenção preventiva dirigida aos sistemas prioritários; análise de causa raiz das falhas recorrentes apontadas na P7.",
      "Rever nível de estoque e ponto de ressuprimento dos itens críticos citados na P7; manter estoque avançado junto à unidade.",
      "Priorizar requisições dos itens críticos; acordos de prazo com o fornecedor; agilizar o trâmite administrativo do pedido.",
      "Capacitar mecânicos e disponibilizar box adicional; reduzir a dependência de apoio externo.",
      "Capacitar o diagnóstico no 1º escalão; padronizar a busca de panes.",
      "Ferramental, publicações técnicas e kits de reparo; treinamento nos sistemas prioritários.",
      "Ferramentas de diagnóstico e roteiros de pesquisa de pane padronizados."
    ),
    stringsAsFactors = FALSE
  )
}

# Média de um modelo de tempo do simulador (horas).
time_model_mean = function(model) {
  type = tolower(as.character(model$type %||% ""))
  if (type == "constant") return(as.numeric(model$value))
  if (type == "exponential") return(1 / as.numeric(model$rate))
  if (type == "weibull") return(as.numeric(model$scale) * gamma(1 + 1 / as.numeric(model$shape)))
  if (type == "lognormal") return(exp(model$meanlog + model$sdlog^2 / 2))
  if (type == "zero_lognormal") return((1 - model$p_zero) * exp(model$meanlog + model$sdlog^2 / 2))
  if (type == "empirical") return(mean(as.numeric(model$values)))
  NA_real_
}

# Multiplica a média de um modelo de tempo por `factor`, preservando a forma
# (CV, forma Weibull, proporção de zeros).
scale_time_model = function(model, factor) {
  if (!is.numeric(factor) || length(factor) != 1L || !is.finite(factor) || factor <= 0) {
    stop("Fator de escala inválido.", call. = FALSE)
  }
  type = tolower(as.character(model$type %||% ""))
  if (type == "constant") {
    model$value = model$value * factor
  } else if (type == "exponential") {
    model$rate = model$rate / factor
  } else if (type == "weibull") {
    model$scale = model$scale * factor
  } else if (type %in% c("lognormal", "zero_lognormal")) {
    model$meanlog = model$meanlog + log(factor)
  } else if (type == "empirical") {
    model$values = as.numeric(model$values) * factor
  } else {
    stop(paste("Modelo de tempo não reconhecido:", type), call. = FALSE)
  }
  model
}

apply_improvement_lever = function(config, params, lever_id, magnitude) {
  catalog = improvement_lever_catalog()
  if (!lever_id %in% catalog$lever_id) stop(paste("Melhoria não reconhecida:", lever_id), call. = FALSE)
  m = suppressWarnings(as.numeric(magnitude))
  if (length(m) != 1L || !is.finite(m) || m <= 0) {
    stop(paste0("Informe uma magnitude positiva para '", catalog$label[catalog$lever_id == lever_id], "'."), call. = FALSE)
  }
  kind = catalog$magnitude_kind[catalog$lever_id == lever_id]
  if (kind == "percent_down" && m >= 100) {
    stop("Reduções percentuais devem ser menores que 100%.", call. = FALSE)
  }
  hours = function(x) paste0(format_number_br(x, 1), " h")
  reduce = function(model, label) {
    before = time_model_mean(model)
    model = scale_time_model(model, 1 - m / 100)
    list(model = model, change = paste0(
      label, " de ", hours(before), " para ", hours(time_model_mean(model)),
      " (−", format_number_br(m, 0), "%)"
    ))
  }
  if (lever_id == "CONFIABILIDADE") {
    before = time_model_mean(params$life_model)
    params$life_model = scale_time_model(params$life_model, 1 + m / 100)
    change = paste0(
      "MTBF de ", hours(before), " para ", hours(time_model_mean(params$life_model)),
      " (+", format_number_br(m, 0), "%)"
    )
  } else if (lever_id == "ESTOQUE") {
    before = as.integer(config$initial_stock)
    after = as.integer(max(before + 1L, ceiling(before * (1 + m / 100))))
    config$initial_stock = after
    change = paste0("Estoque inicial de ", before, " para ", after, " sobressalentes")
  } else if (lever_id == "PRAZO_REPOSICAO") {
    changed = reduce(params$lead_time_model, "Prazo de reposição")
    params$lead_time_model = changed$model
    change = changed$change
  } else if (lever_id == "EQUIPE_REPARO") {
    k = max(1L, as.integer(round(m)))
    before_teams = as.integer(config$repair_teams)
    before_bays = as.integer(config$bays)
    config$repair_teams = before_teams + k
    config$bays = max(before_bays, as.integer(config$repair_teams))
    change = paste0(
      "Equipes de reparo de ", before_teams, " para ", config$repair_teams,
      if (config$bays != before_bays) paste0("; boxes de ", before_bays, " para ", config$bays) else ""
    )
  } else if (lever_id == "EQUIPE_DIAGNOSTICO") {
    k = max(1L, as.integer(round(m)))
    before = as.integer(config$diagnostic_teams)
    config$diagnostic_teams = before + k
    change = paste0("Equipes de diagnóstico de ", before, " para ", config$diagnostic_teams)
  } else if (lever_id == "TEMPO_REPARO") {
    changed = reduce(params$corrective_model, "Reparo corretivo médio")
    params$corrective_model = changed$model
    change = changed$change
  } else if (lever_id == "TEMPO_DIAGNOSTICO") {
    changed = reduce(params$diagnosis_model, "Diagnóstico médio")
    params$diagnosis_model = changed$model
    change = changed$change
  }
  list(config = config, params = params, change = change)
}

# Mesma fórmula da comparação pareada da v1.4.1 (t de Student, IC95%).
paired_difference_ci = function(differences) {
  differences = as.numeric(differences)
  n = length(differences)
  m = mean(differences)
  if (n < 2L) return(c(mean = m, lower = NA_real_, upper = NA_real_))
  half = stats::qt(0.975, df = n - 1L) * stats::sd(differences) / sqrt(n)
  c(mean = m, lower = m - half, upper = m + half)
}

improvement_comparison_row = function(base, alternative, lever_id, label, criterion, change) {
  a = paired_difference_ci(alternative$replications$avg_availability - base$replications$avg_availability)
  s = paired_difference_ci(
    as.numeric(alternative$replications$mission_success) - as.numeric(base$replications$mission_success)
  )
  h = paired_difference_ci(alternative$replications$hours_below_target - base$replications$hours_below_target)
  conclusion = if (!is.finite(a[["lower"]])) {
    "IC não estimável"
  } else if (a[["lower"]] > 0) {
    "Ganho demonstrado"
  } else if (a[["upper"]] < 0) {
    "Piora demonstrada"
  } else {
    "Ganho não demonstrado"
  }
  data.frame(
    lever_id = lever_id, label = label, criterion = criterion, change = change,
    base_availability = base$summary$mean_availability,
    new_availability = alternative$summary$mean_availability,
    delta_availability = a[["mean"]], delta_ci_lower = a[["lower"]], delta_ci_upper = a[["upper"]],
    base_success = base$summary$success_probability,
    new_success = alternative$summary$success_probability,
    delta_success = s[["mean"]], delta_success_ci_lower = s[["lower"]], delta_success_ci_upper = s[["upper"]],
    base_hours_below = base$summary$mean_hours_below_target,
    new_hours_below = alternative$summary$mean_hours_below_target,
    delta_hours_below = h[["mean"]],
    base_downtime_hours = base$summary$mean_downtime_asset_hours,
    new_downtime_hours = alternative$summary$mean_downtime_asset_hours,
    conclusion = conclusion,
    demonstrated = identical(conclusion, "Ganho demonstrado"),
    stringsAsFactors = FALSE
  )
}

# Testa cada melhoria contra a situação de referência com as mesmas sementes.
# `levers` = data.frame(lever_id, magnitude). Ao final, as melhorias com ganho
# demonstrado (até `package_size`, maiores ganhos) são simuladas em conjunto.
evaluate_improvement_levers = function(
  config, params, horizon, target_ready, replications, seed,
  levers = NULL, package_size = 3L, progress = NULL
) {
  catalog = improvement_lever_catalog()
  if (is.null(levers)) {
    levers = data.frame(lever_id = catalog$lever_id, magnitude = catalog$default_magnitude, stringsAsFactors = FALSE)
  }
  levers = levers[levers$lever_id %in% catalog$lever_id, , drop = FALSE]
  if (nrow(levers) == 0L) stop("Selecione ao menos uma melhoria para testar.", call. = FALSE)
  if (!is.numeric(config$fleet_size) || target_ready > config$fleet_size) {
    stop("A frota de referência deve ser maior ou igual à meta M de viaturas prontas.", call. = FALSE)
  }
  total = nrow(levers) + 2L
  notify = function(index, detail) if (is.function(progress)) progress(index, total, detail)
  notify(1L, "situação de referência")
  base = evaluate_configuration(config, params, horizon, target_ready, replications, seed)
  modified_list = purrr::map(seq_len(nrow(levers)), function(i) {
    apply_improvement_lever(config, params, levers$lever_id[[i]], levers$magnitude[[i]])
  })
  rows = without_purrr_index(purrr::map(seq_len(nrow(levers)), function(i) {
    id = levers$lever_id[[i]]
    info = catalog[catalog$lever_id == id, , drop = FALSE]
    notify(i + 1L, info$label)
    modified = modified_list[[i]]
    alternative = evaluate_configuration(
      modified$config, modified$params, horizon, target_ready, replications, seed
    )
    row = improvement_comparison_row(base, alternative, id, info$label, info$criterion, modified$change)
    row$magnitude = as.numeric(levers$magnitude[[i]])
    row
  }))
  lever_table = bind_rows_base(rows)
  row.names(lever_table) = NULL
  chosen = lever_table[lever_table$demonstrated, , drop = FALSE]
  chosen = chosen[order(-chosen$delta_availability), , drop = FALSE]
  chosen = utils::head(chosen, as.integer(package_size))
  package = NULL
  if (nrow(chosen) >= 2L) {
    notify(total, "pacote de melhorias")
    package_config = config
    package_params = params
    changes = character()
    for (k in seq_len(nrow(chosen))) {
      modified = apply_improvement_lever(package_config, package_params, chosen$lever_id[[k]], chosen$magnitude[[k]])
      package_config = modified$config
      package_params = modified$params
      changes = c(changes, modified$change)
    }
    alternative = evaluate_configuration(
      package_config, package_params, horizon, target_ready, replications, seed
    )
    package = improvement_comparison_row(
      base, alternative, "PACOTE",
      paste0("Pacote com as ", nrow(chosen), " intervenções de maior ganho"),
      "PACKAGE", paste(changes, collapse = "; ")
    )
    package$magnitude = NA_real_
  }
  list(
    base = base$summary, base_replications = base$replications,
    levers = lever_table, package = package,
    package_levers = as.character(chosen$lever_id),
    config = config, horizon = horizon, target_ready = as.integer(target_ready),
    replications = as.integer(replications), seed = as.integer(seed),
    availability_target = as.numeric(target_ready) / as.numeric(config$fleet_size),
    parameters = list(
      mtbf = time_model_mean(params$life_model),
      lead_time = time_model_mean(params$lead_time_model),
      corrective = time_model_mean(params$corrective_model),
      diagnosis = time_model_mean(params$diagnosis_model),
      part_probability = params$part_required_probability
    ),
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M")
  )
}

# Em que etapa a viatura perde tempo parada (média simulada por réplica).
simulated_downtime_decomposition = function(summary) {
  empty = data.frame(
    component = character(), hours = numeric(), share = numeric(),
    cumulative_share = numeric(), lever_hint = character(), stringsAsFactors = FALSE
  )
  if (!is.data.frame(summary) || nrow(summary) == 0L) return(empty)
  s = summary[1, , drop = FALSE]
  value = function(name) if (name %in% names(s)) as.numeric(s[[name]][[1]]) else NA_real_
  out = data.frame(
    component = c(
      "Diagnóstico", "Espera por equipe/box", "Espera por peça",
      "Reparo corretivo", "Manutenção preventiva", "Teste e liberação"
    ),
    hours = c(
      value("mean_diagnosis_hours"), value("mean_queue_resource_hours"),
      value("mean_wait_part_hours"), value("mean_corrective_hours"),
      value("mean_preventive_hours"), value("mean_test_hours")
    ),
    lever_hint = c(
      "Diagnóstico mais rápido ou mais equipes de diagnóstico",
      "Mais equipes de reparo e boxes",
      "Estoque de sobressalentes e prazo de reposição",
      "Reparo mais rápido e maior confiabilidade (menos falhas)",
      "Revisar intervalo e duração da preventiva",
      "Agilizar teste e liberação"
    ),
    stringsAsFactors = FALSE
  )
  out$hours[!is.finite(out$hours)] = 0
  total = sum(out$hours)
  out = out[order(-out$hours), , drop = FALSE]
  out$share = if (total > 0) out$hours / total else 0
  out$cumulative_share = cumsum(out$share)
  row.names(out) = NULL
  out[, c("component", "hours", "share", "cumulative_share", "lever_hint")]
}

# Ordena as melhorias: primeiro as de ganho demonstrado; depois pela
# prioridade integrada = ganho simulado de Ao (p.p.) × importância relativa
# (peso AHP do critério atacado ÷ maior peso de critério).
rank_improvements = function(levers, criteria_weights = NULL, bottlenecks = NULL) {
  if (!is.data.frame(levers) || nrow(levers) == 0L) return(levers)
  catalog = improvement_lever_catalog()
  criteria = assessment_criterion_labels()
  w = if (is.numeric(criteria_weights) && length(criteria_weights) > 0L) criteria_weights else NULL
  levers$criterion_label = unname(criteria[levers$criterion])
  levers$ahp_weight = if (is.null(w)) NA_real_ else unname(w[levers$criterion])
  relevance = if (is.null(w)) rep(1, nrow(levers)) else unname(w[levers$criterion] / max(w))
  relevance[!is.finite(relevance)] = 1
  levers$priority_index = 100 * pmax(levers$delta_availability, 0) * relevance
  levers$example_actions = catalog$example_actions[match(levers$lever_id, catalog$lever_id)]
  levers$downtime_component = catalog$downtime_component[match(levers$lever_id, catalog$lever_id)]
  levers$targets_bottleneck = purrr::map_chr(seq_len(nrow(levers)), function(i) {
    if (!is.data.frame(bottlenecks) || nrow(bottlenecks) == 0L) return("")
    subs = strsplit(catalog$subcriteria[catalog$lever_id == levers$lever_id[[i]]], ";")[[1]]
    hit = bottlenecks$dominant_criterion == levers$criterion[[i]] |
      bottlenecks$dominant_subcriterion %in% subs
    if (!any(hit)) return("")
    paste0("Prioridade nº ", bottlenecks$rank[hit], " (", bottlenecks$alternative_name[hit], ")", collapse = "; ")
  })
  ordering = order(!levers$demonstrated, -levers$priority_index, -levers$delta_availability)
  levers = levers[ordering, , drop = FALSE]
  levers$priority_rank = seq_len(nrow(levers))
  row.names(levers) = NULL
  levers
}

# Monta o resultado da avaliação a partir das peças disponíveis. Qualquer
# peça pode faltar: o resultado informa o que falta em vez de falhar.
build_assessment = function(
  multicriteria = NULL, contributions = NULL, p7_mentions = NULL,
  improvements = NULL, ram = NULL, evidence_mode = "HYBRID",
  questionnaire_overview = NULL, simulation = NULL, data_quality_ready = NA,
  top_n = 3L
) {
  mode = toupper(as.character(evidence_mode %||% "HYBRID"))
  overview_value = function(label) {
    if (!is.data.frame(questionnaire_overview)) return(NA_real_)
    value = questionnaire_overview$valor[questionnaire_overview$indicador == label]
    if (length(value) == 0L) NA_real_ else suppressWarnings(as.numeric(value[[1]]))
  }
  respondents = overview_value("Respondentes no modo selecionado")
  p6_coverage = overview_value("Cobertura P6")
  ahp = if (is.list(multicriteria)) multicriteria$ahp else NULL
  topsis = if (is.list(multicriteria)) multicriteria$topsis else NULL
  ahp_ok = isTRUE(ahp$estimable)
  topsis_ok = isTRUE(topsis$estimable)
  contributions = if (is.data.frame(contributions)) contributions else data.frame()
  bottlenecks = utils::head(contributions, as.integer(top_n))
  if (nrow(bottlenecks) > 0L && is.data.frame(p7_mentions)) {
    index = match(bottlenecks$alternative_id, p7_mentions$alternative_id)
    bottlenecks$p7_mentions = p7_mentions$p7_mentions[index]
    bottlenecks$p7_respondents = p7_mentions$p7_respondents[index]
    bottlenecks$p7_examples = p7_mentions$p7_examples[index]
  }
  criteria_weights = if (ahp_ok || is.numeric(ahp$criteria_weights)) ahp$criteria_weights else NULL
  if (nrow(bottlenecks) > 0L) {
    catalog = improvement_lever_catalog()
    bottlenecks$suggested_levers = purrr::map_chr(seq_len(nrow(bottlenecks)), function(i) {
      hits = catalog$criterion == bottlenecks$dominant_criterion[[i]] |
        purrr::map_lgl(strsplit(catalog$subcriteria, ";"), function(s) bottlenecks$dominant_subcriterion[[i]] %in% s)
      if (!any(hits)) return("Ações sobre o tempo total fora de serviço (todas as alavancas reduzem o downtime)")
      paste(catalog$label[hits], collapse = "; ")
    })
  }
  process = if (is.list(improvements)) simulated_downtime_decomposition(improvements$base) else
    simulated_downtime_decomposition(NULL)
  ranked = if (is.list(improvements)) rank_improvements(improvements$levers, criteria_weights, bottlenecks) else NULL
  package = if (is.list(improvements)) improvements$package else NULL

  ao_reference = if (is.list(improvements)) improvements$base$mean_availability[[1]] else NA_real_
  success_reference = if (is.list(improvements)) improvements$base$success_probability[[1]] else NA_real_
  best = if (is.data.frame(ranked) && nrow(ranked) > 0L) ranked[1, , drop = FALSE] else NULL
  final = if (is.data.frame(package) && nrow(package) > 0L) package else best
  ao_observed = if (is.list(ram)) {
    if (is.finite(ram$Ao_calendar %||% NA_real_)) ram$Ao_calendar else if (is.finite(ram$Ao %||% NA_real_)) ram$Ao else NA_real_
  } else NA_real_
  kpis = list(
    ao_reference = ao_reference,
    ao_improved = if (!is.null(final)) final$new_availability[[1]] else NA_real_,
    ao_gain = if (!is.null(final)) final$delta_availability[[1]] else NA_real_,
    ao_observed = ao_observed,
    availability_target = if (is.list(improvements)) improvements$availability_target else NA_real_,
    success_reference = success_reference,
    success_improved = if (!is.null(final)) final$new_success[[1]] else NA_real_,
    improved_label = if (!is.null(final)) final$label[[1]] else "",
    top_bottleneck = if (nrow(bottlenecks) > 0L) bottlenecks$alternative_name[[1]] else "",
    top_bottleneck_driver = if (nrow(bottlenecks) > 0L) bottlenecks$dominant_criterion_label[[1]] else "",
    top_process = if (nrow(process) > 0L && sum(process$hours) > 0) process$component[[1]] else "",
    top_process_share = if (nrow(process) > 0L && sum(process$hours) > 0) process$share[[1]] else NA_real_
  )

  steps = data.frame(
    step = c(
      "Respostas do questionário", "Pesos AHP consistentes", "Ranking TOPSIS dos sistemas",
      "Registros RAM da frota", "Simulação da frota", "Teste das melhorias"
    ),
    target = c("dados", "decisao", "decisao", "dados", "simulacao", "resultado"),
    done = c(
      is.finite(respondents) && respondents > 0, ahp_ok, topsis_ok,
      is.list(ram) && is.finite(ram$MTBF %||% NA_real_),
      !is.null(simulation), is.list(improvements)
    ),
    detail = c(
      if (is.finite(respondents) && respondents > 0) paste(respondents, "avaliador(es) no modo", mode) else "Nenhuma resposta no modo selecionado",
      if (ahp_ok) "Todas as matrizes com RC < 0,10" else (ahp$error %||% "Calcule o AHP"),
      if (topsis_ok) paste("1º:", topsis$ranking$alternative_name[[1]]) else (topsis$reason %||% "Depende do AHP e das notas P6"),
      if (is.list(ram) && is.finite(ram$MTBF %||% NA_real_)) paste0("MTBF ", format_number_br(ram$MTBF, 1), " ", ram$mtbf_unit %||% "") else "Sem registros suficientes",
      if (!is.null(simulation)) paste(nrow(simulation$summary), "configuração(ões) simulada(s)") else "Ainda não executada",
      if (is.list(improvements)) paste(sum(improvements$levers$demonstrated), "de", nrow(improvements$levers), "com ganho demonstrado") else "Ainda não executado"
    ),
    stringsAsFactors = FALSE
  )

  headline = character()
  if (is.finite(ao_reference)) {
    cfg = improvements$config
    headline = c(headline, paste0(
      "Disponibilidade operacional simulada da situação de referência (", cfg$fleet_size,
      " viaturas, ", cfg$diagnostic_teams, " eq. diagnóstico, ", cfg$repair_teams,
      " eq. reparo, ", cfg$bays, " box(es), estoque ", cfg$initial_stock, "): ",
      format_percent_br(ao_reference), ". Probabilidade de manter ", improvements$target_ready,
      " viaturas prontas sob o critério configurado (tolerância ", cfg$mission_tolerance_hours %||% 0, " h) durante as ", format_number_br(improvements$horizon, 0), " h da missão: ",
      format_percent_br(success_reference), "."
    ))
  }
  if (nrow(bottlenecks) > 0L) {
    b = bottlenecks[1, , drop = FALSE]
    headline = c(headline, paste0(
      "Prioridade nº 1 segundo os especialistas: ", b$alternative_name, " (coeficiente TOPSIS ",
      format_number_br(b$coefficient, 3), "). O peso vem principalmente de ",
      b$dominant_criterion_label, " (", format_percent_br(max(unlist(b[, c("share_OCCURRENCE", "share_DOWNTIME", "share_SUPPLY", "share_SUPPORT")])), 0),
      " da prioridade), em especial do subcritério ", b$dominant_subcriterion_label, "."
    ))
    if (nrow(bottlenecks) > 1L) {
      headline = c(headline, paste0(
        "Na sequência: ", paste0(bottlenecks$alternative_name[-1], " (", bottlenecks$dominant_criterion_label[-1], ")", collapse = " e "), "."
      ))
    }
  } else if (!topsis_ok) {
    headline = c(headline, paste0(
      "Gargalos ainda não identificados: ", topsis$reason %||% ahp$error %||% "importe as respostas e calcule AHP–TOPSIS", "."
    ))
  }
  if (nzchar(kpis$top_process)) {
    headline = c(headline, paste0(
      "No processo de manutenção simulado, a maior parte do tempo parado está em ",
      lower_first(kpis$top_process), " (", format_percent_br(kpis$top_process_share, 0), " das horas paradas)."
    ))
  }
  if (!is.null(best)) {
    if (isTRUE(best$demonstrated)) {
      headline = c(headline, paste0(
        "Melhoria de maior prioridade: ", lower_first(best$label), " — ", best$change, ". Disponibilidade ",
        format_percent_br(best$base_availability), " → ", format_percent_br(best$new_availability),
        " (", format_pp_br(best$delta_availability), "; IC95% ", format_pp_br(best$delta_ci_lower),
        " a ", format_pp_br(best$delta_ci_upper), ")."
      ))
    } else {
      headline = c(headline, paste0(
        "Nenhuma das melhorias testadas teve ganho de disponibilidade demonstrado (IC95% inclui zero). ",
        "Aumente as replicações ou teste magnitudes maiores."
      ))
    }
  }
  if (nrow(bottlenecks) > 0L && is.data.frame(ranked) && nrow(ranked) > 0L) {
    top_criterion = bottlenecks$dominant_criterion[[1]]
    related = ranked[ranked$criterion == top_criterion, , drop = FALSE]
    if (nrow(related) > 0L && !any(related$demonstrated)) {
      headline = c(headline, paste0(
        "Divergência a investigar: os especialistas atribuem o gargalo principal a ", bottlenecks$dominant_criterion_label[[1]],
        ", mas no cenário simulado as melhorias desse critério (", paste(lower_first(related$label), collapse = "; "),
        ") não tiveram ganho demonstrado. Confira com os registros apontados na P7 se os parâmetros de ",
        lower_first(bottlenecks$dominant_criterion_label[[1]]), " usados na simulação representam a frota."
      ))
    }
  }
  if (is.data.frame(package) && nrow(package) > 0L) {
    headline = c(headline, paste0(
      "Aplicando juntas as ", length(improvements$package_levers), " melhorias com maior ganho, a disponibilidade vai de ",
      format_percent_br(package$base_availability), " para ", format_percent_br(package$new_availability),
      " (", format_pp_br(package$delta_availability), ") e a probabilidade de cumprir a missão de ",
      format_percent_br(package$base_success), " para ", format_percent_br(package$new_success), "."
    ))
  }
  headline = c(headline, switch(
    mode,
    SYNTHETIC = "Atenção: modo SYNTHETIC — resultado demonstrativo; não é evidência sobre a JLTV.",
    HYBRID = "Modo HYBRID: combina dados reais e hipóteses de cenário; leia como projeção condicional.",
    EMPIRICAL = "Modo EMPIRICAL: resultado condicional ao período, à amostra e às hipóteses do modelo.",
    ""
  ))

  max_cr = if (is.data.frame(ahp$consistency) && any(is.finite(ahp$consistency$CR))) {
    max(ahp$consistency$CR, na.rm = TRUE)
  } else NA_real_
  confidence = data.frame(
    item = c(
      "Modo de evidência", "Avaliadores considerados", "Consistência AHP (maior RC dos grupos)",
      "Cobertura das notas P6", "Registros RAM", "Replicações por cenário de melhoria"
    ),
    value = c(
      mode,
      if (is.finite(respondents)) format_number_br(respondents, 0) else "—",
      if (is.finite(max_cr)) format_number_br(max_cr, 3) else "—",
      if (is.finite(p6_coverage)) paste0(format_number_br(p6_coverage, 1), "%") else "—",
      if (isTRUE(data_quality_ready)) "Sem erros bloqueantes" else if (identical(data_quality_ready, FALSE)) "Há erros bloqueantes" else "—",
      if (is.list(improvements)) as.character(improvements$replications) else "—"
    ),
    status = c(
      if (mode == "EMPIRICAL") "OK" else "Atenção",
      if (is.finite(respondents) && respondents >= 5) "OK" else "Atenção",
      if (is.finite(max_cr) && max_cr < 0.10) "OK" else "Atenção",
      if (is.finite(p6_coverage) && p6_coverage >= 70) "OK" else "Atenção",
      if (isTRUE(data_quality_ready)) "OK" else "Atenção",
      if (is.list(improvements) && improvements$replications >= 50) "OK" else "Atenção"
    ),
    note = c(
      "SYNTHETIC/HYBRID não constituem evidência empírica sobre a JLTV.",
      "Painéis de especialistas costumam ter 5 ou mais avaliadores.",
      "RC < 0,10 é o limite usual de Saaty.",
      "Notas N/A não são imputadas; cobertura baixa reduz a confiança no TOPSIS.",
      "Indicadores RAM dependem de OS completas e verificadas.",
      "Mais replicações estreitam o IC95% dos ganhos."
    ),
    stringsAsFactors = FALSE
  )

  list(
    mode = mode, steps = steps, bottlenecks = bottlenecks, contributions = contributions,
    process = process, improvements = ranked, package = package, kpis = kpis,
    headline = headline, confidence = confidence,
    criteria_weights = criteria_weights
  )
}

# ----------------------------------------------------------------------------
# Gráficos (ggplot2). Usados na interface (via plotly) e no relatório Excel.
# ----------------------------------------------------------------------------

wrap_text = function(text, width = 100) {
  if (is.null(text)) return(NULL)
  paste(strwrap(text, width = width), collapse = "\n")
}

assessment_theme = function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = ggplot2::element_text(face = "bold", colour = "#0B1F33"),
      plot.subtitle = ggplot2::element_text(colour = "#526579"),
      plot.caption = ggplot2::element_text(colour = "#526579", hjust = 0),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = "#24384C")
    )
}

chart_bottlenecks = function(contributions, titles = FALSE) {
  if (!is.data.frame(contributions) || nrow(contributions) == 0L) return(NULL)
  criteria = assessment_criterion_labels()
  colors = assessment_criterion_colors()
  long = bind_rows_base(purrr::map(names(criteria), function(cc) {
    data.frame(
      system = paste0(contributions$alternative_id, " ", contributions$alternative_name),
      rank = contributions$rank, coefficient = contributions$coefficient,
      criterion = unname(criteria[[cc]]),
      value = contributions$coefficient * contributions[[paste0("share_", cc)]],
      share = contributions[[paste0("share_", cc)]],
      stringsAsFactors = FALSE
    )
  }))
  levels_system = paste0(contributions$alternative_id, " ", contributions$alternative_name)[order(contributions$coefficient)]
  long$system = factor(long$system, levels = levels_system)
  long$criterion = factor(long$criterion, levels = rev(unname(criteria)))
  long$tooltip = paste0(
    long$system, "<br>Prioridade TOPSIS: ", vapply(long$coefficient, format_number_br, character(1), digits = 3),
    "<br>", long$criterion, ": ", vapply(long$share, format_percent_br, character(1), digits = 0), " da prioridade"
  )
  totals = data.frame(
    system = factor(paste0(contributions$alternative_id, " ", contributions$alternative_name), levels = levels_system),
    coefficient = contributions$coefficient,
    label = paste0("#", contributions$rank, "  ", vapply(contributions$coefficient, format_number_br, character(1), digits = 3)),
    stringsAsFactors = FALSE
  )
  plot = ggplot2::ggplot(long, ggplot2::aes(x = system, y = value, fill = criterion, text = tooltip)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::geom_text(
      data = totals, ggplot2::aes(x = system, y = coefficient, label = label),
      inherit.aes = FALSE, hjust = -0.12, size = 3.4, colour = "#0B1F33"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = stats::setNames(unname(colors[names(criteria)]), unname(criteria)), breaks = unname(criteria)) +
    ggplot2::scale_y_continuous(limits = c(0, max(1, max(contributions$coefficient) * 1.22)), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = "Prioridade TOPSIS (0 = menos crítico, 1 = mais crítico)") +
    assessment_theme()
  if (isTRUE(titles)) {
    plot = plot + ggplot2::labs(
      title = "Quais sistemas mais pesam na indisponibilidade?",
      subtitle = wrap_text("Ranking TOPSIS com pesos AHP. As cores mostram de qual critério vem a prioridade de cada sistema.")
    )
  }
  plot
}

chart_ahp_weights = function(multicriteria, titles = FALSE) {
  weights = if (is.list(multicriteria)) multicriteria$ahp$subcriteria_weights else NULL
  if (!is.data.frame(weights) || nrow(weights) == 0L) return(NULL)
  criteria = assessment_criterion_labels()
  colors = assessment_criterion_colors()
  sub_labels = subcriterion_label_map()
  criterion_weight = tapply(weights$global_weight, weights$parent_criterion, sum)
  legend_labels = paste0(unname(criteria), " (", vapply(criterion_weight[names(criteria)], format_percent_br, character(1), digits = 0), ")")
  data = data.frame(
    subcriterion = unname(sub_labels[weights$subcriterion_id]),
    criterion = factor(
      legend_labels[match(weights$parent_criterion, names(criteria))],
      levels = legend_labels
    ),
    weight = weights$global_weight,
    stringsAsFactors = FALSE
  )
  data$subcriterion = factor(data$subcriterion, levels = data$subcriterion[order(data$weight)])
  data$label = vapply(data$weight, format_percent_br, character(1), digits = 1)
  data$tooltip = paste0(data$subcriterion, "<br>", data$criterion, "<br>Peso global: ", data$label)
  plot = ggplot2::ggplot(data, ggplot2::aes(x = subcriterion, y = weight, fill = criterion, text = tooltip)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::geom_text(ggplot2::aes(label = label), hjust = -0.15, size = 3.3, colour = "#0B1F33") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = stats::setNames(unname(colors[names(criteria)]), legend_labels)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, max(data$weight) * 1.25), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = "Peso global (soma = 100%)") +
    assessment_theme()
  if (isTRUE(titles)) {
    plot = plot + ggplot2::labs(
      title = "O que os especialistas consideram mais importante?",
      subtitle = "Pesos AHP dos subcritérios; entre parênteses, o peso total de cada critério."
    )
  }
  plot
}

chart_downtime = function(process, titles = FALSE) {
  if (!is.data.frame(process) || nrow(process) == 0L || sum(process$hours) <= 0) return(NULL)
  data = process
  data$component = factor(data$component, levels = rev(data$component))
  data$highlight = ifelse(seq_len(nrow(data)) == 1L, "Maior parcela", "Demais")
  data$label = paste0(
    vapply(data$hours, format_number_br, character(1), digits = 1), " h (",
    vapply(data$share, format_percent_br, character(1), digits = 0), ")"
  )
  data$tooltip = paste0(data$component, "<br>", data$label, "<br>Como reduzir: ", data$lever_hint)
  plot = ggplot2::ggplot(data, ggplot2::aes(x = component, y = hours, fill = highlight, text = tooltip)) +
    ggplot2::geom_col(width = 0.7, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = label), hjust = -0.08, size = 3.4, colour = "#0B1F33") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("Maior parcela" = "#B23A48", "Demais" = "#8FA6BC")) +
    ggplot2::scale_y_continuous(limits = c(0, max(data$hours) * 1.35), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = "Viatura-hora parada por missão simulada (média)") +
    assessment_theme()
  if (isTRUE(titles)) {
    plot = plot + ggplot2::labs(
      title = "Onde a viatura perde tempo parada?",
      subtitle = "Decomposição do tempo fora de serviço na situação de referência (simulação)."
    )
  }
  plot
}

chart_improvements = function(ranked, package = NULL, availability_target = NA_real_, titles = FALSE) {
  if (!is.data.frame(ranked) || nrow(ranked) == 0L) return(NULL)
  columns = c("label", "change", "base_availability", "new_availability", "delta_availability", "delta_ci_lower", "delta_ci_upper", "conclusion")
  data = ranked[, columns, drop = FALSE]
  if (is.data.frame(package) && nrow(package) > 0L) data = rbind(package[, columns, drop = FALSE], data)
  data$group = ifelse(
    data$label == (if (is.data.frame(package) && nrow(package) > 0L) package$label[[1]] else ""),
    "Pacote de intervenções",
    ifelse(data$conclusion == "Ganho demonstrado", "Ganho demonstrado", "Ganho não demonstrado")
  )
  data$label = factor(data$label, levels = rev(unique(data$label[order(-data$new_availability)])))
  base = data$base_availability[[1]]
  data$low = base + data$delta_ci_lower
  data$high = base + data$delta_ci_upper
  data$low[!is.finite(data$low)] = data$new_availability[!is.finite(data$low)]
  data$high[!is.finite(data$high)] = data$new_availability[!is.finite(data$high)]
  data$text_label = vapply(data$delta_availability, format_pp_br, character(1))
  data$tooltip = paste0(
    data$label, "<br>", data$change,
    "<br>Disponibilidade: ", vapply(data$base_availability, format_percent_br, character(1)),
    " → ", vapply(data$new_availability, format_percent_br, character(1)),
    "<br>Ganho: ", data$text_label, " (IC95% ",
    vapply(data$delta_ci_lower, format_pp_br, character(1)), " a ",
    vapply(data$delta_ci_upper, format_pp_br, character(1)), ")<br>", data$conclusion
  )
  span = range(c(data$low, data$high, base), na.rm = TRUE)
  pad = max(0.004, diff(span) * 0.25)
  plot = ggplot2::ggplot(data, ggplot2::aes(y = label, x = new_availability, colour = group, text = tooltip)) +
    ggplot2::geom_vline(xintercept = base, colour = "#526579", linewidth = 0.8) +
    ggplot2::geom_segment(ggplot2::aes(x = low, xend = high, yend = label), linewidth = 1.1, alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(shape = group), size = 3.6) +
    ggplot2::geom_text(ggplot2::aes(x = high, label = text_label), hjust = -0.2, size = 3.3, show.legend = FALSE) +
    ggplot2::annotate("text", x = base, y = 0.45, label = paste("Referência:", format_percent_br(base)), hjust = 1.05, size = 3.2, colour = "#526579") +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0.9, 0.6))) +
    ggplot2::scale_colour_manual(values = c("Pacote de intervenções" = "#0B1F33", "Ganho demonstrado" = "#198F8C", "Ganho não demonstrado" = "#9AA9B8")) +
    ggplot2::scale_shape_manual(values = c("Pacote de intervenções" = 18, "Ganho demonstrado" = 16, "Ganho não demonstrado" = 1)) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(span[[1]] - pad * 0.6, span[[2]] + pad)) +
    ggplot2::labs(x = "Disponibilidade operacional simulada (Ao)", y = NULL) +
    assessment_theme() +
    ggplot2::guides(
      colour = ggplot2::guide_legend(nrow = 2, byrow = TRUE),
      shape = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
    ) +
    ggplot2::theme(legend.text = ggplot2::element_text(size = 9), legend.margin = ggplot2::margin(0, 0, 0, 0))
  if (isTRUE(titles)) {
    plot = plot + ggplot2::labs(
      title = "Quanto cada melhoria aumenta a disponibilidade?",
      subtitle = wrap_text("Ponto = disponibilidade com a melhoria; barra = IC95% do ganho; linha cinza = situação de referência."),
      caption = if (is.finite(availability_target)) wrap_text(paste0(
        "Meta de prontidão da missão: ", format_percent_br(availability_target, 0),
        " da frota pronta o tempo todo (M/N). A probabilidade de cumprir a missão aparece na tabela de melhorias."
      ), 110) else NULL
    )
  }
  plot
}

chart_p6_heatmap = function(aggregated, contributions = NULL, titles = FALSE) {
  if (!is.data.frame(aggregated) || nrow(aggregated) == 0L) return(NULL)
  sub_labels = subcriterion_label_map()
  data = aggregated
  data$system = paste0(data$alternative_id, " ", data$alternative_name)
  order_systems = if (is.data.frame(contributions) && nrow(contributions) > 0L) {
    c(paste0(contributions$alternative_id, " ", contributions$alternative_name), setdiff(unique(data$system), paste0(contributions$alternative_id, " ", contributions$alternative_name)))
  } else unique(data$system)
  data$system = factor(data$system, levels = rev(order_systems))
  data$subcriterion = factor(unname(sub_labels[data$subcriterion_id]), levels = unname(sub_labels))
  data$label = vapply(data$value, format_number_br, character(1), digits = 1)
  data$tooltip = paste0(data$system, "<br>", data$subcriterion, "<br>Nota agregada: ", data$label, "<br>Respostas válidas: ", data$n_valid)
  plot = ggplot2::ggplot(data, ggplot2::aes(x = subcriterion, y = system, fill = value, text = tooltip)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3.1, colour = "#0B1F33") +
    ggplot2::scale_fill_gradient(low = "#EAF1F7", high = "#C8505F", limits = c(1, 10), name = "Nota (1–10)") +
    ggplot2::labs(x = NULL, y = NULL) +
    assessment_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1), panel.grid = ggplot2::element_blank(), legend.title = ggplot2::element_text())
  if (isTRUE(titles)) {
    plot = plot + ggplot2::labs(
      title = "Notas P6: quanto cada sistema pesa em cada subcritério",
      subtitle = wrap_text("Nota agregada dos avaliadores (1 = baixa contribuição para a indisponibilidade; 10 = alta). Sistemas na ordem do ranking TOPSIS.")
    )
  }
  plot
}

chart_mission_outcome = function(replications, target_ready, titles = FALSE) {
  if (!is.data.frame(replications) || nrow(replications) == 0L) return(NULL)
  outcomes = if ("mission_success" %in% names(replications)) as.logical(replications$mission_success) else replications$min_ready >= target_ready
  valid = is.finite(replications$min_ready) & !is.na(outcomes)
  if (!any(valid)) return(NULL)
  counts = stats::aggregate(rep(1L, sum(valid)), list(min_ready = replications$min_ready[valid], mission_success = outcomes[valid]), sum)
  names(counts)[3] = "n"
  counts$share = counts$n / sum(counts$n)
  counts$outcome = ifelse(counts$mission_success, "Critério atendido", "Não atendido")
  counts$label = vapply(counts$share, format_percent_br, character(1), digits = 0)
  counts$tooltip = paste0(counts$min_ready, " viaturas prontas no pior momento<br>", counts$outcome, "<br>", counts$label, " das missões (", counts$n, ")")
  success = sum(counts$share[counts$mission_success])
  plot = ggplot2::ggplot(counts, ggplot2::aes(x = factor(min_ready), y = share, fill = outcome, text = tooltip)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = label), position = ggplot2::position_stack(vjust = 0.5), size = 3.4, colour = "#0B1F33") +
    ggplot2::scale_fill_manual(values = c("Critério atendido" = "#198F8C", "Não atendido" = "#B23A48")) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.05), expand = c(0, 0)) +
    ggplot2::labs(
      x = "Viaturas prontas no pior momento da missão",
      y = "Parcela das missões simuladas"
    ) +
    assessment_theme()
  if (isTRUE(titles)) {
    plot = plot + ggplot2::labs(
      title = "No pior momento da missão, quantas viaturas estavam prontas?",
      subtitle = paste0("O critério configurado (M = ", target_ready, "; tolerância incluída) foi atendido em ", format_percent_br(success, 0), " das missões simuladas.")
    )
  }
  plot
}

chart_configuration_availability = function(summary, p_target, titles = FALSE) {
  if (!is.data.frame(summary) || nrow(summary) == 0L) return(NULL)
  data = summary
  data$approved = ifelse(data$ci_lower >= p_target, "Cumpre a meta da missão", "Não cumpre")
  data$configuration_id = factor(data$configuration_id, levels = data$configuration_id[order(data$mean_availability)])
  data$label = vapply(data$mean_availability, format_percent_br, character(1), digits = 1)
  data$tooltip = paste0(
    data$configuration_id, "<br>Disponibilidade média: ", data$label,
    "<br>Probabilidade de cumprir a missão: ", vapply(data$success_probability, format_percent_br, character(1)),
    " (IC95% inferior ", vapply(data$ci_lower, format_percent_br, character(1)), ")"
  )
  ggplot2::ggplot(data, ggplot2::aes(x = configuration_id, y = mean_availability, colour = approved, text = tooltip)) +
    ggplot2::geom_segment(ggplot2::aes(xend = configuration_id, y = min(mean_availability) * 0.98, yend = mean_availability), linewidth = 0.8) +
    ggplot2::geom_point(size = 3) +
    ggplot2::coord_flip() +
    ggplot2::scale_colour_manual(values = c("Cumpre a meta da missão" = "#198F8C", "Não cumpre" = "#B23A48")) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(
      x = NULL, y = "Disponibilidade média simulada",
      caption = "N = frota · D = equipes de diagnóstico · R = equipes de reparo · B = boxes · S = estoque inicial"
    ) +
    assessment_theme()
}

# ----------------------------------------------------------------------------
# Relatório Excel (.xlsx) — abre diretamente no Microsoft Excel.
# ----------------------------------------------------------------------------

excel_friendly_names = function(data, mapping) {
  keep = intersect(names(mapping), names(data))
  out = data[, keep, drop = FALSE]
  names(out) = unname(mapping[keep])
  out
}

write_assessment_workbook = function(path, report) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Pacote openxlsx não instalado. Execute 01_INSTALAR_DEPENDENCIAS.cmd.", call. = FALSE)
  }
  wb = openxlsx::createWorkbook(creator = "JLTV Sustainment Decision Lab")
  openxlsx::modifyBaseFont(wb, fontSize = 10, fontName = "Calibri")
  style_title = openxlsx::createStyle(fontSize = 16, textDecoration = "bold", fontColour = "#0B1F33")
  style_subtitle = openxlsx::createStyle(fontSize = 10, fontColour = "#526579", textDecoration = "italic")
  style_section = openxlsx::createStyle(fontSize = 12, textDecoration = "bold", fontColour = "#1F5A94")
  style_header = openxlsx::createStyle(
    fontColour = "#FFFFFF", fgFill = "#0B1F33", textDecoration = "bold",
    wrapText = TRUE, valign = "center", border = "TopBottomLeftRight", borderColour = "#C7D3DE"
  )
  style_body = openxlsx::createStyle(wrapText = TRUE, valign = "top", border = "TopBottomLeftRight", borderColour = "#DCE4EC")
  style_pct = openxlsx::createStyle(numFmt = "0.0%")
  style_num1 = openxlsx::createStyle(numFmt = "0.0")
  style_num3 = openxlsx::createStyle(numFmt = "0.000")
  style_pp = openxlsx::createStyle(numFmt = "+0.0\" p.p.\";-0.0\" p.p.\";0.0\" p.p.\"")
  style_wrap = openxlsx::createStyle(wrapText = TRUE, valign = "top")
  style_ok = openxlsx::createStyle(fontColour = "#0F5132", fgFill = "#D1E7DD")
  style_warn = openxlsx::createStyle(fontColour = "#664D03", fgFill = "#FFF3CD")
  mode = toupper(report$meta$evidence_mode %||% "HYBRID")
  banner_fill = switch(mode, SYNTHETIC = "#F8D7DA", HYBRID = "#FFF3CD", EMPIRICAL = "#D1E7DD", "#E8EFF6")
  banner_font = switch(mode, SYNTHETIC = "#842029", HYBRID = "#664D03", EMPIRICAL = "#0F5132", "#0B1F33")
  style_banner = openxlsx::createStyle(fgFill = banner_fill, fontColour = banner_font, textDecoration = "bold", wrapText = TRUE)
  banner_text = switch(
    mode,
    SYNTHETIC = "DADOS SINTÉTICOS — NÃO UTILIZAR COMO EVIDÊNCIA SOBRE A JLTV.",
    HYBRID = "MODO HÍBRIDO — PROJEÇÃO CONDICIONAL COM HIPÓTESES EXPLÍCITAS.",
    EMPIRICAL = "MODO EMPÍRICO — INTERPRETAÇÃO CONDICIONAL À QUALIDADE E À AMOSTRA.",
    mode
  )
  image_dir = tempfile("jltv_xlsx_img_")
  dir.create(image_dir)

  add_sheet = function(name, title, subtitle = NULL, widths = c(34, rep(16, 12))) {
    openxlsx::addWorksheet(wb, name, gridLines = FALSE)
    openxlsx::writeData(wb, name, title, startRow = 1, startCol = 1)
    openxlsx::addStyle(wb, name, style_title, rows = 1, cols = 1)
    if (!is.null(subtitle)) {
      openxlsx::writeData(wb, name, subtitle, startRow = 2, startCol = 1)
      openxlsx::addStyle(wb, name, style_subtitle, rows = 2, cols = 1)
    }
    openxlsx::setColWidths(wb, name, cols = seq_along(widths), widths = widths)
    invisible(name)
  }
  write_table = function(sheet, data, row, title = NULL, formats = list(), note = NULL) {
    if (!is.null(title)) {
      openxlsx::writeData(wb, sheet, title, startRow = row, startCol = 1)
      openxlsx::addStyle(wb, sheet, style_section, rows = row, cols = 1)
      row = row + 1L
    }
    if (!is.null(note)) {
      openxlsx::writeData(wb, sheet, note, startRow = row, startCol = 1)
      openxlsx::addStyle(wb, sheet, style_subtitle, rows = row, cols = 1)
      row = row + 1L
    }
    if (!is.data.frame(data) || nrow(data) == 0L) {
      openxlsx::writeData(wb, sheet, "Não disponível nesta execução.", startRow = row, startCol = 1)
      return(row + 2L)
    }
    openxlsx::writeData(wb, sheet, data, startRow = row, startCol = 1, headerStyle = style_header, keepNA = FALSE)
    body_rows = row + seq_len(nrow(data))
    openxlsx::addStyle(wb, sheet, style_body, rows = body_rows, cols = seq_len(ncol(data)), gridExpand = TRUE, stack = TRUE)
    for (column in names(formats)) {
      index = match(column, names(data))
      if (is.na(index)) next
      style = switch(formats[[column]], pct = style_pct, num1 = style_num1, num3 = style_num3, pp = style_pp, NULL)
      if (!is.null(style)) openxlsx::addStyle(wb, sheet, style, rows = body_rows, cols = index, stack = TRUE)
    }
    row + nrow(data) + 2L
  }
  add_chart = function(sheet, plot, row, col, width = 8.5, height = 4.6) {
    if (is.null(plot)) return(invisible(FALSE))
    file = tempfile(tmpdir = image_dir, fileext = ".png")
    ok = tryCatch({
      ggplot2::ggsave(file, plot, width = width, height = height, dpi = 150, bg = "white")
      TRUE
    }, error = function(e) FALSE)
    if (ok && file.exists(file)) {
      openxlsx::insertImage(wb, sheet, file, startRow = row, startCol = col, width = width, height = height, units = "in")
    }
    invisible(ok)
  }

  assessment = report$assessment
  improvements = report$improvements
  criteria = assessment_criterion_labels()

  # 1. Resumo ---------------------------------------------------------------
  add_sheet("Resumo", "JLTV Sustainment Decision Lab — Resultado da avaliação",
            paste0("Gerado em ", report$meta$generated_at, " | versão ", report$meta$version,
                   " | modo de evidência ", mode), widths = c(44, 22, 22, 22, 22, 22, 4))
  openxlsx::writeData(wb, "Resumo", banner_text, startRow = 3, startCol = 1)
  openxlsx::mergeCells(wb, "Resumo", rows = 3, cols = 1:6)
  openxlsx::addStyle(wb, "Resumo", style_banner, rows = 3, cols = 1:6, gridExpand = TRUE)
  openxlsx::writeData(wb, "Resumo", "Principais conclusões", startRow = 5, startCol = 1)
  openxlsx::addStyle(wb, "Resumo", style_section, rows = 5, cols = 1)
  row = 6L
  for (sentence in assessment$headline) {
    openxlsx::writeData(wb, "Resumo", paste("•", sentence), startRow = row, startCol = 1)
    openxlsx::mergeCells(wb, "Resumo", rows = row, cols = 1:6)
    openxlsx::addStyle(wb, "Resumo", style_wrap, rows = row, cols = 1:6, gridExpand = TRUE)
    openxlsx::setRowHeights(wb, "Resumo", rows = row, heights = max(15, 15 * ceiling(nchar(sentence) / 150)))
    row = row + 1L
  }
  row = row + 1L
  k = assessment$kpis
  kpi_table = data.frame(
    Indicador = c(
      "Disponibilidade simulada — situação de referência",
      "Disponibilidade simulada — com melhorias",
      "Ganho de disponibilidade",
      "Disponibilidade observada nos registros (Ao)",
      "Meta de prontidão da missão (M/N)",
      "Probabilidade de cumprir a missão — referência",
      "Probabilidade de cumprir a missão — com melhorias",
      "Prioridade nº 1 (TOPSIS)",
      "Maior parcela do tempo parado"
    ),
    Valor = c(
      format_percent_br(k$ao_reference), format_percent_br(k$ao_improved), format_pp_br(k$ao_gain),
      format_percent_br(k$ao_observed), format_percent_br(k$availability_target, 0),
      format_percent_br(k$success_reference), format_percent_br(k$success_improved),
      if (nzchar(k$top_bottleneck)) paste0(k$top_bottleneck, " — ", k$top_bottleneck_driver) else "—",
      if (nzchar(k$top_process)) paste0(k$top_process, " (", format_percent_br(k$top_process_share, 0), ")") else "—"
    ),
    stringsAsFactors = FALSE
  )
  row = write_table("Resumo", kpi_table, row, "Indicadores-chave")
  bottleneck_table = if (is.data.frame(assessment$bottlenecks) && nrow(assessment$bottlenecks) > 0L) {
    b = assessment$bottlenecks
    data.frame(
      `Posição` = b$rank, Sistema = paste(b$alternative_id, b$alternative_name),
      `Prioridade TOPSIS` = b$coefficient, `Critério que mais pesa` = b$dominant_criterion_label,
      `Subcritério dominante` = b$dominant_subcriterion_label,
      `Citações na P7` = b$p7_mentions %||% NA_integer_,
      `Melhorias relacionadas` = b$suggested_levers %||% "",
      check.names = FALSE, stringsAsFactors = FALSE
    )
  } else data.frame()
  row = write_table("Resumo", bottleneck_table, row, "Gargalos prioritários (especialistas — AHP/TOPSIS)", list(`Prioridade TOPSIS` = "num3"))
  plan = if (is.data.frame(assessment$improvements) && nrow(assessment$improvements) > 0L) {
    r = assessment$improvements
    data.frame(
      Prioridade = r$priority_rank, Melhoria = r$label, `O que muda no cenário` = r$change,
      `Critério AHP atacado` = r$criterion_label,
      `Disponibilidade com a melhoria` = r$new_availability,
      `Ganho (p.p.)` = 100 * r$delta_availability,
      `IC95% inferior (p.p.)` = 100 * r$delta_ci_lower, `IC95% superior (p.p.)` = 100 * r$delta_ci_upper,
      `Prob. de cumprir a missão` = r$new_success,
      Resultado = r$conclusion, `Relação com os gargalos` = r$targets_bottleneck,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  } else data.frame()
  row = write_table(
    "Resumo", plan, row, "Plano de melhorias priorizado (simulação pareada)",
    list(`Disponibilidade com a melhoria` = "pct", `Ganho (p.p.)` = "pp", `IC95% inferior (p.p.)` = "pp",
         `IC95% superior (p.p.)` = "pp", `Prob. de cumprir a missão` = "pct"),
    note = "Prioridade: primeiro as melhorias com ganho demonstrado (IC95% acima de zero); depois, ganho × peso AHP do critério atacado."
  )
  confidence = assessment$confidence
  names(confidence) = c("Item", "Valor", "Situação", "Observação")
  row = write_table("Resumo", confidence, row, "Confiabilidade do resultado")
  for (r in seq_len(nrow(confidence))) {
    openxlsx::addStyle(
      wb, "Resumo", if (confidence$`Situação`[[r]] == "OK") style_ok else style_warn,
      rows = row - nrow(confidence) - 2L + r, cols = 3, stack = TRUE
    )
  }
  chart_row = row + 1L
  if (add_chart("Resumo", chart_bottlenecks(assessment$contributions, titles = TRUE), chart_row, 1, 9, 4.8)) chart_row = chart_row + 26L
  if (add_chart("Resumo", chart_improvements(assessment$improvements, assessment$package, k$availability_target, titles = TRUE), chart_row, 1, 9, 4.8)) chart_row = chart_row + 26L
  add_chart("Resumo", chart_downtime(assessment$process, titles = TRUE), chart_row, 1, 9, 4)

  # 2. Gargalos -------------------------------------------------------------
  add_sheet("Gargalos", "Gargalos — ranking TOPSIS dos sistemas",
            "Coeficiente TOPSIS de 0 a 1: quanto maior, mais o sistema contribui para a indisponibilidade segundo os avaliadores.")
  contributions = assessment$contributions
  gargalos = if (is.data.frame(contributions) && nrow(contributions) > 0L) {
    mentions = report$p7_mentions
    index = if (is.data.frame(mentions)) match(contributions$alternative_id, mentions$alternative_id) else rep(NA_integer_, nrow(contributions))
    data.frame(
      `Posição` = contributions$rank, `Código` = contributions$alternative_id, Sistema = contributions$alternative_name,
      `Prioridade TOPSIS` = contributions$coefficient,
      `Parcela: Falha/Ocorrência` = contributions$share_OCCURRENCE,
      `Parcela: Impacto na disponibilidade` = contributions$share_DOWNTIME,
      `Parcela: Suprimento` = contributions$share_SUPPLY,
      `Parcela: Manutenção e apoio` = contributions$share_SUPPORT,
      `Critério que mais pesa` = contributions$dominant_criterion_label,
      `Subcritério dominante` = contributions$dominant_subcriterion_label,
      `Citações na P7` = if (is.data.frame(mentions)) mentions$p7_mentions[index] else NA,
      `Exemplos na P7` = if (is.data.frame(mentions)) mentions$p7_examples[index] else "",
      check.names = FALSE, stringsAsFactors = FALSE
    )
  } else data.frame()
  end = write_table("Gargalos", gargalos, 4L, NULL, list(
    `Prioridade TOPSIS` = "num3", `Parcela: Falha/Ocorrência` = "pct", `Parcela: Impacto na disponibilidade` = "pct",
    `Parcela: Suprimento` = "pct", `Parcela: Manutenção e apoio` = "pct"
  ))
  if (nrow(gargalos) > 0L) {
    openxlsx::conditionalFormatting(wb, "Gargalos", cols = 4, rows = 5:(4 + nrow(gargalos)), type = "databar", style = "#C8505F")
    openxlsx::freezePane(wb, "Gargalos", firstActiveRow = 5, firstActiveCol = 4)
  }
  openxlsx::writeData(wb, "Gargalos", "Parcela = peso AHP do critério × posição do sistema nesse critério (0 = melhor sistema, 1 = pior), normalizada para somar 100%.", startRow = end, startCol = 1)
  openxlsx::addStyle(wb, "Gargalos", style_subtitle, rows = end, cols = 1)
  add_chart("Gargalos", chart_bottlenecks(contributions, titles = TRUE), end + 2L, 1, 9, 5)
  add_chart("Gargalos", chart_p6_heatmap(report$multicriteria$aggregated_scores, contributions, titles = TRUE), end + 2L, 8, 10, 5.5)

  # 3. Melhorias ------------------------------------------------------------
  add_sheet("Melhorias", "Melhorias testadas na simulação",
            if (is.list(improvements)) paste0(
              "Situação de referência: ", configuration_id(improvements$config), " | horizonte ",
              format_number_br(improvements$horizon, 0), " h | meta M = ", improvements$target_ready,
              " | ", improvements$replications, " replicações com as mesmas sementes (início ", improvements$seed, ")"
            ) else "Teste de melhorias ainda não executado.")
  detail = if (is.data.frame(assessment$improvements) && nrow(assessment$improvements) > 0L) {
    r = assessment$improvements
    if (is.data.frame(assessment$package) && nrow(assessment$package) > 0L) {
      p = assessment$package
      p$criterion_label = "Vários"; p$ahp_weight = NA_real_; p$priority_index = NA_real_
      p$example_actions = ""; p$downtime_component = "Vários"; p$targets_bottleneck = ""; p$priority_rank = NA_integer_
      r = rbind(r[, intersect(names(r), names(p))], p[, intersect(names(r), names(p))])
    }
    data.frame(
      Prioridade = r$priority_rank, Melhoria = r$label, `O que muda no cenário` = r$change,
      `Critério AHP atacado` = r$criterion_label, `Peso AHP do critério` = r$ahp_weight,
      `Componente do tempo parado` = r$downtime_component,
      `Disponibilidade — referência` = r$base_availability, `Disponibilidade — com melhoria` = r$new_availability,
      `Ganho (p.p.)` = 100 * r$delta_availability, `IC95% inferior (p.p.)` = 100 * r$delta_ci_lower,
      `IC95% superior (p.p.)` = 100 * r$delta_ci_upper,
      `Prob. missão — referência` = r$base_success, `Prob. missão — com melhoria` = r$new_success,
      `Horas abaixo de M — referência` = r$base_hours_below, `Horas abaixo de M — com melhoria` = r$new_hours_below,
      Resultado = r$conclusion, `Índice de prioridade` = r$priority_index,
      `Relação com os gargalos` = r$targets_bottleneck, `Exemplos de ação (a validar)` = r$example_actions,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  } else data.frame()
  end = write_table("Melhorias", detail, 4L, NULL, list(
    `Peso AHP do critério` = "pct", `Disponibilidade — referência` = "pct", `Disponibilidade — com melhoria` = "pct",
    `Ganho (p.p.)` = "pp", `IC95% inferior (p.p.)` = "pp", `IC95% superior (p.p.)` = "pp",
    `Prob. missão — referência` = "pct", `Prob. missão — com melhoria` = "pct",
    `Horas abaixo de M — referência` = "num1", `Horas abaixo de M — com melhoria` = "num1", `Índice de prioridade` = "num1"
  ))
  if (nrow(detail) > 0L) {
    openxlsx::setColWidths(wb, "Melhorias", cols = 1:19, widths = c(10, 34, 44, 22, 12, 22, 14, 14, 12, 12, 12, 14, 14, 14, 14, 20, 12, 30, 60))
    openxlsx::conditionalFormatting(wb, "Melhorias", cols = 9, rows = 5:(4 + nrow(detail)), type = "databar", style = "#198F8C")
    openxlsx::freezePane(wb, "Melhorias", firstActiveRow = 5, firstActiveCol = 3)
  }
  add_chart("Melhorias", chart_improvements(assessment$improvements, assessment$package, assessment$kpis$availability_target, titles = TRUE), end + 1L, 2, 9.5, 5)
  process_table = if (is.data.frame(assessment$process) && nrow(assessment$process) > 0L) {
    data.frame(
      `Etapa` = assessment$process$component, `Horas paradas (média)` = assessment$process$hours,
      `Parcela` = assessment$process$share, `Parcela acumulada` = assessment$process$cumulative_share,
      `Como reduzir` = assessment$process$lever_hint, check.names = FALSE, stringsAsFactors = FALSE
    )
  } else data.frame()
  write_table("Melhorias", process_table, end + 28L, "Onde a viatura perde tempo parada (situação de referência)",
              list(`Horas paradas (média)` = "num1", Parcela = "pct", `Parcela acumulada` = "pct"))

  # 4. Pesos AHP ------------------------------------------------------------
  add_sheet("Pesos AHP", "Pesos AHP — importância dos critérios e subcritérios")
  mc = report$multicriteria
  weights = if (is.list(mc) && is.data.frame(mc$ahp$subcriteria_weights)) {
    w = mc$ahp$subcriteria_weights
    data.frame(
      `Critério` = unname(criteria[w$parent_criterion]),
      `Subcritério` = unname(subcriterion_label_map()[w$subcriterion_id]),
      `Peso do critério` = w$criterion_weight, `Peso local` = w$local_weight, `Peso global` = w$global_weight,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  } else data.frame()
  end = write_table("Pesos AHP", weights, 4L, NULL, list(`Peso do critério` = "pct", `Peso local` = "pct", `Peso global` = "pct"))
  if (nrow(weights) > 0L) openxlsx::conditionalFormatting(wb, "Pesos AHP", cols = 5, rows = 5:(4 + nrow(weights)), type = "databar", style = "#1F5A94")
  consistency = if (is.list(mc) && is.data.frame(mc$ahp$consistency)) {
    x = mc$ahp$consistency
    data.frame(Grupo = x$hierarchy_group, `Estimável` = ifelse(x$estimable, "sim", "não"), Avaliadores = x$respondents,
               `RC (razão de consistência)` = x$CR, `Situação` = x$status, check.names = FALSE, stringsAsFactors = FALSE)
  } else data.frame()
  end = write_table("Pesos AHP", consistency, end, "Consistência das matrizes agregadas", list(`RC (razão de consistência)` = "num3"))
  individual = if (is.list(mc) && is.data.frame(mc$ahp$individual_diagnostics)) {
    x = mc$ahp$individual_diagnostics
    data.frame(Avaliador = x$respondent_id, Grupo = x$hierarchy_group, Completo = ifelse(x$complete, "sim", "não"),
               RC = x$CR, Consistente = ifelse(x$consistent, "sim", "não"), check.names = FALSE, stringsAsFactors = FALSE)
  } else data.frame()
  add_chart("Pesos AHP", chart_ahp_weights(mc, titles = TRUE), 4L, 7, 8.5, 4.8)
  write_table("Pesos AHP", individual, end, "Consistência por avaliador", list(RC = "num3"))

  # 4b. Robustez AHP–TOPSIS --------------------------------------------------
  robust_sheet = "Robustez AHP-TOPSIS"
  add_sheet(robust_sheet, "AHP–TOPSIS — consistência, dispersão e robustez do ranking",
            "Histogramas e reamostragem dos avaliadores. O ranking pode mudar com pesos, normalização e amostra (Sałabun et al., 2020).",
            widths = c(34, rep(18, 10)))
  stability = if (is.list(mc)) tryCatch(mc_rank_stability(mc), error = function(e) NULL) else NULL
  sensitivity = if (is.list(mc)) tryCatch(mc_method_sensitivity(mc), error = function(e) NULL) else NULL
  robust_row = 4L
  for (line in mc_robustness_text(mc, stability, sensitivity)) {
    openxlsx::writeData(wb, robust_sheet, paste("•", line), startRow = robust_row, startCol = 1)
    robust_row = robust_row + 1L
  }
  robust_row = write_table(robust_sheet, mc_stability_display(stability), robust_row + 1L,
                           "Estabilidade da posição de cada sistema (reamostragem dos avaliadores)")
  robust_row = write_table(robust_sheet, mc_method_sensitivity_display(sensitivity), robust_row,
                           "Sensibilidade a pesos e à normalização (posição no ranking)")
  add_chart(robust_sheet, chart_ahp_cr_histogram(mc, titles = TRUE), robust_row, 1, 7, 4)
  add_chart(robust_sheet, chart_ahp_weight_dispersion(mc, titles = TRUE), robust_row, 8, 7, 4)
  add_chart(robust_sheet, chart_p6_histogram(mc, titles = TRUE), robust_row + 22L, 1, 7, 4)
  add_chart(robust_sheet, chart_p6_boxplot_by_system(mc, titles = TRUE), robust_row + 22L, 8, 7, 4)
  add_chart(robust_sheet, chart_rank_probability(stability, titles = TRUE), robust_row + 44L, 1, 9, 4.6)

  # 5. Notas P6 -------------------------------------------------------------
  add_sheet("Notas P6", "Notas P6 agregadas — sistema × subcritério",
            "Nota 1–10 (alta = maior contribuição para a indisponibilidade). N/A não é imputado.", widths = c(30, rep(14, 11)))
  aggregated = if (is.list(mc)) mc$aggregated_scores else NULL
  if (is.data.frame(aggregated) && nrow(aggregated) > 0L) {
    sub_labels = subcriterion_label_map()
    systems = unique(paste(aggregated$alternative_id, aggregated$alternative_name))
    if (is.data.frame(contributions) && nrow(contributions) > 0L) {
      ordered = paste(contributions$alternative_id, contributions$alternative_name)
      systems = c(ordered, setdiff(systems, ordered))
    }
    matrix_data = data.frame(Sistema = systems, stringsAsFactors = FALSE)
    for (sub in names(sub_labels)) {
      cells = aggregated[aggregated$subcriterion_id == sub, , drop = FALSE]
      matrix_data[[unname(sub_labels[[sub]])]] = cells$value[match(systems, paste(cells$alternative_id, cells$alternative_name))]
    }
    end = write_table("Notas P6", matrix_data, 4L, NULL, stats::setNames(as.list(rep("num1", length(sub_labels))), unname(sub_labels)))
    openxlsx::conditionalFormatting(
      wb, "Notas P6", cols = 2:(1 + length(sub_labels)), rows = 5:(4 + nrow(matrix_data)),
      style = c("#EAF1F7", "#F3D9A4", "#C8505F"), rule = c(1, 5.5, 10), type = "colourScale"
    )
    coverage = data.frame(
      Sistema = paste(aggregated$alternative_id, aggregated$alternative_name),
      `Subcritério` = unname(sub_labels[aggregated$subcriterion_id]),
      `Nota agregada` = aggregated$value, `Média` = aggregated$mean, `Mediana` = aggregated$median,
      `Desvio-padrão` = aggregated$sd, `Respostas válidas` = aggregated$n_valid, Cobertura = aggregated$coverage,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    write_table("Notas P6", coverage, end, "Estatísticas por célula", list(`Nota agregada` = "num1", `Média` = "num1", `Mediana` = "num1", `Desvio-padrão` = "num1", Cobertura = "pct"))
  } else {
    write_table("Notas P6", data.frame(), 4L)
  }

  # 6. P7 --------------------------------------------------------------------
  add_sheet("P7 Evidências", "P7 — onde buscar a evidência (rastreabilidade)",
            "P7 aponta peças, ocorrências e registros a confirmar; não altera pesos nem notas.", widths = c(30, 28, 40, 40, 40, 16))
  end = write_table("P7 Evidências", report$p7_summary, 4L, "Síntese por sistema")
  factors = report$factors
  factor_table = if (is.data.frame(factors) && nrow(factors) > 0L) {
    data.frame(Sistema = factors$related_system, `Peça/componente (PN/NSN)` = factors$identification,
               `O que ocorre` = factors$occurrence_detail, `Registro para confirmação` = factors$evidence_reference,
               Origem = factors$data_source, check.names = FALSE, stringsAsFactors = FALSE)
  } else data.frame()
  write_table("P7 Evidências", factor_table, end, "Respostas P7")

  # 7. Simulação -------------------------------------------------------------
  add_sheet("Simulação", "Simulação da frota — configurações avaliadas", widths = c(18, rep(14, 30)))
  simulation = report$simulation
  sim_table = if (is.list(simulation) && is.data.frame(simulation$summary)) {
    s = simulation$summary
    data.frame(
      `Configuração` = s$configuration_id, Frota = s$fleet_size, `Eq. diagnóstico` = s$diagnostic_teams,
      `Eq. reparo` = s$repair_teams, Boxes = s$bays, Estoque = s$initial_stock,
      `Prob. de cumprir a missão` = s$success_probability, `IC95% inferior` = s$ci_lower, `IC95% superior` = s$ci_upper,
      `Disponibilidade média` = s$mean_availability, `Mín. de prontas (média)` = s$mean_min_ready,
      `Horas abaixo de M` = s$mean_hours_below_target, `Falhas por missão` = s$mean_failures,
      `Viatura-hora parada` = s$mean_downtime_asset_hours, `Custo esperado (R$)` = s$expected_cost,
      `Cumpre a meta` = ifelse(s$ci_lower >= simulation$p_target, "sim", "não"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  } else data.frame()
  end = write_table("Simulação", sim_table, 4L, NULL, list(
    `Prob. de cumprir a missão` = "pct", `IC95% inferior` = "pct", `IC95% superior` = "pct",
    `Disponibilidade média` = "pct", `Mín. de prontas (média)` = "num1", `Horas abaixo de M` = "num1",
    `Falhas por missão` = "num1", `Viatura-hora parada` = "num1"
  ))
  if (nrow(sim_table) > 0L) {
    openxlsx::freezePane(wb, "Simulação", firstActiveRow = 5, firstActiveCol = 2)
    add_chart("Simulação", chart_configuration_availability(simulation$summary, simulation$p_target) +
                ggplot2::labs(title = "Disponibilidade média por configuração"), end + 1L, 1, 9, 5)
    if (is.data.frame(simulation$recommended_replications) && nrow(simulation$recommended_replications) > 0L) {
      add_chart("Simulação", chart_mission_outcome(simulation$recommended_replications, simulation$target_ready, titles = TRUE), end + 1L, 9, 8, 4.5)
    }
  }

  # 8. RAM observado ---------------------------------------------------------
  add_sheet("RAM observado", "Indicadores RAM observados nos registros", widths = c(28, 16, 26, 60, 12, 50))
  end = write_table("RAM observado", report$ram_metrics, 4L)
  end = write_table("RAM observado", report$observed_downtime, end, "Horas registradas por componente", list(hours = "num1"))
  write_table("RAM observado", report$quality_summary, end, "Qualidade dos dados")

  # 9. Premissas -------------------------------------------------------------
  add_sheet("Premissas", "Premissas, parâmetros e auditoria", widths = c(46, 60, 80))
  end = write_table("Premissas", report$assumptions, 4L)
  limits = data.frame(
    `Limites de interpretação` = c(
      "Resultados são condicionais aos dados, às distribuições e às hipóteses informadas.",
      "AHP/TOPSIS priorizam investigação; não substituem registros físicos de RAM.",
      "Nota de especialista nunca vira MTBF ou MTTR; P7 indica onde buscar a evidência.",
      "A simulação testa cada melhoria no nível da frota; o efeito restrito a um sistema exige registros por sistema.",
      "As magnitudes das melhorias são hipóteses de cenário, não metas aprovadas.",
      "Ganho 'demonstrado' significa IC95% da diferença pareada acima de zero.",
      "Prontidão instantânea não é disponibilidade operacional Ao."
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  end = write_table("Premissas", limits, end)
  write_table("Premissas", simulation_framing_table(), end, "Enquadramento do simulador (classificação clássica dos modelos de simulação: Taha, 2008; Prado, 2014)")

  openxlsx::activeSheet(wb) = 1
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  unlink(image_dir, recursive = TRUE)
  invisible(path)
}

# ----------------------------------------------------------------------------
# Questionário de demonstração (SYNTHETIC) — para conhecer o painel antes das
# respostas reais. Fica identificado como SYNTHETIC_DEMO, é excluído do modo
# EMPIRICAL e pode ser removido a qualquer momento.
# ----------------------------------------------------------------------------
synthetic_questionnaire_demo_raw = function(n = 24L, seed = 20261003L) {
  has_seed = exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (has_seed) old_seed = get(".Random.seed", envir = globalenv())
  on.exit({
    if (has_seed) assign(".Random.seed", old_seed, envir = globalenv()) else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  set.seed(seed)
  pair = questionnaire_pair_spec()
  alternatives = questionnaire_alternative_spec()
  p6_map = questionnaire_p6_subcriteria()
  reference_weight = c(
    OCCURRENCE = 0.24, DOWNTIME = 0.26, SUPPLY = 0.34, SUPPORT = 0.16,
    FREQUENCY = 0.45, RECURRENCE = 0.35, FLEET_SCOPE = 0.20,
    TOTAL_OUT_OF_SERVICE = 0.60, EMPLOYMENT_IMPACT = 0.40,
    SPARE_DEPENDENCY = 0.40, STOCKOUT = 0.35, LEAD_TIME = 0.25,
    ACTIVE_MAINTENANCE = 0.55, EXTERNAL_SUPPORT = 0.45
  )
  system_level = c(A1 = 5.5, A2 = 6, A3 = 7.5, A4 = 4.5, A5 = 5, A6 = 5, A7 = 6, A8 = 4.5, A9 = 7, A10 = 3.5)
  emphasis = matrix(0, nrow = 10, ncol = 10, dimnames = list(names(alternatives), unname(p6_map)))
  emphasis["A3", c("SPARE_DEPENDENCY", "STOCKOUT", "LEAD_TIME")] = 1.5
  emphasis["A9", c("LEAD_TIME", "EXTERNAL_SUPPORT", "SPARE_DEPENDENCY")] = 1.8
  emphasis["A2", c("ACTIVE_MAINTENANCE", "TOTAL_OUT_OF_SERVICE")] = 1.2
  emphasis["A7", c("FREQUENCY", "RECURRENCE")] = 1.6
  headers = c(
    "Carimbo de data/hora", "a. E-mail e celular", "b. Idade", "c. Formação acadêmica",
    "d. Formação militar", "e. Qual o seu tempo de serviço", "f. Qual sua experiência com a JLTV",
    paste(pair$code, "comparação"),
    unlist(purrr::map(1:10, function(p) paste0("P6.", p, " avaliação [", names(alternatives), " ", alternatives, "]"))),
    unlist(purrr::map(1:4, function(b) paste0(b, c(". Sistema/conjunto", ". Descrição e PN/NSN", ". O que ocorre", ". OS, requisição"))))
  )
  ahp_text = function(j) {
    ratio = reference_weight[[pair$left_item[[j]]]] / reference_weight[[pair$right_item[[j]]]] * exp(stats::rnorm(1, 0, 0.18))
    k = max(1L, min(9L, as.integer(round(if (ratio >= 1) ratio else 1 / ratio))))
    if (k == 1L) "1 — importância equivalente" else if (ratio >= 1) {
      paste0(k, " — ", pair$left_label[[j]], " mais importante")
    } else paste0(k, " — ", pair$right_label[[j]], " mais importante")
  }
  p7_examples = list(
    c("A3 Driveline", "Junta homocinética / semieixo (PN a confirmar)", "Desgaste e quebra em terreno severo", "OS e requisições de suprimento"),
    c("A9 DSDU/MUX", "Módulo eletrônico (PN a confirmar)", "Falha intermitente; aguarda peça importada", "Requisições e registros de apoio externo"),
    c("A7 CTIS/rodas/pneus", "Válvula do CTIS (PN a confirmar)", "Vazamento recorrente", "OS corretivas"),
    c("A2 Transmissão/transaxle", "Conjunto de embreagem (PN a confirmar)", "Reparo demorado no escalão", "OS e fichas de manutenção")
  )
  rows = purrr::map(seq_len(as.integer(n)), function(id) {
    scores = purrr::map(names(alternatives), function(code) {
      purrr::map_chr(unname(p6_map), function(sub) {
        if (stats::runif(1) < 0.05) return("N/A")
        value = system_level[[code]] + emphasis[code, sub] + stats::rnorm(1, 0, 1.1)
        as.character(max(1L, min(10L, as.integer(round(value)))))
      })
    })
    p6 = unlist(purrr::map(1:10, function(p) purrr::map_chr(scores, function(s) s[[p]])))
    p7 = unlist(purrr::map(1:4, function(b) {
      if (stats::runif(1) < 0.45) p7_examples[[sample.int(length(p7_examples), 1)]] else rep("", 4)
    }))
    values = c(
      sprintf("2026-10-%02d 09:%02d:00", (id %% 28) + 1, id %% 60),
      paste0("TESTE SINTÉTICO demo", id, "@teste.invalid"),
      as.character(sample(26:52, 1)), "Engenharia", "Formação militar",
      paste(sample(5:30, 1), "anos"),
      sample(c("Operador", "Manutenção", "Suprimento", "Comando"), 1),
      purrr::map_chr(seq_len(nrow(pair)), ahp_text),
      p6, p7
    )
    stats::setNames(as.list(values), headers)
  })
  as.data.frame(
    do.call(rbind, purrr::map(rows, function(x) as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE))),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

remove_synthetic_questionnaire_demo = function(path) {
  connection = open_database(path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  removed = 0L
  DBI::dbWithTransaction(connection, {
    for (table in c("questionnaire_factors", "topsis_scores", "ahp_pairwise", "questionnaire_respondents")) {
      removed = removed + DBI::dbExecute(connection, paste0("DELETE FROM ", table, " WHERE data_source = 'SYNTHETIC_DEMO'"))
    }
  })
  invisible(removed)
}

load_synthetic_questionnaire_demo = function(path, n = 24L, seed = 20261003L) {
  normalized = transform_google_forms_questionnaire(synthetic_questionnaire_demo_raw(n, seed), evidence_class = "SYNTHETIC")
  for (table in c("questionnaire_respondents", "ahp_pairwise", "topsis_scores", "questionnaire_factors")) {
    if (nrow(normalized[[table]]) > 0L) normalized[[table]]$data_source = "SYNTHETIC_DEMO"
  }
  remove_synthetic_questionnaire_demo(path)
  append_questionnaire_tables(path, normalized)
  invisible(nrow(normalized$questionnaire_respondents))
}

# Abre um arquivo ou pasta no programa padrão do sistema (Excel, no Windows).
open_file_with_system = function(path) {
  if (!file.exists(path)) return(FALSE)
  tryCatch({
    if (.Platform$OS.type == "windows") {
      get("shell.exec", envir = baseenv())(normalizePath(path, winslash = "\\"))
      TRUE
    } else if (identical(Sys.info()[["sysname"]], "Darwin")) {
      system2("open", shQuote(path), wait = FALSE)
      TRUE
    } else if (nzchar(Sys.which("xdg-open"))) {
      system2("xdg-open", shQuote(path), wait = FALSE)
      TRUE
    } else FALSE
  }, error = function(e) FALSE)
}

# Extensão física por componente; preserva o motor agregado e o esquema SQLite 4.

# ========================================================================
# INICIO DO MODULO: jltv_systems.R
# ========================================================================
# JLTV — extensão SYS-1: parâmetros e eventos por componente.
# Implementação própria. Referências externas orientam estrutura, nunca valores JLTV.
JLTV_SYSTEMS_VERSION = 'SYS-1 / JLTV Sustainment Decision Lab 1.5.7 (2026-10-06)'

# ========================================================================
# INICIO DO MODULO: jltv_enhancements.R
# ========================================================================
# Diagnósticos pós-simulação. Nenhuma função altera o motor, sementes ou AHP–TOPSIS.
jltv_new_package_map = function() c(
  shinyWidgets='Busca do alvo na aba 3', reactable='Tabela navegável de sistemas na aba 4',
  data.table='Conciliação de horas por sistema', dplyr='Resumo das replicações por cenário',
  tidyr='Preparação das séries do gráfico comparativo', viridisLite='Paleta legível do diagnóstico',
  patchwork='Composição dos gráficos exportáveis', svglite='Exportação vetorial SVG',
  gganimate='Quadros de prontidão ao longo da missão', gifski='Renderização do GIF',
  survival='Curva censurada do tempo até primeira pane', readr='Exportação CSV da auditoria',
  shinycssloaders='Indicador de processamento do diagnóstico',
  shinyvalidate='Validação imediata das entradas da simulação',
  pointblank='Checagem tabular das horas da auditoria',
  ggdist='Distribuição de Ao entre execuções',
  ggrepel='Rótulo de censura sem colisão na curva',
  rsvg='Exportação PNG do gráfico vetorial',
  rsample='Faixa bootstrap descritiva de Ao',
  binom='Conferência independente do intervalo Wilson',
  thematic='Harmonização dos gráficos Shiny com o tema',
  ggtext='Títulos formatados para leitura dos gráficos'
)

jltv_stage_audit = function(result) {
  expected = result$systems[,c('system_id','system_name','down_h'),drop=FALSE]
  expected$expected_hours = expected$down_h * length(result$seeds)
  if (nrow(result$stage_logs)) {
    x = data.table::as.data.table(result$stage_logs)
    sums = x[,.(logged_hours=sum(hours)),by=system_id]
    expected$logged_hours = sums$logged_hours[match(expected$system_id,sums$system_id)]
  } else expected$logged_hours = 0
  expected$logged_hours[is.na(expected$logged_hours)] = 0
  expected$difference_h = expected$logged_hours - expected$expected_hours
  expected$consistent = abs(expected$difference_h) <= 1e-7*pmax(1,expected$expected_hours)
  expected[,c('system_id','system_name','logged_hours','expected_hours','difference_h','consistent')]
}

jltv_scenario_overview = function(result) {
  x = dplyr::as_tibble(result$replications)
  as.data.frame(dplyr::summarise(dplyr::group_by(x,.data$scenario),
    Ao_mean=mean(.data$Ao), mission_rate=mean(as.numeric(.data$mission_success)),
    n=dplyr::n(),.groups='drop'))
}

jltv_first_failure_data = function(result) {
  assets = paste0('V',seq_len(as.integer(result$config$fleet_size)))
  ev = result$events[result$events$event=='FAILURE',c('time','asset_id'),drop=FALSE]
  first = if(nrow(ev)) stats::aggregate(ev$time,list(asset_id=ev$asset_id),min) else data.frame(asset_id=character(),x=numeric())
  time = first$x[match(assets,first$asset_id)]
  failed = is.finite(time)
  data.frame(asset_id=assets,time=ifelse(failed,time,result$horizon),failed=failed)
}

jltv_first_failure_fit = function(result) {
  x = jltv_first_failure_data(result)
  survival::survfit(survival::Surv(x$time,x$failed)~1)
}

jltv_quality_agent = function(result) {
  x = jltv_stage_audit(result)
  agent = pointblank::create_agent(tbl=x)
  agent = pointblank::col_vals_gte(agent,columns='logged_hours',value=0)
  agent = pointblank::col_vals_gte(agent,columns='expected_hours',value=0)
  isTRUE(pointblank::all_passed(pointblank::interrogate(agent))) && all(x$consistent)
}

jltv_wilson_audit = function(result) {
  baseline = result$baseline
  interval = binom::binom.confint(sum(baseline$mission_success),nrow(baseline),methods='wilson')
  isTRUE(all.equal(as.numeric(interval[1,c('mean','lower','upper')]),
    as.numeric(result$baseline_mission),tolerance=1e-10))
}

jltv_bootstrap_ao = function(result,times=200L) {
  if(nrow(result$baseline)<10L) return(c(NA_real_,NA_real_))
  had_seed = exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE)
  if(had_seed) previous_seed = get('.Random.seed',envir=.GlobalEnv)
  on.exit(if(had_seed) assign('.Random.seed',previous_seed,envir=.GlobalEnv) else if(exists('.Random.seed',envir=.GlobalEnv)) rm('.Random.seed',envir=.GlobalEnv))
  set.seed(as.integer(result$seeds[1]))
  draws = rsample::bootstraps(result$baseline,times=times)
  means = vapply(draws$splits,function(split)mean(rsample::analysis(split)$Ao),numeric(1))
  unname(stats::quantile(means,c(.025,.975)))
}

jltv_diagnostic_chart = function(result) {
  x = jltv_scenario_overview(result)
  long = tidyr::pivot_longer(x,cols=c('Ao_mean','mission_rate'),names_to='measure',values_to='fraction')
  long$measure = factor(long$measure,levels=c('Ao_mean','mission_rate'),labels=c('Disponibilidade média','Missão atendida'))
  palette = viridisLite::viridis(max(2L,nrow(x)),option='D')
  p1 = ggplot2::ggplot(long,ggplot2::aes(x=.data$scenario,y=100*.data$fraction,fill=.data$scenario))+
    ggplot2::geom_col(width=.65)+ggplot2::facet_wrap(~measure)+
    ggplot2::scale_fill_manual(values=stats::setNames(palette[seq_len(nrow(x))],x$scenario))+
    ggplot2::labs(x='Cenário',y='% das execuções ou do tempo',title='<b>Referência e intervenções</b>')+
    ggplot2::theme_minimal()+ggplot2::theme(legend.position='none',plot.title=ggtext::element_markdown(),axis.text.x=ggplot2::element_text(angle=30,hjust=1))
  fit = jltv_first_failure_fit(result)
  curve = data.frame(time=c(0,fit$time),survival=c(1,fit$surv))
  p2 = ggplot2::ggplot(curve,ggplot2::aes(x=.data$time,y=.data$survival))+
    ggplot2::geom_step(colour=palette[2],linewidth=.9)+ggplot2::coord_cartesian(ylim=c(0,1),xlim=c(0,result$horizon))+
    ggrepel::geom_label_repel(data=data.frame(time=tail(curve$time,1),survival=tail(curve$survival,1),
      label=paste('Sem pane:',sum(!jltv_first_failure_data(result)$failed))),ggplot2::aes(label=.data$label),max.overlaps=Inf)+
    ggplot2::labs(x='Horas até a primeira pane',y='Fração sem pane',title='Primeira execução: curva censurada')+ggplot2::theme_minimal()
  p3 = ggplot2::ggplot(result$replications,ggplot2::aes(x=100*.data$Ao,y=.data$scenario,fill=.data$scenario))+
    ggdist::stat_halfeye(adjust=1,point_interval='median_qi',.width=.8,alpha=.7)+
    ggplot2::scale_fill_manual(values=stats::setNames(palette[seq_len(nrow(x))],x$scenario))+
    ggplot2::labs(x='Disponibilidade por execução (%)',y=NULL,title='Distribuição condicional de Ao')+
    ggplot2::theme_minimal()+ggplot2::theme(legend.position='none')
  patchwork::wrap_plots(p1,p2,p3,ncol=1,heights=c(1.2,.9,.9))
}

jltv_export_svg = function(result,file) {
  svglite::svglite(file,width=10,height=11)
  on.exit(grDevices::dev.off(),add=TRUE)
  print(jltv_diagnostic_chart(result))
  invisible(file)
}

jltv_export_png = function(result,file) {
  svg = tempfile(fileext='.svg')
  on.exit(unlink(svg),add=TRUE)
  jltv_export_svg(result,svg)
  rsvg::rsvg_png(svg,file=file,width=1100)
  invisible(file)
}

jltv_export_audit_csv = function(result,file) {
  readr::write_csv(jltv_stage_audit(result),file,na='')
  invisible(file)
}

jltv_export_animation = function(result,file) {
  ev = result$events
  if(nrow(ev)>58L) ev=ev[unique(round(seq(1,nrow(ev),length.out=58L))),,drop=FALSE]
  fleet = as.integer(result$config$fleet_size)
  d = data.frame(frame=seq_len(nrow(ev)+2L),
    time=c(0,ev$time,result$horizon),ready=c(fleet,ev$ready,if(nrow(ev))tail(ev$ready,1) else fleet))
  d$down = fleet-d$ready
  frame = tidyr::pivot_longer(d,cols=c('ready','down'),names_to='state',values_to='vehicles')
  frame$state = factor(frame$state,levels=c('ready','down'),labels=c('Prontas','Paradas'))
  p = ggplot2::ggplot(frame,ggplot2::aes(x=.data$state,y=.data$vehicles,fill=.data$state))+
    ggplot2::geom_col(width=.65)+ggplot2::scale_fill_manual(values=c('Prontas'='#198F8C','Paradas'='#C66454'))+
    ggplot2::coord_cartesian(ylim=c(0,fleet))+
    ggplot2::labs(title='Prontidão simulada — primeira execução',subtitle='Quadro {current_frame}: avance pelos eventos',x=NULL,y='Viaturas')+
    ggplot2::theme_minimal()+ggplot2::theme(legend.position='none')+
    gganimate::transition_manual(frame)
  # gifski é o codificador empregado pelo renderer do gganimate.
  gganimate::animate(p,nframes=nrow(d),fps=2,width=640,height=360,units='px',
    renderer=gganimate::gifski_renderer(file))
  invisible(file)
}

# FIM DO MODULO: jltv_enhancements.R

sys_systems = function() c(A1='Motor/comb.', A2='Transmissão/transaxle', A3='Driveline',
  A4='Cooling package', A5='Suspensão/direção', A6='Freios/pneumático',
  A7='CTIS/rodas/pneus', A8='SISTEMA ELÉTRICO/ ELETRÔNICO', A9='DSDU/MUX', A10='Auxiliares/missão')
sys_demo_profiles = function() {
  data.frame(system_id=names(sys_systems()), component_id=paste0(names(sys_systems()),'_DEMO'),
    component_name=paste('Item demonstrativo',unname(sys_systems())),
    failure_mode='Falha funcional demonstrativa', part_id=paste0('PN_DEMO_',1:10),
    life_distribution='exponential', life_h=1800, diagnosis_h=2,
    repair_h=c(8,8,14,8,8,8,10,8,12,8), test_h=1, time_cv=0.3,
    part_probability=0.5, initial_stock=2, reorder_point=1, reorder_qty=2, lead_h=24,
    evidence_class='SYNTHETIC', source='Hipótese didática SYS-1; sem medição de JLTV',
    stringsAsFactors=FALSE)
}
sys_validate_profiles = function(p, mode='SYNTHETIC') {
  required = names(sys_demo_profiles())
  if (!is.data.frame(p) || nrow(p)==0 || nrow(p)>100 || !all(required %in% names(p)))
    stop('Catálogo: use as colunas do modelo CSV, entre 1 e 100 componentes.')
  text = c('system_id','component_id','component_name','failure_mode','part_id','source','evidence_class','life_distribution')
  for (k in text) {
    p[[k]] = trimws(as.character(p[[k]]))
    if (anyNA(p[[k]]) || any(!nzchar(p[[k]]))) stop(paste('Preencha',k,'em todos os itens.'))
  }
  if (anyDuplicated(p$component_id)) stop('component_id deve ser único.')
  if (any(!p$system_id %in% names(sys_systems()))) stop('system_id deve ser A1 a A10, conforme P6/P7.')
  if (any(!p$life_distribution %in% c('exponential','fixed'))) stop('Vida: exponential ou fixed.')
  nums = setdiff(required,text)
  for (k in nums) {
    p[[k]] = suppressWarnings(as.numeric(p[[k]]))
    if (any(!is.finite(p[[k]]))) stop(paste('Valor numérico ausente ou inválido:',k))
  }
  if (any(p$life_h<=0) || any(p$diagnosis_h<0 | p$repair_h<0 | p$test_h<0 | p$lead_h<0 | p$time_cv<0))
    stop('Vida deve ser positiva; tempos e CV não negativos.')
  if (any(p$part_probability<0 | p$part_probability>1)) stop('Probabilidade de peça: entre 0 e 1.')
  for (k in c('initial_stock','reorder_point','reorder_qty')) {
    if (any(p[[k]]<0 | p[[k]]!=floor(p[[k]]))) stop(paste(k,'deve ser inteiro não negativo.'))
  }
  if (any(p$reorder_qty<1)) stop('Quantidade de reposição deve ser pelo menos 1.')
  if (!mode %in% c('SYNTHETIC','HYBRID','EMPIRICAL')) stop('Modo de evidência inválido.')
  if (any(!p$evidence_class %in% c('SYNTHETIC','HYPOTHESIS','EMPIRICAL'))) stop('Classe de evidência inválida.')
  if (mode=='EMPIRICAL' && any(p$evidence_class!='EMPIRICAL')) stop('Modo empírico exige fonte e classe EMPIRICAL para todos os parâmetros.')
  if (mode=='SYNTHETIC' && any(p$evidence_class=='EMPIRICAL')) stop('Parâmetros empíricos: selecione modo HYBRID ou EMPIRICAL.')
  for (part in unique(p$part_id)) {
    rows = p[p$part_id==part,c('initial_stock','reorder_point','reorder_qty','lead_h','time_cv'),drop=FALSE]
    if (nrow(unique(rows))!=1) stop(paste('Mesmo PN/NSN exige estoque e reposição iguais:',part))
  }
  p
}
sys_validate_config = function(cfg,horizon,target) {
  for (k in c('fleet_size','diagnostic_teams','repair_teams','bays')) {
    x = cfg[[k]]
    if (length(x)!=1 || !is.finite(x) || x<1 || x!=floor(x)) stop(paste('Recurso deve ser inteiro positivo:',k))
  }
  if (length(horizon)!=1 || !is.finite(horizon) || horizon<=0 || horizon>87600) stop('Horizonte: maior que zero e até 87.600 h.')
  if (length(target)!=1 || !is.finite(target) || target<1 || target>cfg$fleet_size || target!=floor(target)) stop('Meta deve estar entre 1 e tamanho da frota.')
  if (length(cfg$utilization)!=1 || !is.finite(cfg$utilization) || cfg$utilization<=0 || cfg$utilization>1) stop('Utilização: maior que zero e até 1.')
  if (cfg$fleet_size>200) stop('Limite desta interface: 200 viaturas.')
  invisible(TRUE)
}
sys_simulate = function(profiles, config, horizon, target, seed, mode='SYNTHETIC') {
  p = sys_validate_profiles(profiles,mode); sys_validate_config(config,horizon,target)
  if (length(seed)!=1 || !is.finite(seed) || seed<0 || seed>1e8 || seed!=floor(seed)) stop('Semente: inteiro entre 0 e 100 milhões.')
  had_seed = exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE)
  if (had_seed) old_seed = get('.Random.seed',envir=.GlobalEnv)
  on.exit(if (had_seed) assign('.Random.seed',old_seed,envir=.GlobalEnv) else if (exists('.Random.seed',envir=.GlobalEnv)) rm('.Random.seed',envir=.GlobalEnv))
  n = config$fleet_size; m = nrow(p); parts = unique(p$part_id); pi = match(p$part_id,parts)
  stock = p$initial_stock[match(parts,p$part_id)]; outstanding = rep(FALSE,length(parts))
  # Streams by stable component/asset/stage/occurrence, independent of event ordering.
  stream_keys = vapply(p$component_id,function(x) strtoi(substr(digest::digest(x,algo='xxhash32',serialize=FALSE),1,7),base=16L),integer(1))
  if (anyDuplicated(stream_keys)) stop('Colisão de identificadores de fluxo; utilize component_id distintos.')
  idkey = function(x) stream_keys[match(x,p$component_id)]
  draw = function(mean,cv,asset,comp,stage,index,distribution='lognormal') {
    set.seed(as.integer((seed+idkey(p$component_id[comp])*100003+asset*1009+stage*1000003+index*7919) %% 2147483646)+1L)
    u = max(1e-12,min(1-1e-12,runif(1)))
    if (distribution=='fixed' || cv==0 || mean==0) return(mean)
    if (distribution=='exponential') return(-log1p(-u)*mean)
    sdlog = sqrt(log1p(cv^2)); exp(log(mean)-sdlog^2/2+sdlog*qnorm(u))
  }
  residual = matrix(0,n,m); count = matrix(0L,n,m)
  for (a in seq_len(n)) for (c in seq_len(m)) residual[a,c] = draw(p$life_h[c],1,a,c,1,1,p$life_distribution[c])
  state = rep('OPERATING',n); started = rep(0,n); operated = rep(0,n); cause = rep(NA_integer_,n)
  diagq = integer(); repairq = integer(); partq = integer(); diag_busy = 0L; repair_busy = 0L
  stages = list(); events = list(); queue = list(); serial = 0L; time = 0; last = 0; area = 0; below = 0; minimum = n
  schedule = function(t,event,a=0L,c=0L,part=0L) {
    serial <<- serial+1L; queue[[length(queue)+1L]] <<- list(time=t,event=event,a=a,c=c,part=part,serial=serial)
  }
  eventlog = function(event,a=0L,c=0L,part=0L) {
    events[[length(events)+1L]] <<- data.frame(time=time,event=event,asset_id=if(a>0) paste0('V',a) else '',
      component_id=if(c>0) p$component_id[c] else '',system_id=if(c>0) p$system_id[c] else '',
      part_id=if(part>0) parts[part] else if(c>0) p$part_id[c] else '',ready=sum(state=='OPERATING'),stringsAsFactors=FALSE)
  }
  close_state = function(a) {
    if (state[a]!='OPERATING' && time>started[a]) {
      c = cause[a]
      stages[[length(stages)+1L]] <<- data.frame(asset_id=paste0('V',a),component_id=p$component_id[c],
        system_id=p$system_id[c],part_id=p$part_id[c],stage=state[a],start=started[a],end=time,hours=time-started[a],stringsAsFactors=FALSE)
    }
  }
  transition = function(a,s) { close_state(a); state[a] <<- s; started[a] <<- time; minimum <<- min(minimum,sum(state=='OPERATING')) }
  operate = function(a) { operated[a] <<- time; c = which.min(residual[a,]); schedule(time+residual[a,c]/config$utilization,'FAILURE',a,c) }
  reorder = function(k,c) {
    if (!outstanding[k] && stock[k]<=p$reorder_point[c]) {
      outstanding[k] <<- TRUE
      idx = sum(vapply(events,function(x) x$event=='ORDER' && x$part_id==parts[k],logical(1)))+1L
      # Shared part delivery uses its canonical row's stream.
      pc = which(pi==k)[1]; duration = draw(p$lead_h[pc],p$time_cv[pc],0,pc,5,idx)
      schedule(time+duration,'DELIVERY',part=k,c=pc); eventlog('ORDER',c=pc,part=k)
    }
  }
  dispatch_repair = function() {
    while (length(repairq)>0 && repair_busy<min(config$repair_teams,config$bays)) {
      a = repairq[1]; repairq <<- repairq[-1]; c = cause[a]
      repair_busy <<- repair_busy+1L; transition(a,'REPAIR')
      schedule(time+draw(p$repair_h[c],p$time_cv[c],a,c,3,count[a,c]),'END_REPAIR',a,c)
    }
  }
  route_part = function(a) {
    c = cause[a]; k = pi[c]
    requires = draw(1,1,a,c,6,count[a,c],distribution='exponential')
    # Exponential transform inverted to the uniform driving the Bernoulli.
    requires = (1-exp(-requires)) < p$part_probability[c]
    if (requires && stock[k]<=0) {
      transition(a,'WAIT_PART'); partq <<- c(partq,a); reorder(k,c)
    } else {
      if (requires) { stock[k] <<- stock[k]-1; eventlog('CONSUME',a,c,k); reorder(k,c) }
      transition(a,'QUEUE_REPAIR'); repairq <<- c(repairq,a); dispatch_repair()
    }
  }
  dispatch_diag = function() {
    while (length(diagq)>0 && diag_busy<config$diagnostic_teams) {
      a = diagq[1]; diagq <<- diagq[-1]; c = cause[a]; diag_busy <<- diag_busy+1L
      transition(a,'DIAGNOSIS'); schedule(time+draw(p$diagnosis_h[c],p$time_cv[c],a,c,2,count[a,c]),'END_DIAGNOSIS',a,c)
    }
  }
  for (a in seq_len(n)) operate(a)
  steps = 0L
  while (length(queue)>0) {
    ix = order(vapply(queue,function(e)e$time,numeric(1)),vapply(queue,function(e)e$serial,integer(1)))[1]
    e = queue[[ix]]; if (e$time>horizon) break
    queue[[ix]] = NULL; steps = steps+1L; if (steps>2000000) stop('Limite de eventos excedido; revise taxas e horizonte.')
    ready = sum(state=='OPERATING'); area = area+(e$time-last)*ready
    if (ready<target) below = below+e$time-last
    time = e$time; last = time; a = e$a; c = e$c
    if (e$event=='FAILURE') {
      residual[a,] = pmax(0,residual[a,]-(time-operated[a])*config$utilization)
      count[a,c] = count[a,c]+1L; cause[a] = c; transition(a,'QUEUE_DIAGNOSIS'); diagq = c(diagq,a); dispatch_diag()
    } else if (e$event=='END_DIAGNOSIS') {
      diag_busy = diag_busy-1L; route_part(a); dispatch_diag()
    } else if (e$event=='END_REPAIR') {
      repair_busy = repair_busy-1L; transition(a,'TEST')
      schedule(time+draw(p$test_h[c],p$time_cv[c],a,c,4,count[a,c]),'END_TEST',a,c); dispatch_repair()
    } else if (e$event=='END_TEST') {
      residual[a,c] = draw(p$life_h[c],1,a,c,1,count[a,c]+1L,p$life_distribution[c])
      transition(a,'OPERATING'); cause[a] = NA_integer_; operate(a)
    } else if (e$event=='DELIVERY') {
      k = e$part; outstanding[k] = FALSE; pc = which(pi==k)[1]; stock[k] = stock[k]+p$reorder_qty[pc]
      waiting = partq[vapply(partq,function(x) pi[cause[x]]==k,logical(1))]
      for (w in waiting) {
        if (stock[k]<=0) break
        stock[k] = stock[k]-1; partq = partq[partq!=w]; eventlog('CONSUME',w,cause[w],k)
        transition(w,'QUEUE_REPAIR'); repairq = c(repairq,w)
      }
      dispatch_repair()
      if (length(partq)>0 && any(vapply(partq,function(x) pi[cause[x]]==k,logical(1)))) reorder(k,pc)
    }
    eventlog(e$event,a,c,e$part)
  }
  ready = sum(state=='OPERATING'); area = area+(horizon-last)*ready
  if (ready<target) below = below+horizon-last
  time = horizon; for (a in seq_len(n)) close_state(a)
  empty = data.frame(asset_id=character(),component_id=character(),system_id=character(),part_id=character(),stage=character(),start=numeric(),end=numeric(),hours=numeric())
  st = if(length(stages)) do.call(rbind,stages) else empty
  ev = if(length(events)) do.call(rbind,events) else data.frame(time=numeric(),event=character(),asset_id=character(),component_id=character(),system_id=character(),part_id=character(),ready=integer())
  if (abs(sum(st$hours)-(n*horizon-area))>1e-7*max(1,n*horizon)) stop('Falha na conservação do tempo.')
  list(metrics=data.frame(seed=seed,Ao=area/(n*horizon),mission_success=minimum>=target,
    min_ready=minimum,hours_below=below,failures=sum(count),down_h=sum(st$hours)),stages=st,events=ev,
    failure_counts=data.frame(component_id=p$component_id,system_id=p$system_id,failures=colSums(count)),
    stocks=data.frame(part_id=parts,final_stock=stock,outstanding=outstanding))
}
sys_intervene = function(p, scope, target_id, lever, change) {
  profiles_before = p
  if (!scope %in% c('component','system')) stop('Escopo inválido.')
  if (length(change)!=1 || !is.finite(change) || change<=0 || change>=100) stop('Alteração: maior que zero e menor que 100%.')
  hit = p[[if(scope=='component') 'component_id' else 'system_id']]==target_id
  if (!any(hit)) stop('Alvo não existe no catálogo.')
  if (lever=='reliability') p$life_h[hit] = p$life_h[hit]*(1+change/100)
  else if (lever=='repair') p$repair_h[hit] = p$repair_h[hit]*(1-change/100)
  else if (lever=='diagnosis') p$diagnosis_h[hit] = p$diagnosis_h[hit]*(1-change/100)
  else if (lever %in% c('lead','stock')) {
    # A PN shared with a second system is one physical stock: expose that reach.
    hit = p$part_id %in% p$part_id[hit]
    if (lever=='lead') p$lead_h[hit] = p$lead_h[hit]*(1-change/100)
    else p$initial_stock[hit] = p$initial_stock[hit]+pmax(1,ceiling(p$initial_stock[hit]*change/100))
  } else stop('Intervenção inválida.')
  changed = vapply(seq_len(nrow(p)),function(i)!identical(p[i,],profiles_before[i,]),logical(1))
  p$evidence_class[changed] = 'HYPOTHESIS'
  p$source[changed] = paste('Cenário hipotético:',lever,change,'%; base:',p$source[changed])
  p
}
sys_ci = function(x) {
  if (length(x)<2) return(c(mean(x),NA_real_,NA_real_))
  half = stats::qt(.975,length(x)-1)*stats::sd(x)/sqrt(length(x))
  c(mean(x),mean(x)-half,mean(x)+half)
}
sys_wilson = function(x) {
  n = length(x); z = stats::qnorm(.975); phat = mean(x)
  center = (phat+z*z/(2*n))/(1+z*z/n)
  half = z*sqrt(phat*(1-phat)/n+z*z/(4*n*n))/(1+z*z/n)
  c(phat,max(0,center-half),min(1,center+half))
}
sys_compare = function(profiles,config,horizon,target,seeds,scope,target_id,lever,change,mode='SYNTHETIC') {
  p = sys_validate_profiles(profiles,mode); sys_validate_config(config,horizon,target)
  if (length(seeds)<2 || length(seeds)>500 || anyDuplicated(seeds)) stop('Use 2 a 500 sementes distintas.')
  baseline = lapply(seeds,function(s) sys_simulate(p,config,horizon,target,s,mode))
  ids = as.character(target_id); comparisons = list(); reps = list(); parameter_sets = list(Referencia=p)
  base = do.call(rbind,lapply(baseline,`[[`,'metrics')); reps[[1]] = cbind(scenario='Referencia',base)
  for (id in ids) {
    altered = sys_intervene(p,scope,id,lever,change); scenario = paste(scope,id,lever,sep=':')
    parameter_sets[[scenario]] = altered
    scenario_mode = if(mode=='EMPIRICAL') 'HYBRID' else mode
    runs = lapply(seeds,function(s) sys_simulate(altered,config,horizon,target,s,scenario_mode))
    r = do.call(rbind,lapply(runs,`[[`,'metrics')); reps[[length(reps)+1]] = cbind(scenario=scenario,r)
    delta = sys_ci(r$Ao-base$Ao); pm = sys_wilson(r$mission_success)
    dpm = sys_ci(as.numeric(r$mission_success)-as.numeric(base$mission_success))
    affected = p$component_id[vapply(seq_len(nrow(p)),function(i)!identical(p[i,],altered[i,]),logical(1))]
    comparisons[[length(comparisons)+1]] = data.frame(target_id=id,scope=scope,lever=lever,change_percent=change,
      Ao=mean(r$Ao),delta_Ao=delta[1],lower95=delta[2],upper95=delta[3],
      mission_probability=pm[1],mission_lower95=pm[2],mission_upper95=pm[3],
      delta_mission=dpm[1],delta_mission_lower95=dpm[2],delta_mission_upper95=dpm[3],
      affected_components=paste(affected,collapse='; '),
      conclusion=if(delta[2]>0) 'Ganho positivo no IC95% pareado' else 'Ganho positivo não demonstrado no IC95%',stringsAsFactors=FALSE)
  }
  st = do.call(rbind,lapply(seq_along(baseline),function(i){x=baseline[[i]]$stages;data.frame(replication=rep(i,nrow(x)),seed=rep(seeds[i],nrow(x)),x)}))
  counts = Reduce(`+`,lapply(baseline,function(x)x$failure_counts$failures))/length(seeds)
  cp = p[,c('system_id','component_id','component_name','failure_mode','part_id','evidence_class','source')]
  cp$failures = counts
  cp$down_h = vapply(cp$component_id,function(id)sum(st$hours[st$component_id==id])/length(seeds),numeric(1))
  cp$lost_Ao_pp = 100*cp$down_h/(config$fleet_size*horizon)
  cp$share_down = if(sum(cp$down_h)>0)cp$down_h/sum(cp$down_h) else 0
  cp = cp[order(-cp$down_h,cp$component_id),]; rownames(cp)=NULL
  sy = aggregate(cp[,c('failures','down_h','lost_Ao_pp','share_down')],list(system_id=cp$system_id),sum)
  sy$system_name = unname(sys_systems()[sy$system_id]); sy = sy[order(-sy$down_h),]; rownames(sy)=NULL
  stage_totals = if(nrow(st)>0) aggregate(st$hours/length(seeds),st[,c('system_id','component_id','stage')],sum) else data.frame(system_id=character(),component_id=character(),stage=character(),x=numeric())
  names(stage_totals)[ncol(stage_totals)] = 'hours'
  comp = do.call(rbind,comparisons); comp = comp[order(-comp$delta_Ao),]; rownames(comp)=NULL
  list(version=JLTV_SYSTEMS_VERSION,mode=mode,created_utc=format(Sys.time(),tz='UTC',usetz=TRUE),profiles=p,
    config=config,horizon=horizon,target=target,seeds=seeds,baseline=base,
    baseline_Ao=sys_ci(base$Ao),baseline_mission=sys_wilson(base$mission_success),
    components=cp,systems=sy,stages=stage_totals,stage_logs=st,events=baseline[[1]]$events,
    comparison=comp,replications=do.call(rbind,reps),parameter_sets=parameter_sets)
}
sys_observed = function(orders,mapping,profiles) {
  if (!all(c('work_order_id','component_id','source') %in% names(mapping))) stop('Vínculo OS: work_order_id, component_id, source.')
  if (anyNA(mapping) || anyDuplicated(mapping$work_order_id) || any(!nzchar(trimws(mapping$source))) ||
      any(!mapping$component_id %in% profiles$component_id) || any(!mapping$work_order_id %in% orders$work_order_id))
    stop('Vínculos devem ser únicos, com OS e componente existentes e fonte preenchida.')
  rows = orders[orders$maintenance_type=='CORRECTIVE' & orders$is_functional_failure==1,,drop=FALSE]
  joined = merge(rows,mapping,by='work_order_id')
  parse_date = function(x) {
    if (exists('parse_datetime_utc',mode='function')) return(parse_datetime_utc(x))
    x = gsub('T',' ',as.character(x),fixed=TRUE)
    as.POSIXct(sub('Z$','',x),format='%Y-%m-%d %H:%M:%S',tz='UTC')
  }
  start = parse_date(joined$detected_at); end = parse_date(joined$released_at)
  joined$down_h = as.numeric(difftime(end,start,units='hours'))
  valid = is.finite(joined$down_h) & joined$down_h>=0
  by = if(any(valid)) aggregate(joined$down_h[valid],list(component_id=joined$component_id[valid]),sum) else data.frame(component_id=character(),x=numeric())
  names(by)[2] = 'down_h'
  list(by_component=by,unmapped=sum(!rows$work_order_id %in% mapping$work_order_id),
    invalid_times=sum(!valid),classified=nrow(joined),total=nrow(rows),records=joined)
}
sys_references = function() {
  data.frame(title=c(
    'McLaren (2025). Identifying Downtime Drivers Using SIMLOX Simulations. SYM-AM-25-426.',
    'DoD (2023). DI-PSSS-82054A — LCOM Analysis Tool Kit.',
    'Blankenbeker (2018). USMC Depot-Level Maintenance of the LAV: A Discrete-Event Simulation Analysis. NPS.',
    'Rautio; Vehviläinen; Lehtonen (2026). Simulation analysis of additive manufacturing support for military maintenance during operations. DOI 10.1177/15485129261473305.',
    'Eisenberger; Fink (2017). Assessment of maintenance strategies for railway vehicles using Petri-Nets. DOI 10.1016/j.trpro.2017.12.012.',
    'Barbosa (2022). Maintenance routine simulation with data-driven approaches applied to naval and shipping sectors. Brunel.',
    'Gallego-García; Gejo-García; García-García (2021). Development of a Maintenance and Spare Parts Distribution Model for Increasing Aircraft Efficiency. DOI 10.3390/app11031333.',
    'Systecon. Swedish FMV — ILS resources for Helicopter 14 (NH90). Relato do fornecedor.',
    'RePyability — documentação de sistemas reparáveis e históricos por causa.',
    'NASA fmdtools — documentação de estrutura e propagação de falhas.',
    'Flight-Data-Analysis — README e previsão por tipo de unidade; contexto Airbus declarado pelo autor.'),
    use=c('Indisponibilidade por item/subsistema.','Recursos, tarefas e componentes em simulação de prontidão.',
      'Filas e módulos no apoio ao LAV; escopo depósito.','Estoques por item e demanda; sem transferência de parâmetros.',
      'Representação de componentes e políticas.','Rotinas de manutenção naval e recursos.',
      'Distribuição e reposição; estudo teórico, sem ganho empírico JLTV.',
      'Contexto de uso de ferramentas; não comprova ganho independente.',
      'Identidade de componentes e rastreabilidade de causas.','Estrutura funcional; não incorporado como motor.',
      'Identidade PN/unidade e previsão; código não incorporado.'),
    url=c('https://www.dair.nps.edu/handle/123456789/5437',
      'https://quicksearch.dla.mil/qsDocDetails.aspx?ident_number=281655','http://hdl.handle.net/10945/59711',
      'https://journals.sagepub.com/doi/10.1177/15485129261473305',
      'https://digitalcollection.zhaw.ch/items/e1eaf57f-d3eb-481e-9406-6c6d2f6d82ea',
      'https://repositorio.marinha.mil.br/handle/20.500.14867/845689',
      'https://www.mdpi.com/2076-3417/11/3/1333',
      'https://www.systecongroup.com/us/knowledge-center/swedish-defence-materiel-administration-build-ils-resources-swedish-helicopter-14',
      'https://github.com/derrynknife/RePyability','https://github.com/nasa/fmdtools','https://github.com/nghi-truyen/Flight-Data-Analysis'),
    accessed='2026-10-03', stringsAsFactors=FALSE)
}
# Freeze only providers used by this module; audit/run identifiers are not sources.
sys_source_snapshot = function(questionnaire,multicriteria,snapshot,mapping,profiles,questionnaire_data=NULL) {
  mc = if(is.list(multicriteria)) multicriteria else list()
  qnames=c('questionnaire_respondents','ahp_pairwise','topsis_scores','questionnaire_factors')
  full_questionnaire=if(is.list(questionnaire_data)) questionnaire_data[intersect(qnames,names(questionnaire_data))] else snapshot[intersect(qnames,names(snapshot))]
  list(questionnaire=questionnaire,questionnaire_data=full_questionnaire,
    multicriteria=list(estimable=isTRUE(mc$topsis$estimable),ranking=mc$topsis$ranking,
      weights=mc$ahp$subcriteria_weights,consistency=mc$ahp$consistency,
      score_aggregation=mc$score_aggregation,include_only_consistent=mc$include_only_consistent,
      min_cell_coverage=mc$min_cell_coverage,evidence_mode=mc$evidence_mode),
    work_orders=snapshot$work_orders,mapping=mapping,profiles=profiles)
}
sys_source_signature = function(sources) {
  strip_audit = function(x) {
    if(is.data.frame(x)) return(x[,setdiff(names(x),c('mc_run_id','simulation_run_id','audit_run_id','result_id')),drop=FALSE])
    if(is.list(x)) {
      if(is.null(names(x))) return(lapply(x,strip_audit))
      return(lapply(x[setdiff(names(x),c('mc_run_id','simulation_run_id','audit_run_id'))],strip_audit))
    }
    x
  }
  digest::digest(strip_audit(sources),algo='sha256',serialize=TRUE)
}
sys_export = function(result,path,questionnaire=NULL,observed=NULL,mapping=NULL,project_references=NULL) {
  wb = openxlsx::createWorkbook()
  limits = data.frame(item=c('Versão','Modo','Execução UTC','Horizonte','Meta','Sementes',
    'Efeito funcional','Relógios','Reposição','Preventiva e redundância','Causalidade','Custos','Registros observados','IC95%'),
    value=c(result$version,result$mode,result$created_utc,result$horizon,result$target,paste(result$seeds,collapse=';'),
      'Todos os itens representam falhas funcionais que param a viatura; uma causa ativa por vez.',
      'Uso pausa durante parada; reparar um item renova somente sua vida. Todos iniciam operantes e novos.',
      'Estoque por PN/NSN: revisão após demanda/consumo, uma reposição pendente; na entrega, novo pedido somente com backlog. Não repõe antecipadamente no início.',
      'SYS-1 não simula preventiva, falhas simultâneas, redundância ou degradação parcial. Motor agregado preservado separadamente.',
      'Ganho condicionado aos parâmetros; prioridade AHP/TOPSIS não é taxa de falha. Intervenções são hipóteses, mesmo com referência empírica.',
      'Não calcula retorno financeiro sem custos verificados.',
      'Horas de OS podem sobrepor; não são Ao observada. Sem mapeamento, sem atribuição ao sistema.',
      'Ao e diferenças pareadas: t de Student; missão: Wilson. Intervalos individuais, sem correção por múltiplos alvos nem incerteza dos parâmetros.'),stringsAsFactors=FALSE)
  parameters = do.call(rbind,lapply(names(result$parameter_sets),function(k)cbind(scenario=k,result$parameter_sets[[k]])))
  summary = data.frame(item=c('Ao referência','Ao IC95% inferior','Ao IC95% superior','Missão referência','Missão IC95% inferior','Missão IC95% superior'),value=c(result$baseline_Ao,result$baseline_mission))
  refs = sys_references()
  if (!is.null(project_references)) refs = rbind(refs,do.call(rbind,lapply(project_references,function(x)data.frame(title=x$title,use=x$use,url=x$url,accessed='Documentação do projeto'))))
  sheets = list(Resumo=summary,Componentes=result$components,Sistemas=result$systems,Etapas=result$stages,
    Intervencoes=result$comparison,Parametros=parameters,Recursos=as.data.frame(result$config),
    Replicacoes=result$replications,Eventos=result$events,Tempos=result$stage_logs,Premissas=limits,Referencias=refs)
  if(!is.null(result$source_signature)) {
    settings = result$settings
    context = list(source_signature=result$source_signature,source_algorithm='SHA-256 / R serialize',
      version=result$version,mode=result$mode,created_utc=result$created_utc,
      scope=settings$scope,target_ids=settings$target_ids,lever=settings$lever,
      change_percent=settings$change_percent,compare_all=settings$compare_all,
      replications=length(result$seeds),initial_seed=result$seeds[1],horizon=result$horizon,target=result$target,
      questionnaire_rows=nrow(result$source_snapshot$questionnaire),
      work_order_rows=nrow(result$source_snapshot$work_orders),mapping_rows=nrow(result$mapping))
    sheets$Contexto = data.frame(item=names(context),value=vapply(context,function(x)paste(x,collapse='; '),character(1)))
    if(is.data.frame(result$source_snapshot$multicriteria$ranking)) sheets$TOPSIS_fonte=result$source_snapshot$multicriteria$ranking
    if(is.data.frame(result$source_snapshot$multicriteria$weights)) sheets$AHP_pesos_fonte=result$source_snapshot$multicriteria$weights
  }
  if (!is.null(questionnaire)) sheets$Questionario = questionnaire
  if (!is.null(observed)) sheets$OS_observadas = observed$records
  if (!is.null(mapping)) sheets$Vinculos_OS = mapping
  for (name in names(sheets)) {
    openxlsx::addWorksheet(wb,name); d = sheets[[name]]
    if (nrow(d)>0) openxlsx::writeDataTable(wb,name,d,tableStyle='TableStyleMedium2') else openxlsx::writeData(wb,name,d)
    openxlsx::freezePane(wb,name,firstRow=TRUE); openxlsx::setColWidths(wb,name,cols=seq_len(ncol(d)),width=20)
  }
  pie = tryCatch(chart_system_downtime_pie(result$systems,titles=TRUE),error=function(e) NULL)
  if(!is.null(pie)) tryCatch({
    png = tempfile(fileext='.png'); ggplot2::ggsave(png,pie,width=8.5,height=4.6,dpi=150,bg='white')
    openxlsx::insertImage(wb,'Resumo',png,startRow=2,startCol=4,width=8.5,height=4.6,units='in')
  },error=function(e) NULL)
  openxlsx::saveWorkbook(wb,path,overwrite=TRUE); invisible(path)
}
sys_stage_labels = function() c(QUEUE_DIAGNOSIS='Fila de diagnóstico',DIAGNOSIS='Diagnóstico',WAIT_PART='Espera por peça',QUEUE_REPAIR='Fila de reparo/box',REPAIR='Reparo corretivo',TEST='Teste/liberação')
sys_documentation = function() {
  node = function(label,text) shiny::tags$div(class='jltv-doc-node',shiny::tags$strong(label),shiny::tags$p(text))
  arrow = function(label='↓') shiny::tags$div(class='jltv-doc-arrow',label)
  shiny::tagList(
    shiny::tags$h3('Motor por sistema e componente — SYS-1'),
    shiny::tags$div(class='jltv-doc-lanes',
      node('Questionário P1–P7','AHP: pesos; P6: prioridade dos sistemas A1–A10; P7: peça/PN e registro a confirmar.'),
      node('Registros e catálogo','OS classificada explicitamente por component_id, sistema A1–A10, PN/NSN e fonte. Parâmetros declarados por item.')),
    arrow('↓ A mesma identidade liga percepção, registros e simulação'),
    node('Viatura → sistema → componente → modo de falha','Cada item tem vida até falha e tempos próprios. As hipóteses não são derivadas das notas do questionário.'),
    arrow(),
    shiny::tags$div(class='jltv-doc-lanes',
      node('Relógios de falha por componente','O primeiro item a falhar para a viatura. Os relógios dos demais pausam e conservam a vida restante.'),
      node('Apoio compartilhado','Diagnóstico, equipes e boxes atendem a frota; estoques por PN/NSN. Reposição revisada na demanda; na entrega, novo pedido apenas para demanda pendente.')),
    arrow('↓ Cada transição conserva a causa da parada'),
    node('Diagnóstico → peça → reparo → teste','Fila de diagnóstico e de reparo também mantêm viatura, sistema e componente. Reparar um item não renova os outros.'),
    arrow(),node('Intervenções no sistema/item selecionado','Confiabilidade, diagnóstico, reparo, prazo ou estoque. PN compartilhado afeta todos os itens que usam a mesma peça; o alcance é informado.'),
    arrow(),node('Resultado por sistema e componente','Falhas, horas de viatura parada, etapa responsável, parcela da perda de Ao, ganho pareado e atendimento da missão. P6/P7 e OS aparecem em colunas distintas.'),
    arrow(),node('Excel rastreável','Referência e cenários, parâmetros e fontes, sementes, recursos, eventos da primeira execução, tempos de todas as execuções e referências.'),
    shiny::tags$h3('Árvore física vinculada ao questionário'),
    shiny::tags$ul(lapply(names(sys_systems()),function(id)shiny::tags$li(shiny::tags$strong(paste(id,sys_systems()[id])),
      shiny::tags$ul(shiny::tags$li('component_id — identificador único; PN/NSN pode ser compartilhado',
        shiny::tags$ul(shiny::tags$li('Modo de falha funcional e fonte'),shiny::tags$li('Vida, diagnóstico, reparo, teste e demanda de peças'),
          shiny::tags$li('Estoque, ponto de pedido, quantidade e prazo de reposição'),shiny::tags$li('OS vinculadas e eventos com causa preservada'))))))),
    shiny::tags$p('Limite SYS-1: todos os itens representam panes que param a viatura; uma causa ativa por vez, todos iniciam operantes/novos, sem preventiva, redundância ou falhas simultâneas. O motor agregado anterior permanece separado.'),
    shiny::tags$h3('Simuladores e estudos comparados'),
    shiny::tags$p('Fontes pesquisadas para o desenho do método. Implementação própria em R; nenhum código externo nem parâmetro dessas frotas foi incorporado.'),
    shiny::tagList(lapply(seq_len(nrow(sys_references())),function(i){r=sys_references()[i,]; shiny::tags$div(class='jltv-doc-ref',shiny::tags$a(href=r$url,target='_blank',rel='noopener noreferrer',r$title),shiny::tags$p(r$use))}))
  )
}
sys_catalog_ui = function() shiny::tags$div(class='jltv-docs',
  shiny::tags$p('Componentes (avançado): confira componentes, parâmetros, fontes e vínculos de OS. A demonstração é sintética e não representa a frota real.'),
  bslib::layout_column_wrap(width=1/3,
        shiny::selectInput('sys_mode','Evidência dos parâmetros',c('Demonstração sintética'='SYNTHETIC','Registros + hipóteses'='HYBRID','Somente registros rastreáveis'='EMPIRICAL')),
        shiny::actionButton('sys_load_demo','Carregar exemplo sintético',class='btn btn-outline-primary'),
        shiny::fileInput('sys_profiles_upload','Importar catálogo CSV (UTF-8, separador vírgula)',accept='.csv')),
      shiny::tags$p('Duplo clique para editar. Para incluir novas peças, baixe o modelo, adicione linhas com component_id único e reimporte. As dez linhas do exemplo são agregações demonstrativas; não identificam peças reais.'),
      DT::DTOutput('sys_profiles_table'),
      shiny::downloadButton('sys_profiles_download','Baixar catálogo/modelo CSV'),
      shiny::actionButton('sys_save_profiles','Salvar catálogo neste computador'),
      shiny::tags$details(shiny::tags$summary('Como preencher os parâmetros'),
        shiny::tags$p('system_id: chave A1–A10 de P6/P7. component_id: chave única. part_id: PN/NSN ou ID interno. failure_mode: pane funcional. life_h: média de horas efetivas de uso; life_distribution: exponential ou fixed. diagnosis_h, repair_h, test_h e lead_h: médias em horas de calendário; time_cv: desvio-padrão/média dos tempos lognormais, zero para tempos fixos. part_probability: probabilidade de exigir peça (0–1). initial_stock, reorder_point e reorder_qty: unidades inteiras; reposição positiva. Mesmo PN exige configuração de estoque/prazo/CV igual.'),
        shiny::tags$p('evidence_class: SYNTHETIC, HYPOTHESIS ou EMPIRICAL. source: arquivo/registro e trecho que sustentam os parâmetros da linha. A declaração EMPIRICAL exige revisão dos registros: a interface não verifica automaticamente se a fonte comprova o valor. Nenhum ajuste é inferido de P6/P7.')),
      shiny::tags$h4('Classificar as ordens de serviço por componente'),
      shiny::tags$p('Baixe os IDs de OS, preencha component_id e source, depois importe. O vínculo é explícito; texto livre não recebe classificação automática. A atribuição observada usa somente OS corretivas com falha funcional.'),
      shiny::fileInput('sys_mapping_upload','Vínculos OS → componente (CSV)',accept='.csv'),
      shiny::downloadButton('sys_mapping_download','Baixar vínculos/modelo de OS'),
      shiny::actionButton('sys_save_mapping','Salvar vínculos neste computador'),
      DT::DTOutput('sys_mapping_table'),shiny::textOutput('sys_data_status')
)
sys_configuration_ui = function() shiny::tags$div(class='jltv-docs',
  shiny::tags$h3('Missão, apoio e intervenção por sistema'),
  shiny::tags$p('Defina a missão e o alvo. Ajuste o apoio e as execuções quando necessário.'),
      bslib::layout_column_wrap(width=1/3,
        shiny::numericInput('sys_fleet','Viaturas',12,min=1,max=200,step=1),
        shiny::numericInput('sys_target','Mínimo pronto durante toda a missão',8,min=1,step=1),
        shiny::numericInput('sys_horizon','Missão (horas de calendário)',168,min=1)),
      bslib::accordion(open=FALSE,
        bslib::accordion_panel('Apoio e execuções',
          bslib::layout_column_wrap(width=1/3,
            shiny::numericInput('sys_util','Uso por hora de calendário',1,min=.01,max=1,step=.1),
            shiny::numericInput('sys_diag','Equipes de diagnóstico',2,min=1,step=1),
            shiny::numericInput('sys_repair','Equipes de reparo',3,min=1,step=1),
            shiny::numericInput('sys_bays','Boxes',2,min=1,step=1),
            shiny::numericInput('sys_n','Execuções por cenário',30,min=2,max=500,step=1),
            shiny::numericInput('sys_seed','Semente inicial',20261003,min=0,max=1e8,step=1)),
          shiny::tags$p('Referência e intervenção usam as mesmas sementes. IC95% mede a incerteza das execuções neste cenário; a incerteza dos parâmetros exige análise própria.'))),
      bslib::layout_column_wrap(width=1/2,
        shiny::selectInput('sys_scope','Alvo da intervenção',c('Sistema'='system','Componente'='component')),
        shinyWidgets::pickerInput('sys_target_id','Qual sistema/item?',choices=stats::setNames(names(sys_systems()),unname(sys_systems())),options=list(`live-search`=TRUE)),
        shiny::selectInput('sys_lever','O que mudar no alvo?',c('Aumentar vida média'='reliability','Reduzir reparo'='repair','Reduzir diagnóstico'='diagnosis','Reduzir prazo da peça'='lead','Aumentar estoque inicial'='stock')),
        shiny::numericInput('sys_change','Alteração (%)',20,min=.1,max=99,step=1)),
      bslib::accordion(open=FALSE,bslib::accordion_panel('Comparação ampliada e alcance',
        shiny::checkboxInput('sys_all','Comparar separadamente todos os sistemas do catálogo',FALSE),
        shiny::tags$p('Por padrão, compare o alvo selecionado. A comparação ampliada usa uma intervenção por sistema, mantendo a mesma referência. Peça compartilhada pode alcançar outros sistemas; filas comuns podem gerar efeitos indiretos.')))
)
sys_results_ui = function() shiny::tags$div(class='jltv-docs',
  shiny::downloadButton('sys_excel','Baixar relatório por sistema (.xlsx)'),
  bslib::navset_card_tab(id='sys_result_tabs',
        bslib::nav_panel('Resumo',
          shiny::uiOutput('sys_summary'),
          bslib::layout_column_wrap(width=1/2,
            bslib::card(bslib::card_header('Horas paradas por sistema'),
              shiny::plotOutput('sys_system_plot',height='340px'),
              shiny::tags$p('O gráfico atribui horas médias de viatura parada ao sistema que causou a pane na referência.')),
            bslib::card(bslib::card_header('Participação de cada sistema no tempo parado'),
              shiny::plotOutput('sys_system_pie',height='340px'),
              shiny::tags$p('Parcela da parada: o denominador é o total de horas paradas de todos os sistemas. A parcela é atribuição simulada da parada, e não a prioridade dada pelos especialistas.'))),
          bslib::layout_column_wrap(width=1/2,
            bslib::card(bslib::card_header('Distribuição da disponibilidade entre execuções'),
              plotly::plotlyOutput('sys_ao_histogram',height='320px'),
              shiny::tags$p('Cada observação é uma execução da referência. O histograma mostra dispersão; a altura de uma barra não é probabilidade de falha de um sistema.')),
            bslib::card(bslib::card_header('Como a prontidão muda durante uma execução'),
              plotly::plotlyOutput('sys_fleet_animation',height='320px'),
              shiny::tags$p('Animação didática dos eventos da primeira execução da referência. Use ▶ para percorrer as mudanças; ela não representa a média das execuções.')))),
        bslib::nav_panel('Sistemas e etapas',
          shiny::tags$p('Veja a perda por sistema e abra os detalhes por componente e etapa. Horas são médias por execução.'),
          reactable::reactableOutput('sys_system_results'),
          bslib::accordion(open=FALSE,
            bslib::accordion_panel('Componentes e suas fontes',DT::DTOutput('sys_component_results')),
            bslib::accordion_panel('Etapas da parada',DT::DTOutput('sys_stage_results')))),
        bslib::nav_panel('Intervenções',
          shiny::tags$p('Ganho de Ao em pontos percentuais contra a referência, com IC95% pareado. Intervalo acima de zero demonstra ganho nestas execuções, condicionado aos parâmetros e à fronteira do modelo.'),
          shiny::tags$p('Use “Colunas” para conferir missão, alcance e detalhes. O Excel conserva todas as colunas.'),
          DT::DTOutput('sys_intervention_results')),
        bslib::nav_panel('Confronto',
          shiny::tags$p('Especialista prioriza; registro parametriza; simulação testa. AHP fornece pesos e TOPSIS ordena a criticidade percebida. P7 indica evidências a confirmar. OS registra tempo e causa classificada; simulação estima horas de viatura parada sob os parâmetros declarados.'),
          shiny::tags$p('Os rankings podem divergir por cobertura dos registros, tempos, estoque, recursos compartilhados ou premissas. Confira essas diferenças antes de concluir; ajuste parâmetros com evidência documentada.'),
          DT::DTOutput('sys_confront'),shiny::textOutput('sys_observed_status'),
          shiny::tags$p('Horas de OS podem se sobrepor e não são Ao observada. Campo vazio indica evidência ausente. Fontes, vínculos e parâmetros ficam congelados por execução; mudança de fonte exige nova execução.')),
        bslib::nav_panel('Auditoria visual',
          shiny::tags$p('A conciliação compara as horas do log de etapas com as perdas por sistema. Diferença fora da tolerância indica resultado inconsistente; revise os dados antes de interpretar.'),
          shiny::textOutput('sys_audit_status'),
          reactable::reactableOutput('sys_audit_table'),
          shiny::tags$p('O gráfico mostra médias entre execuções e, à direita, tempo até a primeira pane na primeira execução, com viaturas sem pane censuradas no fim da missão. Dados sintéticos continuam sintéticos.'),
          shinycssloaders::withSpinner(shiny::plotOutput('sys_diagnostic_chart',height='1050px'),type=6),
          shiny::textOutput('sys_bootstrap_note'),
          shiny::downloadButton('sys_audit_csv','Baixar conciliação CSV'),
          shiny::downloadButton('sys_diagnostic_svg','Baixar gráfico SVG'),
          shiny::downloadButton('sys_diagnostic_png','Baixar gráfico PNG'),
          shiny::downloadButton('sys_animation_gif','Baixar animação GIF'),
          shiny::tags$p('O GIF percorre eventos da primeira execução; não representa a média nem uma medição da frota real.')))
)
# Compatibilidade para integrações que renderizam o módulo isoladamente.
sys_ui = function() shiny::tags$div(sys_catalog_ui(),sys_configuration_ui(),sys_results_ui())
sys_server = function(input,output,session,questionnaire_provider,multicriteria_provider,snapshot_provider,references_provider,storage_dir=getwd(),questionnaire_sources_provider=NULL,on_completed=NULL,result_sink=NULL) {
  thematic::thematic_shiny(bg='#FFFFFF',fg='#0B1F33',accent='#1F5A94')
  input_validator = shinyvalidate::InputValidator$new()
  input_validator$add_rule('sys_target_id',shinyvalidate::sv_required('Escolha um alvo da intervenção.'))
  input_validator$add_rule('sys_horizon',function(value) if(is.null(value)||!is.finite(value)||value<=0) 'Informe duração positiva da missão.')
  input_validator$enable()
  profiles = shiny::reactiveVal(NULL); mapping = shiny::reactiveVal(data.frame(work_order_id=character(),component_id=character(),source=character()))
  result = shiny::reactiveVal(NULL)
  # O resultado também fica disponível ao relatório (Word e Excel) por meio de `result_sink`, um reactiveVal do aplicativo.
  if(is.function(result_sink)) shiny::observe({result_sink(result())})
  status = shiny::reactiveVal('Nenhuma execução por sistema realizada.'); data_status = shiny::reactiveVal('Nenhum catálogo carregado.')
  protect = function(expr) tryCatch(force(expr),error=function(e){shiny::showNotification(conditionMessage(e),type='error',duration=12); status(conditionMessage(e)); NULL})
  mode = function() if(is.null(input$sys_mode))'SYNTHETIC' else input$sys_mode
  profile_path = function() file.path(storage_dir,paste0('sys_profiles_',mode(),'.csv'))
  mapping_path = function() file.path(storage_dir,'sys_os_mapping.csv')
  invalidate = function() { result(NULL); status('Entradas alteradas: execute novamente para atualizar os resultados.') }
  source_state = shiny::reactive(sys_source_snapshot(questionnaire_provider(),multicriteria_provider(),snapshot_provider(),mapping(),profiles(),
    if(is.function(questionnaire_sources_provider))questionnaire_sources_provider() else NULL))
  current_source_signature = shiny::reactive(sys_source_signature(source_state()))
  # A new audit ID does not clear the result; changed data or derived judgments do.
  shiny::observeEvent(current_source_signature(),{
    r=shiny::isolate(result())
    if(!is.null(r) && !identical(r$source_signature,current_source_signature())) {
      result(NULL); status('Fontes alteradas (questionário/P7, AHP–TOPSIS, OS ou catálogo/vínculos): execute novamente. O resultado anterior foi invalidado.')
    }
  },ignoreInit=TRUE,priority=10)
  shiny::observeEvent(input$sys_mode,{
    profiles(NULL); invalidate()
    if(file.exists(profile_path())) protect({profiles(sys_validate_profiles(utils::read.csv(profile_path(),stringsAsFactors=FALSE,fileEncoding='UTF-8'),mode())); data_status('Catálogo salvo carregado.')})
  },ignoreInit=FALSE)
  shiny::observeEvent(input$sys_load_demo,{
    if(mode()!='SYNTHETIC') {shiny::showNotification('Selecione Demonstração sintética para carregar o exemplo.',type='error'); return()}
    profiles(sys_demo_profiles()); invalidate(); data_status('Exemplo SINTÉTICO: valores didáticos, sem registros reais de JLTV.')
  })
  shiny::observeEvent(input$sys_profiles_upload,protect({p=sys_validate_profiles(utils::read.csv(input$sys_profiles_upload$datapath,stringsAsFactors=FALSE,fileEncoding='UTF-8'),mode()); profiles(p); invalidate(); data_status('CSV validado; confira as fontes de cada parâmetro.')}))
  output$sys_profiles_table = DT::renderDT({shiny::req(profiles());p=profiles();
    brief=c('system_id','component_id','component_name','life_h','repair_h','evidence_class','source')
    DT::datatable(p,editable='cell',rownames=FALSE,extensions='Buttons',
      options=list(scrollX=TRUE,pageLength=5,dom='Bfrtip',buttons=list(list(extend='colvis',text='Colunas')),
        columnDefs=list(list(visible=FALSE,targets=which(!names(p)%in%brief)-1L))))})
  shiny::observeEvent(input$sys_profiles_table_cell_edit,{p=profiles(); info=input$sys_profiles_table_cell_edit; p[info$row,info$col+1]=info$value; profiles(p); invalidate()})
  shiny::observeEvent(list(input$sys_scope,profiles()),{
    p=profiles(); if(is.null(p))return()
    choices=if(identical(input$sys_scope,'component')) stats::setNames(p$component_id,paste(p$component_id,p$component_name)) else stats::setNames(unique(p$system_id),unname(sys_systems()[unique(p$system_id)]))
    shinyWidgets::updatePickerInput(session,'sys_target_id',choices=choices)
  })
  output$sys_profiles_download = shiny::downloadHandler(filename=function()paste0('catalogo_componentes_',mode(),'.csv'),content=function(file)utils::write.csv(if(is.null(profiles()))sys_demo_profiles() else profiles(),file,row.names=FALSE,fileEncoding='UTF-8'))
  shiny::observeEvent(input$sys_save_profiles,protect({p=sys_validate_profiles(profiles(),mode()); utils::write.csv(p,profile_path(),row.names=FALSE,fileEncoding='UTF-8'); data_status('Catálogo salvo no computador; será recarregado neste modo de evidência.')}))
  shiny::observeEvent(input$sys_mapping_upload,protect({p=shiny::req(profiles()); q=utils::read.csv(input$sys_mapping_upload$datapath,stringsAsFactors=FALSE,fileEncoding='UTF-8'); sys_observed(snapshot_provider()$work_orders,q,p); mapping(q); invalidate(); data_status('Vínculos de OS validados e carregados.')}))
  if(file.exists(mapping_path())) protect(mapping(utils::read.csv(mapping_path(),stringsAsFactors=FALSE,fileEncoding='UTF-8')))
  output$sys_mapping_table = DT::renderDT(DT::datatable(mapping(),rownames=FALSE,options=list(scrollX=TRUE,pageLength=5)))
  output$sys_mapping_download = shiny::downloadHandler(filename=function()'vinculos_OS_componentes.csv',content=function(file){
    q=mapping(); if(nrow(q)==0){o=snapshot_provider()$work_orders; q=data.frame(work_order_id=o$work_order_id,component_id=rep('',nrow(o)),source=rep('',nrow(o)))}
    utils::write.csv(q,file,row.names=FALSE,fileEncoding='UTF-8')})
  shiny::observeEvent(input$sys_save_mapping,protect({sys_observed(snapshot_provider()$work_orders,mapping(),shiny::req(profiles()));utils::write.csv(mapping(),mapping_path(),row.names=FALSE,fileEncoding='UTF-8');data_status('Vínculos de OS salvos.')}))
  output$sys_data_status = shiny::renderText(data_status())
  build_confront = function(r,sources) {
    t = r$systems[,c('system_id','system_name','down_h','share_down')]; t$questionnaire_position=NA_integer_; t$questionnaire_index=NA_real_
    mc=sources$multicriteria
    if(isTRUE(mc$estimable)) {rank=mc$ranking; ix=match(t$system_id,rank$alternative_id); t$questionnaire_position=ix; t$questionnaire_index=rank$coefficient[ix]}
    f=sources$questionnaire; t$p7_mentions=NA_integer_;t$p7_examples=''
    if(exists('p7_mentions_by_alternative',mode='function')){p7=p7_mentions_by_alternative(f);ix=match(t$system_id,p7$alternative_id); t$p7_mentions=p7$p7_mentions[ix];t$p7_examples=p7$p7_examples[ix]}
    o=sources$work_orders; obs=NULL
    t$observed_OS_hours=NA_real_
    if(nrow(sources$mapping)>0){obs=sys_observed(o,sources$mapping,r$profiles); classified=merge(obs$by_component,r$profiles[,c('component_id','system_id')],by='component_id');if(nrow(classified)>0){s=aggregate(classified$down_h,list(system_id=classified$system_id),sum);t$observed_OS_hours=s$x[match(t$system_id,s$system_id)]}}
    list(table=t,observed=obs)
  }
  shiny::observeEvent(input$sys_run,{
    if(!input_validator$is_valid()) { status('Corrija os campos destacados antes de executar.'); return() }
    result(NULL); status('Simulando referência e intervenções por componente...')
    protect({
      p=sys_validate_profiles(profiles(),mode()); cfg=list(fleet_size=input$sys_fleet,diagnostic_teams=input$sys_diag,repair_teams=input$sys_repair,bays=input$sys_bays,utilization=input$sys_util)
      n=input$sys_n; if(!is.finite(n)||n<2||n>500||n!=floor(n))stop('Execuções: inteiro entre 2 e 500.')
      scope=if(isTRUE(input$sys_all))'system' else input$sys_scope
      ids=if(isTRUE(input$sys_all))unique(p$system_id) else input$sys_target_id
      sources=source_state(); source_signature=sys_source_signature(sources); refs=references_provider()
      r=shiny::withProgress(message='Simulação por sistema',value=.1,sys_compare(p,cfg,input$sys_horizon,input$sys_target,input$sys_seed+seq_len(n)-1,scope,ids,input$sys_lever,input$sys_change,mode()))
      confront=build_confront(r,sources); r$questionnaire=confront$table; r$observed=confront$observed; r$mapping=sources$mapping
      r$source_snapshot=sources; r$source_signature=source_signature; r$project_references=refs
      r$settings=list(scope=scope,target_ids=ids,lever=input$sys_lever,change_percent=input$sys_change,compare_all=isTRUE(input$sys_all))
      result(r); if(is.function(on_completed)) on_completed(); status(paste('Concluído:',length(r$seeds),'execuções por cenário;',nrow(r$comparison),'intervenções.',r$mode,r$version))
    })
  })
  shiny::observeEvent(list(input$sys_fleet,input$sys_diag,input$sys_repair,input$sys_bays,input$sys_util,input$sys_horizon,input$sys_target,input$sys_n,input$sys_seed,input$sys_scope,input$sys_target_id,input$sys_lever,input$sys_change,input$sys_all),invalidate(),ignoreInit=TRUE)
  output$sys_run_status=shiny::renderText(status())
  fmt=function(x,d=1) formatC(x,format='f',digits=d,decimal.mark=',')
  output$sys_summary=shiny::renderUI({r=shiny::req(result());top=r$systems[1,];cp=r$components[1,];
    bslib::card(bslib::card_header(if(sum(r$systems$down_h)>0)paste('Maior perda simulada:',top$system_name) else 'Nenhuma parada nas execuções da referência'),
      shiny::tags$p(if(sum(r$systems$down_h)==0) 'Nenhum sistema causou indisponibilidade nesta amostra; não há gargalo físico demonstrado.' else paste0(top$system_name,' concentra ',fmt(100*top$share_down),'% das horas de viatura parada nesta referência. O componente com maior perda é ',cp$component_name,' (',cp$component_id,').')),
      shiny::tags$p(paste0('Disponibilidade: ',fmt(100*r$baseline_Ao[1]),'%. Missão atendida: ',sum(r$baseline$mission_success),' de ',length(r$seeds),' execuções; IC95% da probabilidade ',fmt(100*r$baseline_mission[2]),'%–',fmt(100*r$baseline_mission[3]),'%.')),
      shiny::tags$p(paste('Evidência:',r$mode,'— conclusão condicionada aos parâmetros. Não comprova o gargalo real sem registros representativos.')))
  })
  output$sys_system_plot=shiny::renderPlot({r=shiny::req(result());s=r$systems; graphics::par(mar=c(5,11,2,1));graphics::barplot(rev(s$down_h),names.arg=rev(s$system_name),horiz=TRUE,las=1,col='#1F5A94',xlab='Horas médias de viatura parada por execução',cex.names=.85)})
  output$sys_system_pie=shiny::renderPlot({r=shiny::req(result()); pie=shiny::req(chart_system_downtime_pie(r$systems)); pie})
  output$sys_ao_histogram=plotly::renderPlotly({
    r=shiny::req(result()); d=data.frame(availability=100*r$baseline$Ao)
    bins=max(5L,min(15L,ceiling(sqrt(nrow(d)))))
    p=ggplot2::ggplot(d,ggplot2::aes(x=availability))+
      ggplot2::geom_histogram(bins=bins,fill='#1F5A94',colour='white')+
      ggplot2::labs(x='Disponibilidade por execução (%)',y='Número de execuções')+
      ggplot2::theme_minimal()
    plotly::ggplotly(p,tooltip=c('x','y'))
  })
  output$sys_fleet_animation=plotly::renderPlotly({
    r=shiny::req(result()); events=r$events; fleet=as.integer(r$config$fleet_size)
    if(nrow(events)>60L) events=events[unique(round(seq(1,nrow(events),length.out=60L))),,drop=FALSE]
    ready=c(fleet,as.integer(events$ready)); times=c(0,as.numeric(events$time))
    frames=rep(seq_along(ready),each=2L)
    d=data.frame(frame=frames,
      situation=rep(c('Prontas','Indisponíveis'),length(ready)),
      vehicles=as.vector(rbind(ready,fleet-ready)),
      momento=rep(paste0('t=',format(round(times,1),trim=TRUE),' h'),each=2L))
    p=plotly::plot_ly(d,x=~situation,y=~vehicles,frame=~frame,type='bar',color=~situation,
      text=~momento,hovertemplate='%{text}: %{y} viaturas<extra></extra>',
      colors=c('Prontas'='#198F8C','Indisponíveis'='#C66454'),showlegend=FALSE) |>
      plotly::layout(yaxis=list(title='Viaturas',range=c(0,fleet+0.5)),xaxis=list(title='Primeira execução da referência'))
    if(length(ready)>1L) p=plotly::animation_opts(p,frame=650,transition=0,redraw=TRUE)
    p
  })
  table=function(d,hidden=integer()) { widget=DT::datatable(d,rownames=FALSE,extensions=if(length(hidden))'Buttons' else NULL,
    options=if(length(hidden))list(scrollX=TRUE,pageLength=5,dom='Bfrtip',buttons=list(list(extend='colvis',text='Colunas')),
      columnDefs=list(list(visible=FALSE,targets=hidden-1L))) else list(scrollX=TRUE,pageLength=5,dom=if(nrow(d)<=5)'t' else 'tip'))
    numeric_cols=names(d)[vapply(d,function(x)is.numeric(x)&&!is.integer(x),logical(1))]
    if(length(numeric_cols)) widget=DT::formatRound(widget,numeric_cols,digits=2,dec.mark=',',mark='.')
    widget
  }
  output$sys_system_results=reactable::renderReactable({r=shiny::req(result());d=r$systems;d$share_down=100*d$share_down;names(d)=c('Sistema ID','Falhas médias','Horas paradas médias','Perda de Ao (p.p.)','Parcela da parada (%)','Sistema');reactable::reactable(d,searchable=TRUE,filterable=TRUE,sortable=TRUE,defaultPageSize=10,highlight=TRUE,striped=TRUE)})
  output$sys_component_results=DT::renderDT({r=shiny::req(result());d=r$components;table(d)})
  output$sys_stage_results=DT::renderDT({r=shiny::req(result());d=r$stages;d$stage=unname(sys_stage_labels()[d$stage]);names(d)=c('Sistema','Componente','Etapa','Horas médias');table(d)})
  output$sys_intervention_results=DT::renderDT({r=shiny::req(result());d=r$comparison;d[,c('Ao','delta_Ao','lower95','upper95','mission_probability','mission_lower95','mission_upper95','delta_mission','delta_mission_lower95','delta_mission_upper95')]=100*d[,c('Ao','delta_Ao','lower95','upper95','mission_probability','mission_lower95','mission_upper95','delta_mission','delta_mission_lower95','delta_mission_upper95')];names(d)=c('Alvo','Escopo','Intervenção','Alteração (%)','Ao (%)','Ganho Ao (p.p.)','IC95% ganho inferior','IC95% ganho superior','Missão (%)','IC95% missão inferior','IC95% missão superior','Ganho missão (p.p.)','IC95% ganho missão inferior','IC95% ganho missão superior','Componentes alcançados','Conclusão');table(d,hidden=c(2,9:15))})
  output$sys_confront=DT::renderDT({r=shiny::req(result());d=r$questionnaire;d$share_down=100*d$share_down;names(d)=c('Sistema ID','Sistema','Horas simuladas','Parcela simulada (%)','Posição questionário','Índice TOPSIS','Menções P7','Exemplos P7 a confirmar','Horas registradas OS');table(d,hidden=8)})
  output$sys_observed_status=shiny::renderText({r=shiny::req(result());o=r$observed;if(is.null(o))'Sem vínculos de OS: atribuição observada não estimável.' else paste(o$classified,'OS classificadas;',o$unmapped,'OS sem vínculo;',o$invalid_times,'OS classificadas sem duração válida. Horas de OS podem sobrepor.')})
  output$sys_excel=shiny::downloadHandler(filename=function()paste0('JLTV_por_sistema_',format(Sys.time(),'%Y%m%d_%H%M%S'),'.xlsx'),content=function(file){r=shiny::req(result());shiny::validate(shiny::need(identical(r$source_signature,current_source_signature()),'Fontes alteradas: execute novamente antes de exportar.'));sys_export(r,file,r$questionnaire,r$observed,r$mapping,r$project_references)})
  output$sys_audit_status=shiny::renderText({r=shiny::req(result());if(jltv_quality_agent(r)&&jltv_wilson_audit(r))'Conciliação aprovada: horas por sistema e intervalo da missão coincidem com os registros.' else 'ATENÇÃO: a conciliação de horas ou do intervalo da missão falhou. Revise antes de interpretar.'})
  output$sys_audit_table=reactable::renderReactable({a=jltv_stage_audit(shiny::req(result()));reactable::reactable(a,searchable=TRUE,defaultPageSize=10,striped=TRUE,columns=list(consistent=reactable::colDef(name='Consistente')))})
  output$sys_diagnostic_chart=shiny::renderPlot({print(jltv_diagnostic_chart(shiny::req(result())))})
  output$sys_bootstrap_note=shiny::renderText({r=shiny::req(result());q=jltv_bootstrap_ao(r);if(anyNA(q))return('Faixa bootstrap descritiva requer pelo menos 10 execuções; aumente o número na aba 3.');paste0('Faixa descritiva de Ao média por reamostragem das execuções: ',round(100*q[1],1),'–',round(100*q[2],1),'%. Condicional aos parâmetros e à amostra simulada; não substitui o IC95% do relatório nem inclui incerteza dos parâmetros.')})
  valid_export=function(){r=shiny::req(result());shiny::validate(shiny::need(identical(r$source_signature,current_source_signature()),'Fontes alteradas: execute novamente antes de exportar.'));r}
  output$sys_audit_csv=shiny::downloadHandler(filename=function()'JLTV_conciliacao_horas.csv',content=function(file)jltv_export_audit_csv(valid_export(),file),contentType='text/csv')
  output$sys_diagnostic_svg=shiny::downloadHandler(filename=function()'JLTV_diagnostico.svg',content=function(file)jltv_export_svg(valid_export(),file),contentType='image/svg+xml')
  output$sys_diagnostic_png=shiny::downloadHandler(filename=function()'JLTV_diagnostico.png',content=function(file)jltv_export_png(valid_export(),file),contentType='image/png')
  output$sys_animation_gif=shiny::downloadHandler(filename=function()'JLTV_prontidao_primeira_execucao.gif',content=function(file)jltv_export_animation(valid_export(),file),contentType='image/gif')
  invisible(shiny::reactive({
    r=result()
    if(is.null(r)||!identical(r$source_signature,current_source_signature()))return(NULL)
    r
  }))
}

# FIM DO MODULO: jltv_systems.R

# Relatório em Word (texto de análise + gráficos; IA opcional).

# ========================================================================
# INICIO DO MODULO: jltv_multicriteria_views.R
# ========================================================================
# ============================================================================
# AHP–TOPSIS: histogramas, dispersão dos especialistas e robustez do ranking.
#
# Tudo é calculado a partir do objeto `mc` de compute_multicriteria() (que passa a
# guardar `raw_pairwise` e `raw_scores`). Usado na aba "2. Prioridades", no Excel e no Word.
# Base metodológica (Drive do TCC):
#   - histograma da razão de consistência (RC < 0,10) e dispersão entre avaliadores;
#   - estabilidade do ranking por reamostragem dos avaliadores (bootstrap);
#   - sensibilidade a pesos (AHP × pesos iguais) e à normalização (vetorial × min–max),
#     pois o ranking TOPSIS pode mudar com método, normalização e pesos
#     (Sałabun, Wątróbski e Shekhovtsov, 2020).
# ============================================================================

mc_has_raw = function(mc) {
  is.list(mc) && is.data.frame(mc$raw_scores) && nrow(mc$raw_scores) > 0L
}

mc_numeric_scores = function(mc) {
  if (!mc_has_raw(mc)) return(data.frame())
  d = mc$raw_scores
  d$value = suppressWarnings(as.numeric(as.character(d$score)))
  d[is.finite(d$value), , drop = FALSE]
}

# ---- Histograma da RC individual -------------------------------------------
# Faixas largas e fixas (e não dezenas de faixas finas): a contagem de cada faixa aparece sobre a barra, e a linha
# tracejada separa as matrizes consistentes (RC < 0,10) das demais, no padrão visual dos outros gráficos.
mc_cr_classes = function() {
  list(
    breaks = c(0, 0.05, 0.10, 0.20, 0.30, 0.50, 1, Inf),
    labels = c("< 0,05", "0,05–0,10", "0,10–0,20", "0,20–0,30", "0,30–0,50", "0,50–1,00", "> 1,00")
  )
}

mc_cr_counts = function(cr) {
  cls = mc_cr_classes()
  faixa = cut(cr, breaks = cls$breaks, labels = cls$labels, right = FALSE, include.lowest = TRUE)
  counts = as.data.frame(table(faixa), stringsAsFactors = FALSE)
  names(counts) = c("faixa", "n")
  counts$faixa = factor(counts$faixa, levels = cls$labels)
  counts$status = factor(
    ifelse(as.integer(counts$faixa) <= 2L, "RC < 0,10 (consistente)", "RC ≥ 0,10 (acima do limite)"),
    levels = c("RC < 0,10 (consistente)", "RC ≥ 0,10 (acima do limite)")
  )
  counts
}

# Histograma no padrão "média e desvios padrão": muitas faixas estreitas; barras laranja até 1 desvio padrão da
# média (μ), roxas entre 1 e 2, azuis além de 2; a faixa que contém a média em rosa; linhas verdes em μ ± 1σ e
# μ ± 2σ (só as que caem dentro dos dados) e, quando informado, um limite de referência em vermelho tracejado.
sigma_histogram_colors = function() c(mu = "#F2B8C6", s1 = "#F28E2B", s2 = "#7E3FA6", tail = "#2E75B6")

chart_sigma_histogram = function(x, xlab, ylab = "Frequência", digits = 3L, bins = 30L, limit = NULL,
                                 limit_label = NULL, caption = NULL, title = NULL, bin_width = NULL, origin = NULL, cap = NULL) {
  x = as.numeric(x)
  x = x[is.finite(x)]
  n = length(x)
  if (n < 3L) return(NULL)
  mu = mean(x)
  s = stats::sd(x)
  lo = min(x)
  hi = max(x)
  n_over = 0L
  if (is.numeric(cap) && length(cap) == 1L && is.finite(cap) && hi > cap && lo < cap) {
    n_over = sum(x > cap)
    hi = cap
  }
  if (hi - lo < 1e-12) { lo = lo - 0.5 * 10^-digits; hi = hi + 0.5 * 10^-digits }
  if (is.numeric(bin_width) && length(bin_width) == 1L && bin_width > 0) {
    width = bin_width
    while ((hi - lo) / width > 45) width = width * 2
    start = if (is.numeric(origin)) origin + floor((lo - origin) / width) * width else floor(lo / width) * width
    k = max(1L, as.integer(ceiling((hi - start) / width - 1e-9)))
    if (start + k * width <= hi) k = k + 1L
    lo = start
    hi = start + k * width
  } else {
    k = as.integer(max(12L, min(as.integer(bins), ceiling(2.5 * sqrt(n)))))
    width = (hi - lo) / k
  }
  breaks = lo + (0:k) * width
  x_in = if (n_over > 0L) x[x <= hi] else x
  idx = pmin(k, pmax(1L, findInterval(x_in, breaks, rightmost.closed = FALSE, all.inside = TRUE)))
  counts = tabulate(idx, k)
  center = breaks[-1L] - width / 2
  if (n_over > 0L) {
    # Faixa final "Mais": valores acima do teto, como no histograma de frequências do Excel.
    k = k + 1L
    counts = c(counts, n_over)
    center = c(center, hi + width / 2)
  }
  dist = abs(center - mu)
  band = ifelse(s > 0 & dist <= s, "s1", ifelse(s > 0 & dist <= 2 * s, "s2", "tail"))
  mu_bin = min(k, max(1L, findInterval(mu, breaks, rightmost.closed = TRUE, all.inside = TRUE)))
  band[mu_bin] = "mu"
  if (s == 0) band[] = "mu"
  lab = vapply(center, format_number_br, character(1), digits = digits)
  if (n_over > 0L) lab[[k]] = "Mais"
  d = data.frame(pos = seq_len(k), n = counts, band = factor(band, levels = names(sigma_histogram_colors())),
                 label = factor(lab, levels = unique(lab)), stringsAsFactors = FALSE)
  if (anyDuplicated(lab)) d$label = factor(seq_len(k), labels = make.unique(lab))
  pos = function(v) (v - lo) / width + 0.5
  inside = function(v) is.finite(v) && v > lo && v < hi
  ymax = max(counts)
  top = ymax * 1.22
  p = ggplot2::ggplot(d, ggplot2::aes(x = pos, y = n, fill = band)) +
    ggplot2::geom_col(width = 0.92, colour = "#3A3A3A", linewidth = 0.15) +
    ggplot2::scale_fill_manual(values = sigma_histogram_colors(), guide = "none") +
    ggplot2::scale_x_continuous(breaks = d$pos, labels = levels(d$label)[as.integer(d$label)],
                                limits = c(0.4, k + 0.6), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, top), expand = c(0, 0),
                                breaks = function(z) { b = pretty(c(0, ymax)); b[b == round(b) & b <= ymax] })
  green = "#2DB84B"
  lines = c(mu - 2 * s, mu - s, mu + s, mu + 2 * s)
  for (v in lines[vapply(lines, inside, logical(1))]) {
    p = p + ggplot2::annotate("segment", x = pos(v), xend = pos(v), y = 0, yend = top * 0.96, colour = green, linewidth = 1.3)
  }
  txt = function(xp, yp, label, size = 3.1, face = "plain") ggplot2::annotate("text", x = xp, y = yp, label = label, size = size,
                                                                               fontface = face, colour = "#1F1F1F", lineheight = 0.9)
  if (counts[[mu_bin]] == 0) {
    # Faixa da média sem observações: marca a média com um traço rosa para não deixar o μ solto.
    p = p + ggplot2::annotate("segment", x = pos(mu), xend = pos(mu), y = 0, yend = ymax * 0.12, colour = "#E07A9A", linewidth = 2.2)
  }
  p = p + txt(if (counts[[mu_bin]] == 0) pos(mu) else mu_bin, max(counts[[mu_bin]], ymax * 0.12) + ymax * 0.06, "μ", size = 5.2, face = "bold")
  for (sg in c(-1, 1)) {
    a = mu + sg * s
    b = mu + sg * 2 * s
    if (s > 0 && inside(a)) p = p + txt((mu_bin + pos(a)) / 2, top * 0.91, "1 desvio\npadrão", size = 2.9)
    if (s > 0 && inside(a)) {
      edge = if (inside(b)) pos(b) else if (sg > 0) k + 0.5 else 0.5
      if (abs(edge - pos(a)) >= 2) p = p + txt((pos(a) + edge) / 2, top * 0.91, "2 desvios\npadrão", size = 3.4)
    }
  }
  if (!is.null(limit) && is.finite(limit) && limit >= lo - width && limit <= hi + width) {
    p = p + ggplot2::annotate("segment", x = pos(limit), xend = pos(limit), y = 0, yend = top * 0.96,
                              colour = "#B23A48", linewidth = 0.8, linetype = "dashed")
    if (!is.null(limit_label)) p = p + ggplot2::annotate("text", x = pos(limit), y = top * 0.985, label = limit_label, size = 2.9,
                                                         colour = "#B23A48", fontface = "bold", vjust = 1, hjust = -0.08)
  }
  p = p + ggplot2::labs(x = xlab, y = ylab, caption = caption, title = title) +
    assessment_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7.5, colour = "#24384C"),
      panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "#9AA9B8", fill = NA, linewidth = 0.4),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
  p
}

sigma_histogram_legend = function() {
  "Barras: laranja até 1 desvio padrão da média (μ, em rosa), roxo entre 1 e 2 desvios padrão e azul além de 2; linhas verdes em μ ± 1 e μ ± 2 desvios padrão."
}

chart_ahp_cr_histogram = function(mc, titles = FALSE) {
  d = if (is.list(mc)) mc$ahp$individual_diagnostics else NULL
  if (!is.data.frame(d) || nrow(d) == 0L) return(NULL)
  spec = questionnaire_hierarchy_spec()
  size = vapply(d$hierarchy_group, function(g) length(spec[[g]]), numeric(1))
  d = d[d$complete & is.finite(d$CR) & size >= 3L, , drop = FALSE]  # RC = 0 por construção com 2 itens
  if (nrow(d) < 3L) return(NULL)
  n_ok = sum(d$CR < 0.10)
  top_cr = min(max(d$CR), 1)
  bw = if (top_cr <= 0.25) 0.0125 else if (top_cr <= 0.5) 0.025 else 0.05
  caption = paste(strwrap(paste0(
    nrow(d), " matrizes; média μ = ", format_number_br(mean(d$CR), 3), " e desvio padrão = ", format_number_br(stats::sd(d$CR), 3), ". ",
    n_ok, " (", format_percent_br(n_ok / nrow(d), 0), ") têm RC < 0,10, o limite usual de Saaty (linha vermelha tracejada). ",
    sigma_histogram_legend(), if (any(d$CR > 1)) " A faixa \"Mais\" reúne RC acima de 1,00." else "", " Grupos com 2 itens têm RC = 0 por construção e ficam fora do gráfico."
  ), width = 100), collapse = "\n")
  chart_sigma_histogram(d$CR, xlab = "Razão de consistência (RC) por avaliador e grupo", ylab = "Número de matrizes",
                        digits = 3L, limit = 0.10, limit_label = "RC = 0,10", caption = caption, bin_width = bw, origin = 0, cap = 1,
                        title = if (titles) "Consistência dos julgamentos AHP" else NULL)
}

# Distribuição da disponibilidade simulada nas replicações de Monte Carlo da situação de referência.
chart_availability_histogram = function(replications, availability_target = NA_real_, titles = FALSE) {
  if (!is.data.frame(replications) || !"avg_availability" %in% names(replications)) return(NULL)
  x = 100 * as.numeric(replications$avg_availability)
  x = x[is.finite(x)]
  if (length(x) < 3L) return(NULL)
  target = if (is.finite(availability_target)) 100 * availability_target else NULL
  caption = paste(strwrap(paste0(
    length(x), " replicações; média μ = ", format_number_br(mean(x), 2), "% e desvio padrão = ", format_number_br(stats::sd(x), 2), " p.p.",
    if (!is.null(target) && target >= min(x) && target <= max(x)) paste0(" Linha vermelha tracejada: fração M/N de ", format_number_br(target, 1), "%.")
    else if (!is.null(target)) paste0(" A fração M/N (", format_number_br(target, 1), "%) fica fora da faixa das replicações.") else "", " ",
    sigma_histogram_legend()), width = 100), collapse = "\n")
  chart_sigma_histogram(x, xlab = "Disponibilidade simulada da frota em cada replicação (%)", ylab = "Número de replicações",
                        digits = 2L, limit = target, limit_label = "M/N", caption = caption,
                        title = if (titles) "Disponibilidade simulada nas replicações" else NULL)
}

# ---- Participação de cada sistema no tempo parado (pizza) -------------------
# `systems` é o quadro `systems` do resultado da simulação por sistema (sys_compare): uma linha por sistema, com as
# horas médias de viatura parada por execução (down_h). A fatia é a parcela do sistema na soma dessas horas.
system_pie_palette = function() {
  c("#1F5A94", "#B23A48", "#198F8C", "#C27C0E", "#6C4F9E", "#5E8C31", "#D2691E", "#4A6C8F", "#9B4F7A", "#7A8794")
}

system_downtime_shares = function(systems) {
  if (!is.data.frame(systems) || nrow(systems) == 0L || !all(c("system_id", "system_name", "down_h") %in% names(systems))) return(NULL)
  d = systems[is.finite(systems$down_h) & systems$down_h > 0, , drop = FALSE]
  if (nrow(d) == 0L || sum(d$down_h) <= 0) return(NULL)
  d = d[order(-d$down_h, d$system_id), , drop = FALSE]
  # Nome todo em maiúsculas e com mais de uma palavra ("SISTEMA ELÉTRICO/ ELETRÔNICO") vira frase; siglas de uma palavra ficam.
  name = as.character(d$system_name)
  caps = name == toupper(name) & grepl(" ", name, fixed = TRUE)
  low = tolower(name)
  name[caps] = paste0(toupper(substr(low[caps], 1L, 1L)), substring(low[caps], 2L))
  d$system_name = name
  d$share = d$down_h / sum(d$down_h)
  rownames(d) = NULL
  d
}

chart_system_downtime_pie = function(systems, titles = FALSE) {
  d = system_downtime_shares(systems)
  if (is.null(d)) return(NULL)
  d$legend = paste0(d$system_id, " ", d$system_name, "  ", vapply(d$share, format_percent_br, character(1), digits = 1))
  d$legend = factor(d$legend, levels = d$legend)
  d$label = ifelse(d$share >= 0.08, vapply(d$share, format_percent_br, character(1), digits = 1), "")
  colors = stats::setNames(rep_len(system_pie_palette(), nrow(d)), levels(d$legend))
  caption = paste(strwrap(
    "Parcela de cada sistema nas horas médias de viatura parada por execução (situação de referência). A parcela é a atribuição simulada da parada ao sistema que a causou; não é a prioridade dos especialistas.",
    width = 82), collapse = "\n")
  p = ggplot2::ggplot(d, ggplot2::aes(x = 1, y = share, fill = legend)) +
    ggplot2::geom_col(width = 1, colour = "white", linewidth = 0.6, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(ggplot2::aes(x = 1.3, label = label), position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
                       colour = "white", fontface = "bold", size = 3.4) +
    ggplot2::coord_polar(theta = "y", start = 0) +
    ggplot2::scale_x_continuous(limits = c(0.5, 1.5), expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(x = NULL, y = NULL, caption = caption) +
    assessment_theme() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(), legend.position = "right", legend.text = ggplot2::element_text(size = 10),
      legend.key.size = ggplot2::unit(0.45, "cm")
    )
  if (titles) p = p + ggplot2::labs(title = "Participação de cada sistema no tempo parado")
  p
}

# ---- Pesos individuais dos critérios (dispersão entre avaliadores) ---------
mc_individual_criteria_weights = function(mc, group = "criteria") {
  if (!is.list(mc) || !is.data.frame(mc$raw_pairwise) || nrow(mc$raw_pairwise) == 0L) return(data.frame())
  spec = questionnaire_hierarchy_spec()
  items = names(spec[[group]])
  diag = mc$ahp$individual_diagnostics
  if (!is.data.frame(diag)) return(data.frame())
  ids = unique(as.character(diag$respondent_id[diag$hierarchy_group == group & diag$complete]))
  rows = lapply(ids, function(id) {
    data = mc$raw_pairwise[mc$raw_pairwise$respondent_id == id & mc$raw_pairwise$hierarchy_group == group, , drop = FALSE]
    r = try(ahp_priority(ahp_matrix_from_rows(data, items)), silent = TRUE)
    if (inherits(r, "try-error")) return(NULL)
    data.frame(respondent_id = id, criterion = names(r$weights), weight = as.numeric(r$weights), stringsAsFactors = FALSE)
  })
  rows = rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) return(data.frame())
  do.call(rbind, rows)
}

chart_ahp_weight_dispersion = function(mc, titles = FALSE) {
  w = mc_individual_criteria_weights(mc)
  if (nrow(w) == 0L) return(NULL)
  labels = unlist(assessment_criterion_labels())
  w$label = factor(unname(labels[w$criterion]), levels = rev(unname(labels)))
  agg = mc$ahp$criteria_weights
  ref = data.frame(
    label = factor(unname(labels[names(agg)]), levels = levels(w$label)),
    weight = as.numeric(agg), stringsAsFactors = FALSE
  )
  p = ggplot2::ggplot(w, ggplot2::aes(x = label, y = weight)) +
    ggplot2::geom_boxplot(fill = "#DCE8F3", colour = "#1F5A94", outlier.shape = NA, width = 0.55) +
    ggplot2::geom_jitter(width = 0.12, height = 0, alpha = 0.55, colour = "#0B1F33", size = 1.6) +
    ggplot2::geom_point(data = ref, shape = 18, size = 4.5, colour = "#B23A48") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      x = NULL, y = "Peso do critério segundo cada avaliador",
      caption = "Pontos: avaliadores. Losango vermelho: peso agregado usado no TOPSIS. Quanto maior a caixa, maior a divergência entre especialistas."
    ) +
    assessment_theme()
  if (titles) p = p + ggplot2::labs(title = "Dispersão dos pesos AHP entre avaliadores")
  p
}

# ---- Distribuição das notas P6 ---------------------------------------------
chart_p6_histogram = function(mc, titles = FALSE) {
  d = mc_numeric_scores(mc)
  if (nrow(d) == 0L) return(NULL)
  d$nota = factor(round(d$value), levels = 1:10)
  p = ggplot2::ggplot(d, ggplot2::aes(x = nota)) +
    ggplot2::geom_bar(fill = "#1F5A94", width = 0.8) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(
      x = "Nota P6 (1 a 10)", y = "Número de avaliações",
      caption = paste0(nrow(d), " avaliações válidas; N/A não entra.")
    ) +
    assessment_theme()
  if (titles) p = p + ggplot2::labs(title = "Distribuição das notas P6")
  p
}

chart_p6_boxplot_by_system = function(mc, titles = FALSE) {
  d = mc_numeric_scores(mc)
  if (nrow(d) == 0L) return(NULL)
  d$system = paste(d$alternative_id, d$alternative_name)
  order_ids = if (isTRUE(mc$topsis$estimable)) mc$topsis$ranking$alternative_id else
    names(sort(tapply(d$value, d$alternative_id, stats::median), decreasing = TRUE))
  lookup = unique(d[, c("alternative_id", "system")])
  levels_system = lookup$system[match(order_ids, lookup$alternative_id)]
  levels_system = levels_system[!is.na(levels_system)]
  d$system = factor(d$system, levels = rev(levels_system))
  d = d[!is.na(d$system), , drop = FALSE]
  p = ggplot2::ggplot(d, ggplot2::aes(x = system, y = value)) +
    ggplot2::geom_boxplot(fill = "#DCE8F3", colour = "#1F5A94", outlier.alpha = 0.4) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(limits = c(1, 10), breaks = 1:10) +
    ggplot2::labs(
      x = NULL, y = "Nota P6 (todas as avaliações dos subcritérios)",
      caption = "Sistemas na ordem do ranking TOPSIS (1º no topo)."
    ) +
    assessment_theme()
  if (titles) p = p + ggplot2::labs(title = "Notas P6 por sistema")
  p
}

# ---- Robustez do ranking TOPSIS --------------------------------------------
# v1.5.7 — a reamostragem é cara com centenas de avaliadores; o resultado é guardado em memória
# (tela, Excel e Word pedem a mesma robustez várias vezes) e a reamostragem é vetorizada.
.mc_stability_cache = new.env(parent = emptyenv())

mc_rank_stability = function(mc, B = 300L, seed = 20261005L) {
  if (!is.list(mc) || !isTRUE(mc$topsis$estimable) || !mc_has_raw(mc)) return(NULL)
  key = tryCatch(paste(
    nrow(mc$raw_scores),
    format(sum(suppressWarnings(as.numeric(as.character(mc$raw_scores$score))), na.rm = TRUE), digits = 15),
    format(sum(mc$ahp$subcriteria_weights$global_weight), digits = 15),
    paste(round(mc$topsis$ranking$coefficient, 10), collapse = ","),
    mc$score_aggregation, B, seed, sep = "|"
  ), error = function(e) NULL)
  if (!is.null(key) && !is.null(.mc_stability_cache[[key]])) return(.mc_stability_cache[[key]])
  res = mc_rank_stability_compute(mc, B, seed)
  if (!is.null(key) && !is.null(res)) {
    if (length(ls(.mc_stability_cache)) > 8L) rm(list = ls(.mc_stability_cache), envir = .mc_stability_cache)
    assign(key, res, envir = .mc_stability_cache)
  }
  res
}

mc_rank_stability_compute = function(mc, B = 300L, seed = 20261005L) {
  sc = mc_numeric_scores(mc)
  ids = unique(as.character(sc$respondent_id))
  if (length(ids) < 3L) return(NULL)
  weights = mc$ahp$subcriteria_weights
  base = mc$topsis$ranking
  keep = as.character(base$alternative_id)
  sc = sc[as.character(sc$alternative_id) %in% keep, , drop = FALSE]
  fun = if (identical(mc$score_aggregation, "median")) stats::median else mean
  by_resp = split(sc, as.character(sc$respondent_id))
  names_by_id = unique(sc[, c("alternative_id", "alternative_name")])
  old_seed = if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  set.seed(seed)
  ranks = matrix(NA_real_, nrow = B, ncol = length(keep), dimnames = list(NULL, keep))
  # Matriz avaliador × (sistema, subcritério): reamostrar vira somar linhas, sem montar tabelas grandes.
  cell_key = paste(as.character(sc$alternative_id), as.character(sc$subcriterion_id), sep = "\r")
  cell_levels = unique(cell_key)
  ri = match(as.character(sc$respondent_id), ids)
  ci = match(cell_key, cell_levels)
  fast = !anyDuplicated(cbind(ri, ci))
  if (fast) {
    V = matrix(NA_real_, length(ids), length(cell_levels))
    V[cbind(ri, ci)] = sc$value
    M = !is.na(V)
    V0 = V
    V0[!M] = 0
    Mn = M * 1
    first = match(cell_levels, cell_key)
    cell_alt = sc$alternative_id[first]
    cell_sub = sc$subcriterion_id[first]
    use_median = identical(mc$score_aggregation, "median")
  }
  for (b in seq_len(B)) {
    if (fast) {
      pick_i = sample.int(length(ids), length(ids), replace = TRUE)
      val = if (use_median) {
        apply(V[pick_i, , drop = FALSE], 2, function(x) {
          x = x[!is.na(x)]
          if (length(x)) stats::median(x) else NA_real_
        })
      } else {
        cnt = tabulate(pick_i, length(ids))
        as.numeric(crossprod(cnt, V0)) / as.numeric(crossprod(cnt, Mn))
      }
      ok_cell = is.finite(val)
      agg = data.frame(
        alternative_id = cell_alt[ok_cell], subcriterion_id = cell_sub[ok_cell], value = val[ok_cell],
        stringsAsFactors = FALSE
      )
    } else {
      pick = sample(ids, length(ids), replace = TRUE)
      boot = do.call(rbind, by_resp[pick])
      agg = stats::aggregate(value ~ alternative_id + subcriterion_id, data = boot, FUN = fun)
    }
    agg$alternative_name = names_by_id$alternative_name[match(agg$alternative_id, names_by_id$alternative_id)]
    res = tryCatch(topsis_rank(agg, weights), error = function(e) NULL)
    if (is.null(res) || !isTRUE(res$estimable)) next
    r = res$ranking
    ranks[b, as.character(r$alternative_id)] = r$rank
  }
  ok = rowSums(is.na(ranks)) == 0L
  ranks = ranks[ok, , drop = FALSE]
  if (nrow(ranks) < 20L) return(NULL)
  summary = data.frame(
    alternative_id = keep,
    alternative_name = as.character(base$alternative_name),
    rank_ref = base$rank, coefficient_ref = base$coefficient,
    median_rank = apply(ranks, 2, stats::median),
    rank_p05 = apply(ranks, 2, function(x) as.numeric(stats::quantile(x, 0.05, type = 1))),
    rank_p95 = apply(ranks, 2, function(x) as.numeric(stats::quantile(x, 0.95, type = 1))),
    prob_top1 = colMeans(ranks == 1), prob_top3 = colMeans(ranks <= 3),
    stringsAsFactors = FALSE
  )
  n_alt = length(keep)
  prob = do.call(rbind, lapply(seq_len(n_alt), function(j) {
    data.frame(
      alternative_id = keep[[j]], alternative_name = as.character(base$alternative_name[[j]]),
      rank = seq_len(n_alt), prob = vapply(seq_len(n_alt), function(r) mean(ranks[, j] == r), numeric(1)),
      stringsAsFactors = FALSE
    )
  }))
  list(replications = nrow(ranks), requested = B, summary = summary, probabilities = prob, respondents = length(ids))
}

chart_rank_probability = function(stability, titles = FALSE) {
  if (is.null(stability)) return(NULL)
  d = stability$probabilities
  s = stability$summary
  d$system = paste(d$alternative_id, d$alternative_name)
  s$system = paste(s$alternative_id, s$alternative_name)
  d$system = factor(d$system, levels = rev(s$system[order(s$rank_ref)]))
  d$label = ifelse(d$prob >= 0.05, paste0(round(100 * d$prob), "%"), "")
  p = ggplot2::ggplot(d, ggplot2::aes(x = factor(rank), y = system, fill = prob)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3.2, colour = "#0B1F33") +
    ggplot2::scale_fill_gradient(low = "#F4F8FB", high = "#1F5A94", labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    ggplot2::labs(
      x = "Posição no ranking TOPSIS", y = NULL,
      caption = paste0(stability$replications, " reamostragens dos ", stability$respondents,
                       " avaliadores (com reposição). Cada célula: frequência da posição.")
    ) +
    assessment_theme() +
    ggplot2::theme(legend.position = "right", panel.grid = ggplot2::element_blank())
  if (titles) p = p + ggplot2::labs(title = "Estabilidade do ranking TOPSIS")
  p
}

# ---- Sensibilidade a pesos e normalização ----------------------------------
mc_topsis_minmax = function(aggregated, weights) {
  criteria = as.character(weights$subcriterion_id)
  mat = tapply(aggregated$value, list(aggregated$alternative_id, aggregated$subcriterion_id), function(x) x[[1]])
  if (is.null(mat) || any(!criteria %in% colnames(mat))) return(NULL)
  mat = mat[, criteria, drop = FALSE]
  if (any(!is.finite(mat))) return(NULL)
  range = apply(mat, 2, function(x) max(x) - min(x))
  norm = sweep(sweep(mat, 2, apply(mat, 2, min), "-"), 2, ifelse(range > 0, range, 1), "/")
  w = weights$global_weight[match(criteria, weights$subcriterion_id)]
  w = w / sum(w)
  weighted = sweep(norm, 2, w, "*")
  d_plus = sqrt(rowSums((sweep(weighted, 2, apply(weighted, 2, max), "-"))^2))
  d_minus = sqrt(rowSums((sweep(weighted, 2, apply(weighted, 2, min), "-"))^2))
  coef = ifelse(d_plus + d_minus > 0, d_minus / (d_plus + d_minus), 0.5)
  data.frame(alternative_id = rownames(mat), coefficient = as.numeric(coef), stringsAsFactors = FALSE)
}

mc_method_sensitivity = function(mc) {
  if (!is.list(mc) || !isTRUE(mc$topsis$estimable)) return(NULL)
  base = mc$topsis$ranking
  weights = mc$ahp$subcriteria_weights
  aggregated = mc$aggregated_scores
  aggregated = aggregated[as.character(aggregated$alternative_id) %in% as.character(base$alternative_id), , drop = FALSE]
  equal = weights
  equal$global_weight = 1 / nrow(weights)
  eq = tryCatch(topsis_rank(aggregated, equal), error = function(e) NULL)
  mm = tryCatch(mc_topsis_minmax(aggregated, weights), error = function(e) NULL)
  out = data.frame(
    alternative_id = as.character(base$alternative_id), alternative_name = as.character(base$alternative_name),
    posicao_ahp_vetorial = base$rank, coeficiente_ahp_vetorial = base$coefficient,
    stringsAsFactors = FALSE
  )
  if (!is.null(eq) && isTRUE(eq$estimable)) {
    out$posicao_pesos_iguais = eq$ranking$rank[match(out$alternative_id, eq$ranking$alternative_id)]
  } else out$posicao_pesos_iguais = NA_integer_
  if (!is.null(mm)) {
    mm$rank = rank(-mm$coefficient, ties.method = "min")
    out$posicao_minmax = mm$rank[match(out$alternative_id, mm$alternative_id)]
  } else out$posicao_minmax = NA_integer_
  rho = function(x) if (sum(is.finite(x)) >= 3L) suppressWarnings(stats::cor(out$posicao_ahp_vetorial, x, method = "spearman", use = "complete.obs")) else NA_real_
  attr(out, "spearman_pesos_iguais") = rho(out$posicao_pesos_iguais)
  attr(out, "spearman_minmax") = rho(out$posicao_minmax)
  out
}

mc_method_sensitivity_display = function(sens) {
  if (!is.data.frame(sens) || nrow(sens) == 0L) return(data.frame())
  data.frame(
    `Código` = sens$alternative_id, Sistema = sens$alternative_name,
    `Posição (pesos AHP, normalização vetorial)` = sens$posicao_ahp_vetorial,
    `Coeficiente` = round(sens$coeficiente_ahp_vetorial, 3),
    `Posição com pesos iguais` = sens$posicao_pesos_iguais,
    `Posição com normalização min–max` = sens$posicao_minmax,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

mc_stability_display = function(stability) {
  if (is.null(stability)) return(data.frame())
  s = stability$summary
  s = s[order(s$rank_ref), , drop = FALSE]
  data.frame(
    `Código` = s$alternative_id, Sistema = s$alternative_name,
    `Posição no ranking` = s$rank_ref, `Posição mediana (reamostragem)` = s$median_rank,
    `Intervalo 90% da posição` = paste0(s$rank_p05, " a ", s$rank_p95),
    `Prob. de ser o 1º` = vapply(s$prob_top1, format_percent_br, character(1), digits = 0),
    `Prob. de estar entre os 3 primeiros` = vapply(s$prob_top3, format_percent_br, character(1), digits = 0),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# Texto de leitura (determinístico) para tela, Excel e Word.
mc_robustness_text = function(mc, stability = NULL, sens = NULL) {
  out = character()
  if (!is.null(stability)) {
    s = stability$summary[order(stability$summary$rank_ref), , drop = FALSE]
    out = c(out, paste0(
      "Em ", stability$replications, " reamostragens dos ", stability$respondents, " avaliadores, ",
      s$alternative_name[[1]], " ficou em 1º lugar em ", format_percent_br(s$prob_top1[[1]], 0),
      " das vezes (",
      if (identical(s$rank_p05[[1]], s$rank_p95[[1]])) paste0("em 90% das reamostragens, a posição foi sempre a ", s$rank_p05[[1]], "ª")
      else paste0("em 90% das reamostragens, a posição variou de ", s$rank_p05[[1]], " a ", s$rank_p95[[1]]), ")."
    ))
    top=s[s$rank_ref<=3,,drop=FALSE]
    out=c(out,paste0("A frequência de permanência entre os três primeiros foi ",
      paste(paste0(top$alternative_name,": ",vapply(top$prob_top3,format_percent_br,character(1),digits=1)),collapse="; "),"."))
    changes=any(s$rank_p05!=s$rank_ref | s$rank_p95!=s$rank_ref)
    out=c(out,if(changes)
      "A hipótese H1 não foi sustentada integralmente: houve variação nas posições dos sistemas entre as reamostragens. Frequência elevada nas primeiras posições sustenta uma prioridade recorrente, mas não a invariância da ordenação."
      else "A faixa central de 90% das reamostragens preservou as posições de referência. Esse resultado descreve estabilidade nessa faixa, sem demonstrar invariância em toda a distribuição.")

    wide = s[(s$rank_p95 - s$rank_p05) >= 3, , drop = FALSE]
    if (nrow(wide) > 0L) out = c(out, paste0(
      "Posição instável (intervalo de 90% com 3 posições ou mais): ", paste(wide$alternative_name, collapse = "; "),
      ". Leia a ordem entre esses sistemas como indefinida."
    ))
  }
  if (is.data.frame(sens)) {
    r1 = attr(sens, "spearman_pesos_iguais"); r2 = attr(sens, "spearman_minmax")
    if (is.finite(r1)) out = c(out, paste0(
      "Correlação de Spearman entre o ranking com pesos AHP e com pesos iguais: ", format_number_br(r1, 2),
      if (r1 < 0.8) ". Os pesos dos especialistas alteram a ordem de forma relevante." else ". Os pesos pouco alteram a ordem."
    ))
    if (is.finite(r2)) out = c(out, paste0(
      "Correlação de Spearman entre a normalização vetorial e a min–max: ", format_number_br(r2, 2), "."
    ))
  }
  out
}

# FIM DO MODULO: jltv_multicriteria_views.R


# ========================================================================
# INICIO DO MODULO: jltv_word_report.R
# ========================================================================
# ============================================================================
# Relatório em Word (.docx) com análise em texto, tabelas e gráficos.
#
# Reaproveita o objeto `report` montado por assemble_report() (o mesmo do Excel)
# e as mesmas funções chart_*(). O texto de interpretação pode vir de duas fontes:
#   - "local": redação determinística a partir dos números (sem internet, sem chave);
#   - "claude" / "chatgpt": a IA recebe SOMENTE os resultados agregados desta execução
#     (nunca respostas individuais nem e-mails) e devolve os parágrafos de interpretação.
# Tabelas e figuras são sempre geradas pelo app; a IA nunca digita número em tabela.
# Dependências extras: officer (Word) e curl + jsonlite (apenas para a IA).
# ============================================================================

word_report_packages_missing = function(need_ai = FALSE) {
  need = c("officer", "ggplot2")
  if (need_ai) need = c(need, "curl", "jsonlite")
  need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
}


# Contexto explícito: evita confundir disponibilidade média com o critério da missão.
word_report_mission_context = function(report) {
  imp = report$improvements
  cfg = imp$config
  asm = report$assumptions
  value = function(item) {
    if (!is.data.frame(asm) || !all(c("Item", "Valor") %in% names(asm))) return(NA_real_)
    x = as.character(asm$Valor[match(item, asm$Item)])
    if (length(x) != 1L || is.na(x)) return(NA_real_)
    suppressWarnings(as.numeric(gsub(",", ".", sub("%$", "", x), fixed = TRUE)))
  }
  list(frota = cfg$fleet_size %||% NA_real_,
       meta_viaturas_prontas = imp$target_ready %||% value("Meta M (viaturas prontas)"),
       horizonte_h = imp$horizon %||% value("Horizonte da missão (h)"),
       tolerancia_h = cfg$mission_tolerance_hours %||% NA_real_,
       probabilidade_alvo = report$simulation$p_target %||% value("Probabilidade-alvo"),
       replicacoes = imp$replications %||% value("Replicações por configuração"),
       semente = imp$seed %||% value("Semente"))
}

# jsonlite não preserva os nomes de um vetor numérico por padrão.
word_report_weight_facts = function(weights) {
  if (!is.numeric(weights) || !length(weights)) return(NULL)
  id = names(weights)
  if (is.null(id)) id = rep(NA_character_, length(weights))
  labels = assessment_criterion_labels()
  label = unname(labels[id])
  label[is.na(label)] = "Critério sem identificação no resultado"
  data.frame(criterio_id = id, criterio = label, peso = as.numeric(weights),
             stringsAsFactors = FALSE)
}


# Núcleo de decisão extraído da execução, reutilizado na seção 4 e na conclusão.
# Nenhum ganho vira promessa de que a medida já funciona na frota.
# ---- Achados da execução: fatos interpretados, com nomes e números já formatados ----------------
# É a base comum da seção 4, da conclusão, do resumo e do pedido à IA. Nada aqui é inventado:
# cada frase deriva de um número desta execução. A tese do trabalho sai da comparação entre o que os
# especialistas priorizam, onde a simulação concentra o tempo parado e quais intervenções têm ganho.
word_report_lever_noun = function(lever_id, label) {
  nouns = c(
    CONFIABILIDADE = "o aumento da confiabilidade (MTBF)",
    ESTOQUE = "a ampliação do estoque de sobressalentes",
    PRAZO_REPOSICAO = "a redução do prazo de reposição de peças",
    EQUIPE_REPARO = "o acréscimo de equipe de reparo com box",
    EQUIPE_DIAGNOSTICO = "o acréscimo de equipe de diagnóstico",
    TEMPO_REPARO = "a redução do tempo de reparo corretivo",
    TEMPO_DIAGNOSTICO = "a redução do tempo de diagnóstico"
  )
  out = unname(nouns[as.character(lever_id)])
  ifelse(is.na(out), lower_first(as.character(label)), out)
}

word_report_pretty_name = function(x) {
  x = gsub("\\s*/\\s*", "/", as.character(x))
  caps = !is.na(x) & x == toupper(x) & grepl("[ /]", x)
  low = tolower(x)
  x[caps] = paste0(toupper(substr(low[caps], 1L, 1L)), substring(low[caps], 2L))
  x
}

word_report_crit = function(x) tolower(gsub("\\s*/\\s*", " e ", as.character(x)))

word_report_plural = function(n, one, many) if (isTRUE(abs(n) == 1)) one else many

# Nome curto (sem o parêntese) quando a frase já traz a mudança testada entre parênteses.
word_report_lever_short = function(lever_id, label) sub("\\s*\\(MTBF\\)$", "", word_report_lever_noun(lever_id, label))

word_report_lever_group = function(lever_id) {
  id = as.character(lever_id)
  ifelse(id %in% c("ESTOQUE", "PRAZO_REPOSICAO"), "suprimento",
    ifelse(id %in% c("EQUIPE_REPARO", "EQUIPE_DIAGNOSTICO"), "capacidade",
      ifelse(id %in% c("CONFIABILIDADE"), "confiabilidade", "tempo")))
}

word_report_join = function(x, last = " e ") {
  x = as.character(x[!is.na(x) & nzchar(x)])
  if (length(x) <= 1L) return(paste(x, collapse = ""))
  paste0(paste(x[-length(x)], collapse = ", "), last, x[[length(x)]])
}

word_report_findings = function(report) {
  a = report$assessment
  k = a$kpis
  ctx = word_report_mission_context(report)
  f = list(ctx = ctx, mode = toupper(as.character(report$meta$evidence_mode %||% "HYBRID")))
  # Especialistas: ranking, pesos, robustez.
  ranking = if (is.data.frame(a$contributions) && nrow(a$contributions) > 0L) a$contributions else a$bottlenecks
  if (is.data.frame(ranking) && nrow(ranking) > 0L && !"rank" %in% names(ranking)) ranking$rank = seq_len(nrow(ranking))
  f$ranking = if (is.data.frame(ranking) && nrow(ranking) > 0L) ranking[order(ranking$rank), , drop = FALSE] else NULL
  if (!is.null(f$ranking)) {
    key = if ("alternative_id" %in% names(f$ranking) && is.data.frame(a$bottlenecks) && "alternative_id" %in% names(a$bottlenecks)) "alternative_id" else "alternative_name"
    if (is.data.frame(a$bottlenecks) && "p7_mentions" %in% names(a$bottlenecks)) {
      f$ranking$p7_mentions = a$bottlenecks$p7_mentions[match(f$ranking[[key]], a$bottlenecks[[key]])]
    }
    f$ranking$alternative_name = word_report_pretty_name(f$ranking$alternative_name)
    if ("dominant_criterion_label" %in% names(f$ranking)) f$ranking$dominant_criterion_label = word_report_crit(f$ranking$dominant_criterion_label)
    if ("dominant_subcriterion_label" %in% names(f$ranking)) f$ranking$dominant_subcriterion_label = word_report_crit(f$ranking$dominant_subcriterion_label)
  }
  w = word_report_weight_facts(a$criteria_weights)
  f$weights = if (is.data.frame(w)) w[order(-w$peso), , drop = FALSE] else NULL
  if (!is.null(f$weights)) f$weights$criterio = word_report_crit(f$weights$criterio)
  f$n_resp = word_report_n_respondents(report)
  mc = report$multicriteria
  f$stability = if (is.list(mc)) tryCatch(mc_rank_stability(mc), error = function(e) NULL) else NULL
  f$sensitivity = if (is.list(mc)) tryCatch(mc_method_sensitivity(mc), error = function(e) NULL) else NULL
  cons = if (is.list(mc) && is.data.frame(mc$ahp$consistency)) mc$ahp$consistency else NULL
  f$max_cr = if (is.data.frame(cons) && any(is.finite(cons$CR))) max(cons$CR, na.rm = TRUE) else NA_real_
  # Simulação: referência, decomposição e fator dominante.
  f$ao_ref = k$ao_reference
  f$p_ref = k$success_reference
  f$ao_target = k$availability_target
  p = a$process
  f$process = if (is.data.frame(p) && nrow(p) > 0L && sum(p$hours, na.rm = TRUE) > 0) p[order(-p$hours), , drop = FALSE] else NULL
  if (!is.null(f$process)) {
    share = function(names) sum(f$process$share[f$process$component %in% names], na.rm = TRUE)
    f$groups = c(
      ciclo = share(c("Reparo corretivo", "Diagnóstico", "Teste e liberação")),
      suprimento = share("Espera por peça"),
      capacidade = share("Espera por equipe/box"),
      preventiva = share("Manutenção preventiva")
    )
    f$binding = names(f$groups)[which.max(f$groups)]
  }
  # Intervenções.
  r = a$improvements
  f$tested = is.data.frame(r) && nrow(r) > 0L
  if (f$tested) {
    if (!"lever_id" %in% names(r)) r$lever_id = NA_character_
    catalog = improvement_lever_catalog()
    miss = is.na(r$lever_id) | !nzchar(as.character(r$lever_id))
    r$lever_id[miss] = catalog$lever_id[match(r$label[miss], catalog$label)]
    ok = r$demonstrated %in% TRUE & is.finite(r$delta_ci_lower) & r$delta_ci_lower > 0
    f$shown = r[ok, , drop = FALSE]
    f$shown = f$shown[order(-f$shown$delta_availability), , drop = FALSE]
    # Principais: ganho de pelo menos metade do maior ganho demonstrado; as demais são complementares.
    f$main = if (nrow(f$shown) > 0L) f$shown[f$shown$delta_availability >= 0.5 * max(f$shown$delta_availability), , drop = FALSE] else f$shown
    f$minor = if (nrow(f$shown) > 0L) f$shown[f$shown$delta_availability < 0.5 * max(f$shown$delta_availability), , drop = FALSE] else f$shown
    f$unshown = r[!ok, , drop = FALSE]
  }
  pk = a$package
  f$package = if (is.data.frame(pk) && nrow(pk) > 0L && word_report_num(pk$new_availability[[1]])) pk[1, , drop = FALSE] else NULL
  # Configurações: probabilidade de missão por tamanho de frota.
  s = report$simulation$summary
  if (is.data.frame(s) && nrow(s) > 0L && all(c("fleet_size", "success_probability") %in% names(s))) {
    by = split(s, s$fleet_size)
    f$by_fleet = data.frame(
      fleet = as.numeric(names(by)),
      p_min = vapply(by, function(d) min(d$success_probability, na.rm = TRUE), numeric(1)),
      p_max = vapply(by, function(d) max(d$success_probability, na.rm = TRUE), numeric(1)),
      ao_max = vapply(by, function(d) max(d$mean_availability, na.rm = TRUE), numeric(1)),
      stringsAsFactors = FALSE
    )
    f$by_fleet = f$by_fleet[order(f$by_fleet$fleet), , drop = FALSE]
  }
  pt = ctx$probabilidade_alvo
  f$mission_known = word_report_num(f$p_ref) && word_report_num(pt)
  f$mission_ok = f$mission_known && f$p_ref >= pt
  f$min_fleet = if (is.data.frame(f$by_fleet) && word_report_num(pt) && any(f$by_fleet$p_max >= pt)) min(f$by_fleet$fleet[f$by_fleet$p_max >= pt]) else NA_real_
  # Simulação por sistema (pizza), quando executada.
  ss = if (is.list(report$systems_simulation)) tryCatch(system_downtime_shares(report$systems_simulation$systems), error = function(e) NULL) else NULL
  f$systems = ss
  f
}

word_report_decision_core = function(report) {
  f = word_report_findings(report)
  a = report$assessment
  ctx = f$ctx
  pct = function(x, d = 1) if (word_report_num(x)) format_percent_br(x, d) else "n/d"
  num = function(x, d = 0) if (word_report_num(x)) format_number_br(x, d) else "n/d"
  pp = function(x) format_pp_br(x)
  ci = function(row) paste0("IC95% de ", pp(row$delta_ci_lower[[1]]), " a ", pp(row$delta_ci_upper[[1]]))
  change_txt = function(x) sub("\\s*\\(([^)]*)\\)\\s*$", ", \\1", lower_first(as.character(x)))
  rk = f$ranking
  has_rank = is.data.frame(rk) && nrow(rk) > 0L
  if (!is.null(f$stability)) f$stability$summary$alternative_name = word_report_pretty_name(f$stability$summary$alternative_name)
  top_names = if (has_rank) as.character(utils::head(rk$alternative_name, 3L)) else character()
  top1 = if (has_rank) top_names[[1]] else NA_character_
  N = ctx$frota; M = ctx$meta_viaturas_prontas; H = ctx$horizonte_h; pt = ctx$probabilidade_alvo
  has_mission = word_report_num(N) && word_report_num(M)
  shown = f$shown
  n_shown = if (is.data.frame(shown)) nrow(shown) else 0L
  out = list()

  # ---- 4.2 Prioridades e 4.3 pesos ---------------------------------------------------------------
  out$prioridades = if (has_rank) {
    lines = vapply(seq_along(top_names), function(i) paste0(
      top_names[[i]], " (coeficiente ", num(rk$coefficient[[i]], 3), "; critério dominante: ", lower_first(rk$dominant_criterion_label[[i]]),
      if ("dominant_subcriterion_label" %in% names(rk) && !is.na(rk$dominant_subcriterion_label[[i]])) paste0(", subcritério ", lower_first(rk$dominant_subcriterion_label[[i]])) else "",
      ")"), character(1))
    c(
      paste0("Os especialistas colocaram em primeiro lugar ", lines[[1]], if (length(lines) > 1L) paste0("; em seguida, ", word_report_join(lines[-1])) else "", ". ",
             "O coeficiente TOPSIS mede a proximidade de cada sistema à solução ideal no julgamento dos avaliadores; é uma ordem de prioridade, e não taxa de falha nem tempo de reparo."),
      if ("p7_mentions" %in% names(rk) && all(is.finite(rk$p7_mentions[seq_along(top_names)]))) paste0(
        "Na pergunta aberta P7, esses sistemas receberam ", word_report_join(paste0(vapply(rk$p7_mentions[seq_along(top_names)], function(v) num(as.numeric(v)), character(1)), " menções")),
        ", respectivamente, o que indica onde procurar, nos registros de manutenção, as ocorrências que sustentam a prioridade.") else NULL
    )
  } else "O ranking TOPSIS não foi estimado nesta execução, por falta de respostas consistentes ou de cobertura das notas P6."
  w = f$weights
  out$pesos = if (is.data.frame(w) && nrow(w) > 0L) c(paste0(
    "Os avaliadores atribuíram o maior peso a ", lower_first(w$criterio[[1]]), " (", pct(w$peso[[1]]), "), seguido de ",
    word_report_join(paste0(lower_first(w$criterio[-1]), " (", vapply(w$peso[-1], pct, character(1)), ")")), ". ",
    if (nrow(w) >= 2L && w$peso[[2]] > 0) paste0("O critério mais pesado vale ", num(w$peso[[1]] / w$peso[[2]], 1), " vezes o segundo e define quais sistemas sobem no ranking.") else ""),
    if (word_report_num(f$max_cr)) paste0("A maior razão de consistência entre os grupos de comparação foi ", num(f$max_cr, 3),
      if (f$max_cr < 0.10) ", abaixo do limite de 0,10; os julgamentos agregados são consistentes." else ", acima do limite de 0,10; esse grupo exige revisão dos julgamentos.")
  ) else "Os pesos AHP não foram estimados nesta execução."

  # Robustez calculada pelas posições e frequências efetivamente obtidas.
  st = f$stability
  out$robustez = mc_robustness_text(report$multicriteria,f$stability,f$sensitivity)

  # ---- 4.1 Síntese e 4.5 processo -----------------------------------------------------------------
  P = f$process
  binding_txt = NULL
  if (!is.null(P)) {
    g = f$groups
    parts = paste0(lower_first(P$component), " (", vapply(P$share, pct, character(1)), ")")
    binding_txt = switch(f$binding,
      ciclo = paste0("O tempo parado concentrou-se no próprio ciclo de manutenção: reparo corretivo, diagnóstico e teste somaram ", pct(g[["ciclo"]]),
                     " das horas paradas, contra ", pct(g[["suprimento"]]), " de espera por peça e ", pct(g[["capacidade"]]), " de espera por equipe ou box. ",
                     "Nas condições simuladas, a disponibilidade depende de quantas vezes a viatura falha e de quanto tempo leva para voltar, e muito menos da fila ou da espera por peça."),
      suprimento = paste0("O tempo parado concentrou-se na espera por peça (", pct(g[["suprimento"]]), " das horas paradas), acima do ciclo de reparo (", pct(g[["ciclo"]]),
                          "). Nas condições simuladas, o suprimento é o fator que mais retém a viatura fora de serviço."),
      capacidade = paste0("O tempo parado concentrou-se na espera por equipe ou box (", pct(g[["capacidade"]]), " das horas paradas). Nas condições simuladas, a capacidade de atendimento é o fator dominante."),
      preventiva = paste0("A manutenção preventiva respondeu pela maior parcela do tempo parado (", pct(g[["preventiva"]]), ")."))
    out$processo = c(
      paste0("Na situação de referência, as horas paradas se distribuíram entre ", word_report_join(parts), "."),
      binding_txt)
  } else out$processo = "A decomposição do tempo parado não foi estimada nesta execução, porque o teste de intervenções não foi executado."

  margin_txt = paste0(num(N - M), word_report_plural(N - M, " viatura", " viaturas"))
  mission_txt = if (has_mission && word_report_num(f$p_ref)) {
    head_txt = paste0("A frota simulada de ", num(N), " viaturas alcançou disponibilidade operacional média de ", pct(f$ao_ref),
      if (word_report_num(f$ao_target)) paste0(", ", if (f$ao_ref >= f$ao_target) "acima" else "abaixo", " da fração ", num(M), "/", num(N), " (", pct(f$ao_target), ") exigida pela meta de prontidão") else "", ". ")
    if (isTRUE(f$mission_ok)) paste0(head_txt,
      "A probabilidade de manter ao menos ", num(M), " viaturas prontas durante as ", num(H), " horas do horizonte foi de ", pct(f$p_ref),
      ", acima da probabilidade-alvo de ", pct(pt), ": a exigência é cumprida, com margem de ", margin_txt, " entre a frota e a meta.",
      if (word_report_num(f$min_fleet) && f$min_fleet > M) paste0(" A grade de configurações (subseção 4.7) mostra que essa folga só se mantém com frota de pelo menos ", num(f$min_fleet), " viaturas.") else "")
    else paste0(head_txt,
      "Ainda assim, a probabilidade de manter ao menos ", num(M), " viaturas prontas durante as ", num(H), " horas do horizonte foi de ", pct(f$p_ref),
      if (word_report_num(pt)) paste0(", abaixo da probabilidade-alvo de ", pct(pt)) else "", ". ",
      "A média elevada não garante a missão: com margem de ", margin_txt, " entre a frota e a meta, a parada simultânea de poucas viaturas basta para descumprir a exigência.")
  } else NULL
  out$sintese = c(
    if (has_rank) paste0("Os especialistas priorizaram ", word_report_join(top_names), ", nessa ordem",
      if (is.data.frame(f$weights)) paste0(", e atribuíram o maior peso ao critério ", lower_first(f$weights$criterio[[1]]), " (", pct(f$weights$peso[[1]]), ")") else "", "."),
    mission_txt,
    if (!is.null(P)) paste0("O tempo parado concentrou-se em ", lower_first(P$component[[1]]), " (", pct(P$share[[1]]), "), e a espera por peça respondeu por ", pct(f$groups[["suprimento"]]), " (subseção 4.5)."),
    if (f$tested) paste0("Das ", nrow(a$improvements), " intervenções testadas, ", n_shown, if (n_shown == 1L) " teve" else " tiveram",
      " ganho de disponibilidade demonstrado",
      if (n_shown > 0L) paste0(": ", word_report_join(paste0(word_report_lever_noun(shown$lever_id, shown$label), " (", vapply(shown$delta_availability, pp, character(1)), ")"))) else "", ".")
  )

  # ---- 4.6 Intervenções e H2 ------------------------------------------------------------------------
  if (f$tested) {
    shown_txt = if (n_shown > 0L) vapply(seq_len(n_shown), function(i) {
      row = shown[i, , drop = FALSE]
      paste0(upper_first(word_report_lever_short(row$lever_id, row$label)), " (", change_txt(row$change[[1]]), ") elevou a disponibilidade de ",
             pct(row$base_availability[[1]]), " para ", pct(row$new_availability[[1]]), ", ganho de ", pp(row$delta_availability[[1]]), " (", ci(row), ")",
             if ("new_success" %in% names(row) && word_report_num(row$new_success[[1]]) && word_report_num(f$p_ref) && abs(row$new_success[[1]] - f$p_ref) >= 0.005)
               paste0(", e a probabilidade de cumprir a missão de ", pct(f$p_ref), " para ", pct(row$new_success[[1]])) else "", ".")
    }, character(1)) else "Nenhuma intervenção teve o limite inferior do IC95% do ganho acima de zero."
    u = f$unshown
    unshown_txt = NULL
    if (is.data.frame(u) && nrow(u) > 0L) {
      grp = word_report_lever_group(u$lever_id)
      why = c(
        suprimento = if (!is.null(P)) paste0("porque a espera por peça representou ", pct(f$groups[["suprimento"]]), " do tempo parado na referência") else "",
        capacidade = if (!is.null(P)) paste0("porque a espera por equipe ou box representou ", pct(f$groups[["capacidade"]]), " do tempo parado") else "",
        tempo = "porque o ganho estimado ficou dentro da variabilidade entre replicações",
        confiabilidade = "porque o ganho estimado ficou dentro da variabilidade entre replicações")
      unshown_txt = vapply(unique(grp), function(gg) {
        rows = u[grp == gg, , drop = FALSE]
        paste0(upper_first(word_report_join(word_report_lever_noun(rows$lever_id, rows$label))), " não ", if (nrow(rows) > 1L) "demonstraram" else "demonstrou",
               " ganho (", word_report_join(paste0(vapply(rows$delta_availability, pp, character(1)), ", ", vapply(seq_len(nrow(rows)), function(j) ci(rows[j, , drop = FALSE]), character(1)))), ")",
               if (nzchar(why[[gg]])) paste0(", ", why[[gg]]) else "", ".")
      }, character(1), USE.NAMES = FALSE)
    }
    pk = f$package
    package_txt = if (!is.null(pk)) paste0(
      "Aplicadas em conjunto, as ", min(3L, n_shown), " intervenções de maior ganho levaram a disponibilidade de ", pct(pk$base_availability[[1]]), " para ",
      pct(pk$new_availability[[1]]), " (", pp(pk$delta_availability[[1]]), ")",
      if (n_shown >= 2L) paste0(", abaixo da soma dos ganhos isolados (", pp(sum(utils::head(shown$delta_availability, 3L))), "), porque as medidas atuam sobre o mesmo tempo parado") else "",
      if (word_report_num(pk$new_success[[1]])) paste0(". A probabilidade de cumprir a missão passou de ", pct(if ("base_success" %in% names(pk) && word_report_num(pk$base_success[[1]])) pk$base_success[[1]] else f$p_ref), " para ", pct(pk$new_success[[1]])) else "",
      if (word_report_num(pt) && word_report_num(pk$new_success[[1]]) && pk$new_success[[1]] < pt) paste0(", ainda abaixo da probabilidade-alvo de ", pct(pt)) else "", ".") else NULL
    h2 = paste0("A hipótese H2 é corroborada para ", n_shown, " das ", nrow(a$improvements), " intervenções",
                if (n_shown < nrow(a$improvements)) " e não é corroborada para as demais" else "", ".")
    out$melhorias = c(shown_txt, unshown_txt, package_txt, h2)
  } else out$melhorias = "O teste de intervenções não foi executado nesta execução; o efeito das ações de manutenção e de suprimento não foi estimado."

  # ---- 4.7 Configurações (margem da frota) -------------------------------------------------------
  bf = f$by_fleet
  out$simulacao = c(
    if (has_mission) paste0("O critério da missão foi manter ao menos ", num(M), " de ", num(N), " viaturas prontas ao longo de ", num(H), " horas",
      if (word_report_num(ctx$tolerancia_h) && ctx$tolerancia_h > 0) paste0(", com tolerância acumulada de ", num(ctx$tolerancia_h), " horas abaixo da meta") else "",
      if (word_report_num(pt)) paste0(", com probabilidade-alvo de ", pct(pt)) else "", ".") else NULL,
    if (is.data.frame(bf) && nrow(bf) > 0L) paste0(
      "Na grade simulada, a probabilidade de cumprir a missão foi de ",
      word_report_join(vapply(seq_len(nrow(bf)), function(i) paste0(
        if (abs(bf$p_max[[i]] - bf$p_min[[i]]) < 0.005) pct(bf$p_max[[i]], 0) else paste0(pct(bf$p_min[[i]], 0), " a ", pct(bf$p_max[[i]], 0)),
        " com ", num(bf$fleet[[i]]), " viaturas"), character(1))),
      ", enquanto a disponibilidade média pouco variou. ",
      if (word_report_num(pt) && any(bf$p_max >= pt)) paste0("A probabilidade-alvo só foi alcançada a partir de ", num(min(bf$fleet[bf$p_max >= pt])), " viaturas.")
      else if (word_report_num(pt)) "Nenhuma configuração da grade alcançou a probabilidade-alvo." else "",
      " O resultado mostra que, para a missão, a margem entre a frota e a meta pesa mais que equipes, boxes ou estoque.") else NULL
  )

  # ---- Conclusão --------------------------------------------------------------------------------
  main = if (is.data.frame(f$main)) f$main else NULL
  minor = if (is.data.frame(f$minor)) f$minor else NULL
  n_main = if (is.null(main)) 0L else nrow(main)
  n_minor = if (is.null(minor)) 0L else nrow(minor)
  support_main = n_main > 0L && any(word_report_lever_group(main$lever_id) %in% c("capacidade", "suprimento"))
  answer = paste0(
    "A pesquisa perguntou quais sistemas da viatura JLTV e quais intervenções de manutenção e de suprimento devem ser priorizados para sustentar a disponibilidade da frota do CFN. ",
    if (has_rank) paste0("Os sistemas a priorizar são ", word_report_join(top_names), ", nessa ordem") else "A prioridade dos sistemas não foi estabelecida nesta execução",
    if (n_main > 0L) paste0("; as intervenções a priorizar são ", word_report_join(word_report_lever_noun(main$lever_id, main$label)),
      if (n_minor > 0L) paste0(", complementadas ", word_report_join(sub("^o ", "pelo ", sub("^a ", "pela ", word_report_lever_noun(minor$lever_id, minor$label)))), ", de efeito menor") else "",
      ", entre as ", nrow(a$improvements), " testadas")
    else if (f$tested) "; nenhuma intervenção testada teve ganho de disponibilidade demonstrado" else "; as intervenções não foram testadas", ".")
  expert = if (has_rank) paste0(
    "O julgamento dos especialistas",
    if (word_report_num(f$n_resp)) paste0(", obtido de ", f$n_resp, " avaliadores,") else "",
    " deu o maior peso a ", if (is.data.frame(f$weights)) paste0(f$weights$criterio[[1]], " (", pct(f$weights$peso[[1]]), ")") else "um critério",
    " e colocou ", top1, " em primeiro lugar (coeficiente ", num(rk$coefficient[[1]], 3), ")",
    if (!is.null(st)) paste0(", posição mantida em ", pct(st$summary$prob_top1[order(st$summary$rank_ref)][[1]], 0), " das reamostragens dos avaliadores") else "", ".") else NULL
  lever_phrase = function(row) paste0(word_report_lever_short(row$lever_id, row$label), " (", change_txt(row$change[[1]]), ") rendeu ",
                                      pp(row$delta_availability[[1]]), " (", ci(row), ")")
  sim = if (word_report_num(f$ao_ref)) paste0(
    "A simulação estimou disponibilidade de referência de ", pct(f$ao_ref),
    if (!is.null(P)) { rs = P$share[P$component == "Reparo corretivo"]
      paste0(", com ", pct(f$groups[["ciclo"]]), " do tempo parado no ciclo de manutenção", if (length(rs) == 1L && is.finite(rs)) paste0(", dos quais ", pct(rs), " de reparo corretivo") else "") } else "", ". ",
    if (n_main > 0L) paste0(upper_first(paste(vapply(seq_len(n_main), function(i) lever_phrase(main[i, , drop = FALSE]), character(1)), collapse = "; ")), ". ") else "",
    if (n_minor > 0L) paste0(upper_first(word_report_join(word_report_lever_short(minor$lever_id, minor$label))), " tiveram ganho pequeno (",
      word_report_join(vapply(minor$delta_availability, pp, character(1))), ")", ". ") else "",
    if (is.data.frame(f$unshown) && nrow(f$unshown) > 0L) paste0(
      upper_first(word_report_join(word_report_lever_short(f$unshown$lever_id, f$unshown$label))), " não demonstraram ganho",
      if (!is.null(P)) paste0("; com espera por equipe ou box de ", pct(f$groups[["capacidade"]]), " e espera por peça de ", pct(f$groups[["suprimento"]]),
                              " do tempo parado, a capacidade de atendimento e o suprimento limitaram pouco a frota nas condições simuladas") else "", ".") else "") else NULL
  converge = if (has_rank && !is.null(P) && f$binding == "ciclo" && n_main > 0L && !support_main) "Especialistas e simulação apontam na mesma direção: a prioridade recai sobre os sistemas cuja falha mais afeta o emprego, e o ganho vem principalmente de falhar menos e de reparar mais depressa, e não de ampliar a estrutura de apoio." else ""
  fleet_drop = if (is.data.frame(bf) && nrow(bf) >= 2L) word_report_join(vapply(rev(seq_len(nrow(bf) - 1L)), function(i) paste0(
    if (abs(bf$p_max[[i]] - bf$p_min[[i]]) < 0.005) pct(bf$p_max[[i]], 0) else paste0(pct(bf$p_min[[i]], 0), " a ", pct(bf$p_max[[i]], 0)), " com ", num(bf$fleet[[i]])), character(1))) else ""
  thesis = if (!is.null(mission_txt) && isTRUE(f$mission_ok)) paste0(
    "O achado central é que, nas condições simuladas, a frota cumpre a exigência de prontidão, mas a folga depende da margem de viaturas: a probabilidade de manter ", num(M), " de ", num(N),
    " viaturas prontas durante todo o horizonte foi de ", pct(f$p_ref), " para um alvo de ", pct(pt),
    if (nzchar(fleet_drop)) paste0(", e caiu para ", fleet_drop, " viaturas na frota") else "", ". ",
    if (word_report_num(f$min_fleet)) paste0("Com frota menor que ", num(f$min_fleet), " viaturas, a missão deixa de ser sustentada com a confiança exigida. ") else "",
    if (!is.null(f$package) && word_report_num(f$package$new_success[[1]])) paste0("As intervenções com ganho ampliam essa folga: com o pacote, a probabilidade chegou a ", pct(f$package$new_success[[1]]), " e a disponibilidade a ", pct(f$package$new_availability[[1]]), ". ") else "",
    converge)
  else if (!is.null(mission_txt)) paste0(
    "O achado central é que a disponibilidade média de ", pct(f$ao_ref), " não assegura a prontidão exigida: a probabilidade de manter ", num(M), " de ", num(N),
    " viaturas prontas durante todo o horizonte foi de ", pct(f$p_ref),
    if (!is.null(f$package) && word_report_num(f$package$new_success[[1]])) paste0(" e chegou a ", pct(f$package$new_success[[1]]), " com o pacote de intervenções (disponibilidade de ", pct(f$package$new_availability[[1]]), ")") else "",
    if (word_report_num(pt)) paste0(", diante de uma probabilidade-alvo de ", pct(pt)) else "", ". ",
    if (is.data.frame(bf) && nrow(bf) >= 2L) paste0("A grade de configurações mostrou que essa probabilidade depende sobretudo da margem entre a frota e a meta: com ",
      num(bf$fleet[[1]]), " viaturas, ficou em ", pct(bf$p_max[[1]], 0), ". ") else "",
    converge) else NULL
  catalog = improvement_lever_catalog()
  ordinal = c("Primeira", "Segunda", "Terceira", "Quarta")
  proposals = if (n_main > 0L) vapply(seq_len(min(3L, n_main)), function(i) {
    row = main[i, , drop = FALSE]
    acts = catalog$example_actions[match(row$lever_id[[1]], catalog$lever_id)]
    targeted = row$lever_id[[1]] %in% c("CONFIABILIDADE", "TEMPO_REPARO", "TEMPO_DIAGNOSTICO") && length(top_names) > 0L
    paste0(ordinal[[i]], ": ", word_report_lever_short(row$lever_id, row$label), ", com a meta testada (", change_txt(row$change[[1]]), "), aplicada ",
           if (targeted) paste0("primeiro aos sistemas priorizados (", word_report_join(utils::head(top_names, 2L)), ")") else "à frota",
           ". ", if (!is.na(acts) && nzchar(acts)) paste0("Ações correspondentes: ", lower_first(acts)) else "")
  }, character(1)) else NULL
  proposal = c(
    if (n_main > 0L) paste0("Propõe-se, portanto, a seguinte ordem de ação para a sustentação da frota. ", paste(proposals, collapse = " "),
      if (n_minor > 0L) paste0(" Em seguida, e de forma complementar, ", word_report_join(word_report_lever_noun(minor$lever_id, minor$label)),
                               ", cujo ganho foi pequeno", if (word_report_num(f$p_ref) && all(abs(minor$new_success - f$p_ref) < 0.005, na.rm = TRUE)) " e não alterou a probabilidade de cumprir a missão" else "", ".") else "",
      " Os ganhos estimados valem como metas de desempenho: a efetividade das ações deve ser acompanhada pelos registros de manutenção.")
    else if (f$tested) "Como nenhuma intervenção teve ganho demonstrado, a execução não sustenta propor uma ação específica; a prioridade dos sistemas orienta a coleta de registros.",
    if (f$tested && is.data.frame(f$unshown) && nrow(f$unshown) > 0L) paste0(
      "Nas condições simuladas, não se justifica priorizar ", word_report_join(word_report_lever_noun(f$unshown$lever_id, f$unshown$label), " ou "),
      ", que não demonstraram ganho; esses recursos devem ser reavaliados se os registros mostrarem esperas maiores que as simuladas."),
    if (!is.null(thesis) && word_report_num(pt) && !is.null(f$package) && word_report_num(f$package$new_success[[1]]) && f$package$new_success[[1]] < pt) paste0(
      "Como o pacote não alcançou a probabilidade-alvo de ", pct(pt), ", propõe-se também submeter ao planejamento a relação entre a frota disponível e a meta de ", num(M),
      " viaturas prontas, porque a margem de ", num(N - M), word_report_plural(N - M, " viatura", " viaturas"), " é o fator que mais limita a missão."),
    if (isTRUE(f$mission_ok) && word_report_num(f$min_fleet) && f$min_fleet < N) paste0(
      "Propõe-se também adotar a frota de ", num(f$min_fleet), " viaturas como limite de alerta para o planejamento: abaixo dele, a probabilidade simulada de manter ",
      num(M), " viaturas prontas fica aquém do alvo de ", pct(pt), ", e as viaturas paradas dos sistemas priorizados devem ter precedência no reparo.")
    else if (isTRUE(f$mission_ok) && word_report_num(f$min_fleet) && f$min_fleet >= N && is.data.frame(f$by_fleet) && any(f$by_fleet$fleet == N - 1)) paste0(
      "Propõe-se também tratar as ", num(N), " viaturas como frota mínima para esta exigência: com uma viatura a menos, por baixa prolongada ou remanejamento, a probabilidade simulada de manter ",
      num(M), " viaturas prontas cai para ", pct(f$by_fleet$p_max[f$by_fleet$fleet == N - 1][[1]], 0), ", abaixo do alvo de ", pct(pt),
      "; por isso, as viaturas paradas dos sistemas priorizados devem ter precedência no reparo e a reposição de uma viatura baixada deve entrar no planejamento."))
  objective = if (has_rank && f$tested && word_report_num(f$ao_ref)) paste0(
    "O objetivo geral foi alcançado: a execução identificou os sistemas e as intervenções a priorizar e quantificou o efeito de cada uma. ",
    "Como contribuição, o trabalho entrega um procedimento rastreável que une julgamento de especialistas e simulação e pode ser repetido a cada novo conjunto de registros da frota.")
    else "O objetivo geral foi atendido de forma parcial, porque faltaram etapas da execução para relacionar a prioridade dos sistemas ao efeito das intervenções."
  limits = paste0(
    if (identical(f$mode, "SYNTHETIC")) "Os números desta execução vêm de dados de demonstração e ilustram o procedimento. " else "",
    "Os parâmetros de vida, de reparo e de suprimento do simulador valem como hipóteses declaradas até serem estimados nas ordens de serviço",
    if (!word_report_num(report$assessment$kpis$ao_observed)) ", e a disponibilidade observada nos registros não estava disponível para validar o modelo" else "",
    ". A continuidade do estudo deve estimar o MTBF e os tempos de reparo por sistema nos registros apontados na pergunta P7 e repetir a simulação com esses valores.")
  out$conclusao = c(answer, expert, sim, thesis, proposal, objective, limits)
  out$conclusao = out$conclusao[!vapply(out$conclusao, function(x) is.null(x) || !nzchar(x), logical(1))]
  out$proximos_passos = character()
  out
}

if (!exists("upper_first", mode = "function")) upper_first = function(x) {
  x = as.character(x)
  ifelse(nzchar(x), paste0(toupper(substr(x, 1L, 1L)), substr(x, 2L, nchar(x))), x)
}


# ---- 1. Fatos estruturados (é isto que a IA enxerga) ------------------------
word_report_facts = function(report) {
  a = report$assessment
  k = a$kpis
  df_or_null = function(x, cols) {
    if (!is.data.frame(x) || nrow(x) == 0L) return(NULL)
    x[, intersect(cols, names(x)), drop = FALSE]
  }
  simulation = report$simulation
  settings = report$assumptions
  settings = if (is.data.frame(settings)) settings[grepl("^Parâmetro técnico", settings$Item), , drop = FALSE] else NULL
  list(
    modo_de_evidencia = report$meta$evidence_mode,
    contexto_da_missao = word_report_mission_context(report),
    definicoes = list(
      disponibilidade = "Ao simulada = horas-viatura disponíveis / (frota * horizonte); inclui os estados de parada representados no motor.",
      horas_do_processo = "Horas-viatura somadas na frota e médias entre replicações; não são duração média de uma OS nem soma de todas as replicações.",
      diferenca_disponibilidade = "Os deltas são frações de disponibilidade; multiplicar por 100 para expressar pontos percentuais.",
      probabilidade_missao = "Fração das replicações que respeitam M viaturas prontas e a tolerância ao longo de H; a probabilidade-alvo difere de M/N.",
      vinculo_sistema_intervencao = "As intervenções foram simuladas sobre a viatura completa; a associação a um sistema é proposta do autor, e não resultado da simulação."),
    rascunho_do_aplicativo = word_report_ai_draft(report),
    palavras_alvo_por_chave = word_report_ai_word_targets(report),
    versao_do_app = report$meta$version,
    gerado_em = report$meta$generated_at,
    indicadores = list(
      disponibilidade_referencia = k$ao_reference,
      disponibilidade_com_melhorias = k$ao_improved,
      ganho_disponibilidade_pontos_percentuais = if (is.finite(k$ao_gain)) 100 * k$ao_gain else NA,
      disponibilidade_observada_nos_registros = k$ao_observed,
      meta_de_disponibilidade = k$availability_target,
      prob_cumprir_missao_referencia = k$success_reference,
      prob_cumprir_missao_com_melhorias = k$success_improved,
      melhoria_aplicada = k$improved_label,
      sistema_prioridade_1 = k$top_bottleneck,
      criterio_dominante_da_prioridade_1 = k$top_bottleneck_driver,
      maior_parcela_do_tempo_parado = k$top_process,
      parcela_do_maior_componente = k$top_process_share
    ),
    gargalos_topsis = df_or_null(a$bottlenecks, c(
      "rank", "alternative_name", "coefficient", "dominant_criterion_label",
      "dominant_subcriterion_label", "p7_mentions"
    )),
    processo_simulado = df_or_null(a$process, c("component", "hours", "share")),
    tempo_parado_por_sistema = if(is.list(report$systems_simulation)) system_downtime_shares(report$systems_simulation$systems) else NULL,
    contexto_simulacao_por_sistema = if(is.list(report$systems_simulation)) report$systems_simulation[c("mode","horizon","config","seeds")] else NULL,
    melhorias = df_or_null(a$improvements, c(
      "priority_rank", "label", "change", "base_availability", "new_availability",
      "delta_availability", "delta_ci_lower", "delta_ci_upper", "base_success", "new_success",
      "demonstrated", "conclusion", "criterion_label", "example_actions", "lever_id", "magnitude"
    )),
    pacote_de_melhorias = df_or_null(a$package, c(
      "base_availability", "new_availability", "delta_availability", "base_success", "new_success"
    )),
    pesos_ahp_criterios = word_report_weight_facts(a$criteria_weights),
    confiabilidade = df_or_null(a$confidence, c("item", "value", "status")),
    configuracoes_simuladas = if (is.list(simulation) && is.data.frame(simulation$summary)) {
      df_or_null(simulation$summary, c(
        "configuration_id", "fleet_size", "diagnostic_teams", "repair_teams", "bays", "initial_stock", "success_probability", "ci_lower", "ci_upper", "mean_availability"
      ))
    } else NULL,
    robustez_ahp_topsis = word_report_robustness_facts(report$multicriteria),
    parametros_do_simulador = if (is.data.frame(settings) && nrow(settings) > 0L) settings else NULL
  )
}

word_report_robustness_facts = function(mc) {
  stab = tryCatch(mc_rank_stability(mc), error = function(e) NULL)
  sens = tryCatch(mc_method_sensitivity(mc), error = function(e) NULL)
  list(
    reamostragens = if (!is.null(stab)) stab$replications else NULL,
    avaliadores = if (!is.null(stab)) stab$respondents else NULL,
    estabilidade_das_posicoes = if (!is.null(stab)) stab$summary else NULL,
    sensibilidade_pesos_e_normalizacao = if (is.data.frame(sens)) as.data.frame(sens) else NULL,
    spearman_pesos_iguais = if (is.data.frame(sens)) attr(sens, "spearman_pesos_iguais") else NULL,
    spearman_normalizacao_minmax = if (is.data.frame(sens)) attr(sens, "spearman_minmax") else NULL
  )
}


# ---- 2. Texto local (determinístico) ---------------------------------------
# Redação em terceira pessoa, voz ativa, linguagem técnica e neutra (Manual ESG).
word_report_num = function(x) length(x) == 1L && is.numeric(x) && is.finite(x)

word_report_n_respondents = function(report) {
  stab = tryCatch(mc_rank_stability(report$multicriteria), error = function(e) NULL)
  n = suppressWarnings(as.integer(stab$respondents)[1])
  if (length(n) == 1L && !is.na(n) && n > 0L) n else NA_integer_
}

word_report_mode_label = function(mode, lang = "pt") {
  if (identical(lang, "en")) {
    return(switch(mode,
      SYNTHETIC = "synthetic (demonstration data)",
      HYBRID = "hybrid (real records combined with scenario assumptions)",
      EMPIRICAL = "empirical (real fleet records)",
      tolower(mode)))
  }
  switch(mode,
    SYNTHETIC = "sintético (dados de demonstração)",
    HYBRID = "híbrido (registros reais combinados a hipóteses de cenário)",
    EMPIRICAL = "empírico (registros reais da frota)",
    tolower(mode))
}

# Resumo (150 a 250 palavras, parágrafo único, verbos no passado para o que foi feito) e Abstract equivalente.
# Palavras-chave: de 3 a 5, conforme o Manual da ESG.
word_report_resumo_local = function(report) {
  f = word_report_findings(report)
  mode = f$mode
  n = f$n_resp
  pct = function(x, d = 1) if (word_report_num(x)) format_percent_br(x, d) else NA_character_
  en_num = function(x) ifelse(is.na(x), x, gsub(",", ".", x, fixed = TRUE))
  en_pct = function(x, d = 1) en_num(pct(x, d))
  rk = f$ranking
  tops = if (is.data.frame(rk) && nrow(rk) > 0L) as.character(utils::head(rk$alternative_name, 2L)) else character()
  shown = if (is.data.frame(f$main)) f$main else f$shown
  ns = if (is.data.frame(shown)) nrow(shown) else 0L
  ctx = f$ctx
  N = ctx$frota; M = ctx$meta_viaturas_prontas; pt = ctx$probabilidade_alvo
  pk = f$package
  lever_en = c(CONFIABILIDADE = "higher reliability (MTBF)", ESTOQUE = "larger spare-parts stock", PRAZO_REPOSICAO = "shorter replenishment lead time",
               EQUIPE_REPARO = "an additional repair team", EQUIPE_DIAGNOSTICO = "an additional diagnostic team",
               TEMPO_REPARO = "shorter corrective repair time", TEMPO_DIAGNOSTICO = "shorter diagnosis time")
  pt_txt = c(
    "O estudo tratou da sustentação da frota de viaturas Joint Light Tactical Vehicle (JLTV) do Corpo de Fuzileiros Navais e perguntou quais sistemas da viatura e quais intervenções de manutenção e de suprimento devem ser priorizados para sustentar a sua disponibilidade.",
    paste0("A pesquisa, quantitativa e aplicada, combinou o julgamento de ", if (!is.na(n)) paste0(n, " especialistas") else "especialistas",
           ", tratado pelo Processo Analítico Hierárquico (AHP) e pela técnica TOPSIS, com uma simulação de eventos discretos da frota, repetida em replicações de Monte Carlo, que testou intervenções em comparação pareada."),
    if (length(tops)) paste0("Os especialistas priorizaram ", word_report_join(tops), if (is.data.frame(f$weights)) paste0(" e deram o maior peso ao critério ", lower_first(f$weights$criterio[[1]])) else "", "."),
    if (word_report_num(f$ao_ref)) paste0("A disponibilidade simulada de referência foi de ", pct(f$ao_ref),
      if (word_report_num(f$p_ref) && word_report_num(M)) paste0(if (isTRUE(f$mission_ok)) ", e a probabilidade de manter " else ", mas a probabilidade de manter ", M, " de ", N, " viaturas prontas durante a missão foi de ", pct(f$p_ref),
        if (word_report_num(pt)) paste0(", ", if (isTRUE(f$mission_ok)) "acima" else "abaixo", " do alvo de ", pct(pt)) else "",
        if (isTRUE(f$mission_ok) && word_report_num(f$min_fleet) && f$min_fleet < N) paste0("; com frota menor que ", f$min_fleet, " viaturas, o alvo deixa de ser atingido") else "") else "", "."),
    if (ns > 0L) paste0(upper_first(word_report_join(word_report_lever_noun(shown$lever_id, shown$label))), " tiveram os maiores ganhos demonstrados (",
      word_report_join(vapply(shown$delta_availability, format_pp_br, character(1))), ")",
      if (!is.null(pk) && word_report_num(pk$new_success[[1]])) paste0("; juntas, elevaram a probabilidade de cumprir a missão a ", pct(pk$new_success[[1]])) else "", ".")
    else if (f$tested) "Nenhuma intervenção testada teve ganho demonstrado.",
    paste0(if (isTRUE(f$mission_ok)) "Concluiu-se que a frota cumpre a exigência de prontidão com folga que depende da margem de viaturas e que a sustentação deve priorizar " else "Concluiu-se que a disponibilidade média não assegura a prontidão exigida e que a sustentação deve priorizar ",
      if (ns > 0L) word_report_join(word_report_lever_noun(shown$lever_id, shown$label)) else "a coleta de registros dos sistemas priorizados",
      if (length(tops)) paste0(", dirigidas primeiro a ", word_report_join(tops)) else "",
      if (ns > 0L && !any(word_report_lever_group(shown$lever_id) %in% c("capacidade", "suprimento"))) ", antes de ampliar equipes ou estoques" else "",
      if (identical(mode, "SYNTHETIC")) "; nesta execução, os dados são de demonstração." else "; os parâmetros do simulador devem ser confirmados nos registros da frota.")
  )
  en_txt = c(
    "The study addressed the sustainment of the Brazilian Marine Corps Joint Light Tactical Vehicle (JLTV) fleet and asked which vehicle systems and which maintenance and supply interventions should be prioritized to sustain its availability.",
    paste0("The quantitative, applied research combined the judgment of ", if (!is.na(n)) paste0(n, " specialists") else "specialists",
           ", processed with the Analytic Hierarchy Process (AHP) and TOPSIS, with a discrete-event simulation of the fleet, repeated in Monte Carlo replications, that tested interventions through paired comparison."),
    if (length(tops)) paste0("The specialists prioritized ", word_report_join(tops, " and "), if (is.data.frame(f$weights)) paste0(" and gave the largest weight to the ", c(OCCURRENCE = "failure occurrence", DOWNTIME = "impact on availability", SUPPLY = "supply", SUPPORT = "maintenance and support")[[f$weights$criterio_id[[1]]]] %||% "first", " criterion.") else "."),
    if (word_report_num(f$ao_ref)) paste0("The simulated baseline availability was ", en_pct(f$ao_ref),
      if (word_report_num(f$p_ref) && word_report_num(M)) paste0(if (isTRUE(f$mission_ok)) ", and the probability of keeping " else ", but the probability of keeping ", M, " of ", N, " vehicles ready throughout the mission was ", en_pct(f$p_ref),
        if (word_report_num(pt)) paste0(", ", if (isTRUE(f$mission_ok)) "above" else "below", " the ", en_pct(pt), " target") else "") else "", "."),
    if (ns > 0L) paste0(upper_first(word_report_join(unname(ifelse(is.na(lever_en[shown$lever_id]), shown$label, lever_en[shown$lever_id])), " and ")), " showed the largest demonstrated gains (",
      word_report_join(en_num(vapply(shown$delta_availability, format_pp_br, character(1))), " and "), ")",
      if (!is.null(pk) && word_report_num(pk$new_success[[1]])) paste0("; together, they raised the probability of mission success to ", en_pct(pk$new_success[[1]])) else "", ".")
    else if (f$tested) "No tested intervention showed a demonstrated gain.",
    paste0(if (isTRUE(f$mission_ok)) "It was concluded that the fleet meets the readiness requirement with a slack that depends on the vehicle margin and that sustainment should prioritize " else "It was concluded that average availability does not ensure the required readiness and that sustainment should prioritize ",
      if (ns > 0L) word_report_join(unname(ifelse(is.na(lever_en[shown$lever_id]), shown$label, lever_en[shown$lever_id])), " and ") else "collecting records on the prioritized systems",
      if (length(tops)) paste0(", directed first at ", word_report_join(tops, " and ")) else "",
      if (ns > 0L && !any(word_report_lever_group(shown$lever_id) %in% c("capacidade", "suprimento"))) ", before adding teams or stock" else "",
      if (identical(mode, "SYNTHETIC")) "; in this run, the data are for demonstration." else "; the simulator parameters must be confirmed in the fleet records.")
  )
  en_txt = gsub("p\\.p\\.", "percentage points", en_txt)
  pt_out = paste(pt_txt[nzchar(pt_txt)], collapse = " ")
  en_out = paste(en_txt[nzchar(en_txt)], collapse = " ")
  # O Manual pede de 150 a 250 palavras: com poucos resultados, o resumo explicita as hipóteses testadas.
  if (word_report_word_count(pt_out) < 150L) pt_out = paste(pt_out, "As hipóteses testadas foram a estabilidade do ranking diante da reamostragem dos avaliadores, de pesos iguais e de outra normalização (H1) e o ganho de cada intervenção sobre a disponibilidade simulada em comparação pareada com a situação de referência (H2).", paste0("A execução operou em modo de evidência ", word_report_mode_label(mode, "pt"), "."))
  if (word_report_word_count(en_out) < 150L) en_out = paste(en_out, "The hypotheses tested were the stability of the ranking under resampling of the evaluators, equal weights and another normalization (H1), and the gain of each intervention on simulated availability in paired comparison with the baseline (H2).", paste0("The run operated in ", word_report_mode_label(mode, "en"), " evidence mode."))
  list(
    pt = pt_out,
    en = en_out,
    kw_pt = "gestão do ciclo de vida; viatura JLTV; disponibilidade; AHP–TOPSIS; simulação de eventos discretos.",
    kw_en = "life cycle management; JLTV vehicle; availability; AHP–TOPSIS; discrete-event simulation."
  )
}

word_report_narrative_local = function(report) {
  a = report$assessment
  k = a$kpis
  mode = toupper(report$meta$evidence_mode %||% "HYBRID")
  pct = function(x, d = 1) if (word_report_num(x)) format_percent_br(x, d) else "n/d"
  # "Prazo de reposição de 240 h para 168 h (−30%)" -> "prazo de reposição de 240 h para 168 h, −30%" (sem parênteses duplos)
  change_txt = function(x) sub("\\s*\\(([^)]*)\\)\\s*$", ", \\1", lower_first(as.character(x)))
  out = list()

  b = a$bottlenecks
  p = a$process
  r = a$improvements
  pk = a$package
  shown = if (is.data.frame(r) && nrow(r) > 0L) r[r$demonstrated %in% TRUE, , drop = FALSE] else NULL

  # Síntese em linguagem de relatório (o texto do painel do aplicativo é dirigido ao usuário e não entra aqui).
  out$sintese = c(
    if (word_report_num(k$ao_reference)) paste0(
      "A situação de referência simulada apresentou disponibilidade operacional de ", pct(k$ao_reference),
      if (word_report_num(k$success_reference)) paste0(" e probabilidade de cumprir a missão de ", pct(k$success_reference)) else "", "."
    ),
    if (is.data.frame(b) && nrow(b) > 0L) paste0(
      "O sistema de maior prioridade segundo os especialistas foi ", b$alternative_name[[1]],
      " (coeficiente TOPSIS ", format_number_br(b$coefficient[[1]], 3), "), com maior peso no critério ", b$dominant_criterion_label[[1]], "."
    ),
    if (is.data.frame(p) && nrow(p) > 0L && sum(p$hours) > 0) paste0(
      "Na simulação, a maior parcela do tempo parado correspondeu a ", lower_first(p$component[[1]]),
      " (", pct(p$share[[1]], 0), " das horas paradas)."
    ),
    if (is.data.frame(shown) && nrow(shown) > 0L) paste0(
      "A intervenção de maior ganho de disponibilidade demonstrado foi ", lower_first(shown$label[[1]]), " (", change_txt(shown$change[[1]]), "): ",
      pct(shown$base_availability[[1]]), " para ", pct(shown$new_availability[[1]]),
      " (", format_pp_br(shown$delta_availability[[1]]), "; IC95% ", format_pp_br(shown$delta_ci_lower[[1]]), " a ", format_pp_br(shown$delta_ci_upper[[1]]), ")."
    ) else if (is.data.frame(r) && nrow(r) > 0L) "Nenhuma das intervenções testadas teve ganho de disponibilidade demonstrado, pois o IC95% do ganho incluiu zero.",
    if (is.data.frame(pk) && nrow(pk) > 0L && word_report_num(pk$new_availability[[1]])) paste0(
      "Com as intervenções de maior ganho aplicadas em conjunto, a disponibilidade passou de ", pct(pk$base_availability[[1]]),
      " para ", pct(pk$new_availability[[1]]), " (", format_pp_br(pk$delta_availability[[1]]), ")."
    )
  )

  out$prioridades = if (is.data.frame(b) && nrow(b) > 0L) {
    c(
      paste0(
        "O ranking TOPSIS ordena os sistemas pela prioridade que os avaliadores lhes atribuem na sustentação da frota. ",
        "O primeiro colocado é ", b$alternative_name[[1]], " (coeficiente ", format_number_br(b$coefficient[[1]], 3),
        "), com maior peso no critério ", b$dominant_criterion_label[[1]], "."
      ),
      "O coeficiente é um ordenamento de prioridade segundo julgamento de especialistas. Ele não é taxa de falha, MTBF ou MTTR e não substitui os registros de manutenção."
    )
  } else "O ranking TOPSIS ainda não está disponível nesta execução (faltam respostas, consistência AHP ou notas P6)."

  out$pesos = "Os pesos do AHP indicam a importância relativa de cada critério e subcritério segundo os especialistas."

  out$processo = if (is.data.frame(p) && nrow(p) > 0L && sum(p$hours) > 0) {
    paste0(
      "Na situação de referência simulada, o componente de maior duração do tempo parado é ", lower_first(p$component[[1]]),
      " (", pct(p$share[[1]], 0), " das horas paradas), seguido de ", lower_first(p$component[[min(2L, nrow(p))]]),
      " (", pct(p$share[[min(2L, nrow(p))]], 0), ")."
    )
  } else "A decomposição do tempo parado depende da execução do teste de intervenções (aba 3 do aplicativo)."

  out$melhorias = if (is.data.frame(r) && nrow(r) > 0L) {
    c(
      paste0(
        "O aplicativo testou ", nrow(r), " intervenções em simulação pareada; ", nrow(shown),
        " tiveram ganho de disponibilidade demonstrado (intervalo de 95% acima de zero)."
      ),
      if (nrow(shown) > 0L) paste0(
        "A de maior prioridade foi ", lower_first(shown$label[[1]]), " (", change_txt(shown$change[[1]]), "), com ganho de ",
        format_pp_br(shown$delta_availability[[1]]), " (IC95% ", format_pp_br(shown$delta_ci_lower[[1]]),
        " a ", format_pp_br(shown$delta_ci_upper[[1]]), ")."
      ) else "Nenhuma intervenção atingiu ganho demonstrado nesta execução; mais replicações ou magnitudes maiores podem alterar esse resultado."
    )
  } else "O teste de intervenções ainda não foi executado."

  out$simulacao = "As configurações simuladas são comparadas quanto à probabilidade de cumprir a missão e à disponibilidade média, com os respectivos intervalos de confiança."

  mc = report$multicriteria
  out$robustez = tryCatch(
    mc_robustness_text(mc, mc_rank_stability(mc), mc_method_sensitivity(mc)),
    error = function(e) character()
  )
  out$limitacoes = c(
    switch(mode,
      SYNTHETIC = "O modo de evidência é SYNTHETIC: os números são demonstrativos e não constituem evidência sobre a frota JLTV.",
      HYBRID = "O modo de evidência é HYBRID: o resultado combina registros reais e hipóteses de cenário e deve ser lido como projeção condicional.",
      EMPIRICAL = "O modo de evidência é EMPIRICAL: o resultado é condicional ao período, à amostra e às hipóteses do modelo.",
      ""
    ),
    "O simulador representa uma causa de parada ativa por vez, sem redundância nem falhas simultâneas. Por ser uma simulação de eventos discretos repetida em replicações de Monte Carlo, os intervalos de confiança refletem a variabilidade entre replicações, e não a incerteza dos parâmetros.",
    character()
  )
  out$proximos_passos = c(
    "Recomenda-se conferir, nos registros apontados na pergunta P7, se os parâmetros do simulador representam a frota antes de usar o resultado em decisão."
  )

  # A mesma evidência é apresentada na análise antes de ser retomada na conclusão.
  core = word_report_decision_core(report)
  for (name in names(core)) if (length(core[[name]]) > 0L || identical(name, "proximos_passos")) out[[name]] = core[[name]]

  out
}

# ---- 3. Texto pela IA (opcional) -------------------------------------------
word_report_ai_catalog = function(db = word_report_ref_db()) {
  one = function(key) {
    e = db[[key]]
    ref = gsub("**", "", e$ref, fixed = TRUE)
    ref = gsub("\\s+", " ", ref)
    if (nchar(ref) > 230L) ref = paste0(substr(ref, 1L, 227L), "...")
    paste0(key, " | ", ref)
  }
  paste(vapply(names(db), one, character(1)), collapse = "\n")
}

word_report_ai_system_prompt = function(db = word_report_ref_db()) {
  paste(
    "Você é o autor-analista de um artigo científico do Curso de Gestão, Logística e Apoio à Decisão da Escola Superior de Guerra (ESG), sobre a sustentação da frota de viaturas JLTV do Corpo de Fuzileiros Navais (CFN). Você é especialista em logística de defesa, confiabilidade, manutenibilidade e disponibilidade (RAM), apoio multicritério à decisão e simulação.",
    "Você recebe os resultados completos de uma execução do simulador (julgamento de especialistas por AHP–TOPSIS, registros da frota, simulação de eventos discretos com replicações de Monte Carlo, teste de intervenções e grade de configurações). As seções 1 a 3 do artigo (introdução, referencial e metodologia) já estão escritas. Sua tarefa é ANALISAR esses resultados e escrever, a partir da sua análise, a seção 4 (Análise e discussão), a seção 5 (Conclusão), o resumo e o abstract.",
    "",
    "ANTES DE ESCREVER, ANALISE (no seu raciocínio, não no texto):",
    "1) Que resposta os números dão à questão de pesquisa? Quais sistemas e quais intervenções, e com que força de evidência?",
    "2) Onde os resultados convergem e onde divergem: prioridade dos especialistas × parcela de cada sistema no tempo parado simulado; disponibilidade média × probabilidade de cumprir a missão; ganho isolado × ganho do pacote; pesos declarados × critério que de fato ordena os sistemas.",
    "3) Qual mecanismo do modelo explica cada resultado: composição do tempo parado (reparo ativo × esperas), filas por equipe e box, espera por peça, margem entre frota (N) e meta (M), variabilidade entre replicações. Uma intervenção só ganha se atuar sobre a parcela do tempo parado que pesa.",
    "4) Que risco para a missão a média esconde: a distribuição das replicações (pior caso, menor número de viaturas prontas, horas abaixo de M) e a sensibilidade ao tamanho da frota.",
    "5) O que as intervenções sem ganho demonstrado ensinam sobre onde não investir nas condições simuladas.",
    "6) O que limita a validade (origem dos parâmetros, cobertura e qualidade dos registros, indicadores não estimáveis, modo de evidência, o que o modelo não representa) e como isso muda a leitura de cada achado.",
    "7) Que decisões concretas de sustentação os resultados justificam, em que ordem, com que metas testadas, e o que deve ser verificado nos registros antes de adotá-las.",
    "Escolha os 4 ou 5 achados mais importantes e construa o texto em torno deles. Não dê o mesmo peso a tudo e não descreva tabela por tabela: interprete.",
    "",
    "COMO O TEXTO DEVE SER:",
    "- Análise, não relato: cada parágrafo afirma uma ideia, traz o número que a sustenta e diz o que ele significa para a sustentação da frota e para a missão. Relacione resultados de origens diferentes (especialistas, registros, simulação).",
    "- Defenda uma tese clara, sustentada pelos fatos, e aponte implicações práticas para o CFN no quadro da Gestão do Ciclo de Vida (GCV) e do Apoio Logístico Integrado (ALI).",
    "- Escrita de artigo, variada e precisa. Evite fórmulas repetidas ('O resultado mostra que', 'Cabe ressaltar', 'Nesse sentido', 'Vale destacar'), ressalvas genéricas repetidas e listas de números sem interpretação. Não repita numa subseção o que outra já disse; a conclusão retoma os achados em síntese, com outras palavras, sem copiar frases da seção 4.",
    "",
    "ORIENTAÇÕES DE REDAÇÃO DO TRABALHO (aplique sem mencioná-las; nunca cite orientador, banca, revisão ou comentários):",
    "- Diga diretamente o que foi feito e o que foi encontrado. Não escreva o que não foi feito, o que não se pretende fazer nem o que o texto 'não afirma'.",
    "- Mantenha o foco no objetivo da pesquisa; não crie outros pontos centrais.",
    "- Um assunto por parágrafo, parágrafos curtos (cerca de cinco linhas, três ou quatro frases). Ao relacionar teoria e resultado, use a sequência conceito (com o autor), dado e associação entre os dois.",
    "- Defina cada sigla e cada conceito na primeira vez que os usar e indique o autor do conceito.",
    "- Dado ausente, cobertura e indicador não estimável são questão de método: trate-os na subseção de limitações, de forma objetiva, sem transformá-los em achado sobre a frota.",
    "- A simulação é instrumento acessório de análise, com restrições decorrentes da disponibilidade de dados: apresente o que ela mostrou e o limite dessa leitura.",
    "- Não presuma deficiência da frota nem da estrutura de apoio; conclua apenas o que os dados sustentam. Linguagem neutra: prefira intervenção, prioridade e tempo parado a melhoria, gargalo e indisponibilidade.",
    "- Terceira pessoa, voz ativa, passado para o que a pesquisa fez e presente para conceitos e resultados. Sem primeira pessoa e sem adjetivos de efeito.",
    "",
    "RIGOR CONCEITUAL:",
    "a) A prioridade AHP–TOPSIS expressa julgamento de especialistas; não é taxa de falha, MTBF, MTTR nem lead time. Notas P6 nunca viram horas ou probabilidades.",
    "b) Não afirme causalidade sobre a frota real: descreva associação e ganho simulado. Quando o IC95% do ganho incluir zero, o ganho não foi demonstrado.",
    "c) Distinga disponibilidade intrínseca (Ai), alcançada (Aa) e operacional (Ao) {p:lacey}; não chame MTBF/(MTBF+MTTR) de Ao. Lead time é o prazo entre a demanda e o item disponível; espera por peça não é reparo ativo. MTTF serve a itens não reparáveis; MTBF, a itens reparáveis.",
    "d) A missão é cumprida quando ao menos M viaturas ficam prontas durante todo o horizonte; a probabilidade-alvo é a confiança exigida, e não M/N. Se a probabilidade de referência estiver acima do alvo, a frota cumpre a exigência e a análise discute a folga; se estiver abaixo, discute o déficit. Nunca escreva o contrário do que os números mostram.",
    "e) Associar uma intervenção a um sistema é proposta do autor: as intervenções foram simuladas sobre a viatura completa.",
    "f) Custo ausente não é zero: não afirme ótimo econômico nem retorno financeiro. Parâmetros e conclusões de outras frotas não se transferem para a JLTV.",
    "g) Se o ranking dos especialistas e a simulação divergirem, explique a diferença (o que cada um mede, origem dos parâmetros, cobertura, pressupostos) sem tratá-la como erro e sem sugerir ajustar parâmetros para igualá-los.",
    "",
    "NÚMEROS: use somente números que aparecem nos dados recebidos ou em comparacoes_calculadas, escritos como lá estão (porcentagens com vírgula decimal, ganhos em p.p., coeficiente TOPSIS com três casas). Não calcule diferenças, razões, médias nem projeções novas; se precisar de uma comparação que não está calculada, descreva-a em palavras, sem número. O aplicativo confere cada número e descarta o parágrafo que contiver número ausente dos dados. Não cite números de tabelas, quadros nem figuras: o aplicativo insere as chamadas.",
    "",
    "CITAÇÕES: cite apenas fontes do catálogo, com {c:chave} quando o autor é sujeito da frase (ex.: '{c:saaty} propõe...') e {p:chave} ou {p:chave1,chave2} no fim da frase ou oração. Cite para sustentar conceito ou método que o título da fonte trata, nunca para sustentar número desta execução, e no máximo duas citações por parágrafo. Usos: Saaty para o AHP e a razão de consistência; Hwang e Yoon para o TOPSIS; Sałabun et al. para a sensibilidade do ranking; Lacey e Hockley para disponibilidade e tempos de parada; Feickert para o histórico e as questões de confiabilidade e manutenibilidade da JLTV; Scarf et al. e Targett para manutenção e estoque; Robinson e Barth et al. para modelo conceitual e uso da simulação; Prado e Taha para simulação e Monte Carlo; MD40-M-01 e MD44-M-01 (Brasil) para GCV e ALI. Schmuller só para a programação em R. Na conclusão, no resumo e no abstract não use citações.",
    "",
    "ESTRUTURA E CONTEÚDO DE CADA CHAVE (as chaves de 'sintese' a 'conclusao' são listas de parágrafos):",
    "- sintese (4.1): visão de conjunto da execução: o critério da missão, a disponibilidade de referência e o que a distribuição entre replicações revela, a probabilidade de cumprir a missão frente ao alvo e o fio condutor dos achados que as subseções seguintes desenvolvem.",
    "- prioridades (4.2): quais sistemas os especialistas priorizaram, por qual critério e subcritério, quão separados estão os coeficientes, o que as menções na P7 indicam e o que essa ordem significa para a sustentação.",
    "- pesos (4.3): a estrutura de preferências: critério e subcritérios dominantes, concentração dos pesos, consistência dos julgamentos e a consequência disso para o ranking.",
    "- robustez (4.4): estabilidade das posições nas reamostragens dos avaliadores e sensibilidade a pesos e normalização; responda explicitamente à hipótese H1 (sustentada, sustentada em parte ou não sustentada, e para quais posições).",
    "- processo (4.5): onde se concentra o tempo parado simulado e qual mecanismo o explica; o que a parcela por sistema mostra e como ela se compara à prioridade dos especialistas; o que os registros da frota mostram sobre os tempos de parada, quando disponíveis.",
    "- melhorias (4.6): o teste das intervenções e a hipótese H2: quais tiveram ganho demonstrado e por quê, quais não tiveram e por quê (ligando ao tempo parado), o efeito sobre a probabilidade de cumprir a missão e o que a diferença entre a soma dos ganhos isolados e o pacote revela.",
    "- simulacao (4.7): a grade de configurações: o que mais move a probabilidade de cumprir a missão (tamanho da frota, equipes, boxes, estoque), a margem entre frota e meta e o risco de perder viaturas.",
    "- limitacoes (4.8): o que condiciona a validade dos resultados (modo de evidência, origem dos parâmetros, registros e indicadores não estimáveis, o que o modelo não representa, intervalos de confiança) e como os resultados devem ser lidos por isso.",
    "- conclusao (5 a 7 parágrafos, sem citações): 1) resposta direta à questão de pesquisa, nomeando os sistemas e as intervenções a priorizar; 2) o que os especialistas decidiram e com que robustez; 3) o que a simulação mostrou e por quê; 4) a tese do trabalho, relacionando disponibilidade média, probabilidade de cumprir a missão e margem de viaturas; 5) a proposta de ação em ordem, com as metas testadas, as ações correspondentes e a verificação nos registros; 6) alcance do objetivo, contribuição e continuidade do estudo. Sem dado novo.",
    "- resumo: uma string com um único parágrafo de 160 a 240 palavras, sem citações: tema, problema, objetivo, método, resultados principais com números e conclusão, em voz ativa e terceira pessoa. abstract: tradução fiel do resumo para o inglês.",
    "- proximos_passos: lista vazia.",
    "Respeite o tamanho indicado em 'extensao': o corpo do artigo tem limite de 20 páginas e as figuras precisam caber nele. Escreva denso: cada frase deve trazer um fato, uma relação ou uma implicação.",
    "",
    "SAÍDA: apenas um objeto JSON com as chaves resumo, abstract, sintese, prioridades, pesos, robustez, processo, melhorias, simulacao, limitacoes, conclusao, proximos_passos. Nada de JSON, campo, chave ou estrutura de dados dentro do texto.",
    "",
    "CATÁLOGO DE FONTES (chave | referência):",
    word_report_ai_catalog(db),
    sep = "\n"
  )
}

# Remove marcadores de citação inválidos (chave inexistente, tabela/figura) e, quando pedido, todos.
word_report_ai_clean_text = function(x, db, allow_cites = TRUE) {
  x = as.character(x)
  vapply(x, function(txt) {
    tokens = unique(regmatches(txt, gregexpr("\\{[a-z]+:[^}]*\\}", txt, perl = TRUE))[[1]])
    for (tok in tokens) {
      kind = substr(tok, 2L, 2L)
      keys = strsplit(sub("^\\{[a-z]+:", "", sub("\\}$", "", tok)), "[,;[:space:]]+")[[1]]
      keys = keys[nzchar(keys)]
      good = intersect(keys, names(db))
      repl = if (allow_cites && kind %in% c("c", "p") && length(good) > 0L &&
                 grepl("^\\{[cp]:", tok)) {
        paste0("{", kind, ":", paste(good, collapse = ","), "}")
      } else ""
      txt = gsub(tok, repl, txt, fixed = TRUE)
    }
    if (!allow_cites) {
      for (ref in db) {
        forms=c(paste0(ref$narr," (",ref$year,")"),paste0("(",ref$par,", ",ref$year,")"))
        for (form in unique(c(forms,toupper(forms)))) txt=gsub(form,"",txt,fixed=TRUE)
      }
    }
    sents = unlist(strsplit(txt, "(?<=[.!?])\\s+", perl = TRUE))
    sents = sents[!grepl("orientador|banca examinadora|observações d[oa]s? (revis|orient|banca)", sents, ignore.case = TRUE)]
    txt = paste(sents, collapse = " ")
    txt = gsub("[[:space:]]+([.,;:])", "\\1", txt)
    gsub("  +", " ", txt)
  }, character(1), USE.NAMES = FALSE)
}

# Saneamento do texto da IA: remove frases sobre a estrutura dos dados, arredonda números longos
# e troca citação narrativa usada no fim da frase pela forma entre parênteses.
word_report_ai_meta_regex = "(?i)\\bjson\\b|\\bpayload\\b|dados recebidos|dados de entrada|nucleo_analitico|contexto_da_missao|targets_bottleneck|não (informa|detalha|associa|indica|especifica) (a|o|qual|quais|se|cada)"
word_report_ai_sanitize = function(x) {
  x = as.character(x)
  vapply(x, function(txt) {
    if (is.na(txt) || !nzchar(txt)) return("")
    # Parágrafo que fala da estrutura dos dados é descartado inteiro (cortar frases deixava trechos soltos).
    if (grepl(word_report_ai_meta_regex, txt, perl = TRUE)) return("")
    m = gregexpr("(?<![0-9])[0-9]+[.,][0-9]{4,}", txt, perl = TRUE)
    regmatches(txt, m) = lapply(regmatches(txt, m), function(v) {
      if (!length(v)) return(v)
      vapply(v, function(s) format_number_br(as.numeric(gsub(",", ".", s, fixed = TRUE)), 3), character(1))
    })
    txt = gsub("(?<!segundo |conforme |para |por |com |de acordo com )\\{c:([a-z0-9_]+)\\}(?=\\s*[.,;:)]|\\s*$)", "{p:\\1}", txt, perl = TRUE)
    trimws(txt)
  }, character(1), USE.NAMES = FALSE)
}

# Rascunho do aplicativo enviado à IA: o mesmo texto local, já com nomes e números formatados.
word_report_ai_text_keys = c("sintese", "prioridades", "pesos", "robustez", "processo", "melhorias", "simulacao", "limitacoes", "conclusao")
word_report_ai_draft = function(report) {
  local = word_report_narrative_local(report)
  ab = word_report_resumo_local(report)
  out = lapply(stats::setNames(word_report_ai_text_keys, word_report_ai_text_keys), function(k) as.character(unlist(local[[k]])))
  out$resumo = ab$pt
  out
}
word_report_ai_word_targets = function(report) {
  d = word_report_ai_draft(report)
  lapply(d, function(x) max(80L, as.integer(round(1.15 * word_report_word_count(x)))))
}

# Números citados num texto (ignora siglas com dígitos, como MD40-M-01, IC95%, H1, A3, e chamadas de seção).
word_report_numbers_in = function(txt) {
  txt = paste(txt, collapse = " ")
  txt = gsub("\\{[cp]:[^}]*\\}", " ", txt)
  txt = gsub("(?i)(sub)?se[cç][aã]o\\s+[0-9](\\.[0-9])?", " ", txt, perl = TRUE)
  txt = gsub("[A-Za-zÀ-ÿ]+[0-9]+(-[A-Za-z]+-[0-9]+)?", " ", txt, perl = TRUE)
  txt = gsub("(?<=[0-9])\\.(?=[0-9]{3}\\b)", "", txt, perl = TRUE)   # milhar 1.080 -> 1080
  m = regmatches(txt, gregexpr("[0-9]+(?:,[0-9]+)?(?:\\.[0-9]+)?", txt, perl = TRUE))[[1]]
  suppressWarnings(as.numeric(gsub(",", ".", m, fixed = TRUE)))
}

# Conjunto de números permitidos: tudo o que o aplicativo calculou (em fração e em porcentagem) e escreveu.
word_report_ai_allowed_numbers = function(report, facts = NULL) {
  facts = facts %||% word_report_facts(report)
  raw = suppressWarnings(as.numeric(unlist(facts, use.names = FALSE)))
  raw = raw[is.finite(raw)]
  draft_nums = word_report_numbers_in(unlist(word_report_ai_draft(report), use.names = FALSE))
  years = suppressWarnings(as.numeric(unlist(lapply(word_report_ref_db(), function(e) sub("[a-z]$", "", e$year)))))
  vals = c(raw, 100 * raw, abs(raw), abs(100 * raw), draft_nums, years, 0:12)
  unique(round(vals[is.finite(vals)], 4))
}

word_report_ai_scope_text = function(replaced) {
  labels = c(resumo = "o resumo", abstract = "o abstract", sintese = "a síntese (4.1)", prioridades = "as prioridades (4.2)", pesos = "os pesos (4.3)",
             robustez = "a robustez (4.4)", processo = "o tempo parado (4.5)", melhorias = "o teste das intervenções (4.6)",
             simulacao = "as configurações (4.7)", limitacoes = "as limitações (4.8)", conclusao = "a conclusão")
  r = intersect(names(labels), as.character(replaced))
  if (!length(r)) return("apenas trechos complementares da análise")
  word_report_join(unname(labels[r]))
}

word_report_ai_numbers_ok = function(txt, allowed, tol = 0.051) {
  nums = word_report_numbers_in(txt)
  nums = nums[is.finite(nums)]
  if (!length(nums)) return(TRUE)
  all(vapply(nums, function(v) any(abs(allowed - v) <= tol), logical(1)))
}

# ---- IA analista: dados completos da execução, enquadramento da pesquisa e orientações de redação ----
# A IA recebe os resultados (não o texto do aplicativo), analisa e redige a seção 4, a conclusão e o resumo.
# Todos os valores vão já formatados em português, para que a IA os copie sem conversão.
word_report_ai_dataset = function(report) {
  a = report$assessment
  k = a$kpis
  f = word_report_findings(report)
  ctx = f$ctx
  P = function(x, d = 1) if (word_report_num(x)) format_percent_br(x, d) else NULL
  PP = function(x, d = 1) if (word_report_num(x)) format_pp_br(x, d) else NULL
  N = function(x, d = 1) if (word_report_num(x)) format_number_br(x, d) else NULL
  D = function(x, d = 1) if (word_report_num(x)) paste0(format_number_br(abs(100 * x), d), " p.p.") else NULL
  df = function(x) if (is.data.frame(x) && nrow(x) > 0L) x else NULL
  num1 = function(x) suppressWarnings(as.numeric(x)[1])
  Nn = num1(ctx$frota); M = num1(ctx$meta_viaturas_prontas); pt = num1(ctx$probabilidade_alvo)
  mode = f$mode

  # Missão e resultado de referência, com a distribuição entre replicações.
  reps = if (is.list(report$improvements)) df(report$improvements$base_replications) else NULL
  dist = NULL
  if (!is.null(reps) && "avg_availability" %in% names(reps)) {
    x = as.numeric(reps$avg_availability); x = x[is.finite(x)]
    if (length(x) >= 3L) {
      q = stats::quantile(x, c(0.1, 0.5, 0.9), names = FALSE, type = 7)
      dist = list(
        replicacoes = length(x), minima = P(min(x)), percentil_10 = P(q[1]), mediana = P(q[2]),
        percentil_90 = P(q[3]), maxima = P(max(x)), desvio_padrao = D(stats::sd(x), 2)
      )
      if ("min_ready" %in% names(reps)) {
        mr = as.integer(reps$min_ready); mr = mr[is.finite(mr)]
        tab = table(factor(mr, levels = sort(unique(mr))))
        dist$menor_numero_de_viaturas_prontas_em_cada_replicacao = data.frame(
          minimo_de_viaturas_prontas = as.integer(names(tab)), replicacoes = as.integer(tab),
          parcela_das_replicacoes = vapply(as.numeric(tab) / sum(tab), format_percent_br, character(1), digits = 0),
          stringsAsFactors = FALSE)
        if (word_report_num(M)) dist$replicacoes_que_ficaram_abaixo_de_M_em_algum_momento = paste0(sum(mr < M), " de ", length(mr))
      }
      if ("hours_below_target" %in% names(reps)) {
        hb = as.numeric(reps$hours_below_target); hb = hb[is.finite(hb)]
        if (any(hb > 0)) dist$horas_abaixo_de_M_nas_replicacoes_que_falharam = list(
          media = paste0(N(mean(hb[hb > 0])), " h"), maxima = paste0(N(max(hb)), " h"))
      }
      if ("failures" %in% names(reps)) dist$falhas_por_replicacao_media = N(mean(as.numeric(reps$failures), na.rm = TRUE))
    }
  }
  sim_ref = NULL
  s = if (is.list(report$simulation)) df(report$simulation$summary) else NULL
  if (!is.null(s) && is.list(report$improvements) && !is.null(report$improvements$config)) {
    cid = tryCatch(configuration_id(report$improvements$config), error = function(e) NA_character_)
    row = s[s$configuration_id %in% cid, , drop = FALSE]
    if (nrow(row) == 1L && all(c("ci_lower", "ci_upper") %in% names(row))) sim_ref = paste0(P(row$ci_lower), " a ", P(row$ci_upper))
  }
  missao = list(
    frota_N_viaturas = Nn, meta_M_viaturas_prontas = M, horizonte_horas = num1(ctx$horizonte_h),
    tolerancia_abaixo_de_M_horas = num1(ctx$tolerancia_h), probabilidade_alvo = P(pt),
    fracao_M_sobre_N = if (word_report_num(M) && word_report_num(Nn) && Nn > 0) P(M / Nn) else NULL,
    margem_N_menos_M_viaturas = if (word_report_num(M) && word_report_num(Nn)) Nn - M else NULL,
    replicacoes = num1(ctx$replicacoes),
    definicao = "Cumprir a missão = manter ao menos M viaturas prontas durante todo o horizonte (respeitada a tolerância). A probabilidade é a fração das replicações que cumprem esse critério; a probabilidade-alvo é a confiança exigida, e não M/N.")
  referencia = list(
    disponibilidade_operacional_media = P(f$ao_ref),
    probabilidade_de_cumprir_a_missao = P(f$p_ref),
    ic95_da_probabilidade_de_missao = sim_ref,
    situacao_frente_ao_alvo = if (isTRUE(f$mission_known)) (if (isTRUE(f$mission_ok)) "acima da probabilidade-alvo" else "abaixo da probabilidade-alvo") else NULL,
    disponibilidade_observada_nos_registros = P(k$ao_observed),
    distribuicao_entre_replicacoes = dist)

  # Especialistas.
  rk = f$ranking
  ranking = if (is.data.frame(rk)) tryCatch({
    out = data.frame(posicao = rk$rank, sistema = rk$alternative_name,
                     coeficiente_topsis = vapply(rk$coefficient, format_number_br, character(1), digits = 3), stringsAsFactors = FALSE)
    if ("dominant_criterion_label" %in% names(rk)) out$criterio_que_mais_pesa = rk$dominant_criterion_label
    if ("dominant_subcriterion_label" %in% names(rk)) out$subcriterio_dominante = rk$dominant_subcriterion_label
    for (cc in c("OCCURRENCE", "DOWNTIME", "SUPPLY", "SUPPORT")) {
      col = paste0("share_", cc)
      if (col %in% names(rk)) out[[paste0("parcela_da_prioridade_", gsub(" ", "_", word_report_crit(assessment_criterion_labels()[[cc]])))]] = vapply(rk[[col]], format_percent_br, character(1), digits = 0)
    }
    if ("p7_mentions" %in% names(rk)) out$mencoes_na_pergunta_P7 = rk$p7_mentions
    out
  }, error = function(e) NULL) else NULL
  w = f$weights
  pesos = if (is.data.frame(w)) data.frame(criterio = w$criterio, peso = vapply(w$peso, format_percent_br, character(1), digits = 1), stringsAsFactors = FALSE) else NULL
  mc = report$multicriteria
  sw = if (is.list(mc)) df(mc$ahp$subcriteria_weights) else NULL
  subpesos = if (!is.null(sw) && all(c("subcriterion_id", "global_weight") %in% names(sw))) {
    lab = subcriterion_label_map()
    o = sw[order(-sw$global_weight), , drop = FALSE]
    data.frame(subcriterio = word_report_crit(ifelse(is.na(lab[o$subcriterion_id]), o$subcriterion_id, lab[o$subcriterion_id])),
               peso_global = vapply(o$global_weight, format_percent_br, character(1), digits = 1), stringsAsFactors = FALSE)
  } else NULL
  cons = if (is.list(mc)) df(mc$ahp$consistency) else NULL
  consist = if (!is.null(cons)) data.frame(grupo = cons$hierarchy_group, avaliadores = cons$respondents,
                                           razao_de_consistencia = vapply(cons$CR, format_number_br, character(1), digits = 3), stringsAsFactors = FALSE) else NULL
  stab = f$stability
  estab = if (is.list(stab) && is.data.frame(stab$summary)) {
    ss = stab$summary[order(stab$summary$rank_ref), , drop = FALSE]
    list(reamostragens = stab$replications, avaliadores = stab$respondents,
         por_sistema = data.frame(sistema = word_report_pretty_name(ss$alternative_name), posicao_original = ss$rank_ref,
                                  posicao_mediana = ss$median_rank, faixa_de_90_por_cento = paste0(ss$rank_p05, " a ", ss$rank_p95),
                                  frequencia_em_1o = vapply(ss$prob_top1, format_percent_br, character(1), digits = 0),
                                  frequencia_entre_os_3_primeiros = vapply(ss$prob_top3, format_percent_br, character(1), digits = 0),
                                  stringsAsFactors = FALSE))
  } else NULL
  sens = f$sensitivity
  sensib = if (is.data.frame(sens)) list(
    posicoes = data.frame(sistema = word_report_pretty_name(sens$alternative_name), ahp_e_normalizacao_vetorial = sens$posicao_ahp_vetorial,
                          pesos_iguais = sens$posicao_pesos_iguais, normalizacao_min_max = sens$posicao_minmax, stringsAsFactors = FALSE),
    spearman_pesos_iguais = N(attr(sens, "spearman_pesos_iguais"), 2), spearman_min_max = N(attr(sens, "spearman_minmax"), 2)) else NULL

  # Registros da frota (o que os dados reais mostraram e o que não permitiram estimar).
  ram = df(report$ram_metrics)
  registros = list(
    indicadores_RAM_dos_registros = ram,
    tempo_parado_registrado_por_etapa = df(report$observed_downtime),
    qualidade_dos_dados = df(report$quality_summary),
    resumo_da_pergunta_P7 = if (is.data.frame(report$p7_summary)) utils::head(report$p7_summary, 15L) else NULL)

  # Tempo parado simulado.
  pr = f$process
  processo = if (is.data.frame(pr)) list(
    componentes = data.frame(componente = pr$component, horas_viatura_medias_por_replicacao = vapply(pr$hours, format_number_br, character(1), digits = 1),
                             parcela = vapply(pr$share, format_percent_br, character(1), digits = 1), stringsAsFactors = FALSE),
    agrupamento = list(ciclo_de_manutencao_reparo_diagnostico_teste = P(f$groups[["ciclo"]]), espera_por_peca = P(f$groups[["suprimento"]]),
                       espera_por_equipe_ou_box = P(f$groups[["capacidade"]]), manutencao_preventiva = P(f$groups[["preventiva"]])),
    nota = "Horas-viatura somadas na frota e médias entre replicações; não são a duração de uma ordem de serviço.") else NULL
  sysx = NULL
  ssys = f$systems
  if (is.data.frame(ssys) && nrow(ssys) > 0L) {
    pos = if (is.data.frame(a$contributions) && nrow(a$contributions) > 0L) a$contributions$rank[match(ssys$system_id, a$contributions$alternative_id)] else rep(NA, nrow(ssys))
    sys_mode = toupper(as.character(report$systems_simulation$mode %||% mode))
    sysx = list(
      modo = sys_mode,
      atencao = if (identical(sys_mode, "SYNTHETIC")) "Esta simulação por sistema usou o catálogo de demonstração (modo SYNTHETIC): as parcelas ilustram o procedimento e não descrevem a frota." else NULL,
      sistemas = data.frame(sistema = word_report_pretty_name(ssys$system_name), horas_paradas_por_execucao = vapply(ssys$down_h, format_number_br, character(1), digits = 1),
                            falhas_por_execucao = if ("failures" %in% names(ssys)) vapply(ssys$failures, format_number_br, character(1), digits = 1) else NA,
                            parcela_do_tempo_parado = vapply(ssys$share, format_percent_br, character(1), digits = 1),
                            posicao_na_prioridade_dos_especialistas = ifelse(is.finite(pos), pos, NA), stringsAsFactors = FALSE),
      nota = "Cada parada é atribuída ao sistema que a causou; as parcelas resultam dos parâmetros de vida, reparo e suprimento do catálogo de componentes.")
  }

  # Intervenções.
  r = a$improvements
  interv = if (is.data.frame(r) && nrow(r) > 0L) tryCatch({
    out = data.frame(
      ordem = r$priority_rank, intervencao = r$label, o_que_muda = r$change,
      disponibilidade_com_a_intervencao = vapply(r$new_availability, format_percent_br, character(1)),
      ganho = vapply(r$delta_availability, format_pp_br, character(1)),
      ic95_do_ganho = paste0(vapply(r$delta_ci_lower, format_pp_br, character(1)), " a ", vapply(r$delta_ci_upper, format_pp_br, character(1))),
      ganho_demonstrado = ifelse(r$demonstrated %in% TRUE & is.finite(r$delta_ci_lower) & r$delta_ci_lower > 0, "sim", "não"),
      stringsAsFactors = FALSE)
    if ("new_success" %in% names(r)) out$probabilidade_de_missao_com_a_intervencao = vapply(r$new_success, format_percent_br, character(1))
    if ("example_actions" %in% names(r)) out$acoes_correspondentes = r$example_actions
    out
  }, error = function(e) NULL) else NULL
  pk = f$package
  pkv = function(col) if (!is.null(pk) && col %in% names(pk)) suppressWarnings(as.numeric(pk[[col]][[1]])) else NA_real_
  pacote = if (!is.null(pk)) list(
    descricao = "As intervenções de maior ganho aplicadas em conjunto.",
    disponibilidade_antes = P(pkv("base_availability")), disponibilidade_depois = P(pkv("new_availability")), ganho = PP(pkv("delta_availability")),
    probabilidade_de_missao_antes = P(pkv("base_success")), probabilidade_de_missao_depois = P(pkv("new_success"))) else NULL

  # Configurações simuladas.
  configs = if (!is.null(s)) tryCatch({
    out = data.frame(configuracao = as.character(s$configuration_id), stringsAsFactors = FALSE)
    if ("fleet_size" %in% names(s)) out$frota = s$fleet_size
    for (col in c("diagnostic_teams", "repair_teams", "bays", "initial_stock")) if (col %in% names(s)) out[[c(diagnostic_teams = "equipes_de_diagnostico", repair_teams = "equipes_de_reparo", bays = "boxes", initial_stock = "estoque_inicial")[[col]]]] = s[[col]]
    out$probabilidade_de_missao = vapply(s$success_probability, format_percent_br, character(1))
    if (all(c("ci_lower", "ci_upper") %in% names(s))) out$ic95 = paste0(vapply(s$ci_lower, format_percent_br, character(1)), " a ", vapply(s$ci_upper, format_percent_br, character(1)))
    out$disponibilidade_media = vapply(s$mean_availability, format_percent_br, character(1))
    if (word_report_num(pt)) out$atinge_o_alvo = ifelse(s$success_probability >= pt, "sim (estimativa pontual)", "não")
    out
  }, error = function(e) NULL) else NULL

  # Parâmetros do simulador (premissas).
  asm = report$assumptions
  premissas = if (is.data.frame(asm) && all(c("Item", "Valor") %in% names(asm))) {
    keep = c("Modelo de falhas", "Diagnóstico médio (h)", "Reparo corretivo médio (h)", "Preventiva média (h)", "Teste/liberação médio (h)",
             "CV dos tempos", "Prazo médio de reposição (h)", "Probabilidade de exigir peça (corretiva)",
             "Apoio-base (diagnóstico / reparo / boxes / estoque)", "Grade simulada (frota; D; R; B; S)", "Utilização na missão (h uso / h calendário)",
             "Agregação das notas P6", "Cobertura mínima P6 por célula", "Situação de referência das intervenções")
    x = asm[asm$Item %in% keep, c("Item", "Valor"), drop = FALSE]
    stats::setNames(as.list(as.character(x$Valor)), x$Item)
  } else NULL
  conf = df(a$confidence)

  # Comparações já calculadas (a IA não calcula números novos).
  comp = list()
  addc = function(name, value) if (!is.null(value) && length(value) == 1L && !is.na(value)) comp[[name]] <<- value
  if (word_report_num(f$ao_ref) && word_report_num(M) && word_report_num(Nn) && Nn > 0) addc("disponibilidade de referência menos a fração M/N", PP(f$ao_ref - M / Nn))
  if (isTRUE(f$mission_known)) addc("probabilidade de missão menos a probabilidade-alvo", PP(f$p_ref - pt))
  if (is.data.frame(f$by_fleet) && nrow(f$by_fleet) >= 2L) {
    bf = f$by_fleet
    for (i in seq_len(nrow(bf))) addc(paste0("probabilidade de missão com frota de ", bf$fleet[[i]], " viaturas (faixa na grade)"),
      if (abs(bf$p_max[[i]] - bf$p_min[[i]]) < 0.005) P(bf$p_max[[i]]) else paste0(P(bf$p_min[[i]]), " a ", P(bf$p_max[[i]])))
    if (word_report_num(Nn) && any(bf$fleet == Nn) && any(bf$fleet == Nn - 1)) addc("queda da melhor probabilidade de missão ao passar de N para N−1 viaturas",
      D(bf$p_max[bf$fleet == Nn][[1]] - bf$p_max[bf$fleet == Nn - 1][[1]]))
    if (word_report_num(f$min_fleet)) addc("menor frota da grade que atinge a probabilidade-alvo", paste0(f$min_fleet, " viaturas"))
    addc("variação da disponibilidade média máxima entre os tamanhos de frota da grade", D(max(bf$ao_max) - min(bf$ao_max)))
  }
  grid_effect = function(col, label) tryCatch({
    if (!is.null(s) && col %in% names(s) && word_report_num(Nn) && length(unique(s[[col]])) > 1L && any(s$fleet_size == Nn)) {
      sub = s[s$fleet_size == Nn, , drop = FALSE]
      t = tapply(sub$success_probability, sub[[col]], mean)
      t = t[order(as.numeric(names(t)))]
      if (length(t) >= 2L) addc(paste0(label, " (de ", names(t)[1], " para ", names(t)[length(t)], "; frota N, média da grade)"), PP(t[[length(t)]] - t[[1]]))
    }
  }, error = function(e) NULL)
  grid_effect("repair_teams", "efeito de mais equipes de reparo na probabilidade de missão")
  grid_effect("initial_stock", "efeito do maior estoque inicial na probabilidade de missão")
  grid_effect("bays", "efeito de mais boxes na probabilidade de missão")
  grid_effect("diagnostic_teams", "efeito de mais equipes de diagnóstico na probabilidade de missão")
  if (is.data.frame(f$shown) && nrow(f$shown) > 0L) {
    addc("número de intervenções testadas", nrow(r)); addc("número de intervenções com ganho demonstrado", nrow(f$shown))
    if (!is.null(pk) && nrow(f$main) > 0L) {
      top_n = min(3L, nrow(f$shown))
      addc("soma dos ganhos isolados das intervenções de maior ganho", PP(sum(utils::head(f$shown$delta_availability, top_n))))
      addc("soma dos ganhos isolados menos o ganho do pacote", PP(sum(utils::head(f$shown$delta_availability, top_n)) - pkv("delta_availability")))
      if (word_report_num(pkv("new_success")) && word_report_num(pkv("base_success"))) addc("ganho do pacote na probabilidade de missão", PP(pkv("new_success") - pkv("base_success")))
    }
    if (nrow(f$shown) >= 2L) addc("razão entre o maior e o segundo maior ganho demonstrado", paste0(N(f$shown$delta_availability[[1]] / f$shown$delta_availability[[2]], 1), " vezes"))
  }
  if (is.data.frame(w) && nrow(w) >= 2L) {
    addc("razão entre o maior e o segundo maior peso de critério", paste0(N(w$peso[[1]] / w$peso[[2]], 1), " vezes"))
    addc("diferença entre o maior e o segundo maior peso de critério", D(w$peso[[1]] - w$peso[[2]]))
  }
  if (is.data.frame(rk) && nrow(rk) >= 2L) addc("diferença de coeficiente TOPSIS entre o 1º e o 2º sistema", format_number_br(rk$coefficient[[1]] - rk$coefficient[[2]], 3))
  if (is.data.frame(ssys) && nrow(ssys) >= 3L) addc("parcela somada dos três sistemas com maior tempo parado simulado", P(sum(ssys$share[1:3])))

  list(
    enquadramento_da_pesquisa = list(
      tema = "Gestão do ciclo de vida da frota de viaturas JLTV do Corpo de Fuzileiros Navais (CFN): manutenção, suprimento e apoio à decisão.",
      questao_de_pesquisa = "Quais sistemas da viatura JLTV e quais intervenções de manutenção e de suprimento devem ser priorizados para sustentar a disponibilidade da frota do CFN?",
      objetivo_geral = "Identificar os sistemas da viatura e as intervenções de manutenção e de suprimento a priorizar para sustentar a disponibilidade da frota JLTV do CFN.",
      metodo = "Questionário a especialistas (AHP para os pesos dos critérios; TOPSIS para ordenar os sistemas) seguido de simulação de eventos discretos da frota, repetida em replicações de Monte Carlo, que estimou a disponibilidade de referência e testou intervenções em comparação pareada (sementes comuns).",
      hipoteses = list(H1 = "As posições do ranking AHP–TOPSIS se mantêm quando mudam a amostra de avaliadores, os pesos ou a normalização.",
                       H2 = "Cada intervenção testada eleva a disponibilidade simulada em relação à situação de referência."),
      modo_de_evidencia = list(codigo = mode, significado = word_report_mode_label(mode)),
      unidade_de_analise = "A viatura JLTV completa; sistemas e componentes explicam causas e intervenções."),
    dados = list(
      missao = missao, resultado_de_referencia = referencia,
      especialistas = list(avaliadores = f$n_resp, pesos_dos_criterios = pesos, pesos_globais_dos_subcriterios = subpesos,
                           consistencia_ahp = consist, maior_razao_de_consistencia = N(f$max_cr, 3), ranking_topsis = ranking,
                           estabilidade_por_reamostragem = estab, sensibilidade_a_pesos_e_normalizacao = sensib),
      registros_da_frota = registros,
      tempo_parado_simulado = processo, tempo_parado_por_sistema = sysx,
      intervencoes = interv, pacote_de_intervencoes = pacote,
      configuracoes_simuladas = configs, parametros_do_simulador = premissas,
      verificacoes_de_confiabilidade = conf),
    comparacoes_calculadas = comp,
    extensao = word_report_ai_word_budget(report)
  )
}

word_report_ai_word_budget = function(report) {
  # O corpo tem limite de 20 páginas e as seções 1 a 3 ocupam cerca de metade dele: cerca de 2.000 palavras deixam espaço para as figuras principais.
  total = 2000L
  share = c(sintese = .07, prioridades = .09, pesos = .06, robustez = .08, processo = .14, melhorias = .15, simulacao = .09, limitacoes = .09, conclusao = .23)
  list(palavras_maximas_no_total_das_chaves_de_analise_e_conclusao = total,
       palavras_sugeridas_por_chave = as.list(stats::setNames(as.integer(round(total * share / 10) * 10), names(share))),
       resumo = "parágrafo único de 160 a 240 palavras")
}

# Números permitidos: todos os que aparecem nos dados enviados (valores e textos), anos das fontes e inteiros pequenos.
word_report_ai_allowed_from_payload = function(payload) {
  leaves = unlist(payload, use.names = FALSE)
  nums = suppressWarnings(as.numeric(leaves[!is.na(suppressWarnings(as.numeric(leaves)))]))
  txt_nums = word_report_numbers_in(as.character(leaves))
  years = suppressWarnings(as.numeric(unlist(lapply(word_report_ref_db(), function(e) sub("[a-z]$", "", e$year)))))
  vals = c(nums, 100 * nums, txt_nums, years, 0:20, 95)
  unique(round(vals[is.finite(vals)], 4))
}

# ---- IA: chamada ao provedor ----
word_report_ai_keys = c("resumo", "abstract", "sintese", "prioridades", "pesos", "robustez",
                        "processo", "melhorias", "simulacao", "limitacoes", "conclusao", "proximos_passos")

# Esquema JSON da resposta: resumo e abstract em texto único; as demais chaves em lista de parágrafos.
word_report_ai_schema = function(keys = word_report_ai_keys) {
  props = lapply(keys, function(k) {
    if (k %in% c("resumo", "abstract")) list(type = "string")
    else list(type = "array", items = list(type = "string"))
  })
  names(props) = keys
  list(type = "object", properties = props, required = as.list(keys), additionalProperties = FALSE)
}

word_report_parse_json = function(text) {
  text = as.character(text %||% "")
  text = if (length(text) == 0L || is.na(text[[1]])) "" else text[[1]]
  text = gsub("^\\s*```(json)?\\s*|\\s*```\\s*$", "", text)
  first = regexpr("\\{", text)[[1]]
  ends = gregexpr("\\}", text)[[1]]
  last = ends[[length(ends)]]
  if (first < 0L || last < first) stop("A IA não devolveu JSON válido", call. = FALSE)
  tryCatch(
    jsonlite::fromJSON(substr(text, first, last), simplifyVector = FALSE),
    error = function(e) stop("A IA devolveu um JSON incompleto ou com erro de formato [", conditionMessage(e), "]", call. = FALSE)
  )
}

# Descreve a resposta recebida (motivo da parada, tipos de bloco e início do texto) para o aviso de erro.
word_report_ai_describe = function(text, stop_reason = "", blocks = character()) {
  text = as.character(text %||% "")
  text = if (length(text) == 0L || is.na(text[[1]])) "" else text[[1]]
  snippet = substr(gsub("\\s+", " ", text), 1L, 160L)
  parts = c(
    if (nzchar(stop_reason %||% "")) paste0("parada: ", stop_reason),
    if (length(blocks) > 0L) paste0("blocos: ", paste(blocks, collapse = ", ")),
    paste0("início da resposta: «", if (nzchar(snippet)) snippet else "vazio", "»")
  )
  paste(parts, collapse = "; ")
}

word_report_http_post = function(url, headers, body, timeout = 300) {
  h = curl::new_handle()
  curl::handle_setopt(h, post = TRUE, postfields = charToRaw(enc2utf8(body)), timeout = timeout, connecttimeout = 30)
  curl::handle_setheaders(h, .list = c(list(`Content-Type` = "application/json"), as.list(headers)))
  res = tryCatch(
    curl::curl_fetch_memory(url, handle = h),
    error = function(e) stop("sem conexão com o provedor de IA (", conditionMessage(e), ")", call. = FALSE)
  )
  text = rawToChar(res$content)
  Encoding(text) = "UTF-8"
  list(status = res$status_code, text = text)
}

word_report_http_error = function(resp, provider) {
  detail = tryCatch({
    body = jsonlite::fromJSON(resp$text, simplifyVector = FALSE)
    body$error$message %||% body$message %||% ""
  }, error = function(e) substr(gsub("\\s+", " ", resp$text), 1L, 200L))
  hint = switch(
    as.character(resp$status),
    "401" = " A chave de API foi recusada: confira se é uma chave de API (não a senha nem a assinatura do chat) e se foi copiada inteira.",
    "403" = " A chave não tem permissão para este modelo ou recurso.",
    "404" = " O modelo informado não existe nesta conta: deixe o campo Modelo vazio ou digite um modelo disponível.",
    "429" = " Limite de uso ou saldo de créditos da API esgotado: confira o saldo em console.anthropic.com (a API tem cobrança separada da assinatura do chat).",
    ""
  )
  paste0("o provedor de IA respondeu HTTP ", resp$status, ": ", detail, ".", hint)
}

# Claude: pede o JSON pelo esquema da API (resposta estruturada), sem raciocínio estendido (que consome o limite de
# tokens da resposta) e com a temperatura baixa. Se o modelo recusar algum desses parâmetros (HTTP 400), tenta de
# novo sem ele; se a resposta for cortada pelo limite de tokens, tenta de novo com o dobro.
word_report_call_claude = function(system_prompt, user_msg, api_key, model, timeout = 900) {
  # Raciocínio adaptativo com esforço alto: a IA analisa os resultados antes de escrever. Se o modelo não aceitar
  # algum parâmetro (HTTP 400), tenta de novo sem ele; o raciocínio conta no limite de tokens da resposta.
  opts = list(thinking = "adaptive", effort = TRUE, temperature = FALSE, schema = TRUE, max_tokens = 32000L)
  schema = word_report_ai_schema()
  for (attempt in seq_len(10L)) {
    body = list(
      model = model, max_tokens = opts$max_tokens,
      system = system_prompt,
      messages = list(list(role = "user", content = user_msg))
    )
    if (opts$temperature) body$temperature = 0.2
    if (identical(opts$thinking, "adaptive")) body$thinking = list(type = "adaptive")
    if (identical(opts$thinking, "disabled")) body$thinking = list(type = "disabled")
    oc = list()
    if (opts$schema) oc$format = list(type = "json_schema", schema = schema)
    if (identical(opts$thinking, "adaptive") && opts$effort) oc$effort = "high"
    if (length(oc)) body$output_config = oc
    resp = word_report_http_post(
      "https://api.anthropic.com/v1/messages",
      list(`x-api-key` = api_key, `anthropic-version` = "2023-06-01"),
      as.character(jsonlite::toJSON(body, auto_unbox = TRUE)), timeout
    )
    if (resp$status == 400L) {
      msg = tolower(resp$text)
      if (identical(opts$thinking, "adaptive") && opts$effort && grepl("effort", msg, fixed = TRUE)) { opts$effort = FALSE; next }
      if (identical(opts$thinking, "adaptive") && grepl("thinking|adaptive", msg)) { opts$thinking = "disabled"; opts$temperature = TRUE; next }
      if (opts$temperature && grepl("temperature", msg, fixed = TRUE)) { opts$temperature = FALSE; next }
      if (identical(opts$thinking, "disabled") && grepl("thinking", msg, fixed = TRUE)) { opts$thinking = "none"; next }
      if (opts$schema && grepl("output_config|json_schema|schema|format", msg)) { opts$schema = FALSE; next }
    }
    if (resp$status != 200L) stop(word_report_http_error(resp, "claude"), call. = FALSE)
    parsed = tryCatch(jsonlite::fromJSON(resp$text, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(parsed)) stop("A resposta do provedor de IA não pôde ser lida (", word_report_ai_describe(resp$text), ").", call. = FALSE)
    blocks = parsed$content %||% list()
    types = vapply(blocks, function(x) as.character(x$type %||% ""), character(1))
    texts = vapply(blocks, function(x) {
      if (as.character(x$type %||% "") %in% c("thinking", "redacted_thinking")) "" else as.character(x$text %||% "")
    }, character(1))
    text = paste(texts, collapse = "")
    stop_reason = as.character(parsed$stop_reason %||% "")
    info = word_report_ai_describe(text, stop_reason, types)
    if (identical(stop_reason, "max_tokens")) {
      if (opts$max_tokens < 64000L) { opts$max_tokens = 64000L; next }
      stop("A resposta da IA foi cortada pelo limite de tamanho (", info, ").", call. = FALSE)
    }
    if (identical(stop_reason, "refusal")) stop("A IA recusou-se a responder (", info, ").", call. = FALSE)
    if (!nzchar(trimws(text))) stop("A IA devolveu resposta vazia (", info, ").", call. = FALSE)
    return(list(text = text, info = info))
  }
  stop("A IA não devolveu uma resposta utilizável após novas tentativas.", call. = FALSE)
}

# ChatGPT: modo JSON; se o modelo recusar a temperatura ou o modo JSON (HTTP 400), tenta de novo sem eles.
word_report_call_openai = function(system_prompt, user_msg, api_key, model, timeout = 300) {
  opts = list(temperature = TRUE, json_mode = TRUE)
  for (attempt in seq_len(5L)) {
    body = list(
      model = model,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = user_msg)
      )
    )
    if (opts$temperature) body$temperature = 0.2
    if (opts$json_mode) body$response_format = list(type = "json_object")
    resp = word_report_http_post(
      "https://api.openai.com/v1/chat/completions",
      list(Authorization = paste("Bearer", api_key)),
      as.character(jsonlite::toJSON(body, auto_unbox = TRUE)), timeout
    )
    if (resp$status == 400L) {
      msg = tolower(resp$text)
      if (opts$temperature && grepl("temperature", msg, fixed = TRUE)) { opts$temperature = FALSE; next }
      if (opts$json_mode && grepl("response_format|json", msg)) { opts$json_mode = FALSE; next }
    }
    if (resp$status != 200L) stop(word_report_http_error(resp, "chatgpt"), call. = FALSE)
    parsed = tryCatch(jsonlite::fromJSON(resp$text, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(parsed)) stop("A resposta do provedor de IA não pôde ser lida (", word_report_ai_describe(resp$text), ").", call. = FALSE)
    choices = parsed$choices %||% list()
    choice = if (length(choices) > 0L) choices[[1]] else list()
    text = as.character(choice$message$content %||% "")
    text = if (length(text) == 0L) "" else paste(text, collapse = "")
    finish = as.character(choice$finish_reason %||% "")
    info = word_report_ai_describe(text, finish)
    if (identical(finish, "length")) stop("A resposta da IA foi cortada pelo limite de tamanho (", info, ").", call. = FALSE)
    if (!nzchar(trimws(text))) stop("A IA devolveu resposta vazia (", info, ").", call. = FALSE)
    return(list(text = text, info = info))
  }
  stop("A IA não devolveu uma resposta utilizável após novas tentativas.", call. = FALSE)
}

word_report_call_ai = function(facts, provider, api_key, model = NULL, timeout = 900, previous = NULL, issues = NULL) {
  api_key = gsub("^[\"']+|[\"']+$", "", trimws(api_key %||% ""))
  if (!nzchar(api_key)) stop("Informe a chave da API (ou defina a variável de ambiente).", call. = FALSE)
  payload = as.character(jsonlite::toJSON(facts, auto_unbox = TRUE, na = "null", null = "null", digits = 6, dataframe = "rows"))
  user_msg = paste0("Enquadramento da pesquisa e resultados completos desta execução do simulador (JSON):\n", payload,
                    "\n\nAnalise os resultados e escreva as chaves pedidas, conforme as instruções.")
  if (!is.null(previous)) {
    prev = as.character(jsonlite::toJSON(previous, auto_unbox = TRUE, null = "null"))
    user_msg = paste0(user_msg, "\n\nSUA VERSÃO ANTERIOR (JSON):\n", prev,
                      "\n\nA conferência automática encontrou os problemas abaixo. Corrija-os e devolva o JSON completo, com todas as chaves; mantenha a análise e o texto que estavam corretos.\n- ",
                      paste(issues, collapse = "\n- "))
  }
  db = word_report_ref_db()
  system_prompt = word_report_ai_system_prompt(db)
  if (provider == "claude") {
    model = if (nzchar(model %||% "")) model else Sys.getenv("JLTV_CLAUDE_MODEL", "claude-opus-5-5")
    got = word_report_call_claude(system_prompt, user_msg, api_key, model, timeout)
  } else if (provider == "chatgpt") {
    model = if (nzchar(model %||% "")) model else Sys.getenv("JLTV_OPENAI_MODEL", "gpt-4o")
    got = word_report_call_openai(system_prompt, user_msg, api_key, model, min(timeout, 600))
  } else stop("Provedor de IA desconhecido.", call. = FALSE)
  parsed = tryCatch(
    word_report_parse_json(got$text),
    error = function(e) stop(conditionMessage(e), " (", got$info, ")", call. = FALSE)
  )
  word_report_ai_postprocess(parsed, db)
}

# Saneamento da resposta: só as chaves do relatório, citações válidas, sem citação na conclusão e no resumo.
word_report_ai_postprocess = function(parsed, db = word_report_ref_db()) {
  parsed = parsed[names(parsed) %in% word_report_ai_keys]
  out = lapply(parsed, function(x) as.character(unlist(x)))
  for (name in names(out)) {
    out[[name]] = word_report_ai_clean_text(out[[name]], db, allow_cites = !name %in% c("resumo", "abstract", "conclusao", "proximos_passos"))
    out[[name]] = word_report_ai_sanitize(out[[name]])
    out[[name]] = out[[name]][nzchar(out[[name]])]
  }
  out
}



# Resolve o texto: IA quando pedida e possível; caso contrário, local (com aviso).
word_report_word_count = function(x) {
  x = trimws(paste(x, collapse = " "))
  if (!nzchar(x)) return(0L)
  length(strsplit(x, "[[:space:]]+")[[1]])
}

# Retém discussão adicional; frases que parafraseiam o núcleo entram uma única vez.
word_report_additional_discussion = function(core, candidate, max_words=80L) {
  sentences=function(x) unlist(strsplit(paste(x,collapse=" "),"(?<=[.!?])\\s+",perl=TRUE),use.names=FALSE)
  tokens=function(x) {
    x=tolower(iconv(gsub("\\{[^}]*\\}","",x),to="ASCII//TRANSLIT"))
    w=unique(strsplit(gsub("[^a-z0-9]+"," ",x),"[[:space:]]+")[[1]])
    w[nchar(w)>=4 | grepl("^[0-9]+$",w)]
  }
  known=lapply(sentences(core),tokens)
  out=character(); used=0L
  for(s in sentences(candidate)) {
    words=tokens(s)
    repeated=length(words)>=6L && any(vapply(known,function(k) {
      length(k)>=6L && sum(words %in% k)/length(words)>=.6
    },logical(1)))
    n=word_report_word_count(s)
    if(nzchar(trimws(s)) && !repeated && used+n<=max_words) {
      out=c(out,s);used=used+n;known=c(known,list(words))
    }
  }
  if(length(out))paste(out,collapse=" ") else character()
}

# Texto que contradiz a situação da missão (ex.: "não cumpre" com a probabilidade acima do alvo).
word_report_ai_mission_contradiction = function(text, report) {
  txt = paste(text, collapse = " ")
  f = word_report_findings(report)
  if (!isTRUE(f$mission_known)) return(FALSE)
  wrong = if (isTRUE(f$mission_ok)) "não (assegura|garante|alcança|atinge|cumpre) a (prontidão|missão|exigência|probabilidade-alvo)|abaixo da probabilidade-alvo de referência|frota não cumpre"
          else "(frota|referência) (cumpre|atende|alcança|supera) (a|à) (exigência|probabilidade-alvo)|acima da probabilidade-alvo"
  grepl(wrong, txt, ignore.case = TRUE, perl = TRUE) && !grepl("pacote não alcançou|abaixo de [0-9]+ viaturas|com frota menor|com uma viatura a menos|com [0-9]+ viaturas", txt, ignore.case = TRUE)
}

# Conferência da conclusão: responde à questão (sistema prioritário), retoma a disponibilidade de referência,
# propõe ação e não contradiz a missão.
word_report_ai_conclusion_problems = function(text, report) {
  txt = paste(text, collapse = " ")
  out = character()
  if (!nzchar(trimws(txt))) return("A conclusão veio vazia.")
  if (grepl(word_report_ai_meta_regex, txt, perl = TRUE)) out = c(out, "A conclusão menciona a estrutura dos dados.")
  f = word_report_findings(report)
  norm = function(x) tolower(gsub("[[:space:]]+", " ", x))
  if (is.data.frame(f$ranking) && nrow(f$ranking) > 0L) {
    names_ok = c(f$ranking$alternative_name[[1]], as.character(report$assessment$bottlenecks$alternative_name[1] %||% ""))
    names_ok = names_ok[nzchar(names_ok)]
    if (length(names_ok) && !any(vapply(names_ok, function(n) grepl(norm(n), norm(txt), fixed = TRUE), logical(1))))
      out = c(out, paste0("A conclusão não nomeia o sistema de maior prioridade (", names_ok[[1]], ")."))
  }
  nums = word_report_numbers_in(txt)
  if (word_report_num(f$ao_ref) && !any(abs(nums - round(100 * f$ao_ref, 1)) <= 0.051))
    out = c(out, paste0("A conclusão não retoma a disponibilidade de referência (", format_percent_br(f$ao_ref), ")."))
  if (is.data.frame(f$main) && nrow(f$main) > 0L && !grepl("propõe|proposta|prioriz|recomenda", txt, ignore.case = TRUE))
    out = c(out, "A conclusão não formula a proposta de ação.")
  if (word_report_ai_mission_contradiction(txt, report))
    out = c(out, "A conclusão contradiz a situação da missão frente à probabilidade-alvo.")
  out
}

# Mantida para compatibilidade: verdadeiro quando a conclusão não tem problema.
word_report_ai_conclusion_supported = function(text, report) length(word_report_ai_conclusion_problems(text, report)) == 0L

# Problemas da resposta da IA, descritos para um pedido de correção.
word_report_ai_issues = function(result, allowed, report, budget) {
  out = character()
  label = c(sintese = "sintese (4.1)", prioridades = "prioridades (4.2)", pesos = "pesos (4.3)", robustez = "robustez (4.4)",
            processo = "processo (4.5)", melhorias = "melhorias (4.6)", simulacao = "simulacao (4.7)", limitacoes = "limitacoes (4.8)", conclusao = "conclusao")
  total = 0L
  for (name in word_report_ai_text_keys) {
    cand = as.character(unlist(result[[name]] %||% character()))
    cand = cand[nzchar(trimws(cand))]
    if (!length(cand)) { out = c(out, paste0("A chave ", label[[name]], " veio vazia.")); next }
    total = total + word_report_word_count(cand)
    for (i in seq_along(cand)) {
      nums = word_report_numbers_in(cand[[i]])
      nums = unique(nums[is.finite(nums)])
      bad = nums[!vapply(nums, function(v) any(abs(allowed - v) <= 0.051), logical(1))]
      if (length(bad)) out = c(out, paste0("Na chave ", label[[name]], ", parágrafo ", i, ", os números ",
        paste(format_number_br_vec(bad), collapse = "; "), " não constam dos dados: reescreva o parágrafo usando apenas números dos dados ou de comparacoes_calculadas, ou descreva a comparação em palavras."))
    }
    if (name != "conclusao" && any(vapply(cand, function(p) word_report_ai_mission_contradiction(p, report), logical(1))))
      out = c(out, paste0("Na chave ", label[[name]], ", o texto contradiz a situação da missão frente à probabilidade-alvo."))
  }
  out = c(out, word_report_ai_conclusion_problems(result$conclusao, report))
  maxw = suppressWarnings(as.integer(budget$palavras_maximas_no_total_das_chaves_de_analise_e_conclusao %||% NA))
  if (!is.na(maxw) && total > 1.1 * maxw)
    out = c(out, paste0("O texto das chaves de análise e conclusão tem ", total, " palavras; reduza para no máximo ", maxw, ", cortando repetições e não interpretações."))
  for (name in c("resumo", "abstract")) {
    n = word_report_word_count(paste(result[[name]], collapse = " "))
    if (n < 150L || n > 250L) out = c(out, paste0("O ", name, " tem ", n, " palavras; deve ter de 160 a 240 palavras, em parágrafo único."))
  }
  unique(out)
}

format_number_br_vec = function(x) vapply(x, function(v) format_number_br(v, if (abs(v - round(v)) < 1e-9) 0 else 1), character(1))

word_report_resolve_narrative = function(report, provider = "local", api_key = "", model = NULL) {
  local = word_report_narrative_local(report)
  abstracts = word_report_resumo_local(report)
  local$resumo = abstracts$pt
  local$abstract = abstracts$en
  if (!provider %in% c("claude", "chatgpt")) {
    return(list(text = local, source = "local", note = NULL, model = NULL, replaced = character()))
  }
  key = if (nzchar(api_key %||% "")) api_key else
    Sys.getenv(if (provider == "claude") "ANTHROPIC_API_KEY" else "OPENAI_API_KEY", "")
  if (!nzchar(model %||% "")) model = if (provider == "claude") Sys.getenv("JLTV_CLAUDE_MODEL", "claude-opus-5-5") else Sys.getenv("JLTV_OPENAI_MODEL", "gpt-4o")
  fallback = function(msg) list(text = local, source = "local", note = msg, model = NULL, replaced = character())
  payload = tryCatch(word_report_ai_dataset(report), error = function(e) e)
  if (inherits(payload, "error")) return(fallback(paste0("Os dados para a IA não puderam ser montados (", conditionMessage(payload), "). O texto abaixo é o padrão do aplicativo.")))
  allowed = unique(c(word_report_ai_allowed_from_payload(payload), round(word_report_numbers_in(unlist(local, use.names = FALSE)), 4)))
  result = tryCatch(word_report_call_ai(payload, provider, key, model), error = function(e) e)
  if (inherits(result, "error")) return(fallback(paste0("A IA não respondeu (", conditionMessage(result), "). O texto abaixo é o padrão do aplicativo.")))

  # Uma rodada de correção: a IA recebe a própria resposta e a lista do que não passou na conferência.
  issues = word_report_ai_issues(result, allowed, report, payload$extensao)
  revised = FALSE
  if (length(issues) > 0L) {
    second = tryCatch(word_report_call_ai(payload, provider, key, model, previous = result, issues = issues), error = function(e) e)
    if (!inherits(second, "error")) {
      for (nm in names(second)) if (length(second[[nm]]) > 0L) result[[nm]] = second[[nm]]
      revised = TRUE
    }
  }

  merged = local
  notes = character()
  dropped_total = 0L
  replaced = character()
  for (name in setdiff(word_report_ai_text_keys, "conclusao")) {
    cand = as.character(unlist(result[[name]] %||% character()))
    cand = cand[nzchar(trimws(cand))]
    if (!length(cand)) next
    ok = vapply(cand, word_report_ai_numbers_ok, logical(1), allowed = allowed) &
      !vapply(cand, function(p) word_report_ai_mission_contradiction(p, report), logical(1))
    dropped_total = dropped_total + sum(!ok)
    cand = cand[ok]
    if (!length(cand)) next
    base = as.character(unlist(local[[name]]))
    if (word_report_word_count(base) > 0L && word_report_word_count(cand) < 0.5 * word_report_word_count(base)) {
      # Resposta curta demais para substituir a subseção: o texto do aplicativo fica e a contribuição da IA é acrescentada.
      merged[[name]] = unique(c(base, cand))
    } else {
      merged[[name]] = unname(cand)
      replaced = c(replaced, name)
    }
  }
  # Limite de extensão: se a IA passou do total, saem os últimos parágrafos das subseções mais longas.
  maxw = suppressWarnings(as.integer(payload$extensao$palavras_maximas_no_total_das_chaves_de_analise_e_conclusao %||% NA))
  concl = as.character(unlist(result$conclusao %||% character()))
  concl = concl[nzchar(trimws(concl))]
  concl_ok = length(concl) > 0L && all(vapply(concl, word_report_ai_numbers_ok, logical(1), allowed = allowed)) &&
    word_report_ai_conclusion_supported(concl, report)
  merged$conclusao = if (concl_ok) unname(concl) else local$conclusao
  if (concl_ok) replaced = c(replaced, "conclusao")
  if (!is.na(maxw)) {
    words = function() sum(vapply(word_report_ai_text_keys, function(k) word_report_word_count(merged[[k]]), integer(1)))
    trimmed = 0L
    while (words() > 1.2 * maxw) {
      keys = intersect(setdiff(replaced, c("conclusao", "resumo", "abstract")), word_report_ai_text_keys)
      keys = keys[vapply(keys, function(k) length(merged[[k]]) > 1L, logical(1))]
      if (!length(keys)) break
      k = keys[which.max(vapply(keys, function(k) word_report_word_count(merged[[k]]), integer(1)))]
      merged[[k]] = merged[[k]][-length(merged[[k]])]
      trimmed = trimmed + 1L
    }
    if (trimmed > 0L) notes = c(notes, paste0(trimmed, " parágrafo(s) final(is) da análise foram retirados para respeitar o limite de páginas."))
  }
  merged$proximos_passos = character()
  if (dropped_total > 0L) notes = c(notes, paste0(dropped_total, " parágrafo(s) da IA com números que não constam dos resultados foram descartados."))
  if (!concl_ok && length(concl)) notes = c(notes, "A conclusão da IA não passou na conferência dos resultados (números, sistema prioritário, proposta ou situação da missão); valeu a conclusão do aplicativo.")
  for (name in c("resumo", "abstract")) {
    candidate = paste(result[[name]], collapse = " ")
    n_words = word_report_word_count(candidate)
    if (n_words >= 140L && n_words <= 260L && word_report_ai_numbers_ok(candidate, allowed) && !word_report_ai_mission_contradiction(candidate, report)) {
      merged[[name]] = word_report_ai_clean_text(candidate, word_report_ref_db(), allow_cites = FALSE); replaced = c(replaced, name)
    } else if (nzchar(candidate)) notes = c(notes, paste0("O ", name, " da IA (", n_words, " palavras) foi substituído pelo texto do aplicativo."))
  }
  if (revised) notes = c(notes, "A primeira versão da IA passou por uma rodada de correção após a conferência automática.")
  list(text = merged, source = provider,
       note = if (length(notes) > 0L) paste(notes, collapse = " ") else NULL,
       model = model, replaced = unique(replaced), issues = issues)
}


# ---- 4. Camada OpenXML no padrão do Manual da ESG (Ensaio Acadêmico) ----------
# A64 retrato 21 x 29,7 cm; margens 3 (esq., sup.) e 2 cm (dir., inf.); Arial 12;
# espaçamento 1,5; recuo de 1,5 cm na 1ª linha; parágrafos sem espaço antes/depois;
# fonte, notas e conteúdo de tabelas em Arial 10; número da página só a partir da
# Introdução, no canto superior direito (2 cm das bordas), em Arial 10.
wx_ns = "xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\""

wx_esc = function(x) {
  x = enc2utf8(as.character(x))
  x = gsub("[\\x01-\\x08\\x0B\\x0C\\x0E-\\x1F]", "", x, perl = TRUE)
  x = gsub("&", "&amp;", x, fixed = TRUE)
  x = gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

wx_int = function(x) sprintf("%d", as.integer(round(x)))
wx_cm = function(x) wx_int(x * 567)

wx_run = function(text, size = 12, bold = FALSE, italic = FALSE) {
  rpr = paste0(
    "<w:rFonts w:ascii=\"Arial\" w:hAnsi=\"Arial\" w:cs=\"Arial\" w:eastAsia=\"Arial\"/>",
    if (bold) "<w:b/><w:bCs/>" else "",
    if (italic) "<w:i/><w:iCs/>" else "",
    "<w:color w:val=\"000000\"/><w:sz w:val=\"", wx_int(size * 2), "\"/><w:szCs w:val=\"", wx_int(size * 2), "\"/>",
    "<w:lang w:val=\"pt-BR\"/>"
  )
  paste0("<w:r><w:rPr>", rpr, "</w:rPr><w:t xml:space=\"preserve\">", wx_esc(text), "</w:t></w:r>")
}

# Texto com **negrito** inline.
wx_marked = function(text, size = 12) {
  parts = strsplit(as.character(text), "**", fixed = TRUE)[[1]]
  if (length(parts) == 0L) return("")
  paste0(vapply(seq_along(parts), function(i) {
    if (!nzchar(parts[[i]])) "" else wx_run(parts[[i]], size, bold = (i %% 2L == 0L))
  }, character(1)), collapse = "")
}

wx_par = function(runs = "", align = "both", line = 240, rule = "auto", before = 0, after = 0,
                  first = 0, left = 0, keep = FALSE, page_break = FALSE, outline = NA, msize = 12,
                  sect = "") {
  ind = if (first < 0) {
    paste0("<w:ind w:left=\"", wx_cm(left), "\" w:right=\"0\" w:hanging=\"", wx_cm(-first), "\"/>")
  } else {
    paste0("<w:ind w:left=\"", wx_cm(left), "\" w:right=\"0\" w:firstLine=\"", wx_cm(first), "\"/>")
  }
  ppr = paste0(
    if (keep) "<w:keepNext/>" else "",
    if (page_break) "<w:pageBreakBefore/>" else "",
    "<w:spacing w:before=\"", wx_int(before), "\" w:after=\"", wx_int(after),
    "\" w:line=\"", wx_int(line), "\" w:lineRule=\"", rule, "\"/>",
    ind,
    "<w:jc w:val=\"", align, "\"/>",
    if (!is.na(outline)) paste0("<w:outlineLvl w:val=\"", wx_int(outline), "\"/>") else "",
    "<w:rPr><w:rFonts w:ascii=\"Arial\" w:hAnsi=\"Arial\" w:cs=\"Arial\"/><w:sz w:val=\"", wx_int(msize * 2),
    "\"/><w:szCs w:val=\"", wx_int(msize * 2), "\"/></w:rPr>",
    sect
  )
  paste0("<w:p ", wx_ns, "><w:pPr>", ppr, "</w:pPr>", runs, "</w:p>")
}

# n linhas em branco da fonte indicada (altura exata = corpo da fonte).
wx_blank = function(n, size = 12) {
  if (n <= 0L) return(character())
  rep(wx_par("", align = "left", line = size * 20, rule = "exact", msize = size), n)
}

# Quebra de seção (próxima página); `start` reinicia a contagem de páginas.
wx_sect = function(start = NA) {
  paste0(
    "<w:sectPr><w:type w:val=\"nextPage\"/>",
    "<w:pgSz w:w=\"11906\" w:h=\"16838\"/>",
    "<w:pgMar w:top=\"1701\" w:right=\"1134\" w:bottom=\"1134\" w:left=\"1701\" w:header=\"1134\" w:footer=\"709\" w:gutter=\"0\"/>",
    if (!is.na(start)) paste0("<w:pgNumType w:start=\"", wx_int(start), "\"/>") else "",
    "</w:sectPr>"
  )
}

wx_is_number = function(x) grepl("^[-+−]?[0-9][0-9.,]*( ?%| ?pp)?$", trimws(x))

# Larguras (em twips) das colunas de uma tabela de 16 cm, proporcionais ao conteúdo.
wx_table_widths = function(data) {
  data = as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  nc = ncol(data)
  total = 9071L # 16 cm
  weights = vapply(seq_len(nc), function(j) {
    v = as.character(data[[j]])
    v[is.na(v)] = "n/d"
    texts = c(names(data)[[j]], v)
    longest_word = max(nchar(unlist(strsplit(texts, "[[:space:]]+"))), 1L)
    typical = as.numeric(stats::quantile(nchar(texts), 0.9, type = 1))
    max(min(longest_word, 16) + 1.5, min(42, typical), 6)
  }, numeric(1))
  widths = total * weights / sum(weights)
  min_w = vapply(seq_len(nc), function(j) {
    v = as.character(data[[j]])
    v[is.na(v)] = "n/d"
    head_word = max(nchar(unlist(strsplit(names(data)[[j]], "[[:space:]]+"))), 1L)
    body_word = max(nchar(unlist(strsplit(v, "[[:space:]]+"))), 1L)
    (max(head_word * 0.22, min(body_word, 14) * 0.19) + 0.3) * 567
  }, numeric(1))
  short = widths < min_w
  if (any(short) && sum(min_w[short]) < 0.6 * total) {
    widths[short] = min_w[short]
    free = which(!short)
    if (length(free) > 0L) widths[free] = (total - sum(widths[short])) * weights[free] / sum(weights[free])
  }
  widths = as.integer(round(widths))
  widths[[nc]] = total - sum(widths[-nc])
  widths
}

# Altura estimada (cm) de uma tabela em Arial 10: cada linha ocupa o maior número de linhas de texto das células.
wx_table_height_cm = function(data) {
  data = as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(data) == 0L || ncol(data) == 0L) return(0)
  widths = wx_table_widths(data)
  chars_per_line = pmax(3, floor((widths / 567 - 0.25) / 0.19))
  row_lines = function(values) {
    values = as.character(values)
    values[is.na(values)] = "n/d"
    max(1, ceiling(nchar(values) / chars_per_line))
  }
  header = row_lines(names(data))
  body = vapply(seq_len(nrow(data)), function(i) row_lines(vapply(data, function(col) as.character(col)[[i]], character(1))), numeric(1))
  (header + sum(body)) * 0.43 + (nrow(data) + 1L) * 0.15
}

# Tabela (dados numéricos, só filetes horizontais) ou quadro (informação qualitativa, grade fechada).
wx_table = function(data, kind = "tabela", size = 10) {
  data = as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  nc = ncol(data)
  cells = lapply(seq_len(nc), function(j) {
    v = as.character(data[[j]])
    v[is.na(v)] = "n/d"
    num = grepl("^[-+]?[0-9]+\\.[0-9]+$", v)
    v[num] = sub(".", ",", v[num], fixed = TRUE)
    v
  })
  total = 9071L # 16 cm
  widths = wx_table_widths(data)

  line = function(sz) paste0("w:val=\"single\" w:sz=\"", sz, "\" w:space=\"0\" w:color=\"000000\"")
  none = "w:val=\"nil\""
  borders = if (identical(kind, "quadro")) {
    paste0("<w:tblBorders>",
           "<w:top ", line(4), "/><w:left ", line(4), "/><w:bottom ", line(4), "/><w:right ", line(4), "/>",
           "<w:insideH ", line(4), "/><w:insideV ", line(4), "/></w:tblBorders>")
  } else {
    paste0("<w:tblBorders>",
           "<w:top ", line(8), "/><w:left ", none, "/><w:bottom ", line(8), "/><w:right ", none, "/>",
           "<w:insideH ", none, "/><w:insideV ", none, "/></w:tblBorders>")
  }
  header_border = if (identical(kind, "quadro")) "" else paste0("<w:tcBorders><w:bottom ", line(6), "/></w:tcBorders>")

  cell_par = function(text, align, bold) {
    wx_par(wx_run(text, size, bold = bold), align = align, line = 240, before = 20, after = 20,
           msize = size)
  }
  row_xml = function(values, header = FALSE) {
    tcs = vapply(seq_len(nc), function(j) {
      align = if (header) "center" else if (wx_is_number(values[[j]])) "center" else "left"
      paste0(
        "<w:tc><w:tcPr><w:tcW w:w=\"", widths[[j]], "\" w:type=\"dxa\"/>",
        if (header) header_border else "",
        "<w:vAlign w:val=\"center\"/></w:tcPr>",
        cell_par(values[[j]], align, header), "</w:tc>"
      )
    }, character(1))
    paste0("<w:tr><w:trPr><w:cantSplit/>", if (header) "<w:tblHeader/>" else "", "</w:trPr>",
           paste0(tcs, collapse = ""), "</w:tr>")
  }
  rows = vapply(seq_len(nrow(data)), function(i) row_xml(vapply(cells, function(col) col[[i]], character(1))), character(1))
  paste0(
    "<w:tbl ", wx_ns, "><w:tblPr><w:tblW w:w=\"", total, "\" w:type=\"dxa\"/><w:jc w:val=\"center\"/>",
    borders, "<w:tblLayout w:type=\"fixed\"/>",
    "<w:tblCellMar><w:top w:w=\"20\" w:type=\"dxa\"/><w:left w:w=\"70\" w:type=\"dxa\"/>",
    "<w:bottom w:w=\"20\" w:type=\"dxa\"/><w:right w:w=\"70\" w:type=\"dxa\"/></w:tblCellMar></w:tblPr>",
    "<w:tblGrid>", paste0("<w:gridCol w:w=\"", widths, "\"/>", collapse = ""), "</w:tblGrid>",
    row_xml(names(data), header = TRUE), paste0(rows, collapse = ""), "</w:tbl>"
  )
}

# Glossário: tabela de coluna única, sem bordas, 16 cm de largura.
wx_glossary_table = function(terms) {
  title = wx_par(wx_run("GLOSSÁRIO", 12, bold = TRUE), align = "center", line = 360, after = 240)
  rows = vapply(terms, function(x) wx_par(wx_marked(x, 12), align = "both", line = 240, after = 240), character(1))
  cell = function(inner) paste0("<w:tr><w:tc><w:tcPr><w:tcW w:w=\"9071\" w:type=\"dxa\"/></w:tcPr>", inner, "</w:tc></w:tr>")
  paste0(
    "<w:tbl ", wx_ns, "><w:tblPr><w:tblW w:w=\"9071\" w:type=\"dxa\"/><w:jc w:val=\"left\"/>",
    "<w:tblBorders><w:top w:val=\"nil\"/><w:left w:val=\"nil\"/><w:bottom w:val=\"nil\"/><w:right w:val=\"nil\"/>",
    "<w:insideH w:val=\"nil\"/><w:insideV w:val=\"nil\"/></w:tblBorders><w:tblLayout w:type=\"fixed\"/>",
    "<w:tblCellMar><w:left w:w=\"0\" w:type=\"dxa\"/><w:right w:w=\"0\" w:type=\"dxa\"/></w:tblCellMar></w:tblPr>",
    "<w:tblGrid><w:gridCol w:w=\"9071\"/></w:tblGrid>",
    cell(title), paste0(vapply(rows, cell, character(1)), collapse = ""), "</w:tbl>"
  )
}

# Dados da folha de rosto, com valores padrão do Manual quando o usuário não informar.
word_report_meta = function(meta = list()) {
  get = function(name, default) {
    v = meta[[name]]
    v = if (is.null(v)) "" else trimws(as.character(v)[1])
    if (is.na(v) || !nzchar(v)) default else v
  }
  list(
    author = get("author", "[NOME DO AUTOR]"),
    advisor = get("advisor", "[titulação e nome completo do orientador]"),
    title = get("title", "GESTÃO DO CICLO DE VIDA DA FROTA JLTV NO CORPO DE FUZILEIROS NAVAIS"),
    subtitle = get("subtitle", "uma abordagem integrada de manutenção, suprimento e apoio à decisão"),
    course = get("course", "[CURSO POR EXTENSO]"),
    nature = get("nature", "Artigo"),
    sector = get("sector", "Instituto Therezinha de Castro"),
    city = get("city", "Rio de Janeiro"),
    year = get("year", format(Sys.Date(), "%Y")),
    max_pages = {
      v = suppressWarnings(as.numeric(get("max_pages", "20")))
      if (is.na(v)) 20 else min(max(v, 12), 20)
    }
  )
}

# ---- 5. Documento Word -----------------------------------------------------
word_report_glossary = function() {
  c(
    "AHP - método de apoio à decisão que obtém pesos de critérios por comparações pareadas e verifica a consistência dos julgamentos.",
    "ALI - Apoio Logístico Integrado: conjunto de elementos de apoio (manutenção, suprimento, pessoal, treinamento, infraestrutura, equipamentos de apoio e de teste, dados técnicos e engenharia) que sustenta o sistema ao longo do ciclo de vida.",
    "BOOTSTRAP (REAMOSTRAGEM) - procedimento que repete o cálculo com amostras sorteadas, com reposição, dos avaliadores, para medir a estabilidade de um resultado.",
    "CFN - Corpo de Fuzileiros Navais.",
    "COEFICIENTE DE PROXIMIDADE - valor entre 0 e 1 calculado pelo TOPSIS; quanto maior, mais bem posicionada a alternativa no ordenamento.",
    "DEPENDABILIDADE - conjunto formado pela confiabilidade, pela manutenibilidade e pela disponibilidade de um sistema.",
    "DISPONIBILIDADE INTRÍNSECA (Ai) E ALCANÇADA (Aa) - frações do tempo em que o item está pronto, considerada apenas a manutenção corretiva ativa (Ai) ou também a manutenção preventiva (Aa).",
    "DISPONIBILIDADE OPERACIONAL (Ao) - fração do tempo em que as viaturas estão prontas para uso, considerados o reparo, a manutenção preventiva e os atrasos administrativos e logísticos.",
    "FALHA - término da capacidade de um item desempenhar a função requerida.",
    "GCV - Gestão do Ciclo de Vida: gestão do sistema de defesa em todas as suas fases, da concepção ao desfazimento.",
    "INTERVALO DE CONFIANÇA DE 95% (IC95%) - faixa de valores que contém o parâmetro estimado com a confiança indicada, nas condições do modelo.",
    "JLTV - Joint Light Tactical Vehicle: viatura tática leve desenvolvida em programa conjunto do Exército e do Corpo de Fuzileiros Navais dos Estados Unidos da América.",
    "LEAD TIME - tempo entre a colocação de uma demanda e o recebimento do material demandado por quem o requisitou.",
    "MALDT, MCMT, MPMT E MTBM - tempo médio de atraso administrativo e logístico (MALDT), tempos médios de manutenção corretiva (MCMT) e preventiva (MPMT) e tempo médio entre manutenções (MTBM), usados na disponibilidade operacional.",
    "MODO DE EVIDÊNCIA - classificação da origem dos números: SYNTHETIC (dados de demonstração), HYBRID (registros reais combinados a hipóteses) ou EMPIRICAL (registros reais da frota).",
    "MODO DE FALHA - forma como um item deixa de desempenhar a função requerida.",
    "MONTE CARLO (SIMULAÇÃO DE) - repetição, muitas vezes, de um sorteio independente do tempo para estimar uma grandeza e seu intervalo de confiança; no simulador, repete a simulação de eventos discretos em replicações.",
    "MTBF - tempo médio entre falhas: média do tempo de funcionamento entre duas falhas consecutivas de um item reparável.",
    "MTTF - tempo médio até a falha: média do tempo correto de operação até a falha.",
    "MTTR - tempo médio de reparo: média do tempo necessário para reparar uma falha.",
    "NSN - NATO Stock Number: número de identificação padronizada de item de suprimento.",
    "ORDEM DE SERVIÇO (OS) - registro de uma intervenção de manutenção, com datas de detecção e de liberação, tipo de manutenção e indicação de falha funcional.",
    "RAM - sigla do inglês reliability, availability and maintainability: confiabilidade, disponibilidade e manutenibilidade.",
    "RAZÃO DE CONSISTÊNCIA (RC) - medida da coerência das comparações pareadas do AHP; valores abaixo de 0,10 indicam consistência aceitável.",
    "SIMULAÇÃO DE EVENTOS DISCRETOS - técnica que reproduz, em computador, a sequência de falhas, esperas e reparos de um sistema ao longo do tempo.",
    "TOPSIS - sigla do inglês Technique for Order of Preference by Similarity to Ideal Solution: técnica que ordena alternativas pela proximidade a uma solução ideal."
  )
}

# ---- 6. Fontes citadas (sistema autor-data) --------------------------------------
# Cada fonte tem: forma narrativa ("Lacey e Hockley (2018)"), forma entre parênteses
# ("(Lacey; Hockley, 2018)"), ano (com letra quando há homônimos), chave de ordenação
# alfabética e a referência completa (ABNT). Só entram na lista as fontes citadas no texto.
# Marcadores no texto: {c:chave} = citação narrativa; {p:chave1,chave2} = citação entre parênteses.
word_report_ref_db = function(year = format(Sys.Date(), "%Y"), version = "") {
  r = function(narr, par, yr, sort, ref) list(narr = narr, par = par, year = yr, sort = sort, ref = ref)
  list(
    abreu = r("Abreu", "Abreu", "2022", "ABREU",
      "ABREU, Tiago Rocha de. Principais conceitos na implantação da sistemática de manutenção nas indústrias. **Research, Society and Development**, [S. l.], v. 11, n. 1, e23911124652, 2022. DOI: 10.33448/rsd-v11i1.24652."),
    smithpt = r("Smith e Powell-Turner", "Smith; Powell-Turner", "2018", "SMITH 2",
      "SMITH, Jeremy C. D.; POWELL-TURNER, Julieanna. Defence logistics in context. In: SMITH, Jeremy C. D. (ed.). **Defence logistics**: enabling and sustaining successful military operations. London: Kogan Page, 2018. p. 64-89."),
    kahn = r("Kahn, Strong e Wang", "Kahn; Strong; Wang", "2002", "KAHN",
      "KAHN, Beverly K.; STRONG, Diane M.; WANG, Richard Y. Information quality benchmarks: product and service performance. **Communications of the ACM**, [S. l.], v. 45, n. 4, p. 184-192, abr. 2002."),
    zabala = r("Zabala-López et al.", "Zabala-López et al.", "2024", "ZABALA",
      "ZABALA-LÓPEZ, Alexandra; LINARES-VÁSQUEZ, Mario; HAIDUC, Sonia; DONOSO, Yezid. A survey of data-centric technologies supporting decision-making before deploying military assets. **Defence Technology**, [S. l.], v. 42, p. 226-246, 2024. DOI: 10.1016/j.dt.2024.07.012."),
    sutton = r("Sutton e Staw", "Sutton; Staw", "1995", "SUTTON",
      "SUTTON, Robert I.; STAW, Barry M. What theory is not. **Administrative Science Quarterly**, [S. l.], v. 40, n. 3, p. 371-384, set. 1995."),
    vasconcelos = r("Vasconcelos Ferreira", "Vasconcelos Ferreira", "2013", "VASCONCELOS",
      "VASCONCELOS FERREIRA, Manuel Aníbal Silva Portugal. Comentário editorial: a construção de hipóteses. **Revista Ibero-Americana de Estratégia**, São Paulo, v. 12, n. 4, p. 1-8, out./dez. 2013."),
    schmuller = r("Schmuller", "Schmuller", "2019", "SCHMULLER",
      "SCHMULLER, Joseph. **Análise estatística com R para leigos**. 2. ed. Rio de Janeiro: Alta Books, 2019."),
    esgman = r("Escola Superior de Guerra", "Escola Superior de Guerra", "2025", "ESCOLA SUPERIOR DE GUERRA",
      "ESCOLA SUPERIOR DE GUERRA. Biblioteca. **Manual para elaboração de trabalhos acadêmicos**: artigo. Versão 3. Rio de Janeiro: ESG, 2025."),
    andrade = r("Andrade e Callim", "Andrade; Callim", "[s. d.]", "ANDRADE",
      "ANDRADE, Eduardo dos Santos; CALLIM, Acacio. **O papel da catalogação na gestão de ciclo de vida de sistemas de defesa**. [S. l.: s. n.], [s. d.]."),
    barth = r("Barth, Meyer e Spitzner", "Barth; Meyer; Spitzner", "2012", "BARTH",
      "BARTH, Rolf; MEYER, Matthias; SPITZNER, Jan. Typical pitfalls of simulation modeling: lessons learned from armed forces and business. **Journal of Artificial Societies and Social Simulation**, [S. l.], v. 15, n. 2, art. 5, 2012."),
    md4001 = r("Brasil", "Brasil", "2019", "BRASIL 2 MD40-M-01",
      "BRASIL. Ministério da Defesa. **MD40-M-01: Manual de Boas Práticas para a Gestão do Ciclo de Vida de Sistemas de Defesa**. Brasília, DF: Ministério da Defesa, 2019."),
    md40n02 = r("Brasil", "Brasil", "2021", "BRASIL 2 MD40-N-02",
      "BRASIL. Ministério da Defesa. **MD40-N-02: Normas para a Governança do Sistema de Gestão de Ciclo de Vida de Sistemas de Defesa (NORGCV)**. Brasília, DF: Ministério da Defesa, 2021."),
    md4401 = r("Brasil", "Brasil", "2023b", "BRASIL 2 MD44-M-01",
      "BRASIL. Ministério da Defesa. **MD44-M-01: Manual de Boas Práticas de Apoio Logístico Integrado de Sistemas de Defesa**. Brasília, DF: Ministério da Defesa, 2023b."),
    feickert = r("Feickert", "Feickert", "2019", "FEICKERT",
      "FEICKERT, Andrew. **Joint Light Tactical Vehicle (JLTV)**: background and issues for Congress. [S. l.]: Congressional Research Service, 2019. (CRS Report, RS22942)."),
    filgueiras = r("Filgueiras", "Filgueiras", "2019", "FILGUEIRAS",
      "FILGUEIRAS, Marcelo Vallim. Apoio logístico integrado e gestão do ciclo de vida dos meios navais, aeronavais e de fuzileiros navais: o emprego do Sistema de Custos da Marinha na gestão do ciclo de vida dos meios da Marinha do Brasil. **Caderno da Escola de Guerra Naval**, Rio de Janeiro, v. 3, p. 313-345, 2019."),
    hwang = r("Hwang e Yoon", "Hwang; Yoon", "1981", "HWANG",
      "HWANG, Ching-Lai; YOON, Kwangsun. **Multiple attribute decision making**: methods and applications. Berlin: Springer, 1981."),
    app = r("JLTV Sustainment Decision Lab", "JLTV Sustainment Decision Lab", year, "JLTV",
      paste0("JLTV SUSTAINMENT DECISION LAB. **Simulador de apoio à decisão para a sustentação da frota JLTV**. Versão ", version, ". [S. l.: s. n.], ", year, ". Programa de computador.")),
    lacey = r("Lacey e Hockley", "Lacey; Hockley", "2018", "LACEY",
      "LACEY, Laura; HOCKLEY, Chris. Achieving dependability in defence systems. In: SMITH, Jeremy C. D. (ed.). **Defence logistics**: enabling and sustaining successful military operations. London: Kogan Page, 2018. p. 162-184."),
    osz2023a = r("Oszczypała, Ziółkowski e Małachowski", "Oszczypała; Ziółkowski; Małachowski", "2023a", "OSZCZYPALA 2",
      "OSZCZYPAŁA, Mateusz; ZIÓŁKOWSKI, Jarosław; MAŁACHOWSKI, Jerzy. Modelling the operation process of light utility vehicles in transport systems using Monte Carlo simulation and semi-Markov approach. **Energies**, [S. l.], v. 16, art. 2210, 2023a."),
    osz2023b = r("Oszczypała, Ziółkowski e Małachowski", "Oszczypała; Ziółkowski; Małachowski", "2023b", "OSZCZYPALA 3",
      "OSZCZYPAŁA, Mateusz; ZIÓŁKOWSKI, Jarosław; MAŁACHOWSKI, Jerzy. Semi-Markov approach for reliability modelling of light utility vehicles. **Eksploatacja i Niezawodność – Maintenance and Reliability**, [S. l.], v. 25, n. 2, 2023b. DOI: 10.17531/ein/161859."),
    perez = r("Perez", "Perez", "2023", "PEREZ",
      "PEREZ, James R. **Weapon system sustainment, the Fourth Industrial Revolution, and effective industry logistics practices**. Monterey, CA: Naval Postgraduate School, 2023. (NPS-LM-23-239)."),
    pina = r("Piña Silva et al.", "Piña Silva et al.", "2025", "PINA SILVA",
      "PIÑA SILVA, Guillermo Azahel; PUGA MENDOZA, Alejandro; PERALTA ENRÍQUEZ, Jorge; ARANDA LÓPEZ, Alejandro; TORRES MENDOZA, Ricardo. Discrete event simulation analysis of vehicle maintenance orders. **Procedia Computer Science**, [S. l.], v. 253, p. 1246-1255, 2025."),
    robinson = r("Robinson", "Robinson", "2015", "ROBINSON",
      "ROBINSON, Stewart. A tutorial on conceptual modeling for simulation. In: WINTER SIMULATION CONFERENCE, 2015, [S. l.]. **Proceedings** [...]. [S. l.]: IEEE, 2015. p. 1820-1834."),
    saaty = r("Saaty", "Saaty", "1980", "SAATY",
      "SAATY, Thomas L. **The analytic hierarchy process**: planning, priority setting, resource allocation. New York: McGraw-Hill, 1980."),
    salabun = r("Sałabun, Wątróbski e Shekhovtsov", "Sałabun; Wątróbski; Shekhovtsov", "2020", "SALABUN",
      "SAŁABUN, Wojciech; WĄTRÓBSKI, Jarosław; SHEKHOVTSOV, Andrii. Are MCDA methods benchmarkable? A comparative study of TOPSIS, VIKOR, COPRAS, and PROMETHEE II methods. **Symmetry**, [S. l.], v. 12, n. 9, 2020."),
    scarf = r("Scarf, Syntetos e Teunter", "Scarf; Syntetos; Teunter", "2024", "SCARF",
      "SCARF, Phil; SYNTETOS, Aris; TEUNTER, Ruud. Joint maintenance and spare-parts inventory models: a review and discussion of practical stock-keeping rules. **IMA Journal of Management Mathematics**, [S. l.], v. 35, p. 83-109, 2024."),
    smith = r("Smith", "Smith", "2018", "SMITH",
      "SMITH, Jeremy C. D. Integrating mission and support systems through the life cycle: integrated logistic support and maintenance strategies. In: SMITH, Jeremy C. D. (ed.). **Defence logistics**: enabling and sustaining successful military operations. London: Kogan Page, 2018. p. 114-134."),
    targett = r("Targett", "Targett", "2018", "TARGETT",
      "TARGETT, Shane. Optimizing the defence inventory. In: SMITH, Jeremy C. D. (ed.). **Defence logistics**: enabling and sustaining successful military operations. London: Kogan Page, 2018. p. 249-265."),
    velmurugan = r("Velmurugan et al.", "Velmurugan et al.", "2022", "VELMURUGAN",
      "VELMURUGAN, K.; SARAVANASANKAR, S.; VENKUMAR, P.; SUDHAKARAPANDIAN, R.; DI BONA, Gianpaolo. Hybrid fuzzy AHP-TOPSIS framework on human error factor analysis: implications to developing optimal maintenance management system in the SMEs. **Sustainable Futures**, [S. l.], v. 4, art. 100087, jun. 2022."),
    aguaron = r("Aguarón et al.", "Aguarón et al.", "2019", "AGUARON",
      "AGUARÓN, Juan; ESCOBAR, María Teresa; MORENO-JIMÉNEZ, José María; TURÓN, Alberto. AHP-group decision making based on consistency. **Mathematics**, [S. l.], v. 7, n. 3, art. 242, 2019. DOI: 10.3390/math7030242."),
    assis22 = r("Assis et al.", "Assis et al.", "2022", "ASSIS 2",
      "ASSIS, Sergio Ricardo de; GAVIÃO, Luiz Octavio; KOSTIN, Sergio; LIMA, Gilson Brito Alves. Prioritizing flight simulators of the Brazilian Air Force by the analytic hierarchy process and hypothesis tests. **Brazilian Journal of Operations & Production Management**, [S. l.], v. 19, n. 4, e20221366, 2022. DOI: 10.14488/BJOPM.1366.2022."),
    cgcfn = r("Brasil", "Brasil", "2020", "BRASIL 1 MARINHA",
      "BRASIL. Marinha do Brasil. Comando-Geral do Corpo de Fuzileiros Navais. **Viatura Blindada Especial Leve 4x4 Oshkosh JLTV**. Apêndice V ao Anexo B do Ofício nº 323/2020. [S. l.]: CGCFN, 2020. Documento de circulação restrita."),
    costa = r("Costa et al.", "Costa et al.", "2022", "COSTA",
      "COSTA, Igor Pinheiro de Araújo; COSTA, Arthur Pinheiro de Araújo; SANSEVERINO, Adriana Manzolillo; GOMES, Carlos Francisco Simões; SANTOS, Marcos dos. Bibliometric studies on multi-criteria decision analysis (MCDA) methods applied in military problems. **Pesquisa Operacional**, [S. l.], v. 42, e249414, 2022. DOI: 10.1590/0101-7438.2022.042.00249414."),
    djukic = r("Đukić et al.", "Đukić et al.", "2022", "DJUKIC",
      "ĐUKIĆ, Đorđe; PETROVIĆ, Ivan; BOŽANIĆ, Darko; DELIBAŠIĆ, Boris. Selection of unmanned aircraft for training of small-range aircraft defense system: AHP-TOPSIS optimization methods. **Yugoslav Journal of Operations Research**, [S. l.], v. 32, n. 3, p. 389-406, 2022. DOI: 10.2298/YJOR211125007D."),
    eua20 = r("Estados Unidos", "Estados Unidos", "2020", "ESTADOS UNIDOS",
      "ESTADOS UNIDOS. Department of the Army. Joint Program Office JLTV. **Joint Light Tactical Vehicle (JLTVA1) operations course**: student guide. Warren, MI: JPO JLTV, 2020. Material de curso."),
    esc1 = r("1º ESCALÃO JLTV", "1º ESCALÃO JLTV", "[s. d.]", "1 ESCALAO",
      "**1º ESCALÃO JLTV**. [S. l.: s. n.], [s. d.]. Material de treinamento."),
    gomes = r("Gomes et al.", "Gomes et al.", "2021", "GOMES",
      "GOMES, Carlos Francisco Simões; RODRIGUES, Marcus Vinícius Gonçalves; COSTA, Igor Pinheiro de Araújo; SANTOS, Marcos dos. Ordering of warships for the Brazilian Navy using the new method: AHP-Gaussian with Pearson's correlation. In: TALLÓN-BALLESTEROS, Antonio J. (ed.). **Modern management based on big data II and machine learning and intelligent systems III**. Amsterdam: IOS Press, 2021. p. 104-111. DOI: 10.3233/FAIA210237."),
    prado = r("Prado", "Prado", "2014", "PRADO",
      "PRADO, Darci Santos do. **Teoria das filas e da simulação**. 5. ed. Nova Lima: Falconi, 2014."),
    taha = r("Taha", "Taha", "2008", "TAHA",
      "TAHA, Hamdy A. **Pesquisa operacional**. 8. ed. São Paulo: Pearson Prentice Hall, 2008."),
    simmer = r("Ucar, Smeets e Azcorra", "Ucar; Smeets; Azcorra", "2019", "UCAR 2",
      "UCAR, Iñaki; SMEETS, Bart; AZCORRA, Arturo. simmer: discrete-event simulation for R. **Journal of Statistical Software**, [S. l.], v. 90, n. 2, p. 1-30, 2019. DOI: 10.18637/jss.v090.i02.")
  )
}

# Troca {c:chave} e {p:chave1,chave2} pela citação; registra as chaves usadas em used$keys.
word_report_cite_resolve = function(text, db, used) {
  tokens = unique(regmatches(text, gregexpr("\\{[cp]:[a-z0-9_,]+\\}", text, perl = TRUE))[[1]])
  for (tok in tokens) {
    kind = substr(tok, 2L, 2L)
    keys = strsplit(substr(tok, 4L, nchar(tok) - 1L), ",", fixed = TRUE)[[1]]
    unknown = setdiff(keys, names(db))
    if (length(unknown) > 0L) stop("Fonte não cadastrada: ", paste(unknown, collapse = ", "), call. = FALSE)
    used$keys = union(used$keys, keys)
    out = if (identical(kind, "c")) {
      k = keys[[1]]
      paste0(db[[k]]$narr, " (", db[[k]]$year, ")")
    } else {
      keys = keys[order(vapply(keys, function(k) db[[k]]$sort, character(1)))]
      paste0("(", paste(vapply(keys, function(k) paste0(db[[k]]$par, ", ", db[[k]]$year), character(1)), collapse = "; "), ")")
    }
    text = gsub(tok, out, text, fixed = TRUE)
  }
  text
}

# Lista de referências (ABNT) só das fontes citadas, em ordem alfabética.
word_report_references = function(db, keys) {
  keys = intersect(names(db), keys)
  if (length(keys) == 0L) return(character())
  ord = order(vapply(keys, function(k) db[[k]]$sort, character(1)), method = "radix")
  vapply(keys[ord], function(k) db[[k]]$ref, character(1), USE.NAMES = FALSE)
}

# ---- 7. Estimativa de páginas do corpo (Introdução → Conclusão) ------------------------
# Calibrada com o render real do formato da ESG: Arial 12, espaçamento 1,5, 16 cm de largura útil,
# ~77 caracteres por linha (já com o recuo de 1,5 cm), 0,73 cm por linha e 24,7 cm úteis por página.
word_report_est_text_cm = function(text, indent = TRUE, left_cm = 0) {
  text = gsub("\\{(fig|tab|quad|apx):[a-z0-9_]+\\}", "Figura 00 (Apêndice A)", text, perl = TRUE)
  cpl = 81 * (16 - left_cm) / 16  # calibrado pelo documento renderizado (linhas de 82 a 86 caracteres)
  n = nchar(text) + if (isTRUE(indent)) 8 else 0
  max(1, ceiling(n / cpl)) * 0.73
}

# Simula o preenchimento das páginas: figuras não se dividem; texto e tabelas fluem.
word_report_est_pages = function(blocks, page_cm = 24.7) {
  pages = 1L
  used = 0
  for (b in blocks) {
    h = b$cm
    type = b$type
    if (type %in% c("h1", "h2") && (page_cm - used) < 3.6) {
      pages = pages + 1L
      used = 0
    }
    if (identical(type, "obj") && identical(b$kind, "figura")) {
      if (used + h > page_cm) {
        pages = pages + 1L
        used = 0
      }
      used = used + h
    } else {
      used = used + h
      while (used > page_cm) {
        pages = pages + 1L
        used = used - page_cm
      }
    }
  }
  pages - 1 + used / page_cm
}

# ---- 8. Texto fixo (Introdução, Referencial teórico e Metodologia), com as fontes do autor ------------
# Linguagem técnica, neutra e em terceira pessoa; o que foi feito aparece no passado e o que a literatura
# afirma, no presente. Cada parágrafo traz uma ideia. As fontes só sustentam o que o trabalho afirma delas;
# parâmetros de outras frotas não são transferidos para a JLTV.
word_report_text_intro = function(n_resp, mode) {
  who = if (!is.na(n_resp)) paste0(n_resp, " especialistas") else "especialistas"
  evidence = switch(mode,
    SYNTHETIC = "A execução analisada operou em modo de evidência sintético: os dados são de demonstração, e os resultados ilustram o procedimento sem constituir evidência sobre a frota.",
    HYBRID = "A execução analisada operou em modo de evidência híbrido: registros reais se combinaram a hipóteses de cenário, e os resultados são condicionais a essas hipóteses.",
    EMPIRICAL = "A execução analisada operou em modo de evidência empírico: os resultados decorrem de registros reais da frota e valem para o período, a amostra e as hipóteses do modelo.",
    paste0("A execução analisada operou em modo de evidência ", word_report_mode_label(mode, "pt"), ".")
  )
  c(
    "O Corpo de Fuzileiros Navais (CFN) adotou a Joint Light Tactical Vehicle (JLTV), viatura blindada leve 4x4 da Oshkosh Defense {p:cgcfn}. A viatura foi desenvolvida pelo Exército e pelo Corpo de Fuzileiros Navais dos Estados Unidos da América para substituir parte da frota de High Mobility Multipurpose Wheeled Vehicle (HMMWV), em serviço desde 1985, cuja vulnerabilidade a artefatos explosivos improvisados levou a priorizar a proteção da guarnição {p:feickert}.",
    "A proteção trouxe exigências de sustentação. {c:feickert} relata que o órgão de avaliação operacional do Departamento de Defesa norte-americano considerou a JLTV operacionalmente inadequada em 2018, por deficiências de confiabilidade, de manutenibilidade, de treinamento e de manuais, e que testes de 2017 não atenderam a todos os requisitos de desempenho ligados à manutenção. O mesmo relatório registra que a manutenção da JLTV pode demorar mais que a do HMMWV, pela remoção de painéis blindados e pelo motor mais complexo.",
    "Para quem a adquire, portanto, a obtenção é o início do ciclo de vida, e não o seu fim. O MD40-M-01 estabelece que a Gestão do Ciclo de Vida (GCV) considera o sistema em todas as fases e que as informações geradas na operação e no apoio retroalimentam as decisões de sustentação {p:md4001}. O MD44-M-01 reúne os elementos do Apoio Logístico Integrado (ALI), como manutenção, suprimento, pessoal, treinamento e infraestrutura, e destaca o acompanhamento da confiabilidade, da manutenibilidade e da disponibilidade {p:md4401}.",
    "O estudo se justifica porque recursos de manutenção e de suprimento são limitados e a viatura só cumpre sua função quando está pronta. Quando a frota é pequena em relação à exigência de prontidão, cada viatura parada consome parte da margem disponível para a missão, e decidir em quais sistemas e em quais intervenções aplicar esses recursos exige critério explícito, quantificado e rastreável.",
    "Diante disso, formulou-se a seguinte questão de pesquisa: quais sistemas da viatura JLTV e quais intervenções de manutenção e de suprimento devem ser priorizados para sustentar a disponibilidade da frota do CFN?",
    "O objetivo geral é identificar os sistemas da viatura e as intervenções de manutenção e de suprimento a priorizar para sustentar a disponibilidade da frota JLTV do CFN.",
    paste0(
      "Para alcançar o objetivo, a pesquisa adotou abordagem quantitativa e aplicada. Um questionário respondido por ", who,
      " alimentou o Processo Analítico Hierárquico (Analytic Hierarchy Process – AHP) {p:saaty} e a técnica Technique for Order of Preference by Similarity to Ideal Solution (TOPSIS) {p:hwang}, que ordenaram os sistemas da viatura. Em seguida, uma simulação de eventos discretos da frota, repetida em replicações de Monte Carlo {p:taha,prado}, estimou a disponibilidade da situação de referência e o ganho de cada intervenção."
    ),
    evidence,
    "O trabalho organiza-se em cinco seções. A Introdução apresenta o contexto, a justificativa, a questão de pesquisa, o objetivo e o método. O Referencial teórico define a GCV e o ALI, a manutenção e a disponibilidade, o suprimento, a qualidade dos dados, o apoio multicritério à decisão e a simulação. A Metodologia descreve o delineamento, a base de dados e os procedimentos de análise. A Análise e discussão apresenta e interpreta os resultados. A Conclusão responde à questão de pesquisa, apresenta a proposta de ação e indica as limitações."
  )
}

# Quadro de enquadramento do simulador na classificação clássica dos modelos de simulação.
simulation_framing_table = function() {
  data.frame(
    `Dimensão` = c("Tempo", "Estado do sistema", "Incerteza", "Técnica", "Sistema de filas",
                   "Entidade e resolução", "Horizonte", "Números aleatórios", "Saídas",
                   "Implementação", "Verificação e validação"),
    `Classificação do simulador` = c(
      "Dinâmico: o relógio avança e o estado muda ao longo do tempo.",
      "Discreto por eventos: o estado muda apenas em falha, fim de diagnóstico, fim de reparo, manutenção preventiva, entrega de peça e liberação.",
      "Estocástico: tempos até a falha, de reparo e de reposição são sorteados.",
      "Simulação de eventos discretos (SED) como núcleo, repetida em replicações de Monte Carlo para os intervalos de confiança.",
      "Rede de filas G/G/c com população finita (m = N viaturas), disciplina FIFO e estações de diagnóstico, reparo e peça; capacidade de reparo = menor valor entre equipes e boxes.",
      "Entidade = viatura; no módulo de sistemas, os subsistemas A1 a A10. Alta resolução (cada viatura é simulada).",
      "Terminante: cada replicação termina no horizonte H definido pelo usuário.",
      "Pseudoaleatórios com semente por replicação; números comuns entre cenários (comparação pareada).",
      "Disponibilidade operacional (Ao), probabilidade de manter ao menos M viaturas prontas, horas abaixo de M e IC95%.",
      "Motor próprio programado em R (lista de eventos, filas e estoque); o pacote simmer é referência da literatura.",
      "Verificação por testes automáticos do código; a validação exige registros reais da frota, e o modo sintético demonstra o método."
    ),
    `Como aparece no aplicativo` = c(
      "Horizonte, em horas (Passo 3).",
      "Estados da viatura no motor; parâmetros de falha, reparo e peça (Passo 3).",
      "MTBF, tempos de diagnóstico e reparo, prazo de reposição e probabilidade de peça.",
      "Passo 4: replicações e IC95%; ganho de cada intervenção com sementes comuns.",
      "Frota, equipes de diagnóstico e de reparo, boxes e estoque (Passo 3).",
      "Frota (N) e, no módulo de sistemas, A1 a A10.",
      "Campo de horizonte; resultados valem para esse período.",
      "Campo de semente e número de replicações.",
      "Passo 4 e relatórios em Word e Excel.",
      "Aba de auditoria e referências (4C).",
      "Aba de auditoria; relatório (limitações)."
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# Subseções do Referencial teórico: lista de list(title, paragraphs). Só conceitos: o que foi feito com eles
# está na Metodologia. Em cada subseção: o que se trata, o conceito, os registros e a associação com o estudo.
word_report_text_theory = function() {
  list(
    list(title = "2.1 Gestão do Ciclo de Vida e Apoio Logístico Integrado", paragraphs = c(
      "A Gestão do Ciclo de Vida (GCV) organiza as decisões relativas ao sistema de defesa desde sua concepção até seu desfazimento {p:md4001}. {c:smith} apresenta o ciclo CADMID, composto por conceito, avaliação, demonstração, fabricação, emprego e descarte, e examina a aplicação do apoio logístico em cada fase. Essa perspectiva articula as decisões de obtenção, uso e sustentação do sistema.",
      "O Ministério da Defesa adota esse conceito. O MD40-M-01 {p:md4001} atribui à Gestão do Ciclo de Vida de Sistemas de Defesa o objetivo de planejar, obter, manter e otimizar as Capacidades Militares de Defesa, considerando desempenho, segurança, qualidade e custo, e organiza o ciclo em seis fases: concepção, desenvolvimento, produção, operação, apoio e desfazimento.",
      "As Normas para a Governança do Sistema de Gestão de Ciclo de Vida de Sistemas de Defesa, MD40-N-02 {p:md40n02}, estabelecem os mecanismos que integram as organizações e os processos do ciclo.",
      "O ALI integra a GCV na dimensão do apoio. {c:smith} define o apoio logístico integrado como uma disciplina de gestão cujo propósito principal é assegurar que o sistema de apoio represente a integração de recursos, processos e atividades logísticas. O MD44-M-01 {p:md4401} reúne os elementos desse apoio: manutenção, suprimento, pessoal, treinamento, infraestrutura, equipamentos de apoio e de teste, dados técnicos e engenharia.",
      "A relação entre os dois conceitos é de inclusão: a GCV orienta todas as fases do ciclo, e o ALI organiza os recursos que sustentam o sistema, sobretudo nas fases de operação e de apoio. {c:filgueiras} examina essa relação para os meios navais, aeronavais e de fuzileiros navais e avalia a capacidade do Sistema de Custos da Marinha de produzir as informações que o custo do ciclo de vida exige.",
      "Para o estudo, a GCV e o ALI delimitam a questão de pesquisa: o que a sustentação da JLTV precisa registrar e decidir para manter a viatura disponível ao longo do ciclo."
    )),
    list(title = "2.2 Manutenção, dependabilidade e disponibilidade", paragraphs = c(
      "{c:lacey} apresentam a dependabilidade como o conjunto formado pela confiabilidade, pela manutenibilidade e pela disponibilidade. A confiabilidade é a capacidade de o sistema permanecer operável antes de uma falha; a manutenibilidade, a facilidade de restaurá-lo; e a disponibilidade, a condição de os meios estarem prontos para uso sob condições especificadas.",
      "Os indicadores partem do conceito de falha, que o MD44-M-01 {p:md4401} define como o término da capacidade de um item desempenhar a função requerida. O modo de falha é a forma como essa perda ocorre: {c:smith} situa a identificação de quais itens se degradam e falham, e de como falham, no domínio da engenharia, e o MD44-M-01 {p:md4401} inclui a análise de modos de falha, efeitos e criticidade (Failure Mode, Effects and Criticality Analysis – FMECA) e a Manutenção Centrada na Confiabilidade, que define a manutenção mais adequada para cada modo de falha.",
      "O tempo médio até a falha (Mean Time To Failure – MTTF) é a medida de confiabilidade que expressa o tempo correto de operação da viatura, obtido pela integral da função de confiabilidade R(t) ao longo do tempo {p:osz2023b}. O tempo médio entre falhas (Mean Time Between Failures – MTBF) aplica-se a itens reparáveis, e o tempo médio de reparo (Mean Time To Repair – MTTR) mede o tempo necessário para restaurar o item {p:lacey}. {c:pina} usam o MTTR como indicador da eficiência do fluxo de reparo.",
      "{c:lacey} distinguem a disponibilidade intrínseca (Ai), que considera apenas a manutenção corretiva ativa, Ai = MTBF/(MTBF + MTTR); a alcançada (Aa), que inclui a manutenção preventiva; e a operacional (Ao), que inclui também os atrasos administrativos e logísticos, Ao = MTBM/(MTBM + MCMT + MPMT + MALDT). Nessa expressão, MTBM é o tempo médio entre manutenções, MCMT e MPMT são os tempos médios de manutenção corretiva e preventiva, e MALDT é o tempo médio de atraso administrativo e logístico.",
      "Essa distinção impede que a espera por peças, por equipes ou por liberação administrativa seja contada como reparo. {c:abreu} sistematizam os conceitos de manutenção corretiva, preventiva e preditiva, de taxa de falhas e de disponibilidade.",
      "Os registros que alimentam os indicadores são as ordens de serviço (OS) de manutenção, com data de detecção, data de liberação, tipo de intervenção e indicação de falha funcional, e as medidas de exposição ao uso. O MD44-M-01 {p:md4401} destaca o registro de anomalias, de falhas e de ações de manutenção e o acompanhamento da confiabilidade, da manutenibilidade e da disponibilidade.",
      "Na análise da frota, a disponibilidade operacional (Ao) da viatura completa é o indicador principal. Os tempos de diagnóstico, de reparo, de espera e de liberação são as parcelas que a compõem, e o MTBF e o MTTR entram na simulação como hipóteses declaradas até que os registros da frota permitam estimá-los."
    )),
    list(title = "2.3 Suprimento, lead time e catalogação", paragraphs = c(
      "O suprimento integra o ALI e sustenta a manutenção sempre que a intervenção depende de material {p:md4401}. {c:targett} descreve a gestão do inventário de defesa como o equilíbrio entre o custo de manter o item e o efeito de sua falta, com apoio em dados, previsão, planejamento, segmentação e codificação.",
      "O lead time é o tempo entre a colocação de uma demanda e o recebimento do material demandado por quem o requisitou {p:smithpt}. Ele difere do reparo ativo e compõe o atraso logístico (MALDT). {c:smithpt} relacionam lead times longos a maiores volumes de inventário no fluxo de suprimento.",
      "{c:scarf} revisam os modelos conjuntos de manutenção e de estoque de sobressalentes e as regras práticas de estocagem, e mostram que o lead time condiciona a política de manutenção e a regra de estocagem. Esse trabalho sustenta o tratamento articulado entre manutenção e suprimento.",
      "A identificação padronizada do item é condição dessa integração. {c:andrade} relacionam a catalogação, a GCV e o NATO Stock Number (NSN), número de identificação padronizada de item de suprimento. {c:perez} destaca, para o Corpo de Fuzileiros Navais dos Estados Unidos, a importância da coerência entre listas de materiais, publicações técnicas, registros e estoques.",
      "No estudo, o suprimento entra na simulação por quatro parâmetros: o estoque de sobressalentes, o ponto e a quantidade de ressuprimento, o lead time e a probabilidade de a intervenção exigir peça."
    )),
    list(title = "2.4 Qualidade dos dados e apoio à decisão", paragraphs = c(
      "{c:kahn} definem qualidade da informação como adequação ao uso e distinguem dimensões mensuráveis, como a precisão, a completude e a ausência de erros. Esses critérios permitem avaliar uma base de dados antes de utilizá-la.",
      "{c:zabala} revisam as tecnologias centradas em dados, como a análise de dados, a ciência de dados e o aprendizado de máquina, que apoiam a decisão antes do emprego de meios militares. Os autores destacam, como fatores de integração, o pessoal especializado, as arquiteturas e a segurança cibernética.",
      "No estudo, a avaliação da base de dados precede a escolha do método: a cobertura das notas, a consistência dos julgamentos e a completude das ordens de serviço determinam o que pode ser estimado e o que permanece como hipótese declarada."
    )),
    list(title = "2.5 Apoio multicritério à decisão", paragraphs = c(
      "O apoio multicritério à decisão explicita critérios, pesos e desempenhos das alternativas. O AHP decompõe o problema em critérios e subcritérios e obtém seus pesos por comparações pareadas, feitas em uma escala de importância relativa {p:saaty}. A agregação dos julgamentos de vários decisores e o controle da consistência são tratados na literatura de AHP em grupo {p:aguaron}.",
      "O estudo bibliométrico de {c:costa} mostra que o AHP é o método multicritério mais aplicado em problemas militares. Entre as aplicações em defesa, estão a ordenação de navios de guerra para a Marinha do Brasil {p:gomes}, a priorização de simuladores de voo da Força Aérea Brasileira {p:assis22} e a combinação AHP–TOPSIS na seleção de aeronaves não tripuladas para o treinamento de um sistema de defesa antiaérea {p:djukic}.",
      "A técnica TOPSIS ordena as alternativas pela proximidade a uma solução ideal e pelo afastamento de uma solução anti-ideal {p:hwang}. A combinação do AHP, para os pesos, com o TOPSIS, para a ordenação, aparece em estudos de manutenção: {c:velmurugan} empregam uma estrutura híbrida de AHP e TOPSIS, em versão fuzzy, na análise de fatores de erro humano para a gestão da manutenção. Os critérios, os pesos e os resultados desse estudo pertencem ao contexto original.",
      "Os resultados dos métodos multicritério variam conforme o método e a normalização {p:salabun}, o que justifica medir a estabilidade do ranking. No estudo, os especialistas compararam critérios e subcritérios aos pares e avaliaram os dez sistemas da viatura, e a estabilidade do ranking foi medida por reamostragem dos avaliadores, como a subseção 3.3 detalha."
    )),
    list(title = "2.6 Modelagem e simulação", paragraphs = c(
      "Na classificação clássica dos modelos {p:taha}, os sistemas e seus modelos são estáticos ou dinâmicos, contínuos ou discretos e determinísticos ou estocásticos. A simulação de Monte Carlo é estática: repete muitas vezes um sorteio independente do tempo, como uma fotografia. A simulação de eventos discretos (SED) é dinâmica: o estado muda apenas em instantes de eventos, com entidades que disputam recursos em filas, como um filme {p:prado}.",
      "{c:robinson} define o modelo conceitual como a descrição, independente de software, que explicita objetivos, entradas, saídas, conteúdo, pressupostos e simplificações, e defende que o modelo represente apenas o necessário para responder à questão de pesquisa. {c:barth} identificam armadilhas da simulação, como a complexidade excessiva e as falhas de implementação, de interpretação e de aceitação, e mostram que o realismo excessivo pode dificultar a validação sem ganho analítico.",
      "{c:osz2023a} modelam a operação de viaturas do sistema de transporte das Forças Armadas polonesas em quatro estados (execução de tarefa, espera de tarefa, manutenção periódica e reparo), com simulação de Monte Carlo e abordagem semi-Markov, e {c:osz2023b} aplicam um modelo semi-Markov de três estados à confiabilidade desses veículos.",
      "{c:pina} simulam, por eventos discretos, ordens de manutenção de veículos, com filas, atendimento, recursos e tempos de manutenção. Para o ambiente R, {c:simmer} descrevem o pacote simmer, que implementa a simulação de eventos discretos por trajetórias.",
      "O simulador do estudo é uma SED dinâmica, estocástica e de alta resolução, em que cada viatura é simulada, repetida em replicações de Monte Carlo com sementes comuns. A subseção 3.4 descreve o modelo conceitual e apresenta o quadro de enquadramento do simulador."
    ))
  )
}

# Metodologia (seção 3): delineamento, base de dados, AHP–TOPSIS, simulação e intervenções.
# `info` traz valores da execução (replicações, horizonte, meta M, cobertura mínima P6, AHP só consistente).
word_report_text_method = function(n_resp, mode, info = list()) {
  val = function(name, default) {
    v = info[[name]]
    if (is.null(v) || length(v) == 0L || is.na(v[[1]]) || !nzchar(as.character(v[[1]]))) default else as.character(v[[1]])
  }
  who = if (!is.na(n_resp)) paste0("foi respondido por ", n_resp, " especialistas") else "foi respondido por especialistas"
  reps = val("replications", NA_character_)
  horizon = val("horizon", NA_character_)
  target = val("target_ready", NA_character_)
  coverage = val("coverage", NA_character_)
  consistent_only = !identical(val("only_consistent", "TRUE"), "FALSE")
  mode_label = word_report_mode_label(mode, "pt")
  list(
    list(title = "3.1 Delineamento da pesquisa", paragraphs = c(
      "O objeto da pesquisa é a frota de viaturas JLTV do CFN, e a unidade de análise é a viatura completa. Os dez sistemas da viatura (A1 a A10) e seus componentes explicam as causas do tempo parado e as intervenções possíveis, e não substituem a disponibilidade da viatura. A identificação dos sistemas partiu da documentação técnica disponível: o guia do curso de operação {p:eua20}, o material de treinamento de manutenção de primeiro escalão {p:esc1} e a caracterização da viatura no âmbito do CFN {p:cgcfn}.",
      "O objetivo é identificar os sistemas da viatura e as intervenções de manutenção e de suprimento a priorizar para sustentar a disponibilidade da frota. Os objetivos intermediários foram avaliar a base de dados, ordenar os sistemas por prioridade e estimar o efeito das intervenções. Os procedimentos usados para alcançar esses resultados são apresentados a seguir.",
      "O método é aplicado e quantitativo, com procedimentos de modelagem e de simulação, e compreendeu quatro etapas:",
      "a) avaliação da base de dados e de sua qualidade (subseção 3.2);",
      "b) priorização dos sistemas por AHP–TOPSIS e teste de robustez do ranking (subseção 3.3);",
      "c) construção do modelo conceitual e simulação da disponibilidade da frota (subseção 3.4);",
      "d) teste das intervenções e cálculo dos indicadores (subseção 3.5).",
      "A pesquisa formulou duas hipóteses, entendidas como proposições sobre relações entre variáveis, com a direção esperada {p:vasconcelos}. A hipótese H1 afirma que a posição de cada sistema no ranking TOPSIS se mantém quando a amostra de avaliadores, os pesos ou a normalização variam. A hipótese H2 afirma que cada intervenção testada eleva a disponibilidade operacional simulada em relação à situação de referência.",
      "As duas hipóteses se apoiam nos argumentos do referencial teórico, porque referências, dados, variáveis e hipóteses, isoladamente, não constituem teoria {p:sutton}. A estrutura e a formatação do trabalho seguiram o Manual para elaboração de trabalhos acadêmicos da Escola Superior de Guerra {p:esgman}."
    )),
    list(title = "3.2 Base de dados e avaliação de sua qualidade", paragraphs = c(
      "Antes de definir o método de análise, a pesquisa avaliou a base de dados quanto à precisão, à completude e à ausência de erros, dimensões da qualidade da informação descritas por {c:kahn}. Esta subseção descreve as duas fontes de dados e os critérios de qualidade aplicados.",
      paste0("A primeira fonte é o questionário aplicado aos especialistas, que ", who, ". Os blocos P1 a P5 reúnem 14 comparações pareadas entre os quatro critérios (falha e ocorrência, impacto na disponibilidade, suprimento, e manutenção e apoio) e entre seus dez subcritérios, na escala de importância relativa do AHP. O bloco P6 pede uma nota de 1 a 10 para cada um dos dez sistemas em cada subcritério, com a opção N/A. O bloco P7 indica peças, ocorrências e registros a confirmar, e não altera pesos nem notas."),
      paste0("A avaliação da qualidade do questionário aplicou três critérios. A completude das notas P6 foi medida pela cobertura mínima por célula, isto é, por sistema e subcritério", if (!is.na(coverage)) paste0(" (mínimo de ", coverage, ")") else "", ", e as notas N/A permaneceram sem imputação. A consistência dos julgamentos foi medida pela razão de consistência (RC) das matrizes do AHP", if (consistent_only) ", e somente matrizes com RC inferior a 0,10 entraram no cálculo" else "", ". A precisão do processamento foi verificada por testes automáticos do código."),
      "A segunda fonte são os registros de manutenção da frota: ordens de serviço (OS) com data de detecção, data de liberação, tipo de manutenção e indicação de falha funcional, além das medidas de exposição, das peças e dos estoques. Esses registros permitem calcular indicadores observados quando as OS estão completas e verificadas {p:md4401}. A exposição em horas e em quilômetros não se combina, e a mudança de unidade exige conversão registrada.",
      paste0("Cada execução recebe um modo de evidência. No modo SYNTHETIC, os dados são de demonstração; no modo HYBRID, registros reais se combinam a hipóteses de cenário; no modo EMPIRICAL, apenas registros reais da frota alimentam o resultado. A execução analisada operou em modo ", mode_label, "."),
      "Os parâmetros físicos de entrada da simulação (MTBF, tempos de diagnóstico, de reparo e de liberação, lead time e probabilidade de peça) valem como hipóteses declaradas até que os registros da frota permitam estimá-los. As notas dos especialistas nunca se convertem em horas, em probabilidades de falha nem em prazos de reposição."
    )),
    list(title = "3.3 Priorização por AHP–TOPSIS e teste de robustez", paragraphs = c(
      "O AHP calculou os pesos pela média geométrica das comparações pareadas dos especialistas {p:saaty}. A razão de consistência de cada matriz mediu a coerência dos julgamentos, com limite de 0,10. Grupos com dois itens têm RC igual a zero por construção, e a verificação só é informativa a partir de três itens. O peso global de cada subcritério resultou do produto do peso do critério pelo peso do subcritério.",
      "O TOPSIS usou as notas P6, normalizadas pelo método vetorial e ponderadas pelos pesos globais do AHP {p:hwang}. Todos os subcritérios foram tratados como benefício, isto é, quanto maior a nota, maior a prioridade. O coeficiente de proximidade C = D−/(D+ + D−), em que D+ e D− são as distâncias à solução ideal e à anti-ideal, ordenou os sistemas, e as notas N/A não foram imputadas.",
      "O coeficiente expressa a prioridade segundo o julgamento dos especialistas e não equivale a taxa de falha, a MTBF nem a MTTR. Como os resultados dos métodos multicritério variam conforme o método e a normalização {p:salabun}, a robustez do ranking foi medida de três formas.",
      "A primeira foi a reamostragem dos avaliadores (bootstrap), que estimou a probabilidade de cada sistema ocupar cada posição. A segunda foi a sensibilidade a pesos iguais, e a terceira, a sensibilidade à normalização min–max. O coeficiente de correlação de postos de Spearman resumiu a concordância entre os rankings. Esses procedimentos testam a hipótese H1."
    )),
    list(title = "3.4 Modelo conceitual e simulação", paragraphs = c(
      "O modelo conceitual seguiu a estrutura de {c:robinson}: objetivo, entradas, saídas, conteúdo, pressupostos e simplificações. O objetivo do modelo é estimar a disponibilidade operacional da frota e comparar intervenções. As entradas são o tamanho da frota, as equipes de diagnóstico e de reparo, os boxes, o estoque, o lead time, o intervalo da manutenção preventiva e as distribuições dos tempos. As saídas são a disponibilidade operacional (Ao), a probabilidade de cumprir a missão e as horas abaixo da meta.",
      "O conteúdo do modelo representa cada viatura como uma entidade que alterna entre operação, falha, fila de diagnóstico, diagnóstico, espera por peça, fila de reparo, reparo, teste e liberação, além da manutenção preventiva. O modelo admite uma causa de parada ativa por vez e não representa redundância nem falhas simultâneas. Essa simplificação mantém o detalhe compatível com a questão de pesquisa e evita a complexidade que {c:barth} apontam como armadilha.",
      "A simulação é de eventos discretos, dinâmica, estocástica e de alta resolução, repetida em replicações de Monte Carlo {p:taha,prado}, e o {quad:enq_sim} resume a classificação. O motor foi programado em R, e o pacote simmer {p:simmer} serviu como referência da literatura.",
      paste0(
        "Cada configuração rodou ", if (!is.na(reps)) paste0(reps, " replicações independentes") else "replicações independentes",
        if (!is.na(horizon)) paste0(" ao longo de um horizonte de ", horizon, " horas") else " ao longo de um horizonte definido",
        if (!is.na(target)) paste0(", com a meta de manter ao menos ", target, " viaturas prontas") else "", ". Todas as configurações usaram as mesmas sementes aleatórias (números aleatórios comuns), o que permite a comparação pareada e reduz a variância da diferença entre os cenários."
      ),
      "A verificação do código usou testes automáticos com casos de resposta conhecida. A validação contra a frota real depende dos registros de manutenção, e a comparação entre a disponibilidade simulada e a observada nos registros, quando existe, orienta essa validação {p:robinson}. No modo SYNTHETIC, o resultado demonstra o método."
    )),
    list(title = "3.5 Intervenções, indicadores e ferramentas", paragraphs = c(
      "Foram testadas intervenções sobre o MTBF, o estoque de sobressalentes, o lead time, as equipes de reparo e de diagnóstico e os tempos de reparo e de diagnóstico. O {apx:premissas} registra a magnitude de cada intervenção e os demais parâmetros da execução.",
      "Cada intervenção foi simulada com as mesmas sementes da situação de referência. O ganho de disponibilidade foi a média das diferenças pareadas, com intervalo de confiança de 95% (IC95%) pela distribuição t, e foi considerado demonstrado quando o limite inferior do IC95% ficou acima de zero. As intervenções com ganho demonstrado foram simuladas também em conjunto. Esse procedimento testa a hipótese H2.",
      "Os indicadores foram a disponibilidade operacional (Ao), a probabilidade de manter ao menos M viaturas prontas durante o horizonte, com intervalo de Wilson, e as horas abaixo de M.",
      "O processamento estatístico e a simulação foram programados em R {p:schmuller}, e o aplicativo {c:app} reúne o questionário, o AHP–TOPSIS, a simulação e a geração deste relatório. Cada execução gera uma assinatura SHA256 das entradas, registrada no {apx:premissas}, o que torna o resultado rastreável e reproduzível."
    ))
  )
}

# ---- 8b. Participação de cada sistema no tempo parado (resultado da simulação por sistema) ----
word_report_system_downtime = function(sysres, contributions = NULL, mode = "HYBRID") {
  ss = if (is.list(sysres)) system_downtime_shares(sysres$systems) else NULL
  if (is.null(ss)) return(NULL)
  n = nrow(ss)
  top = ss[1L, ]
  pct1 = function(x) format_percent_br(x, 1)
  has_rank = is.data.frame(contributions) && nrow(contributions) > 0L &&
    all(c("alternative_id", "alternative_name", "rank") %in% names(contributions))
  table = data.frame(
    Sistema = paste(ss$system_id, ss$system_name),
    `Horas paradas (média por execução)` = round(ss$down_h, 1),
    `Falhas (média por execução)` = if ("failures" %in% names(ss)) round(ss$failures, 1) else NA_real_,
    Parcela = vapply(ss$share, format_percent_br, character(1), digits = 1),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  if (all(is.na(table[["Falhas (média por execução)"]]))) table[["Falhas (média por execução)"]] = NULL
  if (has_rank) {
    pos = contributions$rank[match(ss$system_id, contributions$alternative_id)]
    if (any(is.finite(pos))) table[["Posição na prioridade (TOPSIS)"]] = ifelse(is.finite(pos), as.character(as.integer(pos)), "n/d")
  }
  paragraphs = c(paste0(
    "A simulação por sistema atribui cada parada ao sistema que a causou, e a soma das horas de todos os sistemas forma o tempo parado total da frota na situação de referência. ",
    "A {fig:sys_pie} apresenta a parcela de cada sistema nesse total, e a {tab:sys_tab} traz as horas, as falhas e a posição de cada sistema na prioridade dos especialistas. ",
    "O sistema ", top$system_id, " (", top$system_name, ") responde pela maior parcela, com ", pct1(top$share), " (", format_number_br(top$down_h, 1), " h por execução)",
    if (n >= 3L) paste0("; os três sistemas de maior parcela somam ", pct1(sum(ss$share[1:3]))) else "", "."
  ))
  if (has_rank) {
    best = contributions[which.min(contributions$rank), , drop = FALSE]
    pos = match(best$alternative_id[[1]], ss$system_id)
    if (!is.na(pos)) {
      paragraphs = c(paragraphs, paste0(
        "O sistema de maior prioridade segundo os especialistas, ", word_report_pretty_name(best$alternative_name[[1]]), ", ",
        if (pos == 1L) "também tem a maior parcela do tempo parado simulado" else paste0("ocupa a ", pos, "ª posição na parcela do tempo parado simulado"),
        ". As duas leituras medem coisas distintas: a prioridade expressa o julgamento dos especialistas, e a parcela resulta dos parâmetros de vida, de reparo e de suprimento do catálogo de componentes. A comparação orienta a verificação desses parâmetros nos registros, sem ajuste para igualar as duas."
      ))
    }
  }
  evidence = toupper(as.character(sysres$mode %||% mode))
  paragraphs = c(paragraphs, switch(evidence,
    SYNTHETIC = "Como o catálogo desta execução é demonstrativo, as parcelas ilustram o procedimento e não descrevem a frota.",
    EMPIRICAL = "As parcelas decorrem dos parâmetros estimados nos registros da frota, nos limites do modelo simulado.",
    "As parcelas dependem dos parâmetros do catálogo e das hipóteses declaradas, e valem para o modelo simulado."
  ))
  list(paragraphs = paragraphs, plot = chart_system_downtime_pie(sysres$systems, titles = FALSE), table = table)
}

# ---- 9. Documento Word ---------------------------------------------------------------------
write_assessment_word = function(path, report, provider = "local", api_key = "", model = NULL, meta = list()) {
  missing = word_report_packages_missing(provider %in% c("claude", "chatgpt"))
  if (length(missing) > 0L) {
    stop("Pacotes ausentes: ", paste(missing, collapse = ", "), ". Execute 01_INSTALAR_DEPENDENCIAS.cmd.", call. = FALSE)
  }
  narrative = word_report_resolve_narrative(report, provider, api_key, model)
  txt = narrative$text
  # Quando a IA redigiu a subseção, saem os parágrafos fixos de interpretação; ficam só as chamadas de figuras e tabelas.
  ai_on = function(key) !identical(narrative$source, "local") && key %in% (narrative$replaced %||% character())
  a = report$assessment
  k = a$kpis
  mode = toupper(report$meta$evidence_mode %||% "HYBRID")
  meta = word_report_meta(meta)
  app_version = as.character(report$meta$version %||% "")
  n_resp = word_report_n_respondents(report)
  db = word_report_ref_db(meta$year, app_version)
  used = new.env()
  used$keys = character()
  safe = function(expr) tryCatch(expr, error = function(e) NULL)
  pct = function(x, d = 1) if (word_report_num(x)) format_percent_br(x, d) else "n/d"

  # ---- Montagem em blocos: o texto e os objetos de cada parte do documento ------------------
  seqs = list(body = list(), A = list(), P = list(), R = list())
  cur = "body"
  push = function(b) {
    seqs[[cur]][[length(seqs[[cur]]) + 1L]] <<- b
    invisible(NULL)
  }
  cite = function(x) word_report_cite_resolve(x, db, used)
  body = function(paragraphs) {
    for (p in paragraphs) {
      if (is.null(p) || length(p) == 0L || is.na(p) || !nzchar(p)) next
      p = cite(p)
      if (grepl("^[a-z]\\) ", p)) {
        push(list(type = "item", text = p, cm = word_report_est_text_cm(p, indent = FALSE, left_cm = 1.25)))
      } else {
        push(list(type = "body", text = p, cm = word_report_est_text_cm(p)))
      }
    }
  }
  # Alíneas a), b)...: letras minúsculas seguidas de parêntese.
  items = function(values) {
    for (i in seq_along(values)) {
      p = paste0(letters[[i]], ") ", cite(values[[i]]))
      push(list(type = "item", text = p, cm = word_report_est_text_cm(p, indent = FALSE, left_cm = 1.25)))
    }
  }
  h1 = function(text, first = FALSE) push(list(type = "h1", text = text, first = first, cm = if (first) 1.4 else 2.0))
  h2 = function(text) push(list(type = "h2", text = text, cm = 2.0))
  apx = function(name, title) push(list(type = "apx", name = name, title = title, cm = 0))
  obj = function(kind, id, title, prio = 99, plot = NULL, data = NULL, height = 3.6) {
    if (identical(kind, "figura")) {
      if (is.null(plot)) return(invisible(FALSE))
      cm = height * 2.54 + 2.6
    } else {
      if (!is.data.frame(data) || nrow(data) == 0L) return(invisible(FALSE))
      cm = 1.3 + (if (nchar(title) > 62L) 0.5 else 0) + wx_table_height_cm(data) + 1.0
    }
    push(list(type = "obj", kind = kind, id = id, title = title, prio = prio, plot = plot, data = data,
              height = height, cm = cm))
    invisible(TRUE)
  }

  asm_value = function(item) {
    asm = report$assumptions
    if (!is.data.frame(asm) || !all(c("Item", "Valor") %in% names(asm))) return(NA_character_)
    v = asm$Valor[match(item, asm$Item)]
    if (length(v) == 0L || is.na(v) || !nzchar(as.character(v))) NA_character_ else as.character(v)
  }
  method_info = list(
    replications = asm_value("Replicações por configuração"),
    horizon = asm_value("Horizonte da missão (h)"),
    target_ready = asm_value("Meta M (viaturas prontas)"),
    coverage = asm_value("Cobertura mínima P6 por célula"),
    only_consistent = asm_value("AHP: só matrizes com RC < 0,10")
  )

  # ---- 1 INTRODUÇÃO ---------------------------------------------------------------------------
  h1("1 INTRODUÇÃO", first = TRUE)
  body(word_report_text_intro(n_resp, mode))

  # ---- 2 REFERENCIAL TEÓRICO ------------------------------------------------------------------
  h1("2 REFERENCIAL TEÓRICO")
  for (section in word_report_text_theory()) {
    h2(section$title)
    body(section$paragraphs)
  }

  # ---- 3 METODOLOGIA --------------------------------------------------------------------------
  h1("3 METODOLOGIA")
  body("Esta seção descreve como a pesquisa foi conduzida: o delineamento, a base de dados, a priorização por AHP–TOPSIS, a simulação e o teste das intervenções.")
  for (section in word_report_text_method(n_resp, mode, method_info)) {
    h2(section$title)
    body(section$paragraphs)
    if (startsWith(section$title, "3.4")) obj("quadro", "enq_sim", "Enquadramento do simulador segundo a classificação dos modelos de simulação", prio = 7.5, data = simulation_framing_table())
  }

  # ---- 4 ANÁLISE E DISCUSSÃO ------------------------------------------------------------------
  h1("4 ANÁLISE E DISCUSSÃO")
  body(c(
    "Esta seção apresenta e interpreta os resultados da execução: a prioridade dos sistemas expressa o julgamento dos especialistas, e a disponibilidade resulta da simulação de eventos discretos repetida em replicações de Monte Carlo (subseção 3.4). O {apx:redacao} descreve a origem do texto de interpretação."
  ))

  h2("4.1 Síntese dos indicadores da execução")
  body(c(
    txt$sintese,
    if (!ai_on("sintese")) "A disponibilidade operacional simulada (Ao) inclui as esperas logísticas e administrativas, e não apenas o reparo ativo {p:lacey}.",
    "A {tab:kpi} reúne os indicadores principais da execução: as disponibilidades simuladas, o ganho, a disponibilidade observada nos registros, a meta e as probabilidades de cumprir a missão."
  ))
  kpi_table = data.frame(
    Indicador = c("Disponibilidade simulada – referência", "Disponibilidade simulada – com intervenções", "Ganho de disponibilidade",
                  "Disponibilidade observada nos registros (Ao)", "Meta de prontidão da missão",
                  "Probabilidade de cumprir a missão – referência", "Probabilidade de cumprir a missão – com intervenções"),
    Valor = c(pct(k$ao_reference), pct(k$ao_improved), if (word_report_num(k$ao_gain)) format_pp_br(k$ao_gain) else "n/d",
              pct(k$ao_observed), pct(k$availability_target, 0), pct(k$success_reference), pct(k$success_improved)),
    stringsAsFactors = FALSE
  )
  obj("tabela", "kpi", "Indicadores-chave da execução", prio = 1, data = kpi_table)
  base_reps = if (is.list(report$improvements)) report$improvements$base_replications else NULL
  ao_hist = safe(chart_availability_histogram(base_reps, k$availability_target, titles = FALSE))
  if (!is.null(ao_hist)) {
    body(paste0("A {fig:ao_hist} mostra como a disponibilidade simulada da situação de referência variou entre as ", nrow(base_reps),
                " replicações de Monte Carlo, com a média (μ) e as faixas de um e de dois desvios padrão."))
    obj("figura", "ao_hist", "Distribuição da disponibilidade simulada nas replicações da situação de referência", prio = 2.5, plot = ao_hist, height = 3.7)
  }

  h2("4.2 Prioridades segundo os especialistas")
  body(txt$prioridades)
  contributions = a$contributions
  rank_table = NULL
  rank_plot = NULL
  if (is.data.frame(contributions) && nrow(contributions) > 0L) {
    top = utils::head(contributions, 10L)
    rank_table = data.frame(
      `Posição` = top$rank, Sistema = word_report_pretty_name(top$alternative_name), `Coeficiente TOPSIS` = round(top$coefficient, 3),
      `Critério que mais pesa` = top$dominant_criterion_label, `Subcritério dominante` = top$dominant_subcriterion_label,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    rank_plot = safe(chart_bottlenecks(contributions, titles = FALSE))
    body(paste0(
      "A {tab:rank_tab} apresenta os dez primeiros sistemas do ranking TOPSIS",
      if (!is.null(rank_plot)) ", e a {fig:rank_fig} mostra a contribuição de cada critério à prioridade" else "",
      if (ai_on("prioridades")) "." else ". O resultado é lido como apoio à decisão, e não como decisão automática, e como hipótese a ser confrontada com os registros, na linha dos estudos que combinam AHP e TOPSIS em manutenção {p:velmurugan}."
    ))
    obj("tabela", "rank_tab", "Ranking TOPSIS dos sistemas", prio = 5, data = rank_table)
    obj("figura", "rank_fig", "Contribuição de cada critério à prioridade dos sistemas", prio = 3, plot = rank_plot, height = 4)
  }

  mc = report$multicriteria
  h2("4.3 Pesos dos critérios")
  body(txt$pesos)
  weights_plot = safe(chart_ahp_weights(mc, titles = FALSE))
  cr_table = NULL
  if (is.list(mc) && is.data.frame(mc$ahp$consistency) && nrow(mc$ahp$consistency) > 0L) {
    x = mc$ahp$consistency
    group_label = c(criteria = "Critérios", sub_occurrence = "Subcritérios de falha e ocorrência",
                    sub_downtime = "Subcritérios de impacto na disponibilidade", sub_supply = "Subcritérios de suprimento",
                    sub_support = "Subcritérios de manutenção e apoio")
    cr_table = data.frame(Grupo = ifelse(x$hierarchy_group %in% names(group_label), unname(group_label[x$hierarchy_group]), x$hierarchy_group), `Estimável` = ifelse(x$estimable, "sim", "não"),
                          Avaliadores = x$respondents, RC = round(x$CR, 3), `Situação` = x$status,
                          check.names = FALSE, stringsAsFactors = FALSE)
  }
  if (!is.null(weights_plot) || is.data.frame(cr_table)) {
    body(paste0(
      if (!is.null(weights_plot)) "A {fig:w_fig} apresenta os pesos dos critérios e dos subcritérios" else "",
      if (!is.null(weights_plot) && is.data.frame(cr_table)) ", e a " else "",
      if (is.data.frame(cr_table)) paste0(if (is.null(weights_plot)) "A " else "", "{tab:cr_tab} mostra a razão de consistência de cada grupo de comparações, cujo limite é 0,10 {p:saaty}") else "",
      "."
    ))
  }
  obj("figura", "w_fig", "Pesos AHP dos critérios e subcritérios", prio = 6, plot = weights_plot, height = 3.8)
  obj("tabela", "cr_tab", "Consistência das matrizes AHP agregadas (RC < 0,10)", prio = 9, data = cr_table)

  stability = if (is.list(mc)) tryCatch(mc_rank_stability(mc), error = function(e) NULL) else NULL
  sensitivity = if (is.list(mc)) tryCatch(mc_method_sensitivity(mc), error = function(e) NULL) else NULL
  robust_text = mc_robustness_text(mc, stability, sensitivity)
  h2("4.4 Consistência, dispersão e robustez do AHP–TOPSIS")
  body(c(
    if (!ai_on("robustez")) "Esta subseção examina se os especialistas foram consistentes, o quanto divergiram entre si e se o ranking TOPSIS se mantém quando a amostra de avaliadores, os pesos ou a normalização mudam. Como os métodos multicritério podem produzir ordenações distintas conforme os pesos e a normalização {p:salabun}, a análise de robustez complementa o ranking e responde à hipótese H1.",
    txt$robustez %||% robust_text,
    "Os gráficos e as tabelas de apoio constam no {apx:robustez}."
  ))

  h2("4.5 Tempo parado na simulação")
  body(c(
    txt$processo
  ))
  p = a$process
  dt_plot = NULL
  dt_table = NULL
  if (is.data.frame(p) && nrow(p) > 0L && sum(p$hours) > 0) {
    dt_plot = safe(chart_downtime(p, titles = FALSE))
    dt_table = data.frame(Componente = p$component, `Horas paradas (média)` = round(p$hours, 1),
                          Parcela = vapply(p$share, format_percent_br, character(1), digits = 0),
                          check.names = FALSE, stringsAsFactors = FALSE)
    cycle_share = sum(p$share[p$component %in% c("Reparo corretivo", "Diagnóstico", "Teste e liberação")])
    part_share = sum(p$share[p$component %in% "Espera por peça"])
    if (ai_on("processo")) body(paste0(if (!is.null(dt_plot)) "A {fig:dt_fig} e a {tab:dt_tab} apresentam" else "A {tab:dt_tab} apresenta", " as parcelas do tempo parado na situação de referência."))
    else body(paste0(
      "A decomposição separa o reparo ativo das esperas por peças, por equipes e por liberação administrativa, como na distinção de {c:lacey} entre reparo e atrasos logísticos. ",
      if (!is.null(dt_plot)) "A {fig:dt_fig} e a {tab:dt_tab} apresentam" else "A {tab:dt_tab} apresenta", " as parcelas. ",
      if (cycle_share >= part_share) "O peso do ciclo de reparo é coerente com o relato dos fuzileiros navais norte-americanos de que a manutenção da JLTV pode demorar mais que a do HMMWV, pela necessidade de remover painéis blindados e pelo motor mais complexo {p:feickert}."
      else "O peso da espera por peça remete à articulação entre manutenção e estoque de sobressalentes, em que o prazo de reposição condiciona a regra de estocagem {p:scarf,targett}."
    ))
    obj("figura", "dt_fig", "Decomposição do tempo parado na situação de referência", prio = 7, plot = dt_plot, height = 3.4)
    obj("tabela", "dt_tab", "Decomposição do tempo parado", prio = 10, data = dt_table)
  }

  sysdt = safe(word_report_system_downtime(report$systems_simulation, contributions, mode))
  if (!is.null(sysdt)) {
    if (ai_on("processo")) body("A {fig:sys_pie} apresenta a parcela de cada sistema no tempo parado simulado da frota, e a {tab:sys_tab} traz as horas, as falhas e a posição de cada sistema na prioridade dos especialistas.")
    else body(sysdt$paragraphs)
    obj("figura", "sys_pie", "Participação de cada sistema no tempo parado simulado da frota (situação de referência)", prio = 0.5, plot = sysdt$plot, height = 3.2)
    obj("tabela", "sys_tab", "Tempo parado simulado por sistema (situação de referência)", prio = 9.5, data = sysdt$table)
  }

  h2("4.6 Teste das intervenções")
  body(txt$melhorias)
  r = a$improvements
  if (is.data.frame(r) && nrow(r) > 0L) {
    imp_plot = safe(chart_improvements(r, a$package, k$availability_target, titles = FALSE))
    imp_table = data.frame(
      Ordem = r$priority_rank, `Intervenção` = r$label, `O que muda` = r$change,
      Ao = vapply(r$new_availability, format_percent_br, character(1)),
      `Ganho` = vapply(r$delta_availability, format_pp_br, character(1)),
      `IC95%` = paste0(vapply(r$delta_ci_lower, format_pp_br, character(1)), " a ", vapply(r$delta_ci_upper, format_pp_br, character(1))),
      Resultado = r$conclusion, check.names = FALSE, stringsAsFactors = FALSE
    )
    body(paste0(
      if (!is.null(imp_plot)) "A {fig:imp_fig} mostra o ganho de cada intervenção, e o {quad:imp_quad} reúne as intervenções na ordem de prioridade." else "O {quad:imp_quad} reúne as intervenções na ordem de prioridade.",
      " Cada ganho vale para a magnitude testada; os custos não integram esta execução."
    ))
    obj("figura", "imp_fig", "Ganho de disponibilidade por intervenção testada (IC95%)", prio = 1.2, plot = imp_plot, height = 3.0)
    obj("quadro", "imp_quad", "Intervenções priorizadas pelo ganho de disponibilidade (simulação pareada; Ao = disponibilidade operacional com a intervenção)",
        prio = 4, data = imp_table)
  }

  simulation = report$simulation
  has_sim = is.list(simulation) && is.data.frame(simulation$summary) && nrow(simulation$summary) > 0L
  if (has_sim) {
    h2("4.7 Configurações simuladas")
    body(txt$simulacao)
    s = simulation$summary
    cfg_plot = safe(chart_configuration_availability(s, simulation$p_target))
    cfg_table = data.frame(
      `Configuração` = s$configuration_id,
      `Prob. de cumprir a missão` = vapply(s$success_probability, format_percent_br, character(1)),
      `IC95%` = paste0(vapply(s$ci_lower, format_percent_br, character(1)), " a ", vapply(s$ci_upper, format_percent_br, character(1))),
      `Disponibilidade média` = vapply(s$mean_availability, format_percent_br, character(1)),
      `Cumpre a meta` = ifelse(s$ci_lower >= simulation$p_target, "sim", "não"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    body(paste0(
      if (!is.null(cfg_plot)) "A {fig:cfg_fig} e a {tab:cfg_tab} comparam" else "A {tab:cfg_tab} compara",
      " as configurações simuladas.", if (!ai_on("simulacao")) " Comparar alternativas no modelo, antes de qualquer mudança na frota, exige atenção às armadilhas de interpretação da simulação {p:barth}." else ""
    ))
    obj("figura", "cfg_fig", "Disponibilidade média por configuração", prio = 11, plot = cfg_plot, height = 3.8)
    obj("tabela", "cfg_tab", "Resultado por configuração simulada", prio = 12, data = cfg_table)
  }

  h2(if (has_sim) "4.8 Confiabilidade do resultado e limitações" else "4.7 Confiabilidade do resultado e limitações")
  conf = a$confidence
  conf_table = NULL
  if (is.data.frame(conf) && nrow(conf) > 0L) {
    conf_table = data.frame(Item = conf$item, Valor = conf$value, `Situação` = conf$status, `Observação` = conf$note,
                            check.names = FALSE, stringsAsFactors = FALSE)
  }
  body(c(
    txt$limitacoes,
    if (!ai_on("limitacoes")) "Os parâmetros de entrada do simulador vêm dos registros da JLTV e das hipóteses declaradas, e os valores obtidos em outras frotas {p:osz2023a} servem de referência metodológica. A coerência dos registros com listas de materiais e estoques precisa ser verificada {p:perez}, e a vinculação entre manutenção e suprimento usa identificador padronizado, como o NSN {p:andrade}. A literatura técnica do programa {p:feickert} contextualiza a viatura e não traz taxas de falha da frota do CFN. Essas condições caracterizam o dado e o modelo.",
    if (is.data.frame(conf_table)) "O {quad:conf_quad} reúne as verificações de confiabilidade do resultado."
  ))
  obj("quadro", "conf_quad", "Verificações de confiabilidade", prio = 8, data = conf_table)

  # ---- 5 CONCLUSÃO (sem dado novo, citação ou ilustração) ---------------------------------------
  h1("5 CONCLUSÃO")
  body(txt$conclusao)
  body(txt$proximos_passos)

  # ---- Seleção dos objetos do corpo: respeita o limite de páginas --------------------------------
  budget = meta$max_pages - 1.0
  body_blocks = seqs$body
  is_obj = vapply(body_blocks, function(b) identical(b$type, "obj"), logical(1))
  mandatory = vapply(body_blocks,function(b) identical(b$id,"sys_pie"),logical(1))
  keep = !is_obj | mandatory
  cand = which(is_obj & !mandatory)
  if (length(cand) > 0L) {
    for (i in cand[order(vapply(body_blocks[cand], function(b) as.numeric(b$prio), numeric(1)))]) {
      trial = keep
      trial[[i]] = TRUE
      if (word_report_est_pages(body_blocks[trial]) <= budget) keep = trial
    }
  }
  deferred = body_blocks[is_obj & !keep]
  est_pages = word_report_est_pages(body_blocks[keep])
  seqs$body = body_blocks[keep]

  # ---- Apêndices (cada um em nova folha) --------------------------------------------------------
  cur = "A"
  apx("robustez", "ROBUSTEZ, CONSISTÊNCIA E DISPERSÃO DO AHP–TOPSIS")
  body("Este apêndice reúne os gráficos e as tabelas de apoio à subseção 4.4: consistência dos julgamentos, dispersão das notas e dos pesos, e estabilidade e sensibilidade do ranking.")
  obj("figura", "a_heat", "Notas agregadas (P6) por sistema e subcritério", plot = safe(chart_p6_heatmap(mc$aggregated_scores, contributions, titles = FALSE)), height = 4)
  obj("figura", "a_crhist", "Histograma da razão de consistência (RC) por avaliador e grupo", plot = safe(chart_ahp_cr_histogram(mc, titles = FALSE)), height = 4.2)
  obj("figura", "a_wdisp", "Dispersão dos pesos dos critérios entre avaliadores", plot = safe(chart_ahp_weight_dispersion(mc, titles = FALSE)), height = 3.6)
  obj("figura", "a_p6hist", "Distribuição das notas P6", plot = safe(chart_p6_histogram(mc, titles = FALSE)), height = 3.2)
  obj("figura", "a_p6box", "Notas P6 por sistema, na ordem do ranking TOPSIS", plot = safe(chart_p6_boxplot_by_system(mc, titles = FALSE)), height = 4)
  obj("figura", "a_rankprob", "Probabilidade de cada posição no ranking (reamostragem dos avaliadores)", plot = safe(chart_rank_probability(stability, titles = FALSE)), height = 4)
  obj("tabela", "a_stab", "Estabilidade da posição de cada sistema", data = safe(mc_stability_display(stability)))
  obj("tabela", "a_sens", "Sensibilidade do ranking a pesos e normalização", data = safe(mc_method_sensitivity_display(sensitivity)))
  if (is.list(simulation) && is.data.frame(simulation$recommended_replications) && nrow(simulation$recommended_replications) > 0L) {
    obj("figura", "a_mission", "Resultado das replicações da configuração recomendada",
        plot = safe(chart_mission_outcome(simulation$recommended_replications, simulation$target_ready, titles = FALSE)), height = 3.6)
  }

  cur = "P"
  asm = report$assumptions
  apx("premissas", "PREMISSAS E PARÂMETROS DA EXECUÇÃO")
  body(paste0("Este apêndice registra as premissas, os parâmetros técnicos e a assinatura das entradas da execução, gerada em ",
              report$meta$generated_at %||% "data não registrada", " pela versão ", app_version, " do aplicativo."))
  if (is.data.frame(asm) && nrow(asm) > 0L) obj("quadro", "p_asm", "Premissas, parâmetros técnicos e assinatura das entradas", data = asm)

  cur = "R"
  apx("redacao", "SOBRE A REDAÇÃO DESTE RELATÓRIO")
  fixed_note = "As seções 1, 2 e 3 e os parágrafos de contextualização da seção 4 são texto fixo do aplicativo e citam as fontes de referência do autor; o autor deve conferir o texto com essas fontes antes do uso."
  if (narrative$source == "local") {
    body(c("Os textos de interpretação foram gerados pelo próprio aplicativo a partir dos números desta execução, sem uso de inteligência artificial externa.", fixed_note))
  } else {
    body(c(paste0(
      "Os textos de interpretação foram redigidos por um modelo de inteligência artificial (", if (narrative$source == "claude") "Claude" else "ChatGPT",
      if (!is.null(narrative$model)) paste0(", modelo ", narrative$model) else "",
      ") a partir dos resultados completos desta execução (especialistas, registros, simulação, intervenções e configurações) e das orientações de redação do trabalho. O aplicativo gerou as tabelas e as figuras; a IA analisou os resultados e redigiu ", word_report_ai_scope_text(narrative$replaced), ". Cada número do texto foi conferido com os resultados da execução. O autor deve revisar o texto antes do uso, porque o modelo pode errar a interpretação."
    ), fixed_note))
  }
  if (!is.null(narrative$note)) body(narrative$note)

  # Apêndice dos objetos que não couberam no corpo (se houver).
  extras = list()
  if (length(deferred) > 0L) {
    extras = c(
      list(list(type = "apx", name = "extras", title = "FIGURAS, QUADROS E TABELAS COMPLEMENTARES", cm = 0)),
      list(list(type = "body", cm = 0, text = paste0(
        "Este apêndice reúne as figuras, os quadros e as tabelas que, em razão do limite de extensão do corpo do trabalho (", meta$max_pages,
        " páginas, da Introdução à Conclusão), não constam nas subseções da seção 4. Cada um deles é citado no texto da subseção correspondente."
      ))),
      deferred
    )
  }

  # ---- Letras dos apêndices e numeração dos objetos, na ordem do documento ------------------------
  apx_letters = list()
  next_letter = 1L
  for (nm in c("robustez", if (length(deferred) > 0L) "extras", "premissas", "redacao")) {
    apx_letters[[nm]] = LETTERS[[next_letter]]
    next_letter = next_letter + 1L
  }
  label_of = c(tabela = "Tabela", quadro = "Quadro", figura = "Figura")
  prefix_of = c(tabela = "tab", quadro = "quad", figura = "fig")
  counters = c(tabela = 0L, quadro = 0L, figura = 0L)
  labels = list()
  numbers = list()
  place_names = c("body", "A", "extras", "P")
  place_blocks = list(body = seqs$body, A = seqs$A, extras = extras, P = seqs$P)
  for (place in place_names) {
    letter = if (identical(place, "body")) NA_character_ else apx_letters[[if (identical(place, "A")) "robustez" else if (identical(place, "extras")) "extras" else "premissas"]]
    for (b in place_blocks[[place]]) {
      if (!identical(b$type, "obj")) next
      counters[[b$kind]] = counters[[b$kind]] + 1L
      numbers[[b$id]] = counters[[b$kind]]
      labels[[paste0(prefix_of[[b$kind]], ":", b$id)]] =
        paste0(label_of[[b$kind]], " ", counters[[b$kind]], if (!is.na(letter)) paste0(" (Apêndice ", letter, ")") else "")
    }
  }
  resolve_num = function(x) {
    for (key in names(labels)) x = gsub(paste0("{", key, "}"), labels[[key]], x, fixed = TRUE)
    for (nm in names(apx_letters)) x = gsub(paste0("{apx:", nm, "}"), paste0("Apêndice ", apx_letters[[nm]]), x, fixed = TRUE)
    x
  }

  # ---- Documento -------------------------------------------------------------------------------------
  # Seção final (corpo do trabalho): número da página no canto superior direito,
  # a 2 cm da borda superior e da borda direita, em Arial 10.
  header_par = officer::fpar(
    officer::run_word_field("PAGE", prop = officer::fp_text(font.family = "Arial", font.size = 10)),
    fp_p = officer::fp_par(text.align = "right", line_spacing = 1, padding = 0)
  )
  doc = officer::read_docx()
  doc = officer::body_set_default_section(doc, officer::prop_section(
    page_size = officer::page_size(orient = "portrait", width = 21 / 2.54, height = 29.7 / 2.54),
    page_margins = officer::page_mar(top = 3 / 2.54, bottom = 2 / 2.54, left = 3 / 2.54, right = 2 / 2.54,
                                     header = 2 / 2.54, footer = 1.25 / 2.54, gutter = 0),
    type = "nextPage",
    header_default = officer::block_list(header_par)
  ))
  add = function(xml) for (x in xml) doc <<- officer::body_add_xml(doc, x)
  source_par = function() {
    add(wx_par(wx_marked(paste0("**Fonte:** O Autor, ", meta$year, "."), 10), align = "both", line = 240,
               before = 60, after = 240, msize = 10))
  }
  render_obj = function(b) {
    caption = paste0(label_of[[b$kind]], " ", numbers[[b$id]], " – ", sub("\\.?$", ".", trimws(b$title)))
    if (identical(b$kind, "figura")) {
      file = tempfile(fileext = ".png")
      ok = tryCatch({
        # Legendas longas em uma única linha seriam cortadas na borda da figura: quebra em linhas curtas.
        fig = b$plot
        cap = tryCatch(fig$labels$caption, error = function(e) NULL)
        if (is.character(cap) && length(cap) == 1L && !is.na(cap) && !grepl("\n", cap, fixed = TRUE) && nchar(cap) > 92L) {
          fig = fig + ggplot2::labs(caption = paste(strwrap(cap, width = 92), collapse = "\n"))
        }
        ggplot2::ggsave(file, fig, width = 6.3, height = b$height, dpi = 170, bg = "white")
        file.exists(file)
      }, error = function(e) FALSE)
      if (!isTRUE(ok)) return(invisible(FALSE))
      add(wx_par(wx_run(caption, 12), align = "both", line = 240, before = 240, after = 120, keep = TRUE))
      doc <<- officer::body_add_img(doc, src = file, width = 6.3, height = b$height, style = "centered")
    } else {
      add(wx_par(wx_run(caption, 12), align = "both", line = 240, before = 240, after = 120, keep = TRUE))
      add(wx_table(b$data, kind = b$kind, size = 10))
    }
    source_par()
    invisible(TRUE)
  }
  render_block = function(b) {
    switch(b$type,
      body = add(wx_par(wx_run(resolve_num(b$text), 12), align = "both", line = 360, first = 1.5)),
      item = add(wx_par(wx_run(resolve_num(b$text), 12), align = "both", line = 360, left = 1.25, first = -0.63)),
      h1 = add(wx_par(wx_run(b$text, 12, bold = TRUE), align = "left", line = 360, before = if (isTRUE(b$first)) 0 else 360,
                      after = 360, keep = TRUE, outline = 0)),
      h2 = add(wx_par(wx_run(b$text, 12, bold = TRUE), align = "left", line = 360, before = 360, after = 360,
                      keep = TRUE, outline = 1)),
      apx = {
        add(wx_par(wx_run(paste0("APÊNDICE ", apx_letters[[b$name]]), 12, bold = TRUE), align = "center", line = 360,
                   page_break = TRUE, keep = TRUE, outline = 0))
        add(wx_par(wx_run(b$title, 12, bold = TRUE), align = "center", line = 360, keep = TRUE))
        add(wx_par("", align = "left", line = 360, rule = "exact"))
      },
      obj = render_obj(b)
    )
    invisible(NULL)
  }

  # ---- Folha de rosto ----------------------------------------------------------------------------
  title_text = toupper(sub("[[:punct:][:space:]…]+$", "", meta$title))
  subtitle_text = if (nzchar(meta$subtitle)) tolower(sub("[[:punct:][:space:]…]+$", "", meta$subtitle)) else ""
  add(wx_par(wx_run("ESCOLA SUPERIOR DE GUERRA", 14), align = "center", msize = 14))
  add(wx_blank(2L, 14))
  add(wx_par(wx_run(toupper(meta$author), 14), align = "center", msize = 14))
  add(wx_blank(12L, 14))
  add(wx_par(wx_run(paste0(title_text, if (nzchar(subtitle_text)) ":" else ""), 16, bold = TRUE), align = "center", msize = 16))
  if (nzchar(subtitle_text)) {
    add(wx_blank(2L, 14))
    add(wx_par(wx_run(subtitle_text, 14), align = "center", msize = 14))
    add(wx_blank(if (nchar(subtitle_text) > 55L) 5L else 6L, 14))
  } else {
    add(wx_blank(9L, 14))
  }
  add(wx_par(wx_run(paste0(
    meta$nature, " apresentado ao ", meta$sector, " da Escola Superior de Guerra como requisito parcial à obtenção do certificado do ",
    meta$course, "."), 10), align = "both", left = 8, msize = 10))
  add(wx_blank(1L, 10))
  add(wx_par(wx_run(paste0("Orientador: ", meta$advisor, "."), 10), align = "both", left = 8, msize = 10))
  add(wx_blank(12L, 10))
  add(wx_par(wx_run(meta$city, 12), align = "center"))
  add(wx_blank(1L, 12))
  add(wx_par(wx_run(meta$year, 12), align = "center"))

  # ---- Verso da folha de rosto: cessão de direitos autorais (fora da contagem) ------------------------
  add(wx_par(wx_run(paste0("© ", meta$year, " ESG"), 12), align = "left", line = 360, page_break = TRUE))
  add(wx_blank(1L, 12))
  add(wx_par(wx_run("Este trabalho, nos termos de legislação que resguarda os direitos autorais, é considerado propriedade da ESCOLA SUPERIOR DE GUERRA (ESG). É permitida a transcrição parcial de textos do trabalho, ou mencioná-los, para comentários e citações, desde que sem propósitos comerciais e que seja feita a referência bibliográfica completa.", 12),
             align = "both", left = 7.5, line = 360))
  add(wx_par(wx_run("Os conceitos expressos neste trabalho são de responsabilidade do autor e não expressam qualquer orientação institucional da ESG.", 12),
             align = "both", left = 7.5, line = 360))
  add(wx_blank(2L, 12))
  add(wx_par(wx_run(strrep("_", 30), 12), align = "left", left = 7.5))
  add(wx_par(wx_run(toupper(meta$author), 12), align = "left", left = 7.5, sect = wx_sect()))

  # ---- Resumo e Abstract (contados; número de página não exibido) ----------------------------------------
  add(wx_par(wx_run(title_text, 12, bold = TRUE), align = "center"))
  add(wx_par(wx_run(meta$author, 12), align = "right", before = 240))
  add(wx_par(wx_run("RESUMO", 12, bold = TRUE), align = "center", line = 360, before = 360))
  add(wx_par(wx_run(paste(txt$resumo, collapse = " "), 12), align = "both", line = 240, before = 240))
  abs_kw = word_report_resumo_local(report)
  add(wx_par(wx_marked(paste0("**Palavras-chave:** ", abs_kw$kw_pt), 12), align = "both", line = 240, before = 240))
  add(wx_par(wx_run("ABSTRACT", 12, bold = TRUE), align = "center", line = 360, before = 480))
  add(wx_par(wx_run(paste(txt$abstract, collapse = " "), 12), align = "both", line = 240, before = 240))
  add(wx_par(wx_marked(paste0("**Keywords:** ", abs_kw$kw_en), 12), align = "both", line = 240, before = 240,
             sect = wx_sect(start = 2L)))

  # ---- Corpo do trabalho -------------------------------------------------------------------------------
  for (b in seqs$body) render_block(b)

  # ---- Referências (só as fontes citadas), glossário e apêndices ----------------------------------------
  refs = word_report_references(db, used$keys)
  add(wx_par(wx_run("REFERÊNCIAS", 12, bold = TRUE), align = "center", line = 240, before = 480, after = 240, keep = TRUE, outline = 0))
  for (ref in refs) add(wx_par(wx_marked(ref, 12), align = "left", line = 240, after = 240))

  add(wx_par("", align = "left", line = 360, rule = "exact"))
  add(wx_glossary_table(word_report_glossary()))

  for (b in seqs$A) render_block(b)
  for (b in extras) render_block(b)
  for (b in seqs$P) render_block(b)
  for (b in seqs$R) render_block(b)

  tryCatch(
    doc <- officer::set_doc_properties(doc, title = title_text, creator = meta$author),
    error = function(e) NULL
  )
  print(doc, target = path)
  invisible(list(
    path = path, source = narrative$source,
    note = if(is.null(sysdt)||is.null(sysdt$plot)) paste(c(narrative$note,"Gráfico de pizza ausente: execute a simulação por sistema com horas paradas na referência antes de gerar o Word."),collapse=" ") else narrative$note,
    body_pages_estimate = est_pages, max_pages = meta$max_pages,
    deferred_objects = length(deferred), references = length(refs),
    systems_pie = !is.null(sysdt) && !is.null(sysdt$plot)
  ))
}

# FIM DO MODULO: jltv_word_report.R


jltv_aplicativo_completo = if (!identical(Sys.getenv("JLTV_CORE_ONLY"), "1")) {
  missing_packages = required_runtime_packages()[
    !vapply(required_runtime_packages(), requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages) > 0L) {
    stop(
      paste0(
        "Pacotes ausentes: ", paste(missing_packages, collapse = ", "),
        ". Execute no console: install.packages(c(",
        paste(shQuote(missing_packages), collapse = ", "), "))"
      ),
      call. = FALSE
    )
  }

  database_path = resolve_database_path()
  initialize_database(database_path)
  startup_connection = open_database(database_path)
  startup_asset_count = DBI::dbGetQuery(
    startup_connection, "SELECT COUNT(*) AS n FROM assets"
  )$n[[1]]
  DBI::dbDisconnect(startup_connection)
  seed_demo = !identical(trimws(Sys.getenv("JLTV_SEED_SYNTHETIC", unset = "1")), "0")
  if (startup_asset_count == 0L && seed_demo) seed_synthetic_database(database_path)

  application_theme = bslib::bs_theme(
    version = 5,
    bg = "#F4F7FA",
    fg = "#0B1F33",
    primary = "#1F5A94",
    secondary = "#198F8C",
    success = "#198F8C",
    warning = "#D99A2B",
    danger = "#B23A48"
  )

  application_css = shiny::tags$style(shiny::HTML(paste0(
    ".navbar{background:#0B1F33!important}.navbar-brand,.navbar-nav .nav-link{color:#fff!important}",
    ".app-subtitle{background:#102D49;color:#DCE8F3;padding:8px 20px;font-size:0.95rem}",
    ".evidence-banner{padding:12px 16px;margin:10px 0 16px;border-radius:6px;font-weight:700}",
    ".mode-SYNTHETIC{background:#F8D7DA;color:#842029;border:1px solid #F1AEB5}",
    ".mode-HYBRID{background:#FFF3CD;color:#664D03;border:1px solid #FFECB5}",
    ".mode-EMPIRICAL{background:#D1E7DD;color:#0F5132;border:1px solid #A3CFBB}",
    ".decision-card{padding:20px;border-left:8px solid #1F5A94;background:white}",
    ".decision-card h3{margin-top:0;color:#0B1F33}.small-note{font-size:.88rem;color:#526579}",
    ".form-section{font-weight:700;color:#0B1F33;margin-top:14px}",
    ".bslib-value-box .value-box-value{font-size:1.7rem}",
    ".workflow-strip{display:flex;gap:8px;flex-wrap:wrap;background:#F3F6F9;padding:8px 20px;border-bottom:1px solid #D9E1E8}",
    ".workflow-step{background:#fff;border:1px solid #C7D3DE;border-radius:16px;padding:5px 10px;font-size:.82rem;color:#0B1F33;font-weight:700}",
    ".step-intro{background:#EEF5FB;border-left:5px solid #1F5A94;padding:12px 16px;margin:8px 0 18px;border-radius:4px}",
    ".sync-status-card{font-weight:700;color:#0B1F33;padding:8px 0}",
    ".hierarchy-tree{padding:12px 4px}",
    ".hierarchy-node{background:#fff;border:1px solid #B8C7D5;border-radius:8px;padding:10px;text-align:center;box-shadow:0 1px 3px rgba(0,0,0,.06)}",
    ".hierarchy-objective{max-width:760px;margin:0 auto;background:#E7F0F8;border:2px solid #1F5A94}",
    ".hierarchy-objective span,.hierarchy-traceability span,.hierarchy-alternative span{display:block;font-weight:400;margin-top:4px}",
    ".hierarchy-arrow{text-align:center;font-size:1.8rem;color:#1F5A94;font-weight:700;line-height:1.2;margin:5px 0}",
    ".hierarchy-caption{text-align:center;font-weight:800;color:#0B1F33;margin:6px 0 10px}",
    ".hierarchy-criteria-grid{display:grid;grid-template-columns:repeat(4,minmax(180px,1fr));gap:12px;align-items:start}",
    ".hierarchy-branch{position:relative}",
    ".hierarchy-criterion{background:#DDECF7;border:2px solid #4F7FA7;font-weight:800;min-height:58px;display:flex;align-items:center;justify-content:center}",
    ".hierarchy-children{display:grid;gap:7px;margin-top:12px;padding-top:8px;border-top:2px solid #90AFC7}",
    ".hierarchy-subcriterion{background:#F8FAFC;min-height:54px;display:flex;align-items:center;justify-content:center;font-size:.9rem}",
    ".hierarchy-alt-grid{display:grid;grid-template-columns:repeat(5,minmax(130px,1fr));gap:8px}",
    ".hierarchy-alternative{background:#F7F9FB;font-size:.86rem}",
    ".hierarchy-traceability{max-width:900px;margin:0 auto;background:#F4F0FA;border:2px solid #7B5FA6}",
    "@media(max-width:1100px){.hierarchy-criteria-grid{grid-template-columns:repeat(2,1fr)}.hierarchy-alt-grid{grid-template-columns:repeat(2,1fr)}}",
    "@media(max-width:650px){.hierarchy-criteria-grid,.hierarchy-alt-grid{grid-template-columns:1fr}}",
    # v1.4.2 — identidade visual: marca na barra, ilustração por etapa.
    ".brand-lockup{display:inline-flex;align-items:center;gap:10px;font-weight:700;letter-spacing:.2px}",
    ".brand-badge{width:38px;height:38px;border-radius:50%;background:#E8ECE0;display:inline-flex;align-items:center;justify-content:center;box-shadow:0 0 0 2px #6E7F4F}",
    ".brand-badge img{height:28px;width:auto;display:block}",
    ".brand-version{font-size:.72rem;font-weight:600;color:#B9C9D8;border:1px solid #3C5A78;border-radius:10px;padding:1px 7px}",
    ".step-intro{display:flex;align-items:center;justify-content:space-between;gap:20px}",
    ".step-intro-text{flex:1 1 auto;min-width:0}.step-intro-text h3{margin-top:0}.step-intro-text p{margin-bottom:0}",
    ".step-intro-figure{flex:0 0 auto;margin:0;text-align:center}",
    ".step-intro-figure img{height:118px;width:auto;display:block;margin:0 auto}",
    ".step-intro-figure figcaption{font-size:.7rem;font-weight:700;letter-spacing:.6px;text-transform:uppercase;color:#4F6A3A;margin-top:4px}",
    ".step-intro.kind-modelo{background-color:#F3F7FB;background-image:linear-gradient(rgba(31,90,148,.07) 1px,transparent 1px),linear-gradient(90deg,rgba(31,90,148,.07) 1px,transparent 1px);background-size:18px 18px}",
    ".step-intro.kind-frota{background:linear-gradient(90deg,#EEF5FB 55%,#EEF2E6 100%);border-left-color:#6E7F4F}",
    ".step-intro.kind-marca .step-intro-figure img{height:72px;opacity:.9}",
    "@media(max-width:768px){.step-intro-figure{display:none}}",
    ".run-hint{background:#FFF7E6;border-left:4px solid #D99A2B;padding:8px 12px;margin:8px 0;border-radius:4px;font-size:.9rem}",
    # v1.5.0 — painel de resultado, etapas guiadas e leitura dos gráficos.
    ".workflow-strip{align-items:center}.workflow-label{font-size:.8rem;font-weight:700;color:#526579;margin-right:4px}",
    ".workflow-strip a.workflow-step{text-decoration:none;cursor:pointer}",
    ".workflow-step.done{background:#D1E7DD;border-color:#A3CFBB;color:#0F5132}",
    ".workflow-step.pending{background:#fff;color:#0B1F33}",
    ".result-hero{display:flex;gap:24px;align-items:center;justify-content:space-between;background:linear-gradient(100deg,#0B1F33 0%,#1F5A94 100%);color:#fff;border-radius:10px;padding:20px 24px;margin:6px 0 16px}",
    ".result-hero h2{color:#fff;margin:0 0 6px;font-size:1.45rem;font-weight:800}.result-hero p{margin:0;color:#DCE8F3;max-width:780px}",
    ".result-hero-actions{display:flex;flex-wrap:wrap;gap:8px;justify-content:flex-end;min-width:300px;max-width:430px}",
    ".result-hero-actions .btn-primary{background:#D99A2B;border-color:#D99A2B;color:#0B1F33;font-weight:800}",
    ".result-hero-actions .btn-outline-light{color:#fff;border-color:#9DB7CF}",
    ".steps-grid{display:grid;grid-template-columns:repeat(6,minmax(140px,1fr));gap:10px;margin-bottom:14px}",
    ".step-card{background:#fff;border:1px solid #D9E1E8;border-radius:8px;padding:10px 12px;border-top:4px solid #C7D3DE}",
    ".step-card.done{border-top-color:#198F8C}.step-card.pending{border-top-color:#D99A2B}",
    ".step-card .step-title{font-weight:700;color:#0B1F33;font-size:.9rem}.step-card .step-detail{font-size:.8rem;color:#526579;margin:4px 0 6px}",
    ".step-status{font-size:.7rem;font-weight:800;text-transform:uppercase;letter-spacing:.5px;color:#526579}.step-card.done .step-status{color:#198F8C}.step-card.pending .step-status{color:#B07A14}",
    ".findings-card ul{margin:0;padding-left:1.1rem}.findings-card li{margin:0 0 7px;line-height:1.45}",
    ".how-to-read{font-size:.86rem;color:#24384C;background:#F3F6F9;border-left:4px solid #1F5A94;padding:8px 12px;margin-top:8px;border-radius:4px}",
    ".how-to-read .reading-now{margin-top:4px;color:#0B1F33}",
    ".lever-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(250px,1fr));gap:10px}",
    ".lever-item{border:1px solid #D9E1E8;border-radius:8px;padding:8px 10px 4px;background:#fff}.lever-item .form-group{margin-bottom:6px}",
    ".lever-meta{font-size:.76rem;color:#526579;margin-bottom:4px}",
    ".next-step{display:flex;justify-content:flex-end;gap:8px;margin:18px 0 8px}",
    ".bslib-value-box .value-box-value{font-size:1.45rem}",
    ".step-intro ul{margin:6px 0 0;padding-left:1.1rem}.step-intro li{margin-bottom:2px}",
    "@media(max-width:1200px){.steps-grid{grid-template-columns:repeat(3,1fr)}.result-hero{flex-direction:column;align-items:flex-start}.result-hero-actions{justify-content:flex-start;max-width:none}}"

  )))

  # v1.4.2 — imagens servidas da subpasta www/. O iniciador carrega app.R com
  # chdir = TRUE e executa shiny::runApp(objeto), situação em que a pasta www/
  # não é publicada automaticamente; por isso o caminho é registrado aqui.
  # Sem a pasta (ou sem um arquivo), a interface segue sem a imagem.
  application_asset_dir = file.path(getwd(), "www")
  application_assets = if (dir.exists(application_asset_dir)) {
    shiny::addResourcePath("jltv_img", normalizePath(application_asset_dir, winslash = "/"))
    list.files(application_asset_dir)
  } else character()
  asset_image = function(file, alt) {
    if (!file %in% application_assets) return(NULL)
    shiny::tags$img(src = paste0("jltv_img/", file), alt = alt)
  }
  # Lógica das imagens: ilustração colorida = frota observada (dados de entrada
  # e resultados); desenho técnico com retícula = modelo (decisão e simulação);
  # silhueta = identidade do aplicativo (barra, ícone, relatório e etapa 5).
  step_intro_figures = list(
    frota = list(file = "jltv_frota.png", caption = "Frota observada",
                 alt = "Ilustração da viatura JLTV em serviço"),
    modelo = list(file = "jltv_modelo.png", caption = "Representação do modelo",
                  alt = "Desenho técnico da viatura JLTV com retícula de análise"),
    marca = list(file = "jltv_marca.png", caption = "Registro e auditoria",
                 alt = "Silhueta frontal da viatura JLTV")
  )
  step_intro_ui = function(title, text, kind = c("frota", "modelo", "marca")) {
    kind = match.arg(kind)
    figure = step_intro_figures[[kind]]
    image = asset_image(figure$file, figure$alt)
    shiny::tags$div(
      class = paste("step-intro", paste0("kind-", kind)),
      shiny::tags$div(
        class = "step-intro-text", shiny::tags$h3(title),
        if (is.character(text)) shiny::tags$p(text) else shiny::tags$div(text)
      ),
      if (!is.null(image)) shiny::tags$figure(
        class = "step-intro-figure", image, shiny::tags$figcaption(figure$caption)
      )
    )
  }
  # Relatório HTML exportado: autocontido, por isso a marca vai embutida.
  report_brand_uri = if ("jltv_marca.png" %in% application_assets) {
    brand_file = file.path(application_asset_dir, "jltv_marca.png")
    paste0("data:image/png;base64,", jsonlite::base64_enc(
      readBin(brand_file, "raw", file.info(brand_file)$size)
    ))
  } else ""
  brand_title = shiny::tags$span(
    class = "brand-lockup",
    if ("jltv_marca.png" %in% application_assets) shiny::tags$span(
      class = "brand-badge", asset_image("jltv_marca.png", "")
    ),
    "JLTV Sustainment Decision Lab",
    shiny::tags$span(class = "brand-version", paste0("v", JLTV_MODEL_VERSION))
  )

  evidence_output = function(id) shiny::uiOutput(id)
  plot_card = function(title, output, note = NULL) {
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(title),
      output,
      if (is.character(note)) shiny::tags$p(class = "small-note", note) else note
    )
  }
  # v1.5.0 — botão de navegação para a etapa seguinte.
  next_step_ui = function(target, label) {
    shiny::tags$div(
      class = "next-step",
      shiny::actionButton(paste0("next_to_", target), label, icon = shiny::icon("arrow-right"), class = "btn btn-primary")
    )
  }


  hierarchy_tree_ui = function() {
    spec = questionnaire_hierarchy_spec()
    alternatives = questionnaire_alternative_spec()
    branch = function(title, items) {
      shiny::tags$div(
        class = "hierarchy-branch",
        shiny::tags$div(class = "hierarchy-node hierarchy-criterion", title),
        shiny::tags$div(
          class = "hierarchy-children",
          shiny::tagList(purrr::map(unname(items), function(item) {
            shiny::tags$div(class = "hierarchy-node hierarchy-subcriterion", item)
          }))
        )
      )
    }
    bslib::card(
      bslib::card_header("Árvore hierárquica do modelo AHP–TOPSIS"),
      shiny::tags$div(
        class = "hierarchy-tree",
        shiny::tags$div(
          class = "hierarchy-node hierarchy-objective",
          shiny::tags$strong("OBJETIVO"),
          shiny::tags$span(
            "Priorizar sistemas/conjuntos associados à indisponibilidade da frota JLTV para orientar manutenção, suprimento e apoio à decisão."
          )
        ),
        shiny::tags$div(class = "hierarchy-arrow", "↓"),
        shiny::tags$div(
          class = "hierarchy-caption",
          "CRITÉRIOS E SUBCRITÉRIOS — P1 a P5 / AHP"
        ),
        shiny::tags$div(
          class = "hierarchy-criteria-grid",
          branch(unname(spec$criteria[["OCCURRENCE"]]), spec$sub_occurrence),
          branch(unname(spec$criteria[["DOWNTIME"]]), spec$sub_downtime),
          branch(unname(spec$criteria[["SUPPLY"]]), spec$sub_supply),
          branch(unname(spec$criteria[["SUPPORT"]]), spec$sub_support)
        ),
        shiny::tags$div(class = "hierarchy-arrow", "↓"),
        shiny::tags$div(
          class = "hierarchy-caption",
          "ALTERNATIVAS — P6 / TOPSIS"
        ),
        shiny::tags$div(
          class = "hierarchy-alt-grid",
          shiny::tagList(purrr::map(names(alternatives), function(code) {
            shiny::tags$div(
              class = "hierarchy-node hierarchy-alternative",
              shiny::tags$strong(code),
              shiny::tags$span(unname(alternatives[[code]]))
            )
          }))
        ),
        shiny::tags$div(class = "hierarchy-arrow", "↓"),
        shiny::tags$div(
          class = "hierarchy-node hierarchy-traceability",
          shiny::tags$strong("P7 — RASTREABILIDADE"),
          shiny::tags$span(
            "Peça/componente, ocorrência/necessidade e registro/fonte para confirmação; P7 não altera pesos nem notas."
          )
        )
      ),
      shiny::tags$p(
        class = "small-note",
        "A árvore é gerada a partir da mesma especificação interna usada nos cálculos; assim, a estrutura visual e o processamento permanecem alinhados."
      )
    )
  }

  # Documentação independente das etapas de execução e dos parâmetros físicos.
  documentation_references = local({
    # Mesma lista do relatório em Word: cada fonte do cadastro, na referência ABNT que o relatório imprime.
    db = word_report_ref_db(format(Sys.Date(), "%Y"), JLTV_MODEL_VERSION)
    db = db[setdiff(names(db), "app")]
    ord = order(vapply(db, function(x) x$sort, character(1)), method = "radix")
    drive_links = c(
      md4001 = "https://drive.google.com/file/d/15uRniCH39jc-g39EbEXZqIXaSCvDOsmh/view",
      md40n02 = "https://drive.google.com/file/d/1cl4mOvdn1shOeTdc0579q-wvB7wMaxJg/view",
      md4401 = "https://drive.google.com/file/d/1AubPcT8e5lHghI4bRyBMwnOLMmA7GPeO/view",
      eua20 = "https://drive.google.com/file/d/1IMBBUIqWUB_pfghAN_-f43CrMXzKnx6_/view",
      robinson = "https://drive.google.com/file/d/1IHs0c8qHSyp3CVwSI2bTVdgMfb2z5BoI/view",
      aguaron = "https://drive.google.com/file/d/1QJyewppg7nezlrAuv5ZtqodL-xsm-Zax/view",
      djukic = "https://drive.google.com/file/d/15ouA-KULJNxm9JEgLxAkICvsitXx3T0T/view",
      smith = "https://drive.google.com/file/d/1D4i2UPL9ULaFkggg1Cqg3WlrWwFO888j/view",
      lacey = "https://drive.google.com/file/d/1D4i2UPL9ULaFkggg1Cqg3WlrWwFO888j/view",
      targett = "https://drive.google.com/file/d/1D4i2UPL9ULaFkggg1Cqg3WlrWwFO888j/view",
      smithpt = "https://drive.google.com/file/d/1D4i2UPL9ULaFkggg1Cqg3WlrWwFO888j/view",
      simmer = "https://doi.org/10.18637/jss.v090.i02"
    )
    cited = lapply(names(db)[ord], function(k) list(
      group = "Fontes citadas no relatório em Word",
      title = gsub("**", "", db[[k]]$ref, fixed = TRUE),
      use = "Citada no texto do relatório em Word.",
      url = if (k %in% names(drive_links)) unname(drive_links[[k]]) else ""
    ))
    lectures = list(
      list(group = "Material de aula (não é referência bibliográfica)", title = "Aula 4 — TOPSIS.", use = "Conferência do processamento TOPSIS.", url = "https://drive.google.com/file/d/1c9xnyv1nSFgtH3FehEI_u-g3gb3bo7wq/view"),
      list(group = "Material de aula (não é referência bibliográfica)", title = "Aula 5 — AHP no Excel.", use = "Conferência do processamento AHP.", url = "https://drive.google.com/file/d/1i8vUK-6Dn8_iGKeMl-hyy6JawLRAVwYG/view"),
      list(group = "Material de aula (não é referência bibliográfica)", title = "Aula 6 — AHP no R.", use = "Conferência do processamento AHP.", url = "https://drive.google.com/file/d/13pJ6f6vMy44lfiEO-Iv6p9jdBJzckXCr/view")
    )
    c(cited, lectures)
  })
  documentation_ui = function() {
    node = function(title, text) shiny::tags$div(class="jltv-doc-node", shiny::tags$strong(title), shiny::tags$p(text))
    arrow = function(text="↓") shiny::tags$div(class="jltv-doc-arrow", text)
    software = shiny::tagList(lapply(c("R", required_runtime_packages()), function(pkg) {
      version = if (pkg == "R") as.character(getRversion()) else as.character(utils::packageVersion(pkg))
      cite = tryCatch(paste(format(utils::citation(if (pkg == "R") NULL else pkg), style="text"), collapse="\n\n"), error=function(e) "Consultar a documentação instalada deste pacote.")
      shiny::tags$details(shiny::tags$summary(paste(pkg, "— versão", version)), shiny::tags$pre(class="jltv-doc-citation", cite))
    }))
    shiny::tags$div(class="jltv-docs",
      shiny::tags$style(shiny::HTML(".jltv-docs{max-width:1250px;margin:auto;padding:12px}.jltv-doc-node{border:1px solid #abc0d3;border-top:4px solid #1F5A94;border-radius:8px;padding:14px;background:#fff;text-align:center}.jltv-doc-node p{margin:6px 0 0}.jltv-doc-arrow{text-align:center;font-size:1.1rem;font-weight:700;padding:8px;color:#1F5A94}.jltv-doc-lanes{display:grid;grid-template-columns:1fr 1fr;gap:18px}.jltv-doc-lane{padding:12px;background:#eef5fb;border-radius:8px}.jltv-doc-ref{padding:12px 0;border-bottom:1px solid #d9e1e8}.jltv-doc-citation{white-space:pre-wrap;overflow-wrap:anywhere;padding:12px;background:#f4f7fa}.jltv-docs details{margin:10px 0}@media(max-width:760px){.jltv-doc-lanes{grid-template-columns:1fr}}")),
      shiny::tags$h2("Referências e funcionamento"),
      shiny::tags$p("Esta aba pode ser consultada a qualquer momento. Sua leitura não é uma etapa de execução e não gera marca de concluído."),
      bslib::navset_card_tab(
        bslib::nav_panel("Como o aplicativo funciona",
          shiny::tags$h3("Do problema à comparação de melhorias"),
          shiny::tags$p("O aplicativo reúne prioridades dos especialistas e registros da frota para testar cenários de manutenção. O resultado depende da qualidade dos dados e das hipóteses adotadas."),
          shiny::tags$div(role="group", `aria-label`="Organograma do funcionamento do aplicativo",
            node("JLTV Sustainment Decision Lab", "Objetivo: apoiar a decisão sobre a sustentação da frota, com a análise da disponibilidade e o teste de intervenções de manutenção e de suprimento."),
            arrow("↓ Duas entradas complementares"),
            shiny::tags$div(class="jltv-doc-lanes",
              shiny::tags$div(class="jltv-doc-lane",
                node("1. Questionário dos especialistas", "P1–P5 comparam fatores; P6 avalia alternativas; P7 indica evidências a confirmar."), arrow(),
                node("2. AHP e TOPSIS: prioridades", "O AHP calcula pesos e verifica consistência. O TOPSIS usa os pesos para ordenar alternativas. P7 mantém a ligação com possíveis registros.")),
              shiny::tags$div(class="jltv-doc-lane",
                node("1. Registros da frota", "Viaturas, exposição ao uso, falhas, ordens de serviço, tempos, peças, estoques e recursos."), arrow(),
                node("2. Qualidade e parâmetros físicos", "O aplicativo verifica os registros e calcula indicadores observados quando há dados suficientes. Lacunas exigem hipóteses identificadas."))),
            arrow("↓ Prioridades orientam a investigação; registros e hipóteses sustentam os parâmetros"),
            node("3. Simulação por sistema/componente", "O modo SYS-1 preserva a identidade A1–A10, componente, modo de falha e PN/NSN em cada evento. Usa relógios por item, equipes compartilhadas e estoque por peça. O modo agregado anterior continua separado."), arrow(),
            node("4. Intervenções direcionadas", "No SYS-1, compara uma mudança no sistema/item escolhido ou testa cada sistema separadamente. Mantém sementes e demais parâmetros; informa quando um PN compartilhado amplia o alcance."), arrow(),
            node("5. Causa física e confronto", "Apresenta sistemas e componentes responsáveis pelas horas de viatura parada, etapas, disponibilidade, missão e ganhos pareados. Questionário, registros classificados e simulação permanecem evidências distintas."), arrow(),
            node("6. Relatório Excel", "Reúne resultados, hipóteses e informações de auditoria para revisão e apresentação.")),
          shiny::tags$h3("Como ler o resultado"),
          shiny::tags$ul(
            shiny::tags$li("Disponibilidade operacional (Ao): parcela média do tempo em que a frota está disponível."),
            shiny::tags$li("Probabilidade de missão: frequência simulada de atender ao mínimo pronto sob a tolerância configurada de horas acumuladas abaixo de M; é diferente da disponibilidade média."),
            shiny::tags$li("AHP e TOPSIS expressam prioridades. Notas do questionário não se transformam em tempo entre falhas (MTBF) ou tempo de reparo (MTTR)."),
            shiny::tags$li("Dados sintéticos demonstram o método; dados híbridos combinam registros e hipóteses; dados empíricos precisam de registros suficientes."),
            shiny::tags$li("A simulação compara possibilidades condicionadas aos dados. Ela não garante o desempenho futuro da frota."))
        ),
        bslib::nav_panel("Enquadramento do simulador",
          shiny::tags$h3("Que tipo de simulação é esta?"),
          shiny::tags$p("É uma simulação de eventos discretos (SED), dinâmica, estocástica e de alta resolução, repetida em replicações de Monte Carlo. Segundo a classificação clássica dos modelos de simulação (Taha, 2008; Prado, 2014), Monte Carlo é a simulação estática, uma fotografia repetida; a SED é dinâmica, um filme em que o estado muda nos instantes de eventos (falha, fim de reparo, chegada de peça). O aplicativo faz a SED e a repete para obter o IC95% e comparar a referência com as melhorias com as mesmas sementes."),
          shiny::tags$div(style = "overflow-x:auto",
            shiny::tags$table(class = "table table-sm table-striped",
              shiny::tags$thead(shiny::tags$tr(lapply(names(simulation_framing_table()), shiny::tags$th))),
              shiny::tags$tbody(lapply(seq_len(nrow(simulation_framing_table())), function(i) {
                shiny::tags$tr(lapply(simulation_framing_table()[i, ], function(cell) shiny::tags$td(as.character(cell))))
              })))),
          shiny::tags$p(class = "small-note", "Fontes da classificação: Taha (2008) e Prado (2014), listadas em Referências do projeto. A validação contra a frota real depende de registros; no modo sintético, o resultado demonstra o método.")),
        bslib::nav_panel("Árvore de decisão AHP–TOPSIS", hierarchy_tree_ui()),
        bslib::nav_panel("Árvore física, organograma e estudos", sys_documentation()),
        bslib::nav_panel("Referências do projeto",
          shiny::tags$p("Fontes citadas no relatório em Word, com a mesma referência que o relatório apresenta (no máximo 42). Onde há link, ele abre o arquivo do projeto e pode exigir acesso ao Drive. O material de aula serve apenas para conferir o processamento e não é referência bibliográfica."),
          shiny::tagList(lapply(unique(vapply(documentation_references, function(x) x$group, character(1))), function(group) {
            bslib::card(bslib::card_header(group), shiny::tagList(lapply(Filter(function(x) x$group == group, documentation_references), function(ref) {
              shiny::tags$div(class="jltv-doc-ref", shiny::tags$p(shiny::strong(ref$title)), shiny::tags$p(paste("Uso no projeto:", ref$use)), if (nzchar(ref$url %||% "")) shiny::tags$a(href=ref$url, target="_blank", rel="noopener noreferrer", "Abrir fonte do projeto"))
            })))
          })),
          shiny::tags$p(class="small-note", "Estas fontes sustentam conceitos e métodos; não fornecem automaticamente parâmetros empíricos de falhas, reparos ou custos do JLTV. A implementação própria e suas hipóteses devem ser avaliadas junto aos registros e testes."),
          bslib::card(bslib::card_header("Documentação da implementação"),
            shiny::tags$p(paste("JLTV Sustainment Decision Lab", JLTV_MODEL_VERSION, "| esquema SQLite", JLTV_SCHEMA_VERSION)),
            shiny::tags$p("Arquivos locais: app.R (implementação), README_EXECUCAO.md (execução e hipóteses) e tests_revision.R (verificações)."))
        ),
        bslib::nav_panel("Software e citações", shiny::tags$p("Citações oficiais e versões obtidas dos pacotes instalados, sem consulta à internet. O SQLite é acessado pelo pacote RSQLite. Estes programas executam o método; não são fontes de dados físicos do JLTV."), software)
      )
    )
  }

  current_time_text = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")

  ui = bslib::page_navbar(
    title = brand_title,
    window_title = "JLTV Sustainment Decision Lab",
    id = "main_nav",
    theme = application_theme,
    header = shiny::tagList(
      if ("favicon.png" %in% application_assets) shiny::tags$head(
        shiny::tags$link(rel = "icon", type = "image/png", href = "jltv_img/favicon.png")
      ),
      application_css,
      shiny::tags$style(shiny::HTML(paste0(
        # v1.5.4 — modo simplificado: funções avançadas ficam ocultas (continuam no código e no banco).
        "body:not(.show-adv) .adv-only{display:none!important}",
        "body:not(.show-adv) .navbar a[data-value='catalogo'],body:not(.show-adv) .navbar li:has(> a[data-value='catalogo']){display:none!important}",
        "body:not(.show-adv) a[data-value='1C. Frota e OS'],body:not(.show-adv) li:has(> a[data-value='1C. Frota e OS']){display:none!important}",
        "#adv_toggle{float:right;color:#DCE8F3;font-size:0.85rem;text-decoration:underline;cursor:pointer}"
      ))),
      shiny::tags$div(
        class = "app-subtitle",
        "Prioridades dos especialistas · registros de manutenção · cenários de disponibilidade",
        shiny::tags$a(
          id = "adv_toggle", href = "#",
          onclick = paste0("document.body.classList.toggle('show-adv');",
            "this.textContent=document.body.classList.contains('show-adv')?'Ocultar funções avançadas':'Mostrar funções avançadas';return false;"),
          "Mostrar funções avançadas"
        )
      )
    ),
    fillable = FALSE,
    selected = "dados",
    bslib::nav_panel(
      title = "1. Dados",
      value = "dados",
      evidence_output("evidence_quality"),
      step_intro_ui(
        "Passo 1 — reunir os dados",
        shiny::tagList(
          "O que fazer aqui:",
          shiny::tags$ul(
            shiny::tags$li("1A: traga as respostas do questionário (Google Sheets, arquivo ou cadastro manual). Para conhecer o aplicativo, use as respostas de demonstração."),
            shiny::tags$li("1B: escolha o modo de evidência — EMPIRICAL (só dados reais), SYNTHETIC (só teste) ou HYBRID (ambos)."),
            shiny::tags$li("Registros de manutenção da frota (OS): importe o CSV em 1B. 1D: apoio atual (equipes, boxes, estoque) como parâmetros de cenário.")
          )
        ),
        kind = "frota"
      ),
      bslib::navset_card_tab(
        id = "input_subtabs",
        bslib::nav_panel(
          "1A. Questionário",
      bslib::card(
        bslib::card_header("1A. Questionário on-line e sincronização"),
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            title = "Fonte das respostas",
            shiny::selectInput(
              "questionnaire_source_mode", "Fonte",
              choices = c(
                "Google Sheets — automático" = "GOOGLE_SHEETS",
                "Arquivo CSV/XLSX — fallback" = "FILE",
                "Entrada manual — fallback" = "MANUAL"
              ),
              selected = "GOOGLE_SHEETS"
            ),
            shiny::conditionalPanel(
              condition = "input.questionnaire_source_mode == 'GOOGLE_SHEETS'",
              shiny::textInput(
                "questionnaire_sheet_url", "Google Sheets",
                questionnaire_google_defaults()$sheet_url
              ),
              shiny::textInput(
                "questionnaire_sheet_name", "Aba",
                questionnaire_google_defaults()$sheet_name
              ),
              shiny::selectInput(
                "questionnaire_evidence_class", "Classificação das respostas",
                choices = c("EMPIRICAL", "SYNTHETIC"), selected = "EMPIRICAL"
              ),
              shiny::checkboxInput(
                "questionnaire_auto_refresh",
                "Sincronizar automaticamente enquanto o aplicativo estiver aberto",
                questionnaire_google_defaults()$auto_refresh
              ),
              shiny::numericInput(
                "questionnaire_refresh_seconds",
                "Intervalo de atualização automática (s; mínimo 300)",
                questionnaire_google_defaults()$refresh_seconds,
                min = 300, max = 3600, step = 300
              ),
              shiny::actionButton(
                "questionnaire_google_auth", "Conectar Google",
                class = "btn btn-outline-primary w-100"
              ),
              shiny::actionButton(
                "questionnaire_use_sheet", "Usar esta tabela automaticamente",
                class = "btn btn-primary w-100"
              ),
              shiny::actionButton(
                "questionnaire_sync_now", "Atualizar agora",
                class = "btn btn-secondary w-100"
              )
            ),
            shiny::conditionalPanel(
              condition = "input.questionnaire_source_mode == 'FILE'",
              shiny::fileInput(
                "questionnaire_raw_file", "Arquivo bruto do Google Forms",
                accept = c(".csv", ".xlsx", ".xls")
              ),
              shiny::selectInput(
                "questionnaire_file_evidence_class", "Classificação das respostas",
                choices = c("Seguir o modo de evidência escolhido em Governança dos dados" = "MODE", "EMPIRICAL", "SYNTHETIC"),
                selected = "MODE"
              ),
              shiny::actionButton(
                "questionnaire_import_raw", "Importar arquivo do questionário",
                class = "btn btn-secondary w-100"
              )
            ),
            shiny::conditionalPanel(
              condition = "input.questionnaire_source_mode == 'MANUAL'",
              shiny::actionButton(
                "questionnaire_manual_open", "Cadastrar resposta manual",
                class = "btn btn-secondary w-100"
              )
            ),
            shiny::hr(),
            shiny::tags$p(class = "form-section", "Demonstração"),
            shiny::tags$p(
              class = "small-note",
              "24 respostas fictícias (SYNTHETIC) para conhecer o painel de resultado. Não entram no modo EMPIRICAL e podem ser removidas."
            ),
            shiny::actionButton(
              "load_demo_questionnaire", "Carregar respostas de demonstração",
              icon = shiny::icon("vial"), class = "btn btn-outline-secondary w-100"
            ),
            shiny::actionButton(
              "remove_demo_questionnaire", "Remover demonstração",
              class = "btn btn-link w-100"
            )
          ),
          bslib::layout_column_wrap(
            width = 1 / 2,
            bslib::card(
              bslib::card_header("Status da conexão"),
              shiny::tags$div(
                class = "sync-status-card",
                shiny::textOutput("questionnaire_sync_status")
              ),
              shiny::tags$p(
                class = "small-note",
                "Fluxo principal: Google Forms → Google Sheets → aplicativo. Arquivo e cadastro manual permanecem como alternativas."
              )
            ),
            bslib::card(
              bslib::card_header("Regra de uso"),
              shiny::tags$ul(
                shiny::tags$li("EMPIRICAL: respostas reais verificadas."),
                shiny::tags$li("SYNTHETIC: respostas de teste/demonstração."),
                shiny::tags$li("HYBRID: combina classes preservando a proveniência."),
                shiny::tags$li("As respostas do questionário não viram automaticamente MTBF, MTTR, ALDT, lead time, custo ou probabilidade.")
              )
            )
          )
        )
      ),
      bslib::card(
        bslib::card_header("1B.1 Conferência dos dados do questionário"),
        shiny::tags$p(
          class = "small-note",
          "Confira aqui se as respostas realmente entraram no aplicativo antes de seguir para o AHP–TOPSIS. ",
          "Os contadores RAM/CMMS da seção 1A são independentes do questionário."
        ),
        shiny::textOutput("questionnaire_mode_status"),
        bslib::layout_column_wrap(
          width = 1 / 2,
          bslib::card(
            bslib::card_header("Resumo da importação"),
            DT::DTOutput("questionnaire_import_summary_table")
          ),
          bslib::card(
            bslib::card_header("Visualizar dados carregados"),
            shiny::selectInput(
              "questionnaire_preview_choice", "Tabela",
              choices = c(
                "Respondentes" = "questionnaire_respondents",
                "Comparações AHP" = "ahp_pairwise",
                "Avaliações P6" = "topsis_scores",
                "Fatores P7" = "questionnaire_factors"
              ),
              selected = "questionnaire_respondents"
            ),
            DT::DTOutput("questionnaire_preview_table")
          )
        )
      ),
        ),
        bslib::nav_panel(
          "1B. Qualidade RAM/CMMS",
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                title = "Governança dos dados",
                shiny::selectInput(
                  "mode_choice", "Modo de evidência",
                  choices = c("SYNTHETIC", "HYBRID", "EMPIRICAL")
                ),
                shiny::checkboxInput(
                  "confirm_empirical",
                  "Confirmo que os registros foram verificados antes de marcar EMPIRICAL",
                  FALSE
                ),
                shiny::actionButton(
                  "save_mode", "Aplicar modo", class = "btn btn-primary w-100"
                ),
                shiny::uiOutput("questionnaire_hidden_banner"),
                shiny::hr(),
                shiny::tags$p(
                  class = "small-note",
                  "O questionário on-line é conectado no painel logo abaixo. CSV continua disponível apenas como fallback para dados RAM/CMMS."
                ),
                shiny::selectInput(
                  "upload_table", "Tabela CSV — dados RAM/CMMS (fallback)",
                  choices = c(
                    "assets", "work_orders", "work_order_delays",
                    "resources", "parts", "inventory_transactions", "maintenance_plans"
                  )
                ),
                shiny::fileInput("upload_csv", "Selecionar CSV", accept = ".csv"),
                shiny::radioButtons(
                  "upload_operation", "Operação",
                  choices = c("Adicionar" = "append", "Substituir tabela" = "replace")
                ),
                shiny::actionButton(
                  "import_csv", "Validar e importar", class = "btn btn-secondary w-100"
                ),
                shiny::downloadButton("download_template", "Baixar modelo da tabela"),
                shiny::downloadButton("download_quality", "Baixar relatório de qualidade")
              ),
              bslib::layout_column_wrap(
                width = 1 / 2,
                bslib::card(
                  bslib::card_header("Contagem e cobertura analítica"),
                  DT::DTOutput("coverage_table")
                ),
                bslib::card(
                  bslib::card_header("Resumo dos achados"),
                  DT::DTOutput("quality_summary_table")
                )
              ),
              bslib::card(
                bslib::card_header("Problemas por linha e campo"),
                DT::DTOutput("quality_issues_table")
              ),
              bslib::card(
                bslib::card_header("Dados de vida: falha observada e censura à direita"),
                DT::DTOutput("life_data_table")
              )
            ),
        ),
        bslib::nav_panel(
          "1C. Frota e OS",
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                title = "Cadastrar viatura",
                shiny::textInput("asset_id", "ID", "JLTV-NOVO-01"),
                shiny::textInput("asset_name", "Nome", "Nova viatura"),
                shiny::textInput("asset_model", "Modelo", "JLTV"),
                shiny::dateInput("asset_entry", "Entrada em serviço", Sys.Date()),
                shiny::numericInput("asset_initial_meter", "Medidor inicial", 0, min = 0),
                shiny::numericInput("asset_current_meter", "Medidor atual", 0, min = 0),
                shiny::selectInput("asset_unit", "Unidade", c("HOURS", "KM")),
                shiny::selectInput("asset_state", "Estado inicial", c("OPERATING", "AVAILABLE", "MAINTENANCE")),
                shiny::textInput("asset_source", "Fonte", "USER_VERIFIED"),
                shiny::actionButton("add_asset", "Cadastrar viatura", class = "btn btn-primary w-100")
              ),
              bslib::card(
                bslib::card_header("Cadastro de ativos"),
                DT::DTOutput("assets_table")
              )
            ),
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                title = "Registrar OS concluída",
                shiny::textInput("os_id", "ID da OS", "WO-NOVA-001"),
                shiny::uiOutput("os_asset_ui"),
                shiny::selectInput(
                  "os_type", "Tipo",
                  c("CORRECTIVE", "PREVENTIVE", "CONDITION_BASED")
                ),
                shiny::checkboxInput("os_failure", "Falha funcional", TRUE),
                shiny::textInput("os_failure_mode", "Modo de falha", "NÃO CLASSIFICADO"),
                shiny::textInput("os_failure_class", "Classe", "NÃO CLASSIFICADA"),
                shiny::textInput("os_detected", "Detecção UTC", current_time_text),
                shiny::textInput("os_diag_start", "Início diagnóstico UTC", current_time_text),
                shiny::textInput("os_diag_end", "Fim diagnóstico UTC", current_time_text),
                shiny::textInput("os_repair_start", "Início reparo ativo UTC", current_time_text),
                shiny::textInput("os_repair_end", "Fim reparo ativo UTC", current_time_text),
                shiny::textInput("os_test_start", "Início teste UTC", current_time_text),
                shiny::textInput("os_released", "Liberação UTC", current_time_text),
                shiny::numericInput("os_meter", "Medidor no evento", 0, min = 0),
                shiny::textAreaInput("os_description", "Descrição", "Registro de manutenção."),
                shiny::textInput("os_source", "Fonte", "USER_VERIFIED"),
                shiny::checkboxInput("os_has_delay", "Registrar atraso associado", FALSE),
                shiny::selectInput("delay_type", "Tipo de atraso", c("PART", "RESOURCE", "ADMIN")),
                shiny::textInput("delay_start", "Início atraso UTC", current_time_text),
                shiny::textInput("delay_end", "Fim atraso UTC", current_time_text),
                shiny::selectInput("delay_class", "Responsabilidade", c("LOGISTIC", "ADMINISTRATIVE")),
                shiny::textInput("delay_notes", "Nota do atraso", ""),
                shiny::actionButton("add_order", "Gravar OS completa", class = "btn btn-primary w-100")
              ),
              bslib::card(
                bslib::card_header("Ordens de serviço"),
                DT::DTOutput("orders_table")
              )
            ),
            bslib::card(
              bslib::card_header("Corrigir registros (com trilha de auditoria)"),
              bslib::layout_column_wrap(
                width = 1 / 2,
                shiny::tagList(
                  shiny::uiOutput("fix_asset_ui"),
                  shiny::actionButton("toggle_asset", "Desativar/reativar viatura", class = "btn btn-outline-secondary w-100")
                ),
                shiny::tagList(
                  shiny::uiOutput("fix_order_ui"),
                  shiny::actionButton("delete_order", "Excluir OS (e atrasos vinculados)", class = "btn btn-outline-danger w-100")
                )
              ),
              shiny::textInput("fix_reason", "Motivo da correção (obrigatório)", ""),
              shiny::tags$hr(),
              shiny::tags$h5("Alterar a unidade do medidor (HOURS ↔ KM)"),
              shiny::tags$p(class = "small-note",
                "Use quando a viatura foi cadastrada na unidade errada. A viatura é a escolhida em “Viatura” acima. ",
                "A calibração automática da falha exige HOURS em todas as viaturas ativas."),
              bslib::layout_column_wrap(
                width = 1 / 2,
                shiny::selectInput("fix_unit_target", "Nova unidade", c("HOURS", "KM")),
                shiny::radioButtons(
                  "fix_unit_method", "O que fazer com as leituras do medidor?",
                  choices = c(
                    "A leitura já está na nova unidade (corrigir só o cadastro)" = "relabel",
                    "Converter a leitura com uma velocidade média validada" = "convert"
                  ),
                  selected = "relabel"
                )
              ),
              shiny::conditionalPanel(
                condition = "input.fix_unit_method == 'convert'",
                shiny::numericInput("fix_unit_rate", "Velocidade média validada (km por hora de uso)", NA, min = 0.01, step = 0.5),
                shiny::tags$p(class = "small-note",
                  "KM → HOURS: horas = km ÷ velocidade. HOURS → KM: km = horas × velocidade. Aplica-se às leituras da viatura e ao medidor das suas OS. Registre a fonte da velocidade no motivo.")
              ),
              shiny::checkboxInput("fix_unit_all", "Aplicar a todas as viaturas ativas que estão na outra unidade", FALSE),
              shiny::actionButton("change_asset_unit", "Alterar unidade do medidor", class = "btn btn-outline-primary w-100"),
              shiny::tags$p(class = "small-note", "Viaturas desativadas saem da análise RAM sem perder o histórico. A OS excluída fica registrada em JSON na trilha abaixo."),
              DT::DTOutput("change_log_table")
            ),
        ),
        bslib::nav_panel(
          "1D. Apoio e custos",
            shiny::tags$p(
              class = "small-note",
              "Os valores abaixo são parâmetros de cenário. Eles não são automaticamente valores medidos da JLTV."
            ),
            bslib::layout_column_wrap(
              width = 1 / 3,
              bslib::card(
                bslib::card_header("Capacidade de manutenção"),
                shiny::tags$p(class = "small-note", "Equipes, boxes e estoque inicial ficam no Passo 3 (3A, Situação atual da frota), junto da meta M."),
                shiny::numericInput("support_pm", "Intervalo PM (h; 0 desativa)", 250, min = 0)
              ),
              bslib::card(
                bslib::card_header("Sobressalentes"),
                shiny::numericInput("support_reorder_point", "Ponto de reposição", 2, min = 0, step = 1),
                shiny::numericInput("support_reorder_qty", "Lote de reposição", 6, min = 0, step = 1),
                shiny::numericInput("support_lead", "Prazo médio (h)", 24, min = 0.01),
                shiny::numericInput("support_lead_cv", "CV do prazo de reposição", 0.35, min = 0, step = 0.05),
                shiny::numericInput("support_part_probability", "Probabilidade de exigir peça (corretiva)", 0.65, min = 0, max = 1, step = 0.05),
                shiny::numericInput("support_pm_part_probability", "Probabilidade de exigir peça (preventiva; vazio = igual à corretiva)", NA, min = 0, max = 1, step = 0.05)
              ),
              bslib::card(
                class = "adv-only",
                bslib::card_header("Custos no horizonte"),
                shiny::checkboxInput("costs_complete", "Custos completos e aprovados", FALSE),
                shiny::numericInput("cost_asset", "Por viatura (R$)", 100000, min = 0),
                shiny::numericInput("cost_diag", "Por equipe diagnóstico (R$)", 12000, min = 0),
                shiny::numericInput("cost_repair", "Por equipe reparo (R$)", 18000, min = 0),
                shiny::numericInput("cost_bay", "Por box (R$)", 8000, min = 0),
                shiny::numericInput("cost_part", "Por peça consumida (R$)", 2500, min = 0),
                shiny::numericInput("cost_corrective", "Por corretiva (R$)", 1500, min = 0),
                shiny::numericInput("cost_preventive", "Por preventiva (R$)", 800, min = 0),
                shiny::numericInput("cost_downtime", "Por viatura-hora indisponível (R$)", 500, min = 0),
                shiny::actionButton("save_support", "Salvar configuração-base", class = "btn btn-secondary w-100")
              )
            ),
            bslib::layout_column_wrap(
              width = 1 / 3,
              bslib::card(bslib::card_header("Recursos armazenados"), DT::DTOutput("resources_table")),
              bslib::card(bslib::card_header("Peças armazenadas"), DT::DTOutput("parts_table")),
              bslib::card(bslib::card_header("Planos armazenados"), DT::DTOutput("plans_table"))
            )
        )
      ),
      next_step_ui("decisao", "Próximo: prioridades")
    ),
    bslib::nav_panel(title="2. Prioridades", value="decisao",
      evidence_output("evidence_multicriteria"),
      step_intro_ui("Passo 2 — prioridades dos especialistas (AHP–TOPSIS)",
        "Defina como agregar as respostas P1–P6, calcule e confira aqui o ranking, os pesos, a consistência, as distribuições e a robustez do resultado.",kind="modelo"),
      bslib::card(bslib::card_header("Parâmetros AHP–TOPSIS"),
        shiny::selectInput("score_aggregation","Agregação das notas P6",choices=c("Média"="mean","Mediana"="median")),
        shiny::checkboxInput("only_consistent_ahp","AHP: usar somente matrizes completas com RC < 0,10",TRUE),
        shiny::numericInput("min_cell_coverage","P6: cobertura mínima por célula (% dos avaliadores; 0 = sem mínimo)",0,min=0,max=100,step=5),
        shiny::actionButton("recompute_multicriteria","Calcular prioridades",class="btn btn-primary"),
        shiny::tags$p("P7 aponta a evidência a buscar; não estabelece taxa física de falha.")),
      shiny::downloadButton("download_multicriteria_xlsx","Exportar AHP–TOPSIS (Excel)"),
      bslib::navset_card_tab(
        id = "decision_subtabs",
bslib::nav_panel(
          "Leitura dos avaliadores",
          shiny::tags$p(
            class = "small-note",
            "Esta aba mostra primeiro o que os avaliadores efetivamente responderam. P1–P5 são comparações AHP; P6 é a avaliação 1–10/N/A dos sistemas; P7 aponta peças, serviços, recursos e registros a localizar/confirmar."
          ),
          shiny::uiOutput("questionnaire_interpretation"),
          bslib::layout_column_wrap(
            width = 1 / 2,
            bslib::card(
              bslib::card_header("Cobertura do questionário"),
              DT::DTOutput("questionnaire_response_overview_table")
            ),
            bslib::card(
              bslib::card_header("Perfil agregado dos avaliadores"),
              DT::DTOutput("questionnaire_profile_summary_table")
            )
          ),
          bslib::card(
            bslib::card_header("P1–P5 — como os avaliadores compararam os critérios/subcritérios"),
            DT::DTOutput("questionnaire_ahp_response_summary_table")
          ),
          bslib::layout_column_wrap(
            width = 1 / 2,
            plot_card(
              "P6 — nota média de cada sistema em cada subcritério",
              plotly::plotlyOutput("questionnaire_p6_heatmap", height = "480px"),
              "Cores mais quentes = maior contribuição para a indisponibilidade (1 a 10). Sistemas na ordem do ranking TOPSIS, calculado com os pesos AHP desta aba."
            ),
            bslib::card(
              bslib::card_header("P6 — estatísticas e cobertura 1–10/N/A"),
              DT::DTOutput("questionnaire_p6_response_summary_table")
            )
          )
        ),
        bslib::nav_panel(
          "Pesos e ranking AHP–TOPSIS",
      hierarchy_tree_ui(),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(title="Método e interpretação",
          shiny::tags$p("Confira os pesos, a consistência e o ranking gerados com os parâmetros acima.")),
        bslib::layout_column_wrap(
          width = 1 / 2,
          plot_card(
            "Ranking TOPSIS — quais sistemas são os gargalos",
            plotly::plotlyOutput("topsis_bottleneck_plot", height = "420px"),
            "Coeficiente de 0 a 1: quanto maior, mais o sistema contribui para a indisponibilidade segundo os avaliadores. As cores mostram de qual critério vem a prioridade."
          ),
          plot_card(
            "Pesos AHP dos subcritérios",
            plotly::plotlyOutput("ahp_weights_plot", height = "420px"),
            "Quanto cada subcritério conta no ranking (soma = 100%)."
          )
        ),
        bslib::layout_column_wrap(
          width = 1 / 2,
          bslib::card(
            bslib::card_header("Consistência AHP por grupo"),
            DT::DTOutput("ahp_consistency_table")
          ),
          bslib::card(
            bslib::card_header("Pesos globais dos subcritérios"),
            DT::DTOutput("ahp_weights_table")
          ),
          bslib::card(
            bslib::card_header("Diagnóstico AHP por respondente"),
            DT::DTOutput("ahp_individual_table")
          ),
          bslib::card(
            bslib::card_header("Ranking TOPSIS dos sistemas/conjuntos"),
            DT::DTOutput("topsis_ranking_table")
          )
        ),
        bslib::card(
          bslib::card_header("Cobertura das avaliações P6 — sem imputação de N/A"),
          DT::DTOutput("topsis_coverage_table")
        ),
        bslib::card(
          bslib::card_header("Histórico de processamentos multicritério"),
          DT::DTOutput("multicriteria_runs_table")
        )
      ),
        ),
        bslib::nav_panel(
          "P7 — Mapa de evidências",
          shiny::tags$p(
            class = "small-note",
            "P7 funciona como mapa de evidências: mostra quais sistemas, peças/componentes, ocorrências/necessidades e fontes de confirmação foram apontados. Não transforma percepção em MTBF, MTTR, ALDT, lead time ou outro parâmetro físico sem registro rastreável."
          ),
          
          bslib::card(
            bslib::card_header("Síntese P7 por sistema/conjunto"),
            DT::DTOutput("questionnaire_p7_summary_table")
          ),
          bslib::card(
            bslib::card_header("Respostas P7 — detalhe para rastreabilidade"),
            DT::DTOutput("questionnaire_factors_table")
          )
        ),
        bslib::nav_panel(
          "Consistência e dispersão dos especialistas",
          shiny::tags$p(class = "small-note",
            "Mostra se os especialistas foram consistentes (RC < 0,10) e o quanto divergiram entre si. Sem isso, o ranking não se sustenta numa banca."),
          bslib::layout_column_wrap(
            width = 1 / 2,
            plot_card("Histograma da razão de consistência (RC)",
              plotly::plotlyOutput("ahp_cr_histogram_plot", height = "380px"),
              "Cada barra conta matrizes (avaliador × grupo). À direita da linha tracejada, o julgamento é inconsistente."),
            plot_card("Dispersão dos pesos dos critérios entre avaliadores",
              plotly::plotlyOutput("ahp_weight_dispersion_plot", height = "380px"),
              "O losango é o peso agregado usado no TOPSIS; as caixas mostram a divergência dos especialistas.")
          )
        ),
        bslib::nav_panel(
          "Notas P6 — distribuição",
          bslib::layout_column_wrap(
            width = 1 / 2,
            plot_card("Histograma das notas P6",
              plotly::plotlyOutput("p6_histogram_plot", height = "380px"),
              "Distribuição de todas as notas 1–10 dadas pelos avaliadores."),
            plot_card("Notas P6 por sistema",
              plotly::plotlyOutput("p6_boxplot_plot", height = "380px"),
              "Sistemas na ordem do ranking TOPSIS. Caixas largas indicam desacordo entre os avaliadores.")
          )
        ),
        bslib::nav_panel(
          "Robustez do ranking",
          shiny::tags$p(class = "small-note",
            "Testa se o ranking TOPSIS se mantém quando se reamostram os avaliadores, quando os pesos AHP são trocados por pesos iguais e quando a normalização muda. O resultado depende do método, da normalização e dos pesos (Sałabun et al., 2020)."),
          shiny::uiOutput("mc_robustness_text"),
          bslib::layout_column_wrap(
            width = 1 / 2,
            plot_card("Probabilidade de cada posição (reamostragem dos avaliadores)",
              plotly::plotlyOutput("rank_probability_plot", height = "420px"),
              "Quanto mais escura e concentrada a faixa, mais estável é a posição do sistema."),
            bslib::card(bslib::card_header("Estabilidade da posição de cada sistema"), DT::DTOutput("rank_stability_table"))
          ),
          bslib::card(bslib::card_header("Sensibilidade a pesos e à normalização"), DT::DTOutput("method_sensitivity_table"))
        )
      ),
      next_step_ui("simulacao","Próximo: cenário e simulação")),
    bslib::nav_panel(title="Componentes (avançado)", value="catalogo",
      step_intro_ui("Passo 3 — componentes e fontes", "Carregue ou importe o catálogo, confira parâmetros e vínculos de OS. Depois avance para configurar a missão.", kind="frota"),
      sys_catalog_ui()),
    bslib::nav_panel(title="3. Cenário e simulação", value="simulacao",
      step_intro_ui("Passo 3 — cenário e simulação", "Revise missão e parâmetros de cima para baixo, execute uma vez e consulte a aba de resultados.", kind="modelo"),
      shiny::tags$div(class="adv-only",
        shiny::radioButtons("workflow_engine","Tipo de avaliação",c("Por sistema e componente"="systems","Agregada e dimensionamento de frota"="aggregate"),selected="aggregate",inline=TRUE)),
      shiny::conditionalPanel("input.workflow_engine == 'systems'",
        sys_configuration_ui(),
        shiny::actionButton('sys_run','Executar simulação por sistema',class='btn btn-primary btn-lg')),
      shiny::conditionalPanel("input.workflow_engine == 'aggregate'",
        shiny::tags$div(class = "run-hint",
          shiny::tags$strong("Como gerar a análise: "),
          "revise os campos abaixo e, no FIM desta página, clique em ",
          shiny::tags$em("Executar avaliação agregada completa"), ". ",
          shiny::tags$a(href = "#", onclick = "var el=document.getElementById('run_bar');if(el){el.scrollIntoView({behavior:'smooth',block:'center'});}return false;",
            "Ir para os botões de execução ↓")),
        shiny::tags$h3("Configuração agregada"),
        shiny::tags$div(
        class = "aggregate-input-sections",
        bslib::card(
          bslib::card_header("3A. Grade e missão"),
          bslib::layout_column_wrap(
            width = 1 / 2,
            bslib::card(
              bslib::card_header("Situação atual da frota (a síntese e as melhorias partem daqui)"),
              shiny::numericInput("diag_fleet", "Tamanho da frota (N viaturas)", 12, min = 1, step = 1),
              shiny::numericInput("support_diag", "Equipes de diagnóstico", 2, min = 1, step = 1),
              shiny::numericInput("support_repair", "Equipes de reparo", 3, min = 1, step = 1),
              shiny::numericInput("support_bays", "Boxes", 2, min = 1, step = 1),
              shiny::numericInput("support_stock", "Estoque inicial de peças", 6, min = 0, step = 1),
              shiny::tags$p(class = "small-note", "Estes cinco campos definem a situação de referência que aparece na síntese do Passo 4 (por exemplo, \"8 de 12 viaturas\") e a base contra a qual cada melhoria é testada. A meta M, o horizonte e as replicações estão no cartão \"Missão e execução\"."),
            ),
            bslib::card(
              bslib::card_header("Grade de dimensionamento (alternativas a comparar)"),
              shiny::tags$p(class = "small-note", "Estes campos só alimentam o dimensionamento (qual combinação de frota, equipes, boxes e estoque atinge a probabilidade-alvo). Eles não alteram a situação atual ao lado."),
              shiny::textInput("grid_fleet", "Tamanhos de frota", "10:12"),
              shiny::textInput("grid_diag", "Equipes diagnóstico", "2"),
              shiny::textInput("grid_repair", "Equipes reparo", "2:3"),
              shiny::textInput("grid_bays", "Boxes", "2"),
              shiny::textInput("grid_stock", "Estoques iniciais", "2,6")
            ),
            bslib::card(
              bslib::card_header("Missão e execução"),
              shiny::numericInput("target_ready", "Meta M: mínimo de viaturas prontas (de N = frota)", 8, min = 0, step = 1),
              shiny::numericInput("simulation_horizon", "Horizonte H (h)", 168, min = 1),
              shiny::numericInput("target_probability", "Probabilidade-alvo", 0.80, min = 0, max = 1, step = 0.01),
              shiny::numericInput("simulation_replications", "Replicações por configuração", 50, min = 10, max = 2000, step = 10),
              shiny::numericInput("simulation_seed", "Semente", 20260910, min = 1, step = 1),
              shiny::uiOutput("replications_hint"),
              shiny::uiOutput("field_effect_hint"),
              shiny::tags$p(class = "small-note", "Tipo de simulação: eventos discretos (SED) com replicações de Monte Carlo; detalhes em Referências e funcionamento > Enquadramento do simulador."),
              shiny::tags$p("Revise todos os parâmetros abaixo antes de executar."),
            ),
            bslib::card(
              bslib::card_header("Hipóteses da missão (v1.4.1)"),
              shiny::numericInput(
                "mission_utilization",
                "Utilização na missão (horas de uso por hora de calendário; 1 = uso contínuo 24 h)",
                1, min = 0.01, max = 1, step = 0.05
              ),
              shiny::selectInput(
                "pm_initial_phase", "Ciclo de preventiva no início da missão",
                choices = c(
                  "Todas com preventiva recém-concluída (preparação pré-missão)" = "fresh",
                  "Fase do ciclo distribuída entre as viaturas (frota em regime)" = "staggered"
                ),
                selected = "fresh"
              ),
              shiny::numericInput("initial_unavailable", "Viaturas indisponíveis no início da missão", 0, min = 0, step = 1),
              shiny::numericInput("mission_tolerance", "Tolerância: horas acumuladas abaixo de M (0 = nenhuma queda)", 0, min = 0),
              shiny::checkboxInput("common_random_numbers", "Mesmas sementes para todas as configurações (números aleatórios comuns)", TRUE),
              shiny::tags$p(
                class = "small-note",
                "A vida até a falha e o intervalo de preventiva estão em horas de uso; o horizonte H está em horas de calendário. A utilização converte uma escala na outra e é hipótese de cenário: a utilização observada em tempo de paz não é automaticamente a da missão."
              )
            )
          )
        ),
        bslib::card(
          bslib::card_header("3B. Falhas e tempos"),
          bslib::layout_column_wrap(
            width = 1 / 2,
            bslib::card(
              bslib::card_header("Modelo de falhas"),
              shiny::selectInput("failure_distribution", "Distribuição", c("Exponencial" = "exponential", "Weibull" = "weibull", "Lognormal" = "lognormal")),
              shiny::numericInput("failure_parameter_1", "Média ou escala (h)", 180, min = 0.01),
              shiny::numericInput("failure_parameter_2", "Forma Weibull ou CV lognormal", 1.2, min = 0),
              shiny::tags$p(class = "small-note", "Quando os registros não sustentam calibração, estes valores permanecem hipóteses explícitas de cenário.")
            ),
            bslib::card(
              bslib::card_header("Modelos dos tempos"),
              shiny::numericInput("mean_diagnosis", "Diagnóstico médio (h)", 2, min = 0.01),
              shiny::numericInput("mean_corrective", "Reparo corretivo médio (h)", 8, min = 0.01),
              shiny::numericInput("mean_preventive", "Preventiva média (h)", 4, min = 0.01),
              shiny::numericInput("mean_test", "Teste/liberação médio (h)", 1, min = 0.01),
              shiny::numericInput("duration_cv", "Coeficiente de variação", 0.35, min = 0, step = 0.05)
            )
          )
        ),
        bslib::card(
          bslib::card_header("3C. Calibração"),
          bslib::card(
            bslib::card_header("Calibração a partir dos registros observados"),
            shiny::actionButton("use_observed_calibration", "Usar calibração dos dados observados", class = "btn btn-secondary"),
            shiny::uiOutput("calibration_summary"),
            shiny::tags$p(
              class = "small-note",
              "Transfere apenas parâmetros diretamente estimáveis pelos registros. P7 pode indicar onde procurar a evidência, mas não fornece automaticamente o valor do parâmetro."
            )
          )
        ),
        bslib::card(
          bslib::card_header("3D. Estado da execução"),
          bslib::card(
            bslib::card_header("Estado e metadados da execução"),
            shiny::uiOutput("simulation_status"),
            shiny::tags$p("Os metadados da execução aparecem na aba 4 (Resultados e relatório).")
          )
        )
      ),
        bslib::accordion(
        id = "assessment_settings", open = FALSE,
        bslib::accordion_panel(
          "Ajustar as melhorias testadas e a situação de referência",
          icon = shiny::icon("sliders"),
          bslib::layout_column_wrap(
            width = 1 / 2,
            shiny::radioButtons(
              "diag_reference", "Situação de referência (de onde partem as melhorias)",
              choices = c(
                "Situação atual: os campos \"Situação atual da frota\" (3A)" = "current",
                "Configuração recomendada pela simulação" = "recommended"
              ),
              selected = "current"
            )
          ),
          shiny::tags$p(
            class = "small-note",
            "Cada melhoria é simulada isoladamente contra a situação de referência, com as mesmas sementes, horizonte, meta M e replicações desta etapa. ",
            "As magnitudes abaixo são hipóteses de cenário (o quanto se pretende melhorar), não valores medidos da JLTV. Desmarque o que não quiser testar."
          ),
          shiny::tags$div(
            class = "lever-grid",
            shiny::tagList(purrr::map(seq_len(nrow(improvement_lever_catalog())), function(i) {
              lever = improvement_lever_catalog()[i, , drop = FALSE]
              shiny::tags$div(
                class = "lever-item",
                shiny::checkboxInput(paste0("lever_use_", lever$lever_id), shiny::tags$strong(lever$label), TRUE),
                shiny::numericInput(
                  paste0("lever_mag_", lever$lever_id), paste0("Magnitude (", lever$magnitude_unit, ")"),
                  lever$default_magnitude, min = if (lever$magnitude_kind == "increment") 1 else 1,
                  max = if (lever$magnitude_kind == "percent_down") 90 else NA,
                  step = if (lever$magnitude_kind == "increment") 1 else 5
                ),
                shiny::tags$div(class = "lever-meta", paste0(
                  "Critério AHP: ", unname(assessment_criterion_labels()[[lever$criterion]]),
                  " · reduz: ", lever$downtime_component
                ))
              )
            }))
          )
        )
      ),
        bslib::accordion(open=FALSE,bslib::accordion_panel("Comparação pareada de uma prioridade com cenário",bslib::card(
              bslib::card_header("Vincular prioridade decisória a um cenário de simulação"),
              bslib::layout_column_wrap(
                width = 1 / 3,
                shiny::uiOutput("decision_target_ui"),
                shiny::selectInput(
                  "scenario_parameter", "Parâmetro do simulador",
                  choices = c(
                    "Tamanho da frota" = "fleet_size",
                    "Equipes de diagnóstico" = "diagnostic_teams",
                    "Equipes de reparo" = "repair_teams",
                    "Boxes" = "bays",
                    "Estoque inicial" = "initial_stock",
                    "Prazo médio / espera de suprimento (h)" = "lead_time_hours",
                    "Ponto de reposição" = "reorder_point",
                    "Lote de reposição" = "reorder_qty",
                    "Intervalo preventivo (h)" = "pm_interval",
                    "Diagnóstico médio (h)" = "mean_diagnosis",
                    "Reparo corretivo médio (h)" = "mean_corrective",
                    "Preventiva média (h)" = "mean_preventive",
                    "Teste/liberação médio (h)" = "mean_test",
                    "Probabilidade de exigir peça" = "part_required_probability"
                  )
                ),
                shiny::numericInput("scenario_baseline", "Valor do cenário-base", 0),
                shiny::numericInput("scenario_alternative", "Valor do cenário alternativo", 0),
                shiny::textInput("scenario_evidence_source", "Fonte/evidência do valor", ""),
                shiny::textInput("scenario_notes", "Observações", "")
              ),
              bslib::layout_column_wrap(
                width = 1 / 3,
                shiny::actionButton("save_decision_link", "Salvar vínculo", class = "btn btn-secondary w-100"),
                shiny::actionButton("apply_link_baseline", "Aplicar cenário-base", class = "btn btn-outline-primary w-100"),
                shiny::actionButton("apply_link_alternative", "Aplicar cenário alternativo", class = "btn btn-primary w-100")
              ),
              shiny::actionButton(
                "compare_link_paired", "Comparar base × alternativo (mesmas sementes)",
                class = "btn btn-outline-primary w-100"
              ),
              shiny::tags$p(
                class = "small-note",
                "Roda o vínculo ativo nos dois valores, na configuração recomendada da última simulação (ou na primeira da grade), com as mesmas sementes, horizonte, meta e replicações desta etapa. A diferença é pareada por réplica; o IC95% indica se a mudança é demonstrável."
              ),
              
              
            )
        )),
        shiny::tags$div(class="next-step", id="run_bar",
          shiny::actionButton("run_simulation", "Simular grade sem melhorias", class="btn btn-outline-primary"),
          shiny::actionButton("run_full_assessment", "Executar avaliação agregada completa", icon=shiny::icon("play"), class="btn btn-primary"),
        shiny::actionButton("run_improvements", "Testar só as melhorias", icon=shiny::icon("flask"), class="btn btn-outline-primary"))),
      shiny::textOutput('sys_run_status'),
      shiny::tags$p(class="small-note", "Ao concluir uma execução, a aba 4 (Resultados e relatório) abre automaticamente. Corrija os parâmetros aqui se houver erro.")),
    bslib::nav_panel(title="4. Resultados e relatório", value="resultados", icon=shiny::icon("chart-column"),
      step_intro_ui("Passo 4 — interprete e exporte", "Veja a síntese, confira causas e incerteza, examine detalhes e exporte o relatório. Mudanças anteriores exigem nova execução.", kind="frota"),
      shiny::conditionalPanel("input.workflow_engine == 'systems'",
        shiny::tags$h3("Resultados por sistema"), sys_results_ui()),
      shiny::conditionalPanel("input.workflow_engine == 'aggregate'",
        shiny::tags$h3("Síntese da avaliação agregada"),
        shiny::uiOutput("results_stale_banner"),
        shiny::uiOutput("assessment_headline"),
      bslib::layout_column_wrap(
        width = 1 / 4,
        bslib::value_box(
          title = "Disponibilidade hoje (simulada)", value = shiny::textOutput("kpi_ao_reference"),
          shiny::textOutput("kpi_ao_reference_note"), showcase = shiny::icon("gauge-high"), theme = "primary"
        ),
        bslib::value_box(
          title = "Disponibilidade com as melhorias", value = shiny::textOutput("kpi_ao_improved"),
          shiny::textOutput("kpi_ao_improved_note"), showcase = shiny::icon("arrow-trend-up"), theme = "success"
        ),
        bslib::value_box(
          title = "Chance de cumprir a missão", value = shiny::textOutput("kpi_success"),
          shiny::textOutput("kpi_success_note"), showcase = shiny::icon("bullseye"), theme = "secondary"
        ),
        bslib::value_box(
          title = "Prioridade nº 1 (especialistas)", value = shiny::textOutput("kpi_bottleneck"),
          shiny::textOutput("kpi_bottleneck_note"), showcase = shiny::icon("triangle-exclamation"), theme = "danger"
        )
      ),
      bslib::layout_column_wrap(
        width = 1 / 2,
        plot_card(
          "1 · Quais sistemas são os gargalos? (especialistas — AHP/TOPSIS)",
          plotly::plotlyOutput("assessment_bottleneck_plot", height = "440px"),
          shiny::uiOutput("assessment_bottleneck_note")
        ),
        plot_card(
          "2 · Quanto cada melhoria aumenta a disponibilidade? (simulação)",
          plotly::plotlyOutput("assessment_improvement_plot", height = "440px"),
          shiny::uiOutput("assessment_improvement_note")
        ),
        plot_card(
          "3 · Onde a viatura perde tempo parada? (simulação)",
          plotly::plotlyOutput("assessment_downtime_plot", height = "360px"),
          shiny::uiOutput("assessment_downtime_note")
        ),
        plot_card(
          "4 · O que os especialistas consideram mais importante? (AHP)",
          plotly::plotlyOutput("assessment_ahp_plot", height = "360px"),
          shiny::uiOutput("assessment_ahp_note")
        )
      ),
      bslib::card(
        bslib::card_header("Plano de melhorias priorizado"),
        DT::DTOutput("assessment_plan_table"),
        shiny::tags$div(
          class = "how-to-read",
          shiny::tags$strong("Como a prioridade é definida: "),
          "primeiro vêm as melhorias com ganho demonstrado (IC95% do ganho acima de zero); entre elas, a ordem segue o ganho de disponibilidade multiplicado pela importância que os especialistas deram ao critério atacado (peso AHP). ",
          "Assim, o resultado combina o que os avaliadores priorizaram com os ganhos condicionados às hipóteses da simulação. O índice integrado é uma regra heurística de ordenação, não uma solução ótima comprovada."
        )
      ),
      bslib::layout_column_wrap(
        width = 1 / 2,
        bslib::card(
          bslib::card_header("Gargalos e onde buscar a evidência (P7)"),
          DT::DTOutput("assessment_bottleneck_table")
        ),
        bslib::card(
          bslib::card_header("Confiabilidade do resultado"),
          DT::DTOutput("assessment_confidence_table")
        )
      ),
        bslib::accordion(open=FALSE,
          bslib::accordion_panel("Detalhes da frota e cenários",bslib::navset_card_tab(
        id = "results_subtabs",
        bslib::nav_panel(
          "Síntese decisória",
          bslib::layout_column_wrap(
            width = 1 / 3,
            bslib::value_box(title = "Prontidão instantânea", value = shiny::textOutput("value_readiness"), showcase = shiny::icon("truck"), theme = "primary"),
            bslib::value_box(title = "Disponibilidade medida", value = shiny::textOutput("value_availability"), showcase = shiny::icon("gauge-high"), theme = "success"),
            bslib::value_box(title = "MTBF observado", value = shiny::textOutput("value_mtbf"), showcase = shiny::icon("chart-line"), theme = "primary"),
            bslib::value_box(title = "MTTR ativo", value = shiny::textOutput("value_mttr"), showcase = shiny::icon("wrench"), theme = "warning"),
            bslib::value_box(title = "Qualidade dos dados", value = shiny::textOutput("value_quality"), showcase = shiny::icon("database"), theme = "danger"),
            bslib::value_box(title = "Modo de evidência", value = shiny::textOutput("value_evidence_mode"), showcase = shiny::icon("shield-halved"), theme = "primary")
          ),
          shiny::tags$p(class = "small-note",
            "Os seis quadros acima vêm dos registros OBSERVADOS (viaturas e OS cadastradas na aba 1, sub-abas 1B/1C). ",
            "“sem viaturas/OS registradas” significa que não há registros no banco — não é erro e não afeta a simulação; quando há simulação, a disponibilidade aparece como “simulada”. ",
            "Os resultados da simulação estão no topo desta aba e nas sub-abas Configurações e Cenários."),
          bslib::card(bslib::card_header("Conclusão decisória integrada"), shiny::uiOutput("decision_text"))
        ),
        bslib::nav_panel(
          "Configurações",
          bslib::layout_column_wrap(
            width = 1 / 2,
            bslib::card(bslib::card_header("Configuração recomendada / candidatos Pareto"), DT::DTOutput("recommended_table")),
            plot_card("Probabilidade de sucesso da missão e IC95%", plotly::plotlyOutput("decision_success_plot", height = "360px")),
            plot_card("Decomposição do downtime simulado", plotly::plotlyOutput("decision_downtime_plot", height = "360px"))
          ),
          bslib::card(bslib::card_header("Resultados por configuração"), DT::DTOutput("simulation_results_table"))
        ),
        bslib::nav_panel(
          "Cenários",
          bslib::card(bslib::card_header("Resultados filtráveis"), DT::DTOutput("scenario_results_table")),
          bslib::layout_column_wrap(
            width = 1 / 2,
            plot_card(
              "Disponibilidade média por configuração",
              plotly::plotlyOutput("availability_plot", height = "380px"),
              "Verde = a configuração cumpre a meta da missão (limite inferior do IC95% ≥ probabilidade-alvo). Passe o mouse para ver a probabilidade de sucesso."
            ),
            plot_card(
              "Horas com menos de M viaturas prontas",
              plotly::plotlyOutput("hours_below_plot", height = "380px"),
              "Média, por missão simulada, das horas em que a frota ficou abaixo da meta M. Quanto menor, melhor."
            ),
            plot_card(
              "No pior momento da missão, quantas viaturas estavam prontas?",
              plotly::plotlyOutput("recommended_distribution_plot", height = "380px"),
              shiny::uiOutput("recommended_distribution_note")
            )
          )
        ),
        bslib::nav_panel(
          "Custos e sensibilidade",
          plot_card(
            "Custo esperado versus limite inferior de confiança",
            plotly::plotlyOutput("decision_pareto_plot", height = "360px"),
            "Sem custos verificados, o gráfico informa explicitamente a indisponibilidade da análise monetária."
          ),
          plot_card("Fronteira prontidão–custo", plotly::plotlyOutput("pareto_plot", height = "360px")),
          bslib::card(
            bslib::card_header("Sensibilidade de Spearman — associação descritiva, não causal"),
            DT::DTOutput("sensitivity_table")
          )
        )
      )),
          bslib::accordion_panel("Relatório, auditoria e fontes",bslib::navset_card_tab(
        id = "report_subtabs",
        bslib::nav_panel(
          "4A. Exportações e auditoria",
          bslib::layout_column_wrap(
            width = 1 / 2,
            bslib::card(
              bslib::card_header("Relatório completo em Excel"),
              shiny::actionButton("open_excel_report_step5", "Gerar e abrir no Excel", icon = shiny::icon("file-excel"), class = "btn btn-success w-100"),
              shiny::downloadButton("download_excel_report_step5", "Baixar .xlsx", class = "btn btn-outline-success w-100"),
              shiny::actionButton("open_report_folder", "Abrir a pasta dos relatórios", icon = shiny::icon("folder-open"), class = "btn btn-link w-100"),
              shiny::uiOutput("excel_report_status"),
              shiny::tags$p(
                class = "small-note",
                "Abas: Resumo · Gargalos · Melhorias · Pesos AHP · Robustez AHP-TOPSIS · Notas P6 · P7 Evidências · Simulação · RAM observado · Premissas. ",
                "Execute a avaliação na aba 3 antes de exportar."
              )
            ),
            bslib::card(
              bslib::card_header("Relatório em Word com análise em texto"),
              shiny::tags$p(class = "small-note",
                "Documento .docx no padrão do Manual da ESG (Ensaio Acadêmico): folha de rosto, verso com direitos autorais, resumo e abstract, ",
                "seções 1 a 4, referências, glossário e apêndices (A4, Arial 12, margens 3/2 cm, espaçamento 1,5, número de página só a partir da Introdução). ",
                "O corpo (Introdução à Conclusão) respeita o limite de páginas abaixo: figuras e tabelas que não couberem vão para um apêndice. ",
                "Com IA, apenas os resultados agregados desta execução são enviados ao provedor; respostas individuais e e-mails não saem do computador."),
              shiny::tags$details(
                open = "open",
                shiny::tags$summary("Dados da folha de rosto"),
                shiny::textInput("word_meta_author", "Autor (nome completo, sem patente ou titulação)", value = "Gilberto Ferreira Godoy"),
                shiny::textInput("word_meta_title", "Título do trabalho", value = "GESTÃO DO CICLO DE VIDA DA FROTA JLTV NO CORPO DE FUZILEIROS NAVAIS"),
                shiny::textInput("word_meta_subtitle", "Subtítulo (opcional)", value = "Uma abordagem integrada de manutenção, suprimento e apoio à decisão"),
                shiny::textInput("word_meta_nature", "Natureza do trabalho", value = "Artigo"),
                shiny::textInput("word_meta_sector", "Setor da ESG que recebe o trabalho", value = "Instituto Therezinha de Castro"),
                shiny::textInput("word_meta_course", "Curso, por extenso", value = "Curso de Gestão, Logística e Apoio à Decisão"),
                shiny::textInput("word_meta_advisor", "Orientador (titulação e nome completo)", value = ""),
                shiny::textInput("word_meta_city", "Cidade", value = "Rio de Janeiro"),
                shiny::textInput("word_meta_year", "Ano de depósito", value = format(Sys.Date(), "%Y")),
                shiny::numericInput("word_max_pages", "Limite de páginas do corpo (Introdução à Conclusão)", value = 20, min = 12, max = 20, step = 1),
                shiny::tags$p(class = "small-note", "Campo vazio sai entre colchetes na folha de rosto, para preencher no Word.")
              ),
              shiny::selectInput("word_ai_provider", "Redação da análise",
                choices = c("Texto padrão do aplicativo (sem IA)" = "local", "Claude (Anthropic)" = "claude", "ChatGPT (OpenAI)" = "chatgpt"),
                selected = "local"),
              shiny::conditionalPanel(
                condition = "input.word_ai_provider != 'local'",
                shiny::passwordInput("word_ai_key", "Chave da API (ou defina ANTHROPIC_API_KEY / OPENAI_API_KEY)", value = ""),
                shiny::textInput("word_ai_model", "Modelo (opcional)", value = ""),
                shiny::tags$p(class = "small-note",
                  "Chave: é a chave de API (não a senha nem a assinatura do chat). ChatGPT: platform.openai.com → API keys → Create new secret key (começa com sk-). ",
                  "Claude: console.anthropic.com → API keys. A cobrança da API é separada da assinatura do chat. ",
                  "Modelo: deixe VAZIO para usar o padrão (Claude: claude-opus-5-5; ChatGPT: gpt-4o). Se o provedor recusar, digite o nome de um modelo disponível na sua conta. ",
                  "Com IA, o aplicativo envia os resultados completos da execução e a IA analisa e redige a seção 4, a conclusão e o resumo; cada número é conferido, e a geração pode levar alguns minutos. ",
                  "Sem chave ou se a chamada falhar, o Word sai com o texto padrão do aplicativo e um aviso.")
              ),
              shiny::tags$p(class = "small-note", "O relatório inclui a pizza da parcela de cada sistema no tempo parado quando a simulação por sistema foi executada antes."),
              shiny::actionButton("open_word_report", "Gerar e abrir no Word", icon = shiny::icon("file-word"), class = "btn btn-primary w-100"),
              shiny::downloadButton("download_word_report", "Baixar .docx", class = "btn btn-outline-primary w-100"),
              shiny::uiOutput("word_report_status")
            ),
            bslib::card(bslib::card_header("Auditoria da execução"), DT::DTOutput("audit_table"))
          )
        ),
        bslib::nav_panel(
          "4B. Limites de interpretação",
          bslib::card(
            bslib::card_header("Limites de interpretação"),
            shiny::tags$ul(
              shiny::tags$li("Resultados são condicionais aos dados, às distribuições e às hipóteses informadas."),
              shiny::tags$li("As respostas AHP/TOPSIS priorizam investigação; não substituem registros físicos de RAM."),
              shiny::tags$li("P7 aponta evidências e fontes a confirmar; não cria parâmetros físicos ausentes."),
              shiny::tags$li("Prontidão instantânea não é disponibilidade operacional Ao."),
              shiny::tags$li("A recomendação probabilística usa o limite inferior do IC95%."),
              shiny::tags$li("Sem custos verificados e aprovados, o sistema apresenta Pareto e não um ótimo monetário.")
            )
          )
        ),
        bslib::nav_panel(
          "4C. Referências",
          bslib::card(
            bslib::card_header("Referências e funcionamento"),
            documentation_ui()
          )
        )
      ),
            DT::DTOutput("run_metadata_table"), DT::DTOutput("paired_comparison_table"), DT::DTOutput("decision_links_table")))),
      bslib::accordion(open=FALSE,
        bslib::accordion_panel("Registros RAM observados",
          bslib::navset_card_tab(
        id = "ram_subtabs",
        bslib::nav_panel(
          "RAM observado",
            bslib::card(
              bslib::card_header("Indicadores observados e estimabilidade"),
              DT::DTOutput("ram_metrics_table")
            ),
            bslib::layout_column_wrap(
              width = 1 / 2,
              bslib::card(
                bslib::card_header("Modelos candidatos de vida"),
                DT::DTOutput("life_models_table"),
                shiny::tags$p(
                  class = "small-note",
                  "A seleção usa AIC como comparação, em conjunto com plausibilidade e diagnóstico; AIC isolado não prova adequação."
                )
              ),
              plot_card(
                "Curva de confiabilidade do modelo selecionado",
                plotly::plotlyOutput("reliability_plot", height = "360px"),
                "Curva condicional ao ajuste (sem intervalo paramétrico). Sem falhas registradas, mostra a premissa do simulador, tracejada e marcada como hipótese."
              ),
              plot_card(
                "Exposições completas e censuradas",
                plotly::plotlyOutput("censoring_plot", height = "360px")
              ),
              plot_card(
                "Componentes de downtime (observados; sem OS, simulados)",
                plotly::plotlyOutput("observed_downtime_plot", height = "360px")
              )
            ),
            bslib::card(
              bslib::card_header("Tabela de componentes observados"),
              DT::DTOutput("observed_downtime_table")
            ),
        )
      ))),
      shiny::tags$hr(),
      shiny::tags$details(shiny::tags$summary("Referências e funcionamento"),documentation_ui()))
  )

  server = function(input, output, session) {
    session$onSessionEnded(function() {
      if (identical(Sys.getenv("JLTV_DESKTOP_MODE"), "1")) {
        shiny::stopApp()
      }
    })

    db_version = shiny::reactiveVal(0L)
    simulation_state = shiny::reactiveVal(NULL)
    paired_comparison_state = shiny::reactiveVal(NULL)
    improvement_state = shiny::reactiveVal(NULL)
    last_excel_report = shiny::reactiveVal(NULL)
    multicriteria_state = shiny::reactiveVal(NULL)
    active_decision_context = shiny::reactiveVal(NULL)
    questionnaire_sync_state = shiny::reactiveVal("Aguardando sincronização.")
    questionnaire_sync_busy = shiny::reactiveVal(FALSE)
    questionnaire_google_auth_waiting = shiny::reactiveVal(FALSE)
    questionnaire_google_auth_started = shiny::reactiveVal(NULL)

    snapshot_state = shiny::reactive({
      db_version()
      database_snapshot(database_path)
    })

    # v1.4.1 — tabelas RAM/CMMS filtradas pelo modo de evidência.
    analysis_snapshot_state = shiny::reactive({
      filter_cmms_snapshot_by_mode(snapshot_state(), evidence_mode_state()$mode)
    })

    analysis_state = function(fit_models) {
      snapshot = analysis_snapshot_state()
      tryCatch(
        derive_analysis_inputs(snapshot, fit_models = fit_models),
        error = function(error_condition) list(
          error = conditionMessage(error_condition),
          evidence_mode = get_evidence_mode(database_path)$mode,
          quality = data_quality_report(snapshot)
        )
      )
    }

    observed_state = shiny::reactive(analysis_state(FALSE))
    calibrated_state = shiny::reactive(analysis_state(TRUE))

    evidence_mode_state = shiny::reactive({
      db_version()
      get_evidence_mode(database_path)
    })

    current_questionnaire_config = function() {
      defaults = questionnaire_google_defaults()
      list(
        source_mode = input$questionnaire_source_mode %||% defaults$source_mode,
        sheet_url = trimws(input$questionnaire_sheet_url %||% defaults$sheet_url),
        sheet_name = trimws(input$questionnaire_sheet_name %||% defaults$sheet_name),
        auto_refresh = isTRUE(input$questionnaire_auto_refresh),
        refresh_seconds = max(
          300, suppressWarnings(as.numeric(input$questionnaire_refresh_seconds %||% defaults$refresh_seconds))
        ),
        evidence_class = toupper(input$questionnaire_evidence_class %||% defaults$evidence_class)
      )
    }

    output$questionnaire_sync_status = shiny::renderText({
      questionnaire_sync_state()
    })

    questionnaire_snapshot_for_mode = shiny::reactive({
      snapshot = snapshot_state()
      filter_questionnaire_snapshot_by_mode(
        snapshot, evidence_mode_state()$mode
      )
    })

    questionnaire_summary_state = shiny::reactive({
      db_version()
      questionnaire_response_summaries(
        snapshot_state(), evidence_mode_state()$mode
      )
    })

    output$questionnaire_hidden_banner = shiny::renderUI({
      db_version()
      mode_now = evidence_mode_state()$mode
      if (!mode_now %in% c("EMPIRICAL", "SYNTHETIC")) return(NULL)
      counts = tryCatch(questionnaire_class_counts(database_path), error = function(e) NULL)
      if (is.null(counts) || nrow(counts) == 0L) return(NULL)
      visible = sum(counts$n[counts$evidence_class == mode_now])
      hidden = sum(counts$n[counts$evidence_class != mode_now])
      if (visible > 0L || hidden == 0L) return(NULL)
      shiny::tags$div(
        class = "alert alert-warning mt-2",
        shiny::tags$p(paste0(
          hidden, " resposta(s) do questionário estão gravadas como ", setdiff(c("EMPIRICAL", "SYNTHETIC"), mode_now),
          " e não aparecem no modo ", mode_now, "."
        )),
        shiny::actionButton("questionnaire_reclassify", paste0("Reclassificar respostas como ", mode_now), class = "btn btn-primary w-100")
      )
    })

    output$questionnaire_interpretation = shiny::renderUI({
      summary = questionnaire_summary_state()
      overview = summary$overview
      respondents = overview$valor[overview$indicador == "Respondentes no modo selecionado"]
      coverage_p6 = overview$valor[overview$indicador == "Cobertura P6"]
      p7 = overview$valor[overview$indicador == "Registros P7"]
      if (length(respondents) == 0L || !is.finite(respondents) || respondents == 0L) {
        mode_now = evidence_mode_state()$mode
        hidden_message = tryCatch(questionnaire_hidden_message(database_path, mode_now), error = function(e) "")
        if (nzchar(hidden_message)) {
          return(shiny::tags$div(
            class = "alert alert-warning",
            shiny::tags$p(hidden_message)
          ))
        }
        return(shiny::tags$div(
          class = "alert alert-info",
          "Ainda não há respostas do questionário no modo de evidência selecionado. A aba será preenchida automaticamente após a sincronização/importação."
        ))
      }
      shiny::tags$div(
        class = "alert alert-primary",
        paste0(
          "Leitura atual: ", respondents, " avaliador(es); cobertura válida de P6 = ",
          if (length(coverage_p6) && is.finite(coverage_p6)) paste0(coverage_p6, "%") else "não estimável",
          "; registros P7 = ", p7, ". AHP/TOPSIS sintetizam prioridade; P7 indica onde procurar confirmação documental."
        )
      )
    })

    output$questionnaire_response_overview_table = DT::renderDT({
      DT::datatable(
        questionnaire_summary_state()$overview,
        rownames = FALSE, options = list(dom = "t", pageLength = 10)
      )
    })

    output$questionnaire_profile_summary_table = DT::renderDT({
      display = questionnaire_summary_state()$profile
      if (!is.data.frame(display) || nrow(display) == 0L) {
        display = data.frame(status = "Sem dados de perfil no modo selecionado.")
      }
      DT::datatable(display, rownames = FALSE, options = list(dom = "t", pageLength = 10))
    })

    output$questionnaire_ahp_response_summary_table = DT::renderDT({
      DT::datatable(
        questionnaire_summary_state()$ahp,
        rownames = FALSE, filter = "top",
        options = list(scrollX = TRUE, pageLength = 14)
      )
    })

    output$questionnaire_p6_response_summary_table = DT::renderDT({
      DT::datatable(
        questionnaire_summary_state()$p6,
        rownames = FALSE, filter = "top",
        options = list(scrollX = TRUE, pageLength = 20)
      )
    })

    output$questionnaire_p6_heatmap = plotly::renderPlotly({
      display = questionnaire_summary_state()$p6
      shiny::validate(shiny::need(
        is.data.frame(display) && nrow(display) > 0L && any(is.finite(display$media)),
        "Ainda não há avaliações P6 válidas no modo selecionado."
      ))
      # v1.5.0 — mapa de calor ordenado pelo ranking TOPSIS e com escala 1–10.
      heat = data.frame(
        alternative_id = display$alternativa, alternative_name = display$sistema,
        subcriterion_id = display$subcriterio, value = display$media, n_valid = display$n_validas,
        stringsAsFactors = FALSE
      )
      contributions = tryCatch(topsis_criterion_contributions(multicriteria_state()), error = function(e) NULL)
      as_interactive(chart_p6_heatmap(heat, contributions))
    })

    output$questionnaire_p7_summary_table = DT::renderDT({
      display = questionnaire_summary_state()$p7
      if (!is.data.frame(display) || nrow(display) == 0L) {
        display = data.frame(status = "Sem registros P7 no modo selecionado.")
      }
      DT::datatable(display, rownames = FALSE, filter = "top", options = list(scrollX = TRUE, pageLength = 10))
    })

    output$questionnaire_mode_status = shiny::renderText({
      filtered = questionnaire_snapshot_for_mode()
      mode = evidence_mode_state()$mode
      paste0(
        "Modo de evidência ativo: ", mode,
        " | Respondentes usados neste modo: ",
        nrow(filtered$questionnaire_respondents %||% data.frame()),
        " | AHP: ", nrow(filtered$ahp_pairwise %||% data.frame()),
        " | P6: ", nrow(filtered$topsis_scores %||% data.frame()),
        " | P7: ", nrow(filtered$questionnaire_factors %||% data.frame())
      )
    })

    output$questionnaire_import_summary_table = DT::renderDT({
      snapshot = snapshot_state()
      filtered = questionnaire_snapshot_for_mode()
      table_names = c(
        "questionnaire_respondents", "ahp_pairwise",
        "topsis_scores", "questionnaire_factors"
      )
      labels = c(
        "Respondentes", "Comparações AHP",
        "Avaliações P6", "Registros P7"
      )
      total_count = purrr::map_int(
        purrr::set_names(table_names),
        function(name) nrow(snapshot[[name]] %||% data.frame())
      )
      active_count = purrr::map_int(
        purrr::set_names(table_names),
        function(name) nrow(filtered[[name]] %||% data.frame())
      )
      summary = data.frame(
        conjunto = labels,
        total_carregado = total_count,
        usado_no_modo_atual = active_count,
        stringsAsFactors = FALSE
      )
      DT::datatable(
        summary, rownames = FALSE,
        options = list(dom = "t", pageLength = 4)
      )
    })

    output$questionnaire_preview_table = DT::renderDT({
      filtered = questionnaire_snapshot_for_mode()
      choice = input$questionnaire_preview_choice %||% "questionnaire_respondents"
      if (!choice %in% c(
        "questionnaire_respondents", "ahp_pairwise",
        "topsis_scores", "questionnaire_factors"
      )) choice = "questionnaire_respondents"
      display = filtered[[choice]] %||% data.frame()
      if (!is.data.frame(display) || nrow(display) == 0L) {
        display = data.frame(
          status = paste0(
            "Nenhum registro desta tabela no modo ",
            evidence_mode_state()$mode, "."
          ),
          stringsAsFactors = FALSE
        )
      }
      DT::datatable(
        display, rownames = FALSE, filter = "top",
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    perform_questionnaire_sync = function(config = current_questionnaire_config(), authenticate = FALSE, notify = TRUE) {
      if (isTRUE(questionnaire_sync_busy())) return(invisible(NULL))
      questionnaire_sync_busy(TRUE)
      on.exit(questionnaire_sync_busy(FALSE), add = TRUE)
      if (!identical(config$source_mode, "GOOGLE_SHEETS")) {
        stop("Selecione Google Sheets como fonte para sincronizar on-line.", call. = FALSE)
      }
      if (!nzchar(config$sheet_url) || !nzchar(config$sheet_name)) {
        stop("Informe o link da Google Sheets e o nome da aba.", call. = FALSE)
      }
      result = sync_google_questionnaire(
        database_path, config, authenticate = authenticate
      )
      questionnaire_sync_state(result$message)
      if (!is.null(result$sheet_name) && nzchar(result$sheet_name)) {
        shiny::updateTextInput(
          session, "questionnaire_sheet_name", value = result$sheet_name
        )
      }
      if (isTRUE(result$changed)) {
        db_version(db_version() + 1L)
        multicriteria_state(NULL)
      }
      if (isTRUE(notify)) {
        shiny::showNotification(result$message, type = "message", duration = 8)
      }
      invisible(result)
    }

    session$onFlushed(function() {
      config = get_questionnaire_source_config(database_path)
      shiny::updateSelectInput(
        session, "questionnaire_source_mode", selected = config$source_mode
      )
      shiny::updateTextInput(
        session, "questionnaire_sheet_url", value = config$sheet_url
      )
      shiny::updateTextInput(
        session, "questionnaire_sheet_name", value = config$sheet_name
      )
      shiny::updateCheckboxInput(
        session, "questionnaire_auto_refresh", value = config$auto_refresh
      )
      shiny::updateNumericInput(
        session, "questionnaire_refresh_seconds", value = config$refresh_seconds
      )
      shiny::updateSelectInput(
        session, "questionnaire_evidence_class", selected = config$evidence_class
      )
      last_sync = questionnaire_source_value(
        database_path, "questionnaire_last_sync", ""
      )
      last_rows = questionnaire_source_value(
        database_path, "questionnaire_last_rows", ""
      )
      if (nzchar(last_sync)) {
        questionnaire_sync_state(
          paste0("Última sincronização: ", last_sync, " | respostas: ", last_rows)
        )
      }
    }, once = TRUE)

    shiny::observeEvent(input$questionnaire_google_auth, {
      tryCatch(
        {
          config = current_questionnaire_config()
          set_questionnaire_source_values(database_path, list(
            source_mode = "GOOGLE_SHEETS",
            sheet_url = config$sheet_url,
            sheet_name = config$sheet_name,
            auto_refresh = config$auto_refresh,
            refresh_seconds = config$refresh_seconds,
            evidence_class = config$evidence_class
          ))
          launch_questionnaire_google_auth()
          questionnaire_google_auth_started(Sys.time())
          questionnaire_google_auth_waiting(TRUE)
          questionnaire_sync_state(
            "Autenticação Google iniciada. Conclua o login/autorização no navegador que será aberto."
          )
          shiny::showNotification(
            "Abrindo autenticação Google no navegador. Conclua a autorização e volte ao aplicativo.",
            type = "message", duration = 12
          )
        },
        error = function(error_condition) {
          questionnaire_google_auth_waiting(FALSE)
          questionnaire_sync_state(paste("Falha ao iniciar autenticação:", conditionMessage(error_condition)))
          shiny::showNotification(
            conditionMessage(error_condition), type = "error", duration = 12
          )
        }
      )
    })

    shiny::observe({
      shiny::req(isTRUE(questionnaire_google_auth_waiting()))
      shiny::invalidateLater(1000, session)
      status_file = questionnaire_google_auth_status_file()
      if (!file.exists(status_file)) {
        started = shiny::isolate(questionnaire_google_auth_started())
        if (!is.null(started) && as.numeric(difftime(Sys.time(), started, units = "secs")) > 600) {
          questionnaire_google_auth_waiting(FALSE)
          questionnaire_sync_state(
            "A autenticação Google não foi concluída em 10 minutos. Clique novamente em Conectar Google."
          )
        }
        return(invisible(NULL))
      }
      status = paste(readLines(status_file, warn = FALSE), collapse = " ")
      unlink(status_file)
      questionnaire_google_auth_waiting(FALSE)
      if (identical(status, "OK")) {
        tryCatch(
          {
            if (!restore_questionnaire_google_auth()) {
              stop("A autorização foi concluída, mas o token não pôde ser carregado.", call. = FALSE)
            }
            config = current_questionnaire_config()
            config$auto_refresh = FALSE
            set_questionnaire_source_values(database_path, list(
              source_mode = "GOOGLE_SHEETS",
              sheet_url = config$sheet_url,
              sheet_name = config$sheet_name,
              auto_refresh = FALSE,
              refresh_seconds = config$refresh_seconds,
              evidence_class = config$evidence_class
            ))
            shiny::updateCheckboxInput(
              session, "questionnaire_auto_refresh", value = FALSE
            )
            result = perform_questionnaire_sync(
              config, authenticate = FALSE, notify = TRUE
            )
            questionnaire_sync_state(
              paste0(
                "Google conectado. ", result$message,
                " A sincronização automática permanece desligada por segurança de cota; ",
                "ative-a apenas se desejar atualização periódica."
              )
            )
          },
          error = function(error_condition) {
            questionnaire_sync_state(
              paste("Google autorizado, mas a sincronização falhou:", conditionMessage(error_condition))
            )
            shiny::showNotification(
              conditionMessage(error_condition), type = "error", duration = 12
            )
          }
        )
      } else {
        message = sub("^ERROR\\|", "", status)
        questionnaire_sync_state(paste("Falha na autenticação Google:", message))
        shiny::showNotification(message, type = "error", duration = 12)
      }
    })

    shiny::observeEvent(input$questionnaire_use_sheet, {
      tryCatch(
        {
          config = current_questionnaire_config()
          config$source_mode = "GOOGLE_SHEETS"
          set_questionnaire_source_values(database_path, list(
            source_mode = config$source_mode,
            sheet_url = config$sheet_url,
            sheet_name = config$sheet_name,
            auto_refresh = config$auto_refresh,
            refresh_seconds = config$refresh_seconds,
            evidence_class = config$evidence_class
          ))
          perform_questionnaire_sync(config, authenticate = FALSE, notify = TRUE)
        },
        error = function(error_condition) {
          questionnaire_sync_state(paste("Google Sheets não sincronizada:", conditionMessage(error_condition)))
          shiny::showNotification(
            paste(
              conditionMessage(error_condition),
              "Se a planilha for privada, clique em 'Conectar Google' uma única vez."
            ),
            type = "error", duration = 12
          )
        }
      )
    })

    shiny::observeEvent(input$questionnaire_sync_now, {
      tryCatch(
        perform_questionnaire_sync(current_questionnaire_config(), FALSE, TRUE),
        error = function(error_condition) {
          questionnaire_sync_state(paste("Falha na atualização:", conditionMessage(error_condition)))
          shiny::showNotification(conditionMessage(error_condition), type = "error", duration = 10)
        }
      )
    })

    shiny::observe({
      shiny::req(identical(input$questionnaire_source_mode, "GOOGLE_SHEETS"))
      shiny::req(isTRUE(input$questionnaire_auto_refresh))
      if (!restore_questionnaire_google_auth()) {
        questionnaire_sync_state(
          "Sincronização automática aguardando autenticação. Clique em Conectar Google."
        )
        return(invisible(NULL))
      }
      seconds = max(
        300, suppressWarnings(as.numeric(input$questionnaire_refresh_seconds %||% 600))
      )
      last_sync_text = questionnaire_source_value(
        database_path, "questionnaire_last_sync", ""
      )
      last_sync_time = suppressWarnings(as.POSIXct(
        last_sync_text, format = "%Y-%m-%d %H:%M:%S"
      ))
      if (length(last_sync_time) == 1L && !is.na(last_sync_time)) {
        elapsed = as.numeric(difftime(Sys.time(), last_sync_time, units = "secs"))
        if (is.finite(elapsed) && elapsed >= 0 && elapsed < seconds) {
          shiny::invalidateLater(
            as.integer(max(1, seconds - elapsed) * 1000), session
          )
          return(invisible(NULL))
        }
      }
      shiny::invalidateLater(as.integer(seconds * 1000), session)
      tryCatch(
        perform_questionnaire_sync(current_questionnaire_config(), FALSE, FALSE),
        error = function(error_condition) {
          shiny::updateCheckboxInput(
            session, "questionnaire_auto_refresh", value = FALSE
          )
          set_questionnaire_source_values(
            database_path, list(auto_refresh = FALSE)
          )
          questionnaire_sync_state(
            paste(
              "Sincronização automática pausada após erro:",
              conditionMessage(error_condition)
            )
          )
        }
      )
    })

    shiny::observeEvent(input$questionnaire_import_raw, {
      tryCatch(
        {
          shiny::req(input$questionnaire_raw_file)
          evidence_class = toupper(input$questionnaire_file_evidence_class %||% "MODE")
          if (!evidence_class %in% c("EMPIRICAL", "SYNTHETIC")) {
            # "Seguir o modo": EMPIRICAL no modo EMPIRICAL, SYNTHETIC no modo SYNTHETIC; em HYBRID, EMPIRICAL.
            mode_now = evidence_mode_state()$mode
            evidence_class = if (mode_now %in% c("EMPIRICAL", "SYNTHETIC")) mode_now else "EMPIRICAL"
          }
          result = shiny::withProgress(message = "Importando o questionário", value = 0, {
            sync_questionnaire_file(
              database_path, input$questionnaire_raw_file$datapath, evidence_class,
              progress = function(i, n) shiny::setProgress(
                value = i / n, detail = paste0("lendo resposta ", i, " de ", n)
              )
            )
          })
          set_questionnaire_source_values(database_path, list(
            source_mode = "FILE",
            evidence_class = evidence_class
          ))
          db_version(db_version() + 1L)
          multicriteria_state(NULL)
          message = paste0(
            "Arquivo importado: ", result$imported,
            " respondente(s), ", result$ahp, " comparações AHP e ",
            result$p6, " avaliações P6.",
            if (nzchar(result$notes %||% "")) paste0(" ", result$notes) else ""
          )
          questionnaire_sync_state(message)
          shiny::showNotification(message, type = "message", duration = 8)
          hidden_message = questionnaire_hidden_message(database_path, get_evidence_mode(database_path)$mode)
          if (nzchar(hidden_message)) shiny::showNotification(hidden_message, type = "warning", duration = 20)
        },
        error = function(error_condition) {
          questionnaire_sync_state(paste("Falha no arquivo:", conditionMessage(error_condition)))
          shiny::showNotification(conditionMessage(error_condition), type = "error", duration = 10)
        }
      )
    })

    shiny::observeEvent(input$questionnaire_reclassify, {
      tryCatch({
        target = evidence_mode_state()$mode
        if (!target %in% c("EMPIRICAL", "SYNTHETIC")) stop("No modo HYBRID todas as respostas já aparecem.", call. = FALSE)
        changed = reclassify_questionnaire_responses(database_path, target)
        set_questionnaire_source_values(database_path, list(evidence_class = target))
        db_version(db_version() + 1L)
        multicriteria_state(NULL)
        shiny::showNotification(
          paste0("Respostas reclassificadas como ", target, " (", changed, " linhas atualizadas). Recalcule as prioridades."),
          type = "message", duration = 10
        )
      }, error = function(error_condition) {
        shiny::showNotification(conditionMessage(error_condition), type = "error", duration = 10)
      })
    })

    shiny::observeEvent(input$questionnaire_manual_open, {
      pairs = questionnaire_pair_spec()
      alternatives = questionnaire_alternative_spec()
      manual_mode = if (identical(evidence_mode_state()$mode, "SYNTHETIC")) "SYNTHETIC" else "EMPIRICAL"
      ahp_ui = purrr::map(seq_len(nrow(pairs)), function(i) {
        shiny::selectInput(
          paste0("manual_ahp_", gsub("\\.", "_", pairs$code[[i]])),
          paste0(pairs$code[[i]], " — ", pairs$left_label[[i]], " × ", pairs$right_label[[i]]),
          choices = manual_ahp_choices(pairs$left_label[[i]], pairs$right_label[[i]]),
          selected = "1 — importância equivalente"
        )
      })
      p6_ui = purrr::map(names(alternatives), function(code) {
        shiny::textInput(
          paste0("manual_p6_", code),
          paste0(code, " ", alternatives[[code]], " — 10 valores P6 (1–10 ou N/A; separados por ;)"),
          paste(rep("5", 10), collapse = ";")
        )
      })
      p7_ui = purrr::map(1:4, function(i) {
        shiny::tagList(
          shiny::tags$h5(paste("P7 — bloco", i)),
          shiny::selectInput(
            paste0("manual_p7_system_", i), "Sistema/conjunto",
            choices = c("— não informado —" = "", stats::setNames(paste(names(alternatives), alternatives), paste(names(alternatives), alternatives)))
          ),
          shiny::textInput(paste0("manual_p7_identification_", i), "Descrição e PN/NSN, se conhecido", ""),
          shiny::textInput(paste0("manual_p7_occurs_", i), "O que ocorre / o que é necessário", ""),
          shiny::textInput(paste0("manual_p7_evidence_", i), "Fonte para confirmação", "")
        )
      })
      shiny::showModal(shiny::modalDialog(
        title = "Cadastrar resposta manual do questionário",
        size = "l", easyClose = FALSE,
        shiny::selectInput(
          "manual_evidence_class", "Classificação",
          choices = c("EMPIRICAL", "SYNTHETIC"), selected = manual_mode
        ),
        shiny::textInput("manual_contact", "E-mail/celular ou identificador", ""),
        shiny::numericInput("manual_age", "Idade", 40, min = 18, max = 100),
        shiny::numericInput("manual_service_years", "Tempo de serviço (anos)", 15, min = 0, max = 60),
        shiny::textInput("manual_academic", "Formação acadêmica", ""),
        shiny::textInput("manual_military", "Formação militar", ""),
        shiny::textAreaInput("manual_experience", "Experiência com a JLTV", ""),
        shiny::tags$hr(),
        shiny::tags$h4("P1–P5 — AHP"),
        shiny::tagList(ahp_ui),
        shiny::tags$hr(),
        shiny::tags$h4("P6 — TOPSIS"),
        shiny::tags$p(class = "small-note", "Em cada linha, informe dez valores na ordem dos dez subcritérios do questionário."),
        shiny::tagList(p6_ui),
        shiny::tags$hr(),
        shiny::tags$h4("P7 — rastreabilidade"),
        shiny::tagList(p7_ui),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(
            "questionnaire_manual_save", "Salvar resposta",
            class = "btn btn-primary"
          )
        )
      ))
    })

    shiny::observeEvent(input$questionnaire_manual_save, {
      tryCatch(
        {
          pairs = questionnaire_pair_spec()
          alternatives = questionnaire_alternative_spec()
          ahp_values = stats::setNames(purrr::map(seq_len(nrow(pairs)), function(i) {
            input[[paste0("manual_ahp_", gsub("\\.", "_", pairs$code[[i]]))]]
          }), pairs$code)
          p6_values = stats::setNames(purrr::map(names(alternatives), function(code) {
            input[[paste0("manual_p6_", code)]]
          }), names(alternatives))
          p7_values = purrr::map(1:4, function(i) list(
            system = input[[paste0("manual_p7_system_", i)]] %||% "",
            identification = input[[paste0("manual_p7_identification_", i)]] %||% "",
            occurs = input[[paste0("manual_p7_occurs_", i)]] %||% "",
            evidence = input[[paste0("manual_p7_evidence_", i)]] %||% ""
          ))
          values = list(
            contact = input$manual_contact %||% "",
            age = input$manual_age,
            service_years = input$manual_service_years,
            academic_background = input$manual_academic %||% "",
            military_background = input$manual_military %||% "",
            experience = input$manual_experience %||% "",
            ahp = ahp_values, p6 = p6_values, p7 = p7_values
          )
          normalized = manual_questionnaire_normalized(
            values, input$manual_evidence_class %||% "EMPIRICAL"
          )
          append_questionnaire_tables(database_path, normalized)
          set_questionnaire_source_values(database_path, list(source_mode = "MANUAL"))
          db_version(db_version() + 1L)
          multicriteria_state(NULL)
          shiny::removeModal()
          questionnaire_sync_state("Resposta manual adicionada ao banco local.")
          shiny::showNotification("Resposta manual adicionada.", type = "message")
        },
        error = function(error_condition) {
          shiny::showNotification(conditionMessage(error_condition), type = "error", duration = 12)
        }
      )
    })

    evidence_banner_tag = function() {
      evidence = evidence_mode_state()
      text = switch(
        evidence$mode,
        SYNTHETIC = "DADOS SINTÉTICOS — NÃO UTILIZAR COMO EVIDÊNCIA SOBRE A JLTV.",
        HYBRID = "MODO HÍBRIDO — PROJEÇÃO CONDICIONAL COM HIPÓTESES EXPLÍCITAS.",
        EMPIRICAL = "MODO EMPÍRICO — INTERPRETAÇÃO CONDICIONAL À QUALIDADE E À AMOSTRA.",
        paste("MODO NÃO RECONHECIDO:", evidence$mode)
      )
      shiny::tags$div(
        class = paste("evidence-banner", paste0("mode-", evidence$mode)),
        text
      )
    }

    purrr::walk(
      c(
        "decision", "quality", "cmms", "support", "ram", "multicriteria",
        "simulation", "sensitivity", "audit", "result"
      ),
      function(section) {
        output[[paste0("evidence_", section)]] = shiny::renderUI(
          evidence_banner_tag()
        )
      }
    )

    output$value_readiness = shiny::renderText({
      assets = analysis_snapshot_state()$assets
      if (!is.data.frame(assets) || nrow(assets) == 0L) return("sem viaturas/OS registradas")
      active = asset_active_mask(assets)
      if (!any(active, na.rm = TRUE)) return("sem viaturas ativas")
      ready = active & toupper(assets$initial_state) %in% c("OPERATING", "AVAILABLE")
      paste0(sum(ready, na.rm = TRUE), "/", sum(active, na.rm = TRUE), " prontas agora")
    })

    output$value_availability = shiny::renderText({
      assets = analysis_snapshot_state()$assets
      if (!is.data.frame(assets) || nrow(assets) == 0L) {
        sim_ao = tryCatch(assessment_state()$kpis$ao_reference, error = function(e) NA_real_)
        if (is.numeric(sim_ao) && length(sim_ao) == 1L && is.finite(sim_ao)) {
          return(paste0("sem registros; simulada: ", format_percent_br(sim_ao)))
        }
        return("sem viaturas/OS registradas")
      }
      observed = observed_state()
      if (!is.null(observed$error)) return("não estimável — dados incompletos")
      ram = observed$ram
      if (is.finite(ram$Ao_calendar %||% NA_real_)) return(paste("Ao (calendário)", format_percent_br(ram$Ao_calendar)))
      if (is.finite(ram$Ao)) return(paste("Ao (horas de uso)", format_percent_br(ram$Ao)))
      if (is.finite(ram$Aa)) return(paste("Aa", format_percent_br(ram$Aa)))
      if (is.finite(ram$Ai)) return(paste("Ai", format_percent_br(ram$Ai)))
      "não estimável — registros insuficientes"
    })

    output$value_mtbf = shiny::renderText({
      assets = analysis_snapshot_state()$assets
      if (!is.data.frame(assets) || nrow(assets) == 0L) return("sem viaturas/OS registradas")
      observed = observed_state()
      if (!is.null(observed$error) || !is.finite(observed$ram$MTBF)) {
        return("não estimável — registros insuficientes")
      }
      unit = observed$ram$mtbf_unit %||% ""
      paste0(
        format(round(observed$ram$MTBF, 1), decimal.mark = ","),
        if (nzchar(unit)) paste0(" ", unit) else ""
      )
    })

    output$value_mttr = shiny::renderText({
      assets = analysis_snapshot_state()$assets
      if (!is.data.frame(assets) || nrow(assets) == 0L) return("sem viaturas/OS registradas")
      observed = observed_state()
      if (!is.null(observed$error) || !is.finite(observed$ram$MTTR)) {
        return("não estimável — registros insuficientes")
      }
      paste0(format(round(observed$ram$MTTR, 1), decimal.mark = ","), " h")
    })

    output$value_quality = shiny::renderText({
      snapshot = analysis_snapshot_state()
      if (!is.data.frame(snapshot$assets) || nrow(snapshot$assets) == 0L) return("sem viaturas/OS registradas")
      quality = data_quality_report(snapshot)
      errors = sum(quality$issues$severity == "ERROR")
      if (errors == 0L) "sem bloqueios" else paste(errors, "bloqueio(s)")
    })
    output$value_evidence_mode = shiny::renderText(evidence_mode_state()$mode)

    current_decision = shiny::reactive({
      simulation = simulation_state()
      if (is.null(simulation)) {
        selection = list(status = "NO_RESULT", recommended = NULL)
        target = input$target_ready %||% 0
        horizon = input$simulation_horizon %||% 0
        probability = input$target_probability %||% 0
      } else {
        selection = simulation$selection
        target = simulation$target_ready
        horizon = simulation$horizon
        probability = simulation$p_target
      }
      make_decision_text(
        selection, target, horizon, probability,
        evidence_mode_state()$mode
      )
    })

    output$decision_text = shiny::renderUI({
      decision = current_decision()
      shiny::tags$div(
        class = "decision-card",
        shiny::tags$h3(decision$headline),
        shiny::tags$p(decision$body),
        shiny::tags$p(class = "small-note", decision$warning)
      )
    })

    output$recommended_table = DT::renderDT({
      simulation = simulation_state()
      shiny::validate(
        shiny::need(
          !is.null(simulation),
          "Abra 3. Executar Simulação e clique em Executar avaliação agregada completa (ou Simular grade sem melhorias)."
        )
      )
      recommended = simulation$recommended
      if (is.null(recommended) || nrow(recommended) == 0L) {
        recommended = simulation$selection$candidates %||% data.frame()
      }
      DT::datatable(recommended, options = list(scrollX = TRUE, pageLength = 5), rownames = FALSE)
    })

    output$decision_success_plot = plotly::renderPlotly({
      simulation = simulation_state()
      shiny::validate(shiny::need(!is.null(simulation), "Sem resultado de simulação: na aba 3, clique em Executar avaliação agregada completa (ou Simular grade sem melhorias)."))
      results = simulation$summary
      results$support = paste0(
        "D", results$diagnostic_teams, " R", results$repair_teams,
        " B", results$bays, " S", results$initial_stock
      )
      results$approved = results$ci_lower >= simulation$p_target
      plot = ggplot2::ggplot(
        results,
        ggplot2::aes(
          x = fleet_size, y = success_probability, color = support,
          text = paste0(
            configuration_id, "<br>P=", round(success_probability, 3),
            "<br>IC95%=[", round(ci_lower, 3), ", ", round(ci_upper, 3), "]"
          )
        )
      ) +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
          width = 0.08, position = ggplot2::position_dodge(width = 0.25)
        ) +
        ggplot2::geom_point(
          ggplot2::aes(shape = approved), size = 3,
          position = ggplot2::position_dodge(width = 0.25)
        ) +
        ggplot2::geom_hline(
          yintercept = simulation$p_target, linetype = "dashed", color = "#B23A48"
        ) +
        ggplot2::scale_y_continuous(labels = scales::percent_format()) +
        ggplot2::labs(x = "Tamanho da frota", y = "Probabilidade de sucesso", color = "Apoio", shape = "Aprovada") +
        ggplot2::theme_minimal()
      plotly::ggplotly(plot, tooltip = "text")
    })

    output$decision_pareto_plot = plotly::renderPlotly({
      simulation = simulation_state()
      shiny::validate(shiny::need(!is.null(simulation), "Sem resultado de simulação: na aba 3, clique em Executar avaliação agregada completa (ou Simular grade sem melhorias)."))
      results = simulation$summary
      shiny::validate(shiny::need(
        "expected_cost" %in% names(results) && any(is.finite(results$expected_cost)),
        "Custos não disponíveis: marque 'Custos completos e aprovados' somente quando os valores do cenário estiverem sustentados."
      ))
      results$approved = results$ci_lower >= simulation$p_target
      plot = ggplot2::ggplot(
        results,
        ggplot2::aes(
          x = ci_lower, y = expected_cost, color = approved,
          size = fleet_size,
          text = paste0(configuration_id, "<br>Custo=", round(expected_cost, 2))
        )
      ) +
        ggplot2::geom_point(alpha = 0.8) +
        ggplot2::geom_vline(
          xintercept = simulation$p_target, linetype = "dashed", color = "#B23A48"
        ) +
        ggplot2::scale_x_continuous(labels = scales::percent_format()) +
        ggplot2::scale_y_continuous(labels = scales::label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
        ggplot2::scale_color_manual(values = c("FALSE" = "#B23A48", "TRUE" = "#198F8C")) +
        ggplot2::labs(x = "Limite inferior IC95%", y = "Custo esperado", color = "Aprovada", size = "Frota") +
        ggplot2::theme_minimal()
      plotly::ggplotly(plot, tooltip = "text")
    })

    output$decision_downtime_plot = plotly::renderPlotly({
      simulation = simulation_state()
      shiny::validate(shiny::need(!is.null(simulation), "Sem resultado de simulação: na aba 3, clique em Executar avaliação agregada completa (ou Simular grade sem melhorias)."))
      selected = simulation$recommended
      if (is.null(selected) || nrow(selected) == 0L) {
        selected = simulation$summary[which.max(simulation$summary$ci_lower), , drop = FALSE]
      }
      components = data.frame(
        component = c("Diagnóstico", "Fila de recurso", "Espera por peça", "Corretiva", "Preventiva", "Teste"),
        hours = c(
          selected$mean_diagnosis_hours, selected$mean_queue_resource_hours,
          selected$mean_wait_part_hours, selected$mean_corrective_hours,
          selected$mean_preventive_hours, selected$mean_test_hours
        )
      )
      plot = ggplot2::ggplot(
        components,
        ggplot2::aes(x = stats::reorder(component, hours), y = hours, fill = component)
      ) +
        ggplot2::geom_col(show.legend = FALSE) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = rep(c("#1F5A94", "#198F8C", "#D99A2B"), 2)) +
        ggplot2::labs(x = NULL, y = "Viatura-hora média") +
        ggplot2::theme_minimal()
      plotly::ggplotly(plot)
    })

    output$coverage_table = DT::renderDT({
      snapshot = analysis_snapshot_state()
      questionnaire = questionnaire_snapshot_for_mode()
      excluded = attr(snapshot, "cmms_excluded") %||% list()
      life_data = tryCatch(build_life_data(snapshot$assets, snapshot$work_orders), error = function(e) data.frame())
      coverage = data.frame(
        item = c(
          "Ativos RAM/CMMS", "OS RAM/CMMS", "Falhas funcionais RAM/CMMS",
          "Atrasos RAM/CMMS", "Observações censuradas RAM/CMMS",
          "Ativos excluídos pelo modo de evidência", "OS excluídas pelo modo de evidência",
          "Respondentes do questionário", "Comparações AHP",
          "Avaliações P6", "Registros P7"
        ),
        count = c(
          nrow(snapshot$assets), nrow(snapshot$work_orders),
          sum(functional_failure_flags(snapshot$work_orders$is_functional_failure)),
          nrow(snapshot$work_order_delays),
          if ("status" %in% names(life_data)) sum(life_data$status == 0L) else NA_integer_,
          as.integer(excluded$assets %||% 0L), as.integer(excluded$work_orders %||% 0L),
          nrow(questionnaire$questionnaire_respondents %||% data.frame()),
          nrow(questionnaire$ahp_pairwise %||% data.frame()),
          nrow(questionnaire$topsis_scores %||% data.frame()),
          nrow(questionnaire$questionnaire_factors %||% data.frame())
        )
      )
      DT::datatable(coverage, rownames = FALSE, options = list(dom = "t"))
    })

    output$quality_summary_table = DT::renderDT({
      quality = data_quality_report(analysis_snapshot_state())
      DT::datatable(quality$summary, rownames = FALSE, options = list(dom = "t"))
    })
    output$quality_issues_table = DT::renderDT({
      quality = data_quality_report(analysis_snapshot_state())
      display = quality$issues
      if (nrow(display) == 0L) {
        display = data.frame(message = "Nenhum erro bloqueante detectado pelas regras implementadas.")
      }
      DT::datatable(display, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })
    output$life_data_table = DT::renderDT({
      calibrated = calibrated_state()
      shiny::validate(shiny::need(is.null(calibrated$error), calibrated$error %||% "Dados inválidos"))
      DT::datatable(calibrated$life_data, rownames = FALSE, options = list(pageLength = 10))
    })

    shiny::observeEvent(input$save_mode, {
      tryCatch(
        {
          set_evidence_mode(
            database_path, input$mode_choice,
            confirmed = isTRUE(input$confirm_empirical)
          )
          db_version(db_version() + 1L)
          multicriteria_state(NULL)
          shiny::showNotification("Modo de evidência atualizado; AHP/TOPSIS será recalculado para o modo selecionado.", type = "message")
          hidden_after_mode = tryCatch(questionnaire_hidden_message(database_path, get_evidence_mode(database_path)$mode), error = function(e) "")
          if (nzchar(hidden_after_mode)) shiny::showNotification(hidden_after_mode, type = "warning", duration = 20)
        },
        error = function(error_condition) {
          shiny::showNotification(conditionMessage(error_condition), type = "error", duration = 8)
        }
      )
    })

    validate_import = function(table, data) {
      if (table == "assets") return(validate_assets_data(data))
      if (table == "work_orders") return(validate_work_orders_data(data))
      if (table == "work_order_delays") return(validate_delays_data(data))
      if (table == "ahp_pairwise") return(validate_ahp_pairwise_data(data))
      if (table == "topsis_scores") {
        validation = validate_topsis_scores_data(data)
        if (validation$valid) return(list(valid = TRUE, issues = empty_issues()))
        return(list(valid = FALSE, issues = issue_row(
          "ERROR", table, 0L, "score", "INVALID_TOPSIS", validation$message
        )))
      }
      required = switch(
        table,
        resources = c("resource_id", "resource_type", "quantity", "capacity", "cost_horizon", "data_source"),
        parts = c("part_id", "part_name", "criticality", "initial_stock", "reorder_point", "reorder_qty", "lead_time_hours", "unit_cost", "data_source"),
        inventory_transactions = c("part_id", "work_order_id", "occurred_at", "quantity", "transaction_type", "data_source"),
        maintenance_plans = c("plan_id", "plan_name", "trigger_unit", "interval_value", "active_duration_hours", "active", "data_source"),
        questionnaire_respondents = c("respondent_id", "age", "service_years", "academic_background", "military_background", "direct_jltv_experience", "experience_area", "data_source"),
        questionnaire_factors = c("respondent_id", "factor_type", "identification", "related_system", "influence_score", "evidence_reference", "data_source")
      )
      missing = setdiff(required, names(data))
      if (length(missing) > 0L) {
        return(list(valid = FALSE, issues = issue_row(
          "ERROR", table, 0L, paste(missing, collapse = ","), "MISSING_COLUMN",
          paste("Colunas ausentes:", paste(missing, collapse = ", "))
        )))
      }
      if (table == "questionnaire_factors") {
        influence = suppressWarnings(as.numeric(as.character(data$influence_score)))
        invalid = !is.na(influence) & (!is.finite(influence) | influence < 1 | influence > 10)
        if (any(invalid)) return(list(valid = FALSE, issues = issue_row(
          "ERROR", table, which(invalid)[[1]], "influence_score", "INVALID_SCORE",
          "influence_score deve estar entre 1 e 10 ou vazio."
        )))
      }
      list(valid = TRUE, issues = empty_issues())
    }

    shiny::observeEvent(input$import_csv, {
      tryCatch(
        {
          shiny::req(input$upload_csv)
          data = read_csv_flexible(input$upload_csv$datapath)
          validation = validate_import(input$upload_table, data)
          if (!validation$valid) {
            stop(paste(validation$issues$message, collapse = "; "), call. = FALSE)
          }
          connection = open_database(database_path)
          on.exit(DBI::dbDisconnect(connection), add = TRUE)
          DBI::dbWithTransaction(connection, {
            if (identical(input$upload_table, "topsis_scores")) {
              p6_validation = validate_topsis_import_data(
                DBI::dbReadTable(connection, "topsis_scores"), data, input$upload_operation
              )
              if (!isTRUE(p6_validation$valid)) stop(p6_validation$message, call. = FALSE)
            }
            if (input$upload_operation == "replace") {
              if (input$upload_table %in% c("assets", "work_orders", "work_order_delays", "inventory_transactions")) {
                stop(
                  "Por integridade referencial, estas tabelas só podem ser adicionadas pela importação.",
                  call. = FALSE
                )
              }
              DBI::dbExecute(
                connection,
                paste("DELETE FROM", DBI::dbQuoteIdentifier(connection, input$upload_table))
              )
            }
            DBI::dbAppendTable(connection, input$upload_table, data)
          })
          db_version(db_version() + 1L)
          shiny::showNotification("CSV validado e importado.", type = "message")
        },
        error = function(error_condition) {
          shiny::showNotification(conditionMessage(error_condition), type = "error", duration = 10)
        }
      )
    })

    output$download_template = shiny::downloadHandler(
      filename = function() paste0(input$upload_table, "_modelo.csv"),
      content = function(file) {
        columns = csv_template_columns(input$upload_table)
        if (is.null(columns)) stop("Modelo CSV não disponível para esta tabela.", call. = FALSE)
        template = as.data.frame(stats::setNames(rep(list(character()), length(columns)), columns), stringsAsFactors = FALSE)
        utils::write.csv(template, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    output$download_quality = shiny::downloadHandler(
      filename = function() "relatorio_qualidade_JLTV.csv",
      content = function(file) {
        quality = data_quality_report(analysis_snapshot_state())
        data = quality$issues
        if (nrow(data) == 0L) data = data.frame(message = "Sem problemas detectados")
        utils::write.csv(data, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    output$assets_table = DT::renderDT({
      DT::datatable(snapshot_state()$assets, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 8))
    })
    output$orders_table = DT::renderDT({
      DT::datatable(snapshot_state()$work_orders, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 8))
    })
    output$fix_asset_ui = shiny::renderUI({
      assets = snapshot_state()$assets
      if (!is.data.frame(assets) || nrow(assets) == 0L) return(shiny::tags$p(class = "small-note", "Sem viaturas cadastradas."))
      labels = paste0(assets$asset_id, " — ", ifelse(asset_active_mask(assets), "ativa", "inativa"),
                      " — ", assets$data_source)
      shiny::selectInput("fix_asset", "Viatura", choices = stats::setNames(assets$asset_id, labels))
    })
    output$fix_order_ui = shiny::renderUI({
      orders = snapshot_state()$work_orders
      if (!is.data.frame(orders) || nrow(orders) == 0L) return(shiny::tags$p(class = "small-note", "Sem OS cadastradas."))
      labels = paste0(orders$work_order_id, " — ", orders$asset_id, " — ", orders$maintenance_type,
                      " — medidor ", orders$meter_at_event)
      shiny::selectInput("fix_order", "Ordem de serviço", choices = stats::setNames(orders$work_order_id, labels))
    })
    output$change_log_table = DT::renderDT({
      log = snapshot_state()$cmms_change_log
      if (!is.data.frame(log) || nrow(log) == 0L) log = data.frame(status = "Nenhuma correção registrada.")
      DT::datatable(log, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 5, order = list()))
    })
    shiny::observeEvent(input$toggle_asset, {
      tryCatch({
        shiny::req(input$fix_asset)
        new_value = toggle_asset_active(database_path, input$fix_asset, input$fix_reason)
        db_version(db_version() + 1L)
        shiny::updateTextInput(session, "fix_reason", value = "")
        shiny::showNotification(
          paste("Viatura", input$fix_asset, if (new_value == 1L) "reativada." else "desativada."),
          type = "message"
        )
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 10))
    })
    shiny::observeEvent(input$change_asset_unit, {
      tryCatch({
        assets = snapshot_state()$assets
        target = toupper(as.character(input$fix_unit_target %||% "HOURS"))
        ids = if (isTRUE(input$fix_unit_all)) {
          assets$asset_id[asset_active_mask(assets) & toupper(as.character(assets$meter_unit)) != target]
        } else {
          shiny::req(input$fix_asset)
          input$fix_asset
        }
        if (length(ids) == 0L) stop("Nenhuma viatura precisa mudar para ", target, ".", call. = FALSE)
        changed = change_asset_meter_unit(
          database_path, ids, target_unit = target, method = input$fix_unit_method %||% "relabel",
          rate = input$fix_unit_rate, reason = input$fix_reason
        )
        db_version(db_version() + 1L)
        shiny::updateTextInput(session, "fix_reason", value = "")
        shiny::showNotification(
          if (length(changed) == 0L) "Nada a alterar: as viaturas já estavam nessa unidade." else
            paste0("Unidade alterada para ", target, " em ", length(changed), " viatura(s). Registrado na trilha."),
          type = "message", duration = 10
        )
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 12))
    })
    shiny::observeEvent(input$delete_order, {
      tryCatch({
        shiny::req(input$fix_order)
        delete_work_order(database_path, input$fix_order, input$fix_reason)
        db_version(db_version() + 1L)
        shiny::updateTextInput(session, "fix_reason", value = "")
        shiny::showNotification(paste("OS", input$fix_order, "excluída e registrada na trilha."), type = "message")
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 10))
    })

    output$os_asset_ui = shiny::renderUI({
      assets = snapshot_state()$assets
      shiny::selectInput("os_asset", "Viatura", choices = assets$asset_id)
    })

    shiny::observeEvent(input$add_asset, {
      tryCatch(
        {
          row = data.frame(
            asset_id = trimws(input$asset_id), asset_name = trimws(input$asset_name),
            model = trimws(input$asset_model),
            entry_service_date = format(input$asset_entry, "%Y-%m-%d"),
            initial_meter = input$asset_initial_meter,
            current_meter = input$asset_current_meter,
            meter_unit = input$asset_unit, initial_state = input$asset_state,
            active = 1L, data_source = trimws(input$asset_source),
            stringsAsFactors = FALSE
          )
          append_asset(database_path, row)
          db_version(db_version() + 1L)
          shiny::showNotification("Viatura cadastrada.", type = "message")
        },
        error = function(error_condition) {
          shiny::showNotification(conditionMessage(error_condition), type = "error", duration = 8)
        }
      )
    })

    shiny::observeEvent(input$add_order, {
      tryCatch(
        {
          shiny::req(input$os_asset)
          order_row = data.frame(
            work_order_id = trimws(input$os_id), asset_id = input$os_asset,
            maintenance_type = input$os_type,
            is_functional_failure = as.integer(isTRUE(input$os_failure)),
            failure_mode = trimws(input$os_failure_mode),
            failure_class = trimws(input$os_failure_class),
            detected_at = trimws(input$os_detected),
            diagnosis_start_at = trimws(input$os_diag_start),
            diagnosis_end_at = trimws(input$os_diag_end),
            active_repair_start_at = trimws(input$os_repair_start),
            active_repair_end_at = trimws(input$os_repair_end),
            test_start_at = trimws(input$os_test_start),
            released_at = trimws(input$os_released),
            meter_at_event = input$os_meter, status = "COMPLETED",
            description = input$os_description,
            data_source = trimws(input$os_source), stringsAsFactors = FALSE
          )
          delays = NULL
          if (isTRUE(input$os_has_delay)) {
            delays = data.frame(
              work_order_id = trimws(input$os_id), delay_type = input$delay_type,
              start_at = trimws(input$delay_start), end_at = trimws(input$delay_end),
              responsibility_class = input$delay_class, notes = input$delay_notes,
              stringsAsFactors = FALSE
            )
          }
          append_completed_work_order(database_path, order_row, delays)
          db_version(db_version() + 1L)
          shiny::showNotification("OS e atrasos gravados em uma transação.", type = "message")
        },
        error = function(error_condition) {
          shiny::showNotification(conditionMessage(error_condition), type = "error", duration = 10)
        }
      )
    })

    output$resources_table = DT::renderDT({
      DT::datatable(snapshot_state()$resources, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })
    output$parts_table = DT::renderDT({
      DT::datatable(snapshot_state()$parts, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })
    output$plans_table = DT::renderDT({
      DT::datatable(snapshot_state()$maintenance_plans, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })

    shiny::observeEvent(input$save_support, {
      tryCatch(
        {
          mode = evidence_mode_state()$mode
          source = paste0(mode, "_SCENARIO")
          resources = data.frame(
            resource_id = c("BASE-DIAG", "BASE-REPAIR", "BASE-BAY"),
            resource_type = c("DIAGNOSTIC_TEAM", "REPAIR_TEAM", "BAY"),
            quantity = as.integer(c(input$support_diag, input$support_repair, input$support_bays)),
            capacity = as.integer(c(input$support_diag, input$support_repair, input$support_bays)),
            cost_horizon = c(input$cost_diag, input$cost_repair, input$cost_bay),
            data_source = source, stringsAsFactors = FALSE
          )
          parts = data.frame(
            part_id = "BASE-CRITICAL-PART", part_name = "Sobressalente crítico genérico",
            criticality = "HIGH", initial_stock = as.integer(input$support_stock),
            reorder_point = as.integer(input$support_reorder_point),
            reorder_qty = as.integer(input$support_reorder_qty),
            lead_time_hours = input$support_lead, unit_cost = input$cost_part,
            data_source = source, stringsAsFactors = FALSE
          )
          plans = data.frame(
            plan_id = "BASE-PM", plan_name = "Plano-base do cenário",
            trigger_unit = "HOURS", interval_value = max(input$support_pm, 0.01),
            active_duration_hours = input$mean_preventive,
            active = as.integer(input$support_pm > 0), data_source = source,
            stringsAsFactors = FALSE
          )
          replace_support_configuration(database_path, resources, parts, plans)
          db_version(db_version() + 1L)
          shiny::showNotification("Configuração-base do apoio salva.", type = "message")
        },
        error = function(error_condition) {
          shiny::showNotification(conditionMessage(error_condition), type = "error", duration = 8)
        }
      )
    })

    build_ram_metrics_frame = function(calibrated) {
      ram = calibrated$ram
      trend = calibrated$trend %||% list(estimable = FALSE, U = NA_real_, interpretation = "")
      active_assets = calibrated$assets[asset_active_mask(calibrated$assets), , drop = FALSE]
      metrics = data.frame(
        metric = c("MTBF", "MTTR", "ALDT", "Ai", "Aa", "Ao (base horas de uso)",
                   "Ao (calendário)", "Utilização observada", "Tendência de Laplace (U)",
                   "Prontidão instantânea"),
        value = c(
          if (is.finite(ram$MTBF)) round(ram$MTBF, 3) else NA,
          if (is.finite(ram$MTTR)) round(ram$MTTR, 3) else NA,
          if (is.finite(ram$ALDT)) round(ram$ALDT, 3) else NA,
          if (is.finite(ram$Ai)) round(ram$Ai, 5) else NA,
          if (is.finite(ram$Aa)) round(ram$Aa, 5) else NA,
          if (is.finite(ram$Ao)) round(ram$Ao, 5) else NA,
          if (is.finite(ram$Ao_calendar %||% NA_real_)) round(ram$Ao_calendar, 5) else NA,
          if (is.finite(ram$observed_utilization %||% NA_real_)) round(ram$observed_utilization, 4) else NA,
          if (is.finite(trend$U)) round(trend$U, 3) else NA,
          if (nrow(active_assets) > 0L) {
            sum(toupper(active_assets$initial_state) %in% c("OPERATING", "AVAILABLE")) /
              nrow(active_assets)
          } else NA_real_
        ),
        unit = c(
          if (identical(ram$exposure_unit, "KM")) "km/falha" else "horas de exposição/falha",
          "horas ativas", "horas/falha", "%", "%", "%", "%",
          "h de uso / h de calendário", "estatística z", "%"
        ),
        definition = c(
          "Exposição operacional das viaturas ativas dividida por suas falhas funcionais.",
          "Média do reparo corretivo ativo.",
          "Atraso logístico registrado por falha.",
          "Falha e reparo corretivo ativo.",
          "Inclui manutenção ativa corretiva e preventiva.",
          "Exposição / (exposição + downtime); horas de uso no numerador.",
          "1 − downtime / tempo de calendário observado; comparável à disponibilidade simulada.",
          "Referência de tempo de paz para o parâmetro de utilização da missão (não é aplicada automaticamente).",
          "Testa se os intervalos entre falhas mudam com o uso (hipótese de renovação).",
          "Estado atual das viaturas ativas; não é Ao."
        ),
        estimable = c(
          is.finite(ram$MTBF), is.finite(ram$MTTR), is.finite(ram$ALDT),
          is.finite(ram$Ai), is.finite(ram$Aa), is.finite(ram$Ao),
          is.finite(ram$Ao_calendar %||% NA_real_), is.finite(ram$observed_utilization %||% NA_real_),
          isTRUE(trend$estimable), nrow(active_assets) > 0L
        ),
        reason = c(
          if (is.finite(ram$MTBF)) "" else "Faltam exposição ou falhas identificadas.",
          if (is.finite(ram$MTTR)) "" else "Faltam tempos de reparo ativo completos.",
          if (is.finite(ram$ALDT)) "" else "Faltam atrasos completos por falha.",
          ram$availability_labels[["Ai"]], ram$availability_labels[["Aa"]],
          ram$availability_labels[["Ao"]], ram$availability_labels[["Ao_calendar"]],
          if (is.finite(ram$observed_utilization %||% NA_real_)) "" else "Requer exposição em HOURS e exposure_log de todas as viaturas ativas.",
          trend$interpretation %||% "",
          ram$availability_labels[["snapshot_readiness"]]
        ),
        stringsAsFactors = FALSE
      )
      metrics
    }
    output$ram_metrics_table = DT::renderDT({
      calibrated = calibrated_state()
      shiny::validate(shiny::need(is.null(calibrated$error), calibrated$error %||% "Dados inválidos"))
      DT::datatable(build_ram_metrics_frame(calibrated), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })

    recompute_multicriteria = function(persist = FALSE) {
      aggregation = input$score_aggregation %||% "mean"
      consistent_only = isTRUE(input$only_consistent_ahp %||% TRUE)
      coverage = suppressWarnings(as.numeric(input$min_cell_coverage %||% 0))
      if (!is.finite(coverage)) coverage = 0
      result = compute_multicriteria(
        snapshot_state(),
        score_aggregation = aggregation,
        include_only_consistent = consistent_only,
        evidence_mode = evidence_mode_state()$mode,
        min_cell_coverage = coverage / 100
      )
      if (isTRUE(persist)) {
        result$mc_run_id = persist_multicriteria_result(
          database_path, result, aggregation, consistent_only
        )
        db_version(db_version() + 1L)
      }
      result$input_signature = multicriteria_signature()
      multicriteria_state(result)
      result
    }

    shiny::observeEvent(input$recompute_multicriteria, {
      tryCatch({
        result = recompute_multicriteria(TRUE)
        shiny::showNotification(
          paste("AHP e TOPSIS recalculados e auditados:", result$mc_run_id),
          type = "message", duration = 8
        )
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 10))
    })

    # Signatures use effective sources; audit inserts do not change scientific inputs.
    multicriteria_signature = shiny::reactive({
      snap = snapshot_state()
      digest::digest(list(sources = snap[c("questionnaire_respondents", "ahp_pairwise", "topsis_scores", "questionnaire_factors")],
        mode = evidence_mode_state()$mode, aggregation = input$score_aggregation %||% "mean",
        consistent = input$only_consistent_ahp %||% TRUE, coverage = input$min_cell_coverage %||% 0), algo = "sha256")
    })
    shiny::observe({
      signature = multicriteria_signature()
      current = shiny::isolate(multicriteria_state())
      if (is.null(current) || !identical(current$input_signature, signature)) {
        try(recompute_multicriteria(FALSE), silent = TRUE)
      }
    }, priority = 200)

    scenario_settings = shiny::reactive({
      values = shiny::reactiveValuesToList(input)
      ids = names(values)
      keep = grepl("^(target_|simulation_|mission_|initial_unavailable$|failure_|mean_|duration_cv$|support_|grid_|cost_|costs_complete$|common_random_numbers$|pm_initial_phase$|diag_fleet$|diag_reference$|lever_use_|lever_mag_)", ids)
      values[sort(ids[keep])]
    })
    scenario_signature = shiny::reactive({
      sources = analysis_snapshot_state()
      sources = sources[c("assets", "exposure_log", "work_orders", "work_order_delays", "resources", "parts", "inventory_transactions", "maintenance_plans", "decision_scenario_links")]
      digest::digest(list(sources = sources, mode = evidence_mode_state()$mode,
        questionnaire = multicriteria_signature(), settings = scenario_settings(), decision_context = active_decision_context()), algo = "sha256")
    })
    previous_scenario_signature = shiny::reactiveVal(NULL)
    shiny::observe({
      signature = scenario_signature()
      previous = shiny::isolate(previous_scenario_signature())
      if (!is.null(previous) && !identical(signature, previous)) {
        had_result = shiny::isolate(!is.null(simulation_state()) || !is.null(improvement_state()) || !is.null(paired_comparison_state()))
        simulation_state(NULL)
        paired_comparison_state(NULL)
        improvement_state(NULL)
        last_excel_report(NULL)
        if (had_result) shiny::showNotification("Dados ou parâmetros alterados: resultados anteriores retirados. Execute novamente para atualizar gráficos e relatório.", type = "warning", duration = 12)
      }
      previous_scenario_signature(signature)
    }, priority = 100)

    ensure_current_results = function() {
      current = scenario_signature()
      for (result in list(simulation_state(), improvement_state(), paired_comparison_state())) {
        if (!is.null(result) && !identical(result$input_signature, current)) {
          stop("Os dados ou parâmetros mudaram desde a execução. Execute novamente antes de gerar o relatório.", call. = FALSE)
        }
      }
      invisible(TRUE)
    }

    output$topsis_bottleneck_plot = plotly::renderPlotly({
      result = multicriteria_state()
      contributions = tryCatch(topsis_criterion_contributions(result), error = function(e) data.frame())
      shiny::validate(shiny::need(
        nrow(contributions) > 0L,
        result$topsis$reason %||% result$ahp$error %||% "Importe as respostas e clique em Recalcular AHP + TOPSIS."
      ))
      as_interactive(chart_bottlenecks(contributions))
    })
    output$ahp_weights_plot = plotly::renderPlotly({
      result = multicriteria_state()
      shiny::validate(shiny::need(
        is.list(result) && is.data.frame(result$ahp$subcriteria_weights) && nrow(result$ahp$subcriteria_weights) > 0L,
        result$ahp$error %||% "Pesos AHP ainda não estimáveis."
      ))
      as_interactive(chart_ahp_weights(result))
    })

    output$ahp_consistency_table = DT::renderDT({
      result = multicriteria_state()
      shiny::validate(shiny::need(!is.null(result), "Recalcule AHP + TOPSIS."))
      display = result$ahp$consistency %||% data.frame(status = result$ahp$error %||% "AHP não estimável")
      DT::datatable(display, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })

    output$ahp_weights_table = DT::renderDT({
      result = multicriteria_state()
      shiny::validate(shiny::need(!is.null(result), "Recalcule AHP + TOPSIS."))
      display = result$ahp$subcriteria_weights %||% data.frame(status = result$ahp$error %||% "AHP não estimável")
      DT::datatable(display, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })

    output$ahp_individual_table = DT::renderDT({
      result = multicriteria_state()
      shiny::validate(shiny::need(!is.null(result), "Recalcule AHP + TOPSIS."))
      display = result$ahp$individual_diagnostics %||% data.frame(status = "Sem comparações AHP.")
      DT::datatable(display, rownames = FALSE, filter = "top", options = list(scrollX = TRUE, pageLength = 12))
    })

    output$topsis_ranking_table = DT::renderDT({
      result = multicriteria_state()
      shiny::validate(shiny::need(!is.null(result), "Recalcule AHP + TOPSIS."))
      display = if (isTRUE(result$topsis$estimable)) result$topsis$ranking else data.frame(status = result$topsis$reason %||% "TOPSIS não estimável")
      excluded = result$topsis$excluded
      caption = if (is.data.frame(excluded) && nrow(excluded) > 0L) {
        paste0(
          "Fora do ranking por falta de nota (N/A ou cobertura insuficiente): ",
          paste0(excluded$alternative_id, " ", excluded$alternative_name, " [", excluded$subcriterios_sem_nota, "]", collapse = "; "),
          "."
        )
      } else NULL
      DT::datatable(display, rownames = FALSE, caption = caption, options = list(dom = "t", scrollX = TRUE))
    })

    # v1.5.4 — histogramas, dispersão dos especialistas e robustez do ranking (aba 2).
    mc_robustness_state = shiny::reactive({
      mc = multicriteria_state()
      if (is.null(mc) || !isTRUE(mc$topsis$estimable)) return(NULL)
      list(
        stability = tryCatch(mc_rank_stability(mc), error = function(e) NULL),
        sensitivity = tryCatch(mc_method_sensitivity(mc), error = function(e) NULL)
      )
    })
    mc_plot_output = function(builder, empty_message) {
      plotly::renderPlotly({
        mc = multicriteria_state()
        plot = if (is.null(mc)) NULL else tryCatch(builder(mc), error = function(e) NULL)
        shiny::validate(shiny::need(!is.null(plot), empty_message))
        as_interactive(plot)
      })
    }
    output$ahp_cr_histogram_plot = mc_plot_output(chart_ahp_cr_histogram, "Calcule as prioridades com respostas completas de ao menos um avaliador.")
    output$ahp_weight_dispersion_plot = mc_plot_output(chart_ahp_weight_dispersion, "Calcule as prioridades para ver a dispersão dos pesos.")
    output$p6_histogram_plot = mc_plot_output(chart_p6_histogram, "Sem notas P6 válidas.")
    output$p6_boxplot_plot = mc_plot_output(chart_p6_boxplot_by_system, "Sem notas P6 válidas.")
    output$rank_probability_plot = plotly::renderPlotly({
      state = mc_robustness_state()
      plot = if (is.null(state)) NULL else chart_rank_probability(state$stability)
      shiny::validate(shiny::need(!is.null(plot), "A robustez exige ranking TOPSIS estimável e ao menos 3 avaliadores."))
      as_interactive(plot)
    })
    output$mc_robustness_text = shiny::renderUI({
      state = mc_robustness_state()
      shiny::req(!is.null(state))
      lines = mc_robustness_text(multicriteria_state(), state$stability, state$sensitivity)
      if (length(lines) == 0L) return(NULL)
      shiny::tags$ul(purrr::map(lines, shiny::tags$li))
    })
    output$rank_stability_table = DT::renderDT({
      state = mc_robustness_state()
      shiny::validate(shiny::need(!is.null(state) && !is.null(state$stability), "Sem reamostragem disponível."))
      DT::datatable(mc_stability_display(state$stability), rownames = FALSE, options = list(dom = "t", scrollX = TRUE, pageLength = 12))
    })
    output$method_sensitivity_table = DT::renderDT({
      state = mc_robustness_state()
      shiny::validate(shiny::need(!is.null(state) && is.data.frame(state$sensitivity), "Sem ranking TOPSIS estimável."))
      DT::datatable(mc_method_sensitivity_display(state$sensitivity), rownames = FALSE, options = list(dom = "t", scrollX = TRUE, pageLength = 12))
    })

    output$topsis_coverage_table = DT::renderDT({
      result = multicriteria_state()
      shiny::validate(shiny::need(!is.null(result), "Recalcule AHP + TOPSIS."))
      display = result$aggregated_scores
      if (!is.data.frame(display) || nrow(display) == 0L) display = data.frame(status = "Sem avaliações P6 válidas.")
      DT::datatable(display, rownames = FALSE, filter = "top", options = list(scrollX = TRUE, pageLength = 15))
    })

    output$questionnaire_factors_table = DT::renderDT({
      factors = questionnaire_snapshot_for_mode()$questionnaire_factors
      if (!is.data.frame(factors) || nrow(factors) == 0L) factors = data.frame(status = "Sem fatores P7 importados.")
      DT::datatable(factors, rownames = FALSE, filter = "top", options = list(scrollX = TRUE, pageLength = 10))
    })

    output$multicriteria_runs_table = DT::renderDT({
      runs = snapshot_state()$multicriteria_runs
      if (!is.data.frame(runs) || nrow(runs) == 0L) runs = data.frame(status = "Nenhum processamento auditado.")
      DT::datatable(runs, rownames = FALSE, filter = "top", options = list(scrollX = TRUE, pageLength = 8))
    })

    decision_target_choices = shiny::reactive({
      result = multicriteria_state()
      choices = character()
      if (!is.null(result) && isTRUE(result$topsis$estimable)) {
        rank = result$topsis$ranking
        labels = paste0("TOPSIS #", rank$rank, " — ", rank$alternative_name)
        values = paste0("TOPSIS:", rank$alternative_id)
        choices = stats::setNames(values, labels)
      }
      factors = questionnaire_snapshot_for_mode()$questionnaire_factors
      if (is.data.frame(factors) && nrow(factors) > 0L) {
        labels = paste0("P7 — ", factors$identification)
        values = paste0("P7:", factors$factor_id)
        choices = c(choices, stats::setNames(values, labels))
      }
      choices
    })

    output$decision_target_ui = shiny::renderUI({
      choices = decision_target_choices()
      if (length(choices) == 0L) return(shiny::tags$p(class = "small-note", "Importe e processe o questionário primeiro."))
      shiny::selectInput("decision_target", "Prioridade/fator", choices = choices)
    })

    shiny::observeEvent(input$save_decision_link, {
      tryCatch({
        shiny::req(input$decision_target)
        source_kind = if (grepl("^TOPSIS:", input$decision_target)) "TOPSIS" else "P7"
        target_id = sub("^[^:]+:", "", input$decision_target)
        target_label = names(decision_target_choices())[match(input$decision_target, decision_target_choices())]
        if (length(target_label) != 1L || is.na(target_label) || !nzchar(target_label)) target_label = target_id
        source_text = trimws(input$scenario_evidence_source)
        if (!nzchar(source_text)) stop("Informe a fonte/evidência dos valores do cenário.", call. = FALSE)
        baseline = suppressWarnings(as.numeric(input$scenario_baseline))
        alternative = suppressWarnings(as.numeric(input$scenario_alternative))
        if (!is.finite(baseline) || !is.finite(alternative)) stop("Valores do cenário devem ser finitos.", call. = FALSE)
        parameter = as.character(input$scenario_parameter)
        integer_nonnegative = c("fleet_size", "initial_stock", "reorder_point", "reorder_qty")
        integer_positive = c("diagnostic_teams", "repair_teams", "bays")
        positive = c("lead_time_hours", "mean_diagnosis", "mean_corrective", "mean_preventive", "mean_test")
        if (parameter %in% integer_nonnegative && any(c(baseline, alternative) < 0 | c(baseline, alternative) != floor(c(baseline, alternative)))) {
          stop("Este parâmetro exige valores inteiros não negativos.", call. = FALSE)
        }
        if (parameter %in% integer_positive && any(c(baseline, alternative) < 1 | c(baseline, alternative) != floor(c(baseline, alternative)))) {
          stop("Este parâmetro exige valores inteiros positivos.", call. = FALSE)
        }
        if (parameter %in% positive && any(c(baseline, alternative) <= 0)) {
          stop("Este parâmetro exige valores positivos.", call. = FALSE)
        }
        if (parameter == "pm_interval" && any(c(baseline, alternative) < 0)) {
          stop("Intervalo preventivo deve ser >= 0; zero desativa PM.", call. = FALSE)
        }
        if (parameter == "part_required_probability" && any(c(baseline, alternative) < 0 | c(baseline, alternative) > 1)) {
          stop("Probabilidade de exigir peça deve estar entre 0 e 1.", call. = FALSE)
        }
        link_id = paste0("LINK-", format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC"), "-", sample.int(9999, 1))
        current_mc = multicriteria_state()
        mc_run_id = if (!is.null(current_mc) && !is.null(current_mc$mc_run_id)) current_mc$mc_run_id else NA_character_
        row = data.frame(
          link_id = link_id,
          created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
          decision_source = source_kind, target_id = target_id,
          target_label = target_label, simulation_parameter = parameter,
          baseline_value = baseline, alternative_value = alternative,
          evidence_source = source_text, multicriteria_run_id = mc_run_id,
          notes = trimws(input$scenario_notes), active = 1L,
          stringsAsFactors = FALSE
        )
        connection = open_database(database_path)
        on.exit(DBI::dbDisconnect(connection), add = TRUE)
        DBI::dbWithTransaction(connection, {
          DBI::dbExecute(connection, "UPDATE decision_scenario_links SET active = 0 WHERE active = 1")
          DBI::dbAppendTable(connection, "decision_scenario_links", row)
        })
        db_version(db_version() + 1L)
        shiny::showNotification("Vínculo decisório–simulação salvo.", type = "message")
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 10))
    })

    output$decision_links_table = DT::renderDT({
      links = snapshot_state()$decision_scenario_links
      if (!is.data.frame(links) || nrow(links) == 0L) links = data.frame(status = "Nenhum vínculo salvo.")
      DT::datatable(links, rownames = FALSE, filter = "top", options = list(scrollX = TRUE, pageLength = 8))
    })

    selected_decision_link = function() {
      links = snapshot_state()$decision_scenario_links
      if (!is.data.frame(links) || nrow(links) == 0L) stop("Salve um vínculo antes de aplicá-lo.", call. = FALSE)
      active = links[links$active == 1L, , drop = FALSE]
      if (nrow(active) == 0L) stop("Não há vínculo ativo.", call. = FALSE)
      active[nrow(active), , drop = FALSE]
    }

    apply_scenario_parameter = function(link, value, variant) {
      parameter = as.character(link$simulation_parameter[[1]])
      if (parameter == "fleet_size") shiny::updateTextInput(session, "grid_fleet", value = as.character(round(value)))
      if (parameter == "diagnostic_teams") shiny::updateTextInput(session, "grid_diag", value = as.character(round(value)))
      if (parameter == "repair_teams") shiny::updateTextInput(session, "grid_repair", value = as.character(round(value)))
      if (parameter == "bays") shiny::updateTextInput(session, "grid_bays", value = as.character(round(value)))
      if (parameter == "initial_stock") shiny::updateTextInput(session, "grid_stock", value = as.character(round(value)))
      if (parameter == "lead_time_hours") shiny::updateNumericInput(session, "support_lead", value = value)
      if (parameter == "reorder_point") shiny::updateNumericInput(session, "support_reorder_point", value = round(value))
      if (parameter == "reorder_qty") shiny::updateNumericInput(session, "support_reorder_qty", value = round(value))
      if (parameter == "pm_interval") shiny::updateNumericInput(session, "support_pm", value = value)
      if (parameter == "mean_diagnosis") shiny::updateNumericInput(session, "mean_diagnosis", value = value)
      if (parameter == "mean_corrective") shiny::updateNumericInput(session, "mean_corrective", value = value)
      if (parameter == "mean_preventive") shiny::updateNumericInput(session, "mean_preventive", value = value)
      if (parameter == "mean_test") shiny::updateNumericInput(session, "mean_test", value = value)
      if (parameter == "part_required_probability") shiny::updateNumericInput(session, "support_part_probability", value = value)
      active_decision_context(list(
        link_id = as.character(link$link_id[[1]]), variant = variant,
        decision_source = as.character(link$decision_source[[1]]),
        target_id = as.character(link$target_id[[1]]), target_label = as.character(link$target_label[[1]]),
        simulation_parameter = parameter, applied_value = value,
        evidence_source = as.character(link$evidence_source[[1]]),
        multicriteria_run_id = if ("multicriteria_run_id" %in% names(link)) as.character(link$multicriteria_run_id[[1]]) else NA_character_
      ))
    }

    shiny::observeEvent(input$apply_link_baseline, {
      tryCatch({
        link = selected_decision_link()
        apply_scenario_parameter(link, as.numeric(link$baseline_value[[1]]), "BASELINE")
        shiny::showNotification("Cenário-base aplicado aos controles do simulador.", type = "message")
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 8))
    })
    shiny::observeEvent(input$apply_link_alternative, {
      tryCatch({
        link = selected_decision_link()
        apply_scenario_parameter(link, as.numeric(link$alternative_value[[1]]), "ALTERNATIVE")
        shiny::showNotification("Cenário alternativo aplicado aos controles do simulador.", type = "message")
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 8))
    })

    shiny::observeEvent(input$compare_link_paired, {
      tryCatch({
        link = selected_decision_link()
        require_finite_inputs(simulation_numeric_labels)
        parameter = as.character(link$simulation_parameter[[1]])
        simulation = simulation_state()
        reference = if (!is.null(simulation) && !is.null(simulation$recommended) &&
                        nrow(simulation$recommended) > 0L) {
          simulation$recommended[1, , drop = FALSE]
        } else {
          data.frame(
            fleet_size = min(parse_integer_sequence(input$grid_fleet)),
            diagnostic_teams = min(parse_integer_sequence(input$grid_diag)),
            repair_teams = min(parse_integer_sequence(input$grid_repair)),
            bays = min(parse_integer_sequence(input$grid_bays)),
            initial_stock = min(parse_integer_sequence(input$grid_stock))
          )
        }
        config = build_base_config_from_inputs(reference$fleet_size[[1]])
        config$diagnostic_teams = as.integer(reference$diagnostic_teams[[1]])
        config$repair_teams = as.integer(reference$repair_teams[[1]])
        config$bays = as.integer(reference$bays[[1]])
        config$initial_stock = as.integer(reference$initial_stock[[1]])
        params = build_params_from_inputs()
        lead_cv = input$support_lead_cv %||% input$duration_cv
        base = apply_scenario_value(config, params, parameter, as.numeric(link$baseline_value[[1]]), input$duration_cv, lead_cv)
        alt = apply_scenario_value(config, params, parameter, as.numeric(link$alternative_value[[1]]), input$duration_cv, lead_cv)
        if (min(base$config$fleet_size, alt$config$fleet_size) < input$target_ready) {
          stop("O tamanho de frota do cenário é menor que a meta M.", call. = FALSE)
        }
        result = shiny::withProgress(message = "Comparando cenários com as mesmas sementes...", value = 0.2, {
          compare_scenarios_paired(
            base$config, base$params, alt$config, alt$params,
            horizon = input$simulation_horizon, target_ready = as.integer(input$target_ready),
            replications = as.integer(input$simulation_replications),
            seed = as.integer(input$simulation_seed)
          )
        })
        result$link_id = as.character(link$link_id[[1]])
        result$parameter = parameter
        result$reference = configuration_id(as.list(config))
        result$input_signature = scenario_signature()
        result$input_settings = scenario_settings()
        paired_comparison_state(result)
        shiny::showNotification("Comparação pareada concluída.", type = "message")
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 10))
    })

    output$paired_comparison_table = DT::renderDT({
      result = paired_comparison_state()
      shiny::validate(shiny::need(!is.null(result), "Salve um vínculo e clique em Comparar base × alternativo."))
      display = result$comparison
      numeric_columns = c("base", "alternativo", "diferenca", "ic95_inferior", "ic95_superior")
      display[numeric_columns] = purrr::map(display[numeric_columns], function(x) round(x, 4))
      display$parametro = result$parameter
      display$configuracao_referencia = result$reference
      DT::datatable(display, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })

    output$life_models_table = DT::renderDT({
      calibrated = calibrated_state()
      shiny::validate(shiny::need(is.null(calibrated$error), calibrated$error %||% "Dados inválidos"))
      fitted = calibrated$fitted_models
      display = fitted$models
      if (!isTRUE(fitted$estimable) || nrow(display) == 0L) {
        life_now = calibrated$life_data
        n_fail = if (is.data.frame(life_now) && nrow(life_now) > 0L) sum(life_now$status == 1L, na.rm = TRUE) else 0L
        n_exposure = if (is.data.frame(life_now)) nrow(life_now) else 0L
        display = data.frame(
          status = "não estimável com os registros atuais",
          reason = fitted$reason %||% "Registros insuficientes.",
          falhas_observadas = n_fail,
          falhas_necessarias = 2L,
          viaturas_com_exposicao = n_exposure,
          como_habilitar = "Cadastre OS corretivas e exposição em horas (aba 1. Dados). Enquanto isso, a curva ao lado mostra a premissa do simulador.",
          stringsAsFactors = FALSE
        )
      } else {
        display$selecionado = display$model == fitted$selected_model
        display$criterio = "AICc"
      }
      DT::datatable(display, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })

    output$reliability_plot = plotly::renderPlotly({
      calibrated = calibrated_state()
      shiny::validate(shiny::need(is.null(calibrated$error), calibrated$error %||% "Dados inválidos"))
      curve = selected_reliability_curve(calibrated$fitted_models)
      assumed = FALSE
      if (nrow(curve) == 0L) {
        curve = tryCatch(assumed_reliability_curve(build_failure_model()), error = function(e) NULL)
        assumed = TRUE
      }
      shiny::validate(shiny::need(is.data.frame(curve) && nrow(curve) > 0L, "Informe a distribuição de falhas na aba 3 (premissa do simulador) para ver a curva."))
      plot = ggplot2::ggplot(curve, ggplot2::aes(x = time, y = reliability)) +
        ggplot2::geom_line(color = if (assumed) "#D99A2B" else "#1F5A94", linewidth = 1.1, linetype = if (assumed) "dashed" else "solid") +
        ggplot2::scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
        ggplot2::labs(
          x = if (assumed) "Tempo até a falha (h)" else "Exposição desde renovação", y = "Confiabilidade R(t)",
          subtitle = if (assumed) paste0("PREMISSA do simulador (", unique(curve$model), "), não observada: sem falhas registradas") else paste("Modelo selecionado:", unique(curve$model))
        ) +
        ggplot2::theme_minimal()
      plotly::ggplotly(plot)
    })

    output$censoring_plot = plotly::renderPlotly({
      calibrated = calibrated_state()
      shiny::validate(shiny::need(is.null(calibrated$error), calibrated$error %||% "Dados inválidos"))
      life = calibrated$life_data
      if (!is.data.frame(life) || nrow(life) == 0L) {
        simulation = simulation_state()
        sim_rep = if (is.null(simulation)) NULL else (simulation$recommended_replications %||% simulation$replications)
        shiny::validate(shiny::need(
          is.data.frame(sim_rep) && nrow(sim_rep) > 0L && "failures" %in% names(sim_rep),
          "Sem exposição/falha observadas. Para ver as falhas esperadas, rode a simulação na aba 3 (Executar avaliação agregada completa); para dados observados, cadastre OS e exposição na aba 1."
        ))
        plot = ggplot2::ggplot(data.frame(falhas = sim_rep$failures), ggplot2::aes(x = falhas)) +
          ggplot2::geom_histogram(bins = 25, fill = "#D99A2B", colour = "white") +
          ggplot2::labs(
            x = "Falhas na frota por réplica", y = "Réplicas",
            subtitle = "SIMULADO (não é exposição observada): distribuição das falhas ao longo do horizonte"
          ) +
          ggplot2::theme_minimal()
        plotly::ggplotly(plot)
      } else {
      life$observation = ifelse(life$status == 1L, "Falha", "Censura à direita")
      plot = ggplot2::ggplot(
        life,
        ggplot2::aes(x = stats::reorder(asset_id, duration), y = duration, color = observation)
      ) +
        ggplot2::geom_point(size = 3, alpha = 0.8) +
        ggplot2::coord_flip() +
        ggplot2::scale_color_manual(values = c("Falha" = "#B23A48", "Censura à direita" = "#198F8C")) +
        ggplot2::labs(x = "Viatura", y = "Exposição", color = "Observação") +
        ggplot2::theme_minimal()
      plotly::ggplotly(plot)
      }
    })

    downtime_observed = shiny::reactive({
      calibrated = calibrated_state()
      shiny::req(is.null(calibrated$error))
      observed_downtime_components(calibrated$work_orders, calibrated$delays)
    })
    downtime_has_observed = function(data) {
      is.data.frame(data) && nrow(data) > 0L && any(is.finite(data$hours)) && sum(data$hours, na.rm = TRUE) > 0
    }
    output$observed_downtime_table = DT::renderDT({
      data = downtime_observed()
      if (!downtime_has_observed(data)) {
        sim = simulated_downtime_components(simulation_state())
        if (!is.null(sim)) {
          data = data.frame(component = sim$component, hours = round(sim$hours, 2), fonte = "simulação (viatura-hora média); sem OS observadas", stringsAsFactors = FALSE)
        } else {
          data = data.frame(component = "Sem OS observadas e sem simulação", hours = NA_real_,
                            fonte = "Rode a simulação (aba 3) ou cadastre OS (aba 1)", stringsAsFactors = FALSE)
        }
      }
      DT::datatable(data, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })
    output$observed_downtime_plot = plotly::renderPlotly({
      data = downtime_observed()
      simulated = FALSE
      if (!downtime_has_observed(data)) {
        data = simulated_downtime_components(simulation_state())
        simulated = TRUE
      }
      shiny::validate(shiny::need(
        downtime_has_observed(data),
        "Sem OS observadas. Para ver a decomposição do downtime, rode a simulação na aba 3 (Executar avaliação agregada completa) ou cadastre OS na aba 1."
      ))
      plot = ggplot2::ggplot(
        data, ggplot2::aes(x = stats::reorder(component, hours), y = hours, fill = component)
      ) +
        ggplot2::geom_col(show.legend = FALSE) + ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = rep(c("#1F5A94", "#198F8C", "#D99A2B", "#B23A48", "#607D8B", "#7A5C99"), 2)) +
        ggplot2::labs(
          x = NULL, y = if (simulated) "Viatura-hora média (simulado)" else "Horas registradas",
          subtitle = if (simulated) "SIMULADO (não é observação): sem OS registradas" else NULL
        ) + ggplot2::theme_minimal()
      plotly::ggplotly(plot)
    })

    shiny::observeEvent(input$use_observed_calibration, {
      tryCatch({
        calibrated = calibrated_state()
        if (!is.null(calibrated$error)) stop(calibrated$error, call. = FALSE)
        fitted = calibrated$fitted_models
        if (!identical(calibrated$ram$exposure_unit %||% "", "HOURS")) {
          stop(
            "A calibração automática do modelo de falhas exige exposição em HOURS. Registros em KM não são convertidos sem uma taxa de utilização explicitamente validada.",
            call. = FALSE
          )
        }
        if (!isTRUE(fitted$estimable)) stop(fitted$reason %||% "Modelo de vida não estimável.", call. = FALSE)
        model = fitted$selected_model
        pars = fitted$parameters
        if (model == "exponential") {
          shiny::updateSelectInput(session, "failure_distribution", selected = "exponential")
          shiny::updateNumericInput(session, "failure_parameter_1", value = unname(pars[["mean"]]))
        } else if (model == "weibull") {
          shiny::updateSelectInput(session, "failure_distribution", selected = "weibull")
          shiny::updateNumericInput(session, "failure_parameter_1", value = unname(pars[["scale"]]))
          shiny::updateNumericInput(session, "failure_parameter_2", value = unname(pars[["shape"]]))
        } else if (model == "lognormal") {
          mean_value = exp(unname(pars[["meanlog"]]) + unname(pars[["sdlog"]])^2 / 2)
          cv_value = sqrt(exp(unname(pars[["sdlog"]])^2) - 1)
          shiny::updateSelectInput(session, "failure_distribution", selected = "lognormal")
          shiny::updateNumericInput(session, "failure_parameter_1", value = mean_value)
          shiny::updateNumericInput(session, "failure_parameter_2", value = cv_value)
        }
        wo = calibrated$work_orders
        hours_between = function(a, b) as.numeric(difftime(parse_datetime_utc(b), parse_datetime_utc(a), units = "hours"))
        set_if_valid = function(input_id, values) {
          values = values[is.finite(values) & values > 0]
          if (length(values) > 0L) shiny::updateNumericInput(session, input_id, value = mean(values))
        }
        maintenance_type = toupper(as.character(wo$maintenance_type))
        set_if_valid("mean_diagnosis", hours_between(wo$diagnosis_start_at, wo$diagnosis_end_at))
        active = hours_between(wo$active_repair_start_at, wo$active_repair_end_at)
        set_if_valid("mean_corrective", active[maintenance_type == "CORRECTIVE"])
        set_if_valid("mean_preventive", active[maintenance_type == "PREVENTIVE"])
        set_if_valid("mean_test", hours_between(wo$test_start_at, wo$released_at))
        # v1.4.1 — variabilidade observada do reparo corretivo (exige >= 3 registros).
        corrective_values = active[maintenance_type == "CORRECTIVE"]
        corrective_values = corrective_values[is.finite(corrective_values) & corrective_values > 0]
        if (length(corrective_values) >= 3L) {
          shiny::updateNumericInput(
            session, "duration_cv",
            value = round(stats::sd(corrective_values) / mean(corrective_values), 3)
          )
        }
        # Viaturas ativas que hoje não estão prontas iniciam a missão indisponíveis.
        active_assets = calibrated$assets[asset_active_mask(calibrated$assets), , drop = FALSE]
        unavailable_now = sum(!toupper(active_assets$initial_state) %in% c("OPERATING", "AVAILABLE"))
        shiny::updateNumericInput(session, "initial_unavailable", value = unavailable_now)
        shiny::showNotification(
          "Calibração observada aplicada aos campos estimáveis (vida, tempos, CV do reparo e viaturas indisponíveis). Utilização da missão, suprimento e custos não foram inferidos.",
          type = "message", duration = 10
        )
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 10))
    })

    output$calibration_summary = shiny::renderUI({
      calibrated = calibrated_state()
      if (!is.null(calibrated$error)) {
        return(shiny::tags$p(class = "small-note text-danger", calibrated$error))
      }
      ram = calibrated$ram
      trend = calibrated$trend %||% list(estimable = FALSE)
      merged = attr(calibrated$life_data, "merged_failures") %||% 0L
      shiny::tags$ul(
        class = "small-note",
        shiny::tags$li(paste0(
          "Modelo de vida selecionado (AICc): ",
          if (isTRUE(calibrated$fitted_models$estimable)) calibrated$fitted_models$selected_model else "não estimável",
          ". Preventivas registradas encerram o intervalo de vida (censura), coerente com a renovação na simulação."
        )),
        shiny::tags$li(paste0(
          "Utilização observada: ",
          if (is.finite(ram$observed_utilization %||% NA_real_)) {
            paste0(format(round(ram$observed_utilization, 3), decimal.mark = ","),
                   " h de uso por h de calendário (referência; defina a utilização da missão na aba 3).")
          } else "não estimável (requer exposição em HOURS e exposure_log)."
        )),
        shiny::tags$li(paste0(
          "Tendência de Laplace: ",
          if (isTRUE(trend$estimable)) paste0("U = ", format(round(trend$U, 2), decimal.mark = ","), ". ", trend$interpretation)
          else trend$interpretation %||% "não estimável."
        )),
        if (merged > 0L) shiny::tags$li(paste0(merged, " falha(s) no mesmo medidor foram consolidadas em eventos únicos."))
      )
    })

    build_failure_model = function() {
      type = input$failure_distribution
      if (type == "exponential") {
        return(list(type = "exponential", rate = 1 / input$failure_parameter_1))
      }
      if (type == "weibull") {
        return(list(
          type = "weibull", shape = input$failure_parameter_2,
          scale = input$failure_parameter_1
        ))
      }
      mean_cv_lognormal_model(
        input$failure_parameter_1, input$failure_parameter_2
      )
    }

    build_base_config_from_inputs = function(fleet_size = 1L) {
      costs = if (isTRUE(input$costs_complete)) {
        c(
          input$cost_asset, input$cost_diag, input$cost_repair, input$cost_bay,
          input$cost_part, input$cost_corrective, input$cost_preventive,
          input$cost_downtime
        )
      } else {
        rep(NA_real_, 8)
      }
      list(
        fleet_size = as.integer(fleet_size),
        diagnostic_teams = as.integer(input$support_diag),
        repair_teams = as.integer(input$support_repair),
        bays = as.integer(input$support_bays),
        initial_stock = as.integer(input$support_stock),
        reorder_point = as.integer(input$support_reorder_point),
        reorder_qty = as.integer(input$support_reorder_qty),
        pm_interval = if (is.finite(input$support_pm) && input$support_pm > 0) input$support_pm else Inf,
        utilization = as.numeric(input$mission_utilization %||% 1),
        stagger_pm = identical(input$pm_initial_phase, "staggered"),
        initial_unavailable = as.integer(input$initial_unavailable %||% 0),
        mission_tolerance_hours = as.numeric(input$mission_tolerance %||% 0),
        asset_cost_horizon = costs[[1]],
        diagnostic_team_cost_horizon = costs[[2]],
        repair_team_cost_horizon = costs[[3]],
        bay_cost_horizon = costs[[4]], part_unit_cost = costs[[5]],
        corrective_event_cost = costs[[6]], preventive_event_cost = costs[[7]],
        downtime_hour_cost = costs[[8]]
      )
    }

    build_params_from_inputs = function() {
      list(
        life_model = build_failure_model(),
        diagnosis_model = mean_cv_lognormal_model(input$mean_diagnosis, input$duration_cv),
        corrective_model = mean_cv_lognormal_model(input$mean_corrective, input$duration_cv),
        preventive_model = mean_cv_lognormal_model(input$mean_preventive, input$duration_cv),
        test_model = mean_cv_lognormal_model(input$mean_test, input$duration_cv),
        lead_time_model = mean_cv_lognormal_model(input$support_lead, input$support_lead_cv %||% input$duration_cv),
        part_required_probability = input$support_part_probability,
        pm_part_required_probability = if (is.null(input$support_pm_part_probability) ||
                                           !is.finite(input$support_pm_part_probability)) {
          NA_real_
        } else {
          input$support_pm_part_probability
        }
      )
    }

    # v1.4.1 — campos numéricos vazios geram mensagem clara em vez de erro interno.
    require_finite_inputs = function(labels) {
      missing = names(labels)[!purrr::map_lgl(purrr::set_names(names(labels)), function(id) {
        value = input[[id]]
        is.numeric(value) && length(value) == 1L && is.finite(value)
      })]
      if (length(missing) > 0L) {
        stop(paste0("Preencha com um número válido: ", paste(unname(labels[missing]), collapse = "; "), "."), call. = FALSE)
      }
      invisible(TRUE)
    }

    simulation_numeric_labels = c(
      target_ready = "Meta M", simulation_horizon = "Horizonte H",
      target_probability = "Probabilidade-alvo", simulation_replications = "Replicações",
      simulation_seed = "Semente", mission_utilization = "Utilização na missão",
      initial_unavailable = "Viaturas indisponíveis no início", mission_tolerance = "Tolerância abaixo de M",
      failure_parameter_1 = "Parâmetro 1 do modelo de falhas", failure_parameter_2 = "Parâmetro 2 do modelo de falhas",
      mean_diagnosis = "Diagnóstico médio", mean_corrective = "Reparo corretivo médio",
      mean_preventive = "Preventiva média", mean_test = "Teste/liberação médio",
      duration_cv = "Coeficiente de variação", support_diag = "Equipes de diagnóstico (base)",
      support_repair = "Equipes de reparo (base)", support_bays = "Boxes (base)",
      support_pm = "Intervalo PM", support_stock = "Estoque inicial (base)",
      support_reorder_point = "Ponto de reposição", support_reorder_qty = "Lote de reposição",
      support_lead = "Prazo médio de reposição", support_lead_cv = "CV do prazo de reposição",
      support_part_probability = "Probabilidade de exigir peça"
    )

    # v1.5.7 — avisa quando um campo, por construção do modelo, não pode alterar o resultado
    # (por exemplo, preventiva que não chega a ocorrer dentro do horizonte).
    output$field_effect_hint = shiny::renderUI({
      notes = character()
      pm = input$support_pm; horizon = input$simulation_horizon; util = input$mission_utilization
      numeric_ok = function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
      if (numeric_ok(pm) && numeric_ok(horizon) && numeric_ok(util) && pm > 0 &&
          identical(input$pm_initial_phase %||% "fresh", "fresh") && horizon * util < pm) {
        notes = c(notes, paste0(
          "Com a preventiva recém-concluída no início, o intervalo PM (", format(pm, decimal.mark = ","),
          " h de uso) é maior que o horizonte × utilização (", format(round(horizon * util, 1), decimal.mark = ","),
          " h de uso): nenhuma preventiva ocorre na missão. Por isso o intervalo PM, a duração da preventiva e a probabilidade de peça na preventiva não alteram o resultado. Para testá-los, use a fase de preventiva distribuída ou um horizonte maior."
        ))
      }
      rt = input$support_repair; bays = input$support_bays
      if (numeric_ok(rt) && numeric_ok(bays) && rt != bays) {
        notes = c(notes, paste0(
          "A capacidade de reparo é o menor valor entre equipes (", rt, ") e boxes (", bays, "), isto é, ", min(rt, bays),
          " reparos simultâneos. Aumentar só o ", if (rt > bays) "número de equipes" else "número de boxes",
          " não altera o resultado; aumente os dois."
        ))
      }
      notes = c(notes, "A meta M altera a chance de cumprir a missão; a disponibilidade média não depende de M.",
                "Para a grade de dimensionamento, valem os campos da grade; a frota, as equipes, os boxes e o estoque acima definem a situação de referência das melhorias.")
      shiny::tags$div(class = "small-note", purrr::map(notes, shiny::tags$p))
    })

    output$replications_hint = shiny::renderUI({
      p = input$target_probability
      n = input$simulation_replications
      if (!is.numeric(p) || !is.finite(p) || p < 0 || p > 1) return(NULL)
      n_min = minimum_replications_for_target(p)
      ok = is.numeric(n) && is.finite(n) && n >= n_min
      shiny::tags$p(
        class = if (ok) "small-note" else "small-note text-danger",
        if (is.finite(n_min)) {
          paste0("Para demonstrar p_alvo = ", format(p, decimal.mark = ","),
                 " pelo limite inferior do IC95% são necessárias ao menos ", n_min,
                 " replicações por configuração", if (ok) "." else " — aumente as replicações.")
        } else {
          "p_alvo = 1 nunca é demonstrável pelo limite inferior do IC95%; use valor menor que 1."
        }
      )
    })

    save_simulation_audit = function(result, params, decision_context = NULL, base_config = NULL) {
      run_id = paste0(
        "RUN-", format(Sys.time(), "%Y%m%d%H%M%OS3", tz = "UTC"),
        "-", result$seed, "-", sprintf("%04d", sample.int(9999L, 1L))
      )
      run_id = gsub("[.]", "", run_id)
      scenario_id = paste0("SCENARIO-", run_id)
      connection = open_database(database_path)
      on.exit(DBI::dbDisconnect(connection), add = TRUE)
      DBI::dbWithTransaction(connection, {
        DBI::dbExecute(
          connection,
          paste0(
            "INSERT INTO simulation_scenarios(scenario_id, scenario_name, evidence_mode, ",
            "parameters_json, created_at) VALUES(?, ?, ?, ?, ?)"
          ),
          params = list(
            scenario_id,
            if (is.null(decision_context)) "Grade integrada frota e apoio" else paste("Grade integrada —", decision_context$variant, decision_context$target_label),
            evidence_mode_state()$mode,
            jsonlite::toJSON(list(
              simulation_parameters = params,
              base_config = purrr::map(base_config %||% list(), function(x) if (is.numeric(x) && length(x) == 1L && is.infinite(x)) "Inf" else x),
              horizon = result$horizon, target_ready = result$target_ready,
              p_target = result$p_target,
              replications_per_configuration = result$replications_per_configuration,
              common_random_numbers = isTRUE(result$common_random_numbers),
              grid = result$grid_spec,
              decision_context = decision_context
            ), auto_unbox = TRUE, null = "null", na = "null", digits = NA),
            format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
          )
        )
        DBI::dbExecute(
          connection,
          paste0(
            "INSERT INTO simulation_runs(run_id, scenario_id, seed, replications, ",
            "model_version, started_at, completed_at, status) VALUES(?, ?, ?, ?, ?, ?, ?, ?)"
          ),
          params = list(
            run_id, scenario_id, as.integer(result$seed),
            as.integer(result$replications_per_configuration), JLTV_MODEL_VERSION,
            format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
            format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"), "COMPLETED"
          )
        )
        for (row_index in seq_len(nrow(result$summary))) {
          row = result$summary[row_index, , drop = FALSE]
          configuration_columns = c(
            "fleet_size", "diagnostic_teams", "repair_teams", "bays", "initial_stock"
          )
          DBI::dbExecute(
            connection,
            paste0(
              "INSERT INTO simulation_results(run_id, configuration_json, metrics_json) ",
              "VALUES(?, ?, ?)"
            ),
            params = list(
              run_id,
              jsonlite::toJSON(row[configuration_columns], dataframe = "rows", auto_unbox = TRUE),
              jsonlite::toJSON(row, dataframe = "rows", auto_unbox = TRUE)
            )
          )
        }
      })
      run_id
    }

    execute_simulation = function() {
      {
        {
          quality = data_quality_report(analysis_snapshot_state())
          if (!quality$analysis_ready) {
            stop("Corrija os erros bloqueantes na aba 1. Dados.", call. = FALSE)
          }
          require_finite_inputs(simulation_numeric_labels)
          fleet = parse_integer_sequence(input$grid_fleet)
          diagnosis = parse_integer_sequence(input$grid_diag)
          repair = parse_integer_sequence(input$grid_repair)
          bays = parse_integer_sequence(input$grid_bays)
          stock = parse_integer_sequence(input$grid_stock)
          if (any(fleet < input$target_ready)) {
            stop("Todo tamanho de frota deve ser maior ou igual à meta M.", call. = FALSE)
          }
          if (any(fleet < input$initial_unavailable)) {
            stop("Viaturas indisponíveis no início não podem exceder o menor tamanho de frota da grade.", call. = FALSE)
          }
          if (input$mission_utilization <= 0 || input$mission_utilization > 1) {
            stop("A utilização na missão deve estar entre 0 (exclusive) e 1.", call. = FALSE)
          }
          if (input$target_probability < 0 || input$target_probability > 1) {
            stop("A probabilidade-alvo deve estar entre 0 e 1.", call. = FALSE)
          }
          n_min = minimum_replications_for_target(input$target_probability)
          if (input$simulation_replications < n_min) {
            stop(
              if (is.finite(n_min)) {
                paste0(
                  "Com probabilidade-alvo ", format(input$target_probability, decimal.mark = ","),
                  " são necessárias ao menos ", n_min,
                  " replicações por configuração; com menos, nenhuma configuração pode ser aprovada, ",
                  "mesmo com 100% de sucessos."
                )
              } else {
                "Probabilidade-alvo igual a 1 não é demonstrável pelo limite inferior do IC95%."
              },
              call. = FALSE
            )
          }
          combination_count = length(fleet) * length(diagnosis) * length(repair) *
            length(bays) * length(stock)
          if (combination_count > 250L) {
            stop("A grade excede 250 configurações; reduza as sequências.", call. = FALSE)
          }
          if (combination_count * input$simulation_replications > 50000) {
            stop("A execução excede 50.000 replicações; reduza a grade ou as replicações.", call. = FALSE)
          }
          if (any(repair > min(bays))) {
            shiny::showNotification(
              "Há configurações com mais equipes de reparo do que boxes. A capacidade efetiva de reparo é o menor dos dois; as equipes excedentes não alteram o resultado (coluna effective_repair_capacity).",
              type = "warning", duration = 12
            )
          }
          if (!identical(input$pm_initial_phase, "staggered") && input$support_pm > 0 &&
              input$simulation_horizon * input$mission_utilization >= input$support_pm) {
            shiny::showNotification(
              paste0(
                "Com todas as viaturas iniciando com preventiva recém-concluída e horizonte maior que o intervalo de PM, ",
                "as viaturas que não falharem chegarão juntas à preventiva (onda simultânea). Se a frota não parte de uma ",
                "preparação comum, use 'Fase do ciclo distribuída' na aba 3."
              ),
              type = "warning", duration = 15
            )
          }
          if (input$support_reorder_qty == 0 && input$support_part_probability > 0) {
            shiny::showNotification(
              "Lote de reposição igual a zero: não há ressuprimento. Viaturas que exigirem peça com estoque esgotado ficarão em espera até o fim do horizonte.",
              type = "warning", duration = 12
            )
          }
          base_config = build_base_config_from_inputs(min(fleet))
          params = build_params_from_inputs()
          grid = build_configuration_grid(
            fleet_sizes = fleet, diagnostic_teams = diagnosis,
            repair_teams = repair, bays = bays, initial_stocks = stock,
            base_config = base_config
          )
          result = shiny::withProgress(
            message = "Simulando configurações de frota e apoio...",
            value = 0,
            {
              evaluated = evaluate_grid(
                grid = grid, params = params,
                horizon = input$simulation_horizon,
                target_ready = as.integer(input$target_ready),
                replications = as.integer(input$simulation_replications),
                seed = as.integer(input$simulation_seed),
                p_target = input$target_probability,
                costs_complete = isTRUE(input$costs_complete),
                common_random_numbers = isTRUE(input$common_random_numbers),
                progress = function(index, total) {
                  shiny::incProgress(
                    0.9 / total,
                    detail = paste0("configuração ", index, " de ", total)
                  )
                }
              )
              shiny::incProgress(0.05, detail = "Consolidando decisão e auditoria")
              evaluated
            }
          )
          result$evidence_mode = evidence_mode_state()$mode
          result$model_version = JLTV_MODEL_VERSION
          result$decision_context = active_decision_context()
          result$grid_spec = list(
            fleet = input$grid_fleet, diagnostic_teams = input$grid_diag,
            repair_teams = input$grid_repair, bays = input$grid_bays,
            initial_stock = input$grid_stock
          )
          result$base_config = base_config
          result$input_signature = scenario_signature()
          result$input_settings = scenario_settings()
          # v1.4.1: o resultado é exibido antes da gravação da auditoria; uma
          # falha de gravação não descarta a simulação.
          result$run_id = "não gravado"
          simulation_state(result)
          audit_id = tryCatch(
            save_simulation_audit(result, params, result$decision_context, base_config),
            error = function(error_condition) {
              shiny::showNotification(
                paste("Simulação concluída, mas a auditoria não foi gravada no banco:", conditionMessage(error_condition)),
                type = "warning", duration = 15
              )
              NA_character_
            }
          )
          if (!is.na(audit_id)) {
            result$run_id = audit_id
            simulation_state(result)
            db_version(db_version() + 1L)
            shiny::showNotification(
              paste("Simulação concluída:", result$run_id), type = "message", duration = 8
            )
          }
        }
      }
      invisible(simulation_state())
    }
    run_simulation_with_feedback = function() {
      tryCatch(
        execute_simulation(),
        error = function(error_condition) {
          shiny::showNotification(conditionMessage(error_condition), type = "error", duration = 12)
        }
      )
    }
    shiny::observeEvent(input$run_simulation, {
      completed = run_simulation_with_feedback()
      if (is.list(completed) && !is.null(completed$summary)) navigate_to("resultados")
    })

    output$simulation_status = shiny::renderUI({
      simulation = simulation_state()
      if (is.null(simulation)) {
        return(shiny::tags$p(
          "Nenhuma execução nesta sessão. Revise os parâmetros e clique em Executar avaliação agregada completa (ou Simular grade sem melhorias)."
        ))
      }
      decision = current_decision()
      shiny::tagList(
        shiny::tags$h4(decision$headline),
        shiny::tags$p(decision$body),
        shiny::tags$p(class = "small-note", decision$warning)
      )
    })
    output$run_metadata_table = DT::renderDT({
      simulation = simulation_state()
      shiny::validate(shiny::need(!is.null(simulation), "Sem execução."))
      metadata = data.frame(
        field = c("run_id", "seed", "modelo", "modo", "configurações", "replicações/configuração",
                  "horizonte", "meta M", "p_alvo", "utilização na missão", "ciclo de PM no início",
                  "viaturas indisponíveis no início", "tolerância abaixo de M (h)",
                  "números aleatórios comuns"),
        value = c(
          simulation$run_id, simulation$seed, simulation$model_version,
          simulation$evidence_mode, nrow(simulation$summary),
          simulation$replications_per_configuration, simulation$horizon,
          simulation$target_ready, simulation$p_target,
          simulation$base_config$utilization %||% 1,
          if (isTRUE(simulation$base_config$stagger_pm)) "fase distribuída" else "recém-concluída (todas)",
          simulation$base_config$initial_unavailable %||% 0,
          simulation$base_config$mission_tolerance_hours %||% 0,
          if (isTRUE(simulation$common_random_numbers)) "sim" else "não"
        ),
        stringsAsFactors = FALSE
      )
      DT::datatable(metadata, rownames = FALSE, options = list(dom = "t"))
    })
    output$simulation_results_table = DT::renderDT({
      simulation = simulation_state()
      shiny::validate(shiny::need(!is.null(simulation), "Sem resultado de simulação: na aba 3, clique em Executar avaliação agregada completa (ou Simular grade sem melhorias)."))
      DT::datatable(
        simulation$summary, rownames = FALSE,
        filter = "top", options = list(scrollX = TRUE, pageLength = 10)
      )
    })
    output$scenario_results_table = DT::renderDT({
      simulation = simulation_state()
      shiny::validate(shiny::need(!is.null(simulation), "Sem resultado de simulação: na aba 3, clique em Executar avaliação agregada completa (ou Simular grade sem melhorias)."))
      DT::datatable(
        simulation$summary, rownames = FALSE,
        filter = "top", options = list(scrollX = TRUE, pageLength = 12)
      )
    })

    output$pareto_plot = plotly::renderPlotly({
      simulation = simulation_state()
      shiny::validate(shiny::need(!is.null(simulation), "Sem resultado de simulação: na aba 3, clique em Executar avaliação agregada completa (ou Simular grade sem melhorias)."))
      results = simulation$summary
      shiny::validate(shiny::need(
        "expected_cost" %in% names(results) && any(is.finite(results$expected_cost)),
        "Custos não disponíveis: marque 'Custos completos e aprovados' somente quando os valores do cenário estiverem sustentados."
      ))
      results$feasible = results$ci_lower >= simulation$p_target
      plot = ggplot2::ggplot(
        results,
        ggplot2::aes(
          x = ci_lower, y = expected_cost, color = feasible,
          size = fleet_size, text = configuration_id
        )
      ) +
        ggplot2::geom_point(alpha = 0.8) +
        ggplot2::geom_vline(xintercept = simulation$p_target, linetype = "dashed") +
        ggplot2::scale_x_continuous(labels = scales::percent_format()) +
        ggplot2::labs(x = "Limite inferior IC95%", y = "Custo esperado", color = "Capaz", size = "Frota") +
        ggplot2::theme_minimal()
      plotly::ggplotly(plot, tooltip = "text")
    })
    output$availability_plot = plotly::renderPlotly({
      simulation = simulation_state()
      shiny::validate(shiny::need(!is.null(simulation), "Sem resultado de simulação: na aba 3, clique em Executar avaliação agregada completa (ou Simular grade sem melhorias)."))
      as_interactive(chart_configuration_availability(simulation$summary, simulation$p_target))
    })
    output$hours_below_plot = plotly::renderPlotly({
      simulation = simulation_state()
      shiny::validate(shiny::need(!is.null(simulation), "Sem resultado de simulação: na aba 3, clique em Executar avaliação agregada completa (ou Simular grade sem melhorias)."))
      results = simulation$summary
      results$configuration_id = factor(results$configuration_id, levels = results$configuration_id[order(-results$mean_hours_below_target)])
      results$label = vapply(results$mean_hours_below_target, format_number_br, character(1), digits = 1)
      results$tooltip = paste0(results$configuration_id, "<br>", results$label, " h abaixo de M por missão (média)")
      plot = ggplot2::ggplot(
        results,
        ggplot2::aes(x = configuration_id, y = mean_hours_below_target, text = tooltip)
      ) +
        ggplot2::geom_col(fill = "#D99A2B", width = 0.7) +
        ggplot2::geom_text(ggplot2::aes(label = label), hjust = -0.15, size = 3.2) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(limits = c(0, max(1, max(results$mean_hours_below_target) * 1.2)), expand = c(0, 0)) +
        ggplot2::labs(x = NULL, y = "Horas abaixo de M por missão (média)") +
        assessment_theme()
      as_interactive(plot)
    })
    output$recommended_distribution_plot = plotly::renderPlotly({
      simulation = simulation_state()
      shiny::validate(shiny::need(!is.null(simulation), "Sem resultado de simulação: na aba 3, clique em Executar avaliação agregada completa (ou Simular grade sem melhorias)."))
      replications = simulation$recommended_replications
      if (is.null(replications) || nrow(replications) == 0L) {
        best = simulation$summary$configuration_id[[which.max(simulation$summary$ci_lower)]]
        replications = simulation$replications[simulation$replications$configuration_id == best, , drop = FALSE]
      }
      shiny::validate(shiny::need(nrow(replications) > 0L, "Sem replicações para exibir."))
      as_interactive(chart_mission_outcome(replications, simulation$target_ready))
    })
    output$recommended_distribution_note = shiny::renderUI({
      simulation = simulation_state()
      if (is.null(simulation)) return(NULL)
      replications = simulation$recommended_replications
      label = "solução recomendada"
      if (is.null(replications) || nrow(replications) == 0L) {
        best = simulation$summary$configuration_id[[which.max(simulation$summary$ci_lower)]]
        replications = simulation$replications[simulation$replications$configuration_id == best, , drop = FALSE]
        label = paste("melhor configuração da grade,", best)
      }
      success = mean(replications$min_ready >= simulation$target_ready)
      shiny::tags$div(
        class = "how-to-read",
        shiny::tags$strong("Leitura atual: "),
        paste0(
          "na ", label, ", a frota manteve ao menos ", simulation$target_ready, " viaturas prontas sob o critério configurado (incluindo tolerância) em ",
          format_percent_br(success, 0), " das ", nrow(replications), " missões simuladas. ",
          "Barras vermelhas = missões que não atenderam ao critério de missão configurado."
        )
      )
    })
    output$sensitivity_table = DT::renderDT({
      simulation = simulation_state()
      shiny::validate(shiny::need(!is.null(simulation), "Sem resultado de simulação: na aba 3, clique em Executar avaliação agregada completa (ou Simular grade sem melhorias)."))
      sensitivity = spearman_sensitivity(simulation$summary)
      DT::datatable(sensitivity, rownames = FALSE, options = list(pageLength = 12))
    })

    output$audit_table = DT::renderDT({
      simulation = simulation_state()
      snapshot = analysis_snapshot_state()
      dates = parse_datetime_utc(snapshot$work_orders$detected_at)
      period = if (length(dates) > 0L && any(!is.na(dates))) {
        paste(
          format(min(dates, na.rm = TRUE), "%Y-%m-%d"), "a",
          format(max(dates, na.rm = TRUE), "%Y-%m-%d")
        )
      } else {
        "não disponível"
      }
      audit = data.frame(
        item = c("Banco", "Modo", "Período dos dados", "Ativos", "OS", "Falhas", "Execução", "Semente", "Modelo",
                 "Tipo de simulação", "Camada de Monte Carlo", "Sistema de filas", "Motor"),
        value = c(
          normalizePath(database_path, winslash = "/", mustWork = FALSE),
          evidence_mode_state()$mode, period, nrow(snapshot$assets),
          nrow(snapshot$work_orders),
          sum(snapshot$work_orders$is_functional_failure == 1L, na.rm = TRUE),
          if (is.null(simulation)) "não executada" else simulation$run_id,
          if (is.null(simulation)) NA else simulation$seed,
          if (is.null(simulation)) JLTV_MODEL_VERSION else simulation$model_version,
          "SED (eventos discretos): dinâmica, estocástica, alta resolução",
          "Replicações com sementes comuns; IC95% e comparação pareada",
          "G/G/c, população finita (m = N), FIFO",
          "Motor próprio em R (simmer só como referência)"
        ),
        stringsAsFactors = FALSE
      )
      DT::datatable(audit, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })


    # ------------------------------------------------------------------
    # v1.5.0 — Resultado da avaliação, navegação guiada e relatório Excel
    # ------------------------------------------------------------------
    navigate_to = function(value) bslib::nav_select("main_nav", selected = value, session = session)
    shiny::observeEvent(input$home_dados, navigate_to("dados"))
    shiny::observeEvent(input$home_sistemas, navigate_to("catalogo"))
    shiny::observeEvent(input$open_reference_tab, navigate_to("resultados"), ignoreInit=TRUE)
    purrr::walk(c("dados", "decisao", "catalogo", "simulacao", "resultados"), function(value) {
      shiny::observeEvent(input[[paste0("strip_go_", value)]], navigate_to(value), ignoreInit = TRUE)
      shiny::observeEvent(input[[paste0("next_to_", value)]], navigate_to(value), ignoreInit = TRUE)
    })

    session$onFlushed(function() {
      assets = shiny::isolate(analysis_snapshot_state()$assets)
      active = if (is.data.frame(assets) && nrow(assets) > 0L) sum(asset_active_mask(assets), na.rm = TRUE) else 0L
      if (active > 0L) shiny::updateNumericInput(session, "diag_fleet", value = active)
    }, once = TRUE)

    assessment_state = shiny::reactive({
      mc = multicriteria_state()
      contributions = tryCatch(topsis_criterion_contributions(mc), error = function(e) data.frame())
      factors = questionnaire_snapshot_for_mode()$questionnaire_factors
      observed = observed_state()
      ram = if (is.null(observed$error)) observed$ram else NULL
      quality_ready = tryCatch(data_quality_report(analysis_snapshot_state())$analysis_ready, error = function(e) NA)
      build_assessment(
        multicriteria = mc, contributions = contributions,
        p7_mentions = p7_mentions_by_alternative(factors),
        improvements = improvement_state(), ram = ram,
        evidence_mode = evidence_mode_state()$mode,
        questionnaire_overview = questionnaire_summary_state()$overview,
        simulation = simulation_state(), data_quality_ready = quality_ready
      )
    })

    # Resultado da simulação por sistema, compartilhado com os relatórios (pizza do tempo parado por sistema).
    sys_result_state = sys_server(input, output, session,
      questionnaire_provider = function() questionnaire_snapshot_for_mode()$questionnaire_factors,
      multicriteria_provider = function() multicriteria_state(),
      snapshot_provider = function() {
        snap = analysis_snapshot_state()
        pop = active_work_order_population(snap$assets, snap$work_orders, snap$work_order_delays)
        snap$work_orders = pop$work_orders
        snap$work_order_delays = pop$delays
        snap
      },
      references_provider = function() documentation_references,
      storage_dir = getwd(),
      questionnaire_sources_provider = function() questionnaire_snapshot_for_mode(),
      on_completed = function() navigate_to("resultados")
    )
    shiny::observeEvent(input$open_system_simulation, navigate_to("catalogo"))
    shiny::observeEvent(input$open_system_questionnaire, navigate_to("catalogo"))

    output$workflow_strip = shiny::renderUI({
      assessment = assessment_state()
      done = assessment$steps$done
      status = c(
        resultado = isTRUE(done[[6]]),
        dados = isTRUE(done[[1]]) || isTRUE(done[[4]]),
        decisao = isTRUE(done[[3]]),
        simulacao = isTRUE(done[[5]]),
        resultados = isTRUE(done[[5]]),
        relatorio = !is.null(last_excel_report())
      )
      labels = c(
        resultado = "Resultado", dados = "1 · Dados", decisao = "2 · AHP–TOPSIS",
        simulacao = "3 · Simulação", resultados = "4 · Detalhes da simulação", relatorio = "5 · Relatório Excel"
      )
      shiny::tags$div(
        class = "workflow-strip",
        shiny::tags$span(class = "workflow-label", "Andamento:"),
        shiny::tagList(purrr::map(names(labels), function(value) {
          shiny::actionLink(
            paste0("strip_go_", value),
            label = shiny::tagList(if (status[[value]]) "✓ " else "○ ", labels[[value]]),
            class = paste("workflow-step", if (status[[value]]) "done" else "pending")
          )
        }))
      )
    })

    output$assessment_steps = shiny::renderUI({
      steps = assessment_state()$steps
      shiny::tags$div(
        class = "steps-grid",
        shiny::tagList(purrr::map(seq_len(nrow(steps)), function(i) {
          shiny::tags$div(
            class = paste("step-card", if (steps$done[[i]]) "done" else "pending"),
            shiny::tags$div(class = "step-status", if (steps$done[[i]]) "✓ concluído" else "pendente"),
            shiny::tags$div(class = "step-title", steps$step[[i]]),
            shiny::tags$div(class = "step-detail", steps$detail[[i]]),
            if (!steps$done[[i]] && steps$target[[i]] != "resultado") {
              shiny::actionLink(paste0("step_go_", i), "Ir para o passo →")
            }
          )
        }))
      )
    })
    purrr::walk(1:6, function(i) {
      shiny::observeEvent(input[[paste0("step_go_", i)]], {
        navigate_to("resultados")
      }, ignoreInit = TRUE)
    })

    output$assessment_headline = shiny::renderUI({
      assessment = assessment_state()
      has_result = is.finite(assessment$kpis$ao_reference) || nrow(assessment$bottlenecks) > 0L
      guide = shiny::tags$ol(
        class = "small-note",
        shiny::tags$li("Passo 1: importe as respostas do questionário (ou carregue a demonstração) e, se houver, os registros de manutenção."),
        shiny::tags$li("Confira os parâmetros da missão e dos tempos na aba 3 (Cenário e simulação)."),
        shiny::tags$li("Na aba 3, clique em ", shiny::tags$strong("Executar avaliação agregada completa"), " (AHP–TOPSIS → simulação → teste das melhorias)."),
        shiny::tags$li("Nesta aba, em Relatório, auditoria e fontes, clique em ", shiny::tags$strong("Gerar e abrir no Excel"), " (ou em Word) para registrar o resultado.")
      )
      if (!has_result) {
        return(bslib::card(
          class = "findings-card",
          bslib::card_header("Ainda não há resultado — como obter"),
          guide
        ))
      }
      bslib::card(
        class = "findings-card",
        bslib::card_header(shiny::tagList(shiny::icon("lightbulb"), " Principais conclusões")),
        shiny::tags$ul(shiny::tagList(purrr::map(assessment$headline, shiny::tags$li))),
        if (!is.list(improvement_state())) shiny::tags$p(
          class = "small-note",
          "As melhorias ainda não foram simuladas: na aba 3, clique em Executar avaliação agregada completa ou em Testar só as melhorias."
        )
      )
    })

    kpi = function() assessment_state()$kpis
    output$kpi_ao_reference = shiny::renderText(format_percent_br(kpi()$ao_reference))
    output$kpi_ao_reference_note = shiny::renderText({
      k = kpi()
      parts = c(
        if (is.finite(k$availability_target)) paste0("meta de prontidão M/N = ", format_percent_br(k$availability_target, 0)),
        if (is.finite(k$ao_observed)) paste0("Ao observada nos registros = ", format_percent_br(k$ao_observed))
      )
      if (length(parts) == 0L) "execute a avaliação" else paste(parts, collapse = " · ")
    })
    output$kpi_ao_improved = shiny::renderText(format_percent_br(kpi()$ao_improved))
    output$kpi_ao_improved_note = shiny::renderText({
      k = kpi()
      if (!is.finite(k$ao_gain)) return("teste as melhorias")
      paste0(format_pp_br(k$ao_gain), " · ", k$improved_label)
    })
    output$kpi_success = shiny::renderText({
      k = kpi()
      if (!is.finite(k$success_reference)) return("—")
      paste0(format_percent_br(k$success_reference, 0), " → ", format_percent_br(k$success_improved, 0))
    })
    output$kpi_success_note = shiny::renderText({
      improvements = improvement_state()
      if (!is.list(improvements)) return("critério de missão configurado, incluindo tolerância")
      paste0("manter ", improvements$target_ready, " de ", improvements$config$fleet_size, " prontas por ",
             format_number_br(improvements$horizon, 0), " h; tolerância ", improvements$config$mission_tolerance_hours %||% 0, " h (referência → cenário)")
    })
    output$kpi_bottleneck = shiny::renderText({
      k = kpi()
      if (nzchar(k$top_bottleneck)) k$top_bottleneck else "—"
    })
    output$kpi_bottleneck_note = shiny::renderText({
      k = kpi()
      if (nzchar(k$top_bottleneck)) paste("puxado por", k$top_bottleneck_driver) else "calcule o AHP–TOPSIS"
    })

    # ggplot -> plotly com legendas limpas e barra de ferramentas enxuta.
    as_interactive = function(plot) {
      widget = plotly::ggplotly(plot, tooltip = "text")
      widget = plotly::plotly_build(widget)
      widget$x$data = purrr::map(widget$x$data, function(trace) {
        if (!is.null(trace$name)) trace$name = sub("^\\((.*?),[^)]*\\)$", "\\1", trace$name)
        if (!is.null(trace$legendgroup)) trace$legendgroup = sub("^\\((.*?),[^)]*\\)$", "\\1", trace$legendgroup)
        trace
      })
      widget = plotly::layout(
        widget,
        legend = list(orientation = "h", x = 0, y = -0.2, title = list(text = "")),
        margin = list(l = 10, r = 20, t = 10, b = 10),
        font = list(family = "Segoe UI, Arial, sans-serif")
      )
      plotly::config(
        widget, displaylogo = FALSE, locale = "pt-BR",
        modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian")
      )
    }
    how_to_read = function(how, reading = NULL) {
      shiny::tags$div(
        class = "how-to-read",
        shiny::tags$div(shiny::tags$strong("Como ler: "), how),
        if (!is.null(reading)) shiny::tags$div(class = "reading-now", shiny::tags$strong("Leitura atual: "), reading)
      )
    }

    output$assessment_bottleneck_plot = plotly::renderPlotly({
      assessment = assessment_state()
      shiny::validate(shiny::need(
        nrow(assessment$contributions) > 0L,
        "Os gargalos aparecem quando há respostas do questionário e o AHP–TOPSIS é estimável (passo 2). Importe as respostas ou carregue a demonstração no passo 1."
      ))
      as_interactive(chart_bottlenecks(assessment$contributions))
    })
    output$assessment_bottleneck_note = shiny::renderUI({
      assessment = assessment_state()
      b = assessment$bottlenecks
      reading = if (nrow(b) > 0L) {
        paste0(
          b$alternative_name[[1]], " lidera (", format_number_br(b$coefficient[[1]], 3), "), puxado por ",
          b$dominant_criterion_label[[1]], " — sobretudo ", lower_first(b$dominant_subcriterion_label[[1]]), ".",
          if (nrow(b) > 1L) paste0(" Completam o grupo prioritário: ", paste(b$alternative_name[-1], collapse = " e "), ".") else "",
          if (isTRUE(b$p7_mentions[[1]] > 0)) paste0(" A P7 cita ", b$alternative_name[[1]], " ", b$p7_mentions[[1]], " vez(es)",
                                                       if (nzchar(b$p7_examples[[1]])) paste0(": ", b$p7_examples[[1]]) else "", ".") else ""
        )
      } else NULL
      how_to_read(
        "cada barra é um sistema; quanto mais longa, maior a prioridade (coeficiente TOPSIS de 0 a 1). As cores mostram de qual critério vem essa prioridade (peso AHP × posição do sistema no critério).",
        reading
      )
    })

    output$assessment_improvement_plot = plotly::renderPlotly({
      assessment = assessment_state()
      shiny::validate(shiny::need(
        is.data.frame(assessment$improvements) && nrow(assessment$improvements) > 0L,
        "Na aba 3, clique em Executar avaliação agregada completa (ou Testar só as melhorias) para simular as melhorias."
      ))
      as_interactive(chart_improvements(assessment$improvements, assessment$package, assessment$kpis$availability_target))
    })
    output$assessment_improvement_note = shiny::renderUI({
      assessment = assessment_state()
      r = assessment$improvements
      reading = if (is.data.frame(r) && nrow(r) > 0L) {
        demonstrated = sum(r$demonstrated)
        paste0(
          demonstrated, " de ", nrow(r), " melhorias tiveram ganho demonstrado. ",
          if (isTRUE(r$demonstrated[[1]])) paste0(
            "A de maior prioridade é ", lower_first(r$label[[1]]), " (", r$change[[1]], "): ",
            format_percent_br(r$base_availability[[1]]), " → ", format_percent_br(r$new_availability[[1]]),
            " (", format_pp_br(r$delta_availability[[1]]), ")."
          ) else "Nenhuma teve ganho com IC95% acima de zero; revise as hipóteses e a precisão amostral. Não aumente magnitudes apenas para produzir ganho.",
          if (is.data.frame(assessment$package) && nrow(assessment$package) > 0L) paste0(
            " Juntas, as melhores levam a disponibilidade a ", format_percent_br(assessment$package$new_availability),
            " (", format_pp_br(assessment$package$delta_availability), ")."
          ) else ""
        )
      } else NULL
      how_to_read(
        "a linha cinza é a disponibilidade de hoje (situação de referência). Cada ponto mostra a disponibilidade com uma melhoria; a barra é o IC95% do ganho. Verde = ganho demonstrado; cinza = ganho não demonstrado; losango escuro = todas as melhores juntas.",
        reading
      )
    })

    output$assessment_downtime_plot = plotly::renderPlotly({
      assessment = assessment_state()
      shiny::validate(shiny::need(
        nrow(assessment$process) > 0L && sum(assessment$process$hours) > 0,
        "A decomposição do tempo parado aparece após o teste das melhorias (situação de referência simulada)."
      ))
      as_interactive(chart_downtime(assessment$process))
    })
    output$assessment_downtime_note = shiny::renderUI({
      p = assessment_state()$process
      reading = if (nrow(p) > 1L && sum(p$hours) > 0) {
        paste0(
          p$component[[1]], " responde por ", format_percent_br(p$share[[1]], 0), " do tempo parado; com ",
          lower_first(p$component[[2]]), ", somam ", format_percent_br(p$cumulative_share[[2]], 0),
          ". Como reduzir: ", lower_first(p$lever_hint[[1]]), "."
        )
      } else NULL
      how_to_read(
        "horas que, em média, as viaturas ficaram paradas em cada etapa durante uma missão simulada. A barra vermelha é a etapa que mais consome tempo — o gargalo do processo de manutenção.",
        reading
      )
    })

    output$assessment_ahp_plot = plotly::renderPlotly({
      mc = multicriteria_state()
      shiny::validate(shiny::need(
        is.list(mc) && is.data.frame(mc$ahp$subcriteria_weights) && nrow(mc$ahp$subcriteria_weights) > 0L,
        mc$ahp$error %||% "Os pesos aparecem depois do cálculo AHP (passo 2)."
      ))
      as_interactive(chart_ahp_weights(mc))
    })
    output$assessment_ahp_note = shiny::renderUI({
      mc = multicriteria_state()
      w = if (is.list(mc)) mc$ahp$subcriteria_weights else NULL
      reading = if (is.data.frame(w) && nrow(w) > 0L) {
        criteria = assessment_criterion_labels()
        totals = tapply(w$global_weight, w$parent_criterion, sum)
        top_c = names(totals)[which.max(totals)]
        top_s = w[which.max(w$global_weight), , drop = FALSE]
        paste0(
          "o critério mais importante é ", unname(criteria[[top_c]]), " (", format_percent_br(max(totals), 0),
          "); o subcritério de maior peso é ", unname(subcriterion_label_map()[[top_s$subcriterion_id]]),
          " (", format_percent_br(top_s$global_weight, 1), ").",
          if (!isTRUE(mc$ahp$estimable)) paste0(" Atenção: ", mc$ahp$error) else ""
        )
      } else NULL
      how_to_read(
        "peso que os avaliadores deram a cada subcritério nas comparações P1–P5 (soma = 100%). Os pesos definem quanto cada nota P6 conta no ranking TOPSIS.",
        reading
      )
    })

    output$assessment_plan_table = DT::renderDT({
      r = assessment_state()$improvements
      shiny::validate(shiny::need(is.data.frame(r) && nrow(r) > 0L, "Execute a avaliação completa para gerar o plano de melhorias."))
      package = assessment_state()$package
      display = data.frame(
        Prioridade = r$priority_rank,
        Melhoria = r$label,
        `O que muda no cenário` = r$change,
        `Critério AHP` = paste0(r$criterion_label, ifelse(is.finite(r$ahp_weight), paste0(" (", vapply(r$ahp_weight, format_percent_br, character(1), digits = 0), ")"), "")),
        Disponibilidade = paste0(vapply(r$base_availability, format_percent_br, character(1)), " → ", vapply(r$new_availability, format_percent_br, character(1))),
        `Ganho (IC95%)` = paste0(vapply(r$delta_availability, format_pp_br, character(1)), " (", vapply(r$delta_ci_lower, format_pp_br, character(1)), " a ", vapply(r$delta_ci_upper, format_pp_br, character(1)), ")"),
        `Chance de cumprir a missão` = paste0(vapply(r$base_success, format_percent_br, character(1), digits = 0), " → ", vapply(r$new_success, format_percent_br, character(1), digits = 0)),
        Resultado = r$conclusion,
        `Associação ao questionário (heurística)` = r$targets_bottleneck,
        `Exemplos de ação (a validar)` = r$example_actions,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      if (is.data.frame(package) && nrow(package) > 0L) {
        display = rbind(display, data.frame(
          Prioridade = NA_integer_, Melhoria = package$label, `O que muda no cenário` = package$change,
          `Critério AHP` = "Vários",
          Disponibilidade = paste0(format_percent_br(package$base_availability), " → ", format_percent_br(package$new_availability)),
          `Ganho (IC95%)` = paste0(format_pp_br(package$delta_availability), " (", format_pp_br(package$delta_ci_lower), " a ", format_pp_br(package$delta_ci_upper), ")"),
          `Chance de cumprir a missão` = paste0(format_percent_br(package$base_success, 0), " → ", format_percent_br(package$new_success, 0)),
          Resultado = package$conclusion, `Associação ao questionário (heurística)` = "", `Exemplos de ação (a validar)` = "Combinação das melhorias acima com ganho demonstrado.",
          check.names = FALSE, stringsAsFactors = FALSE
        ))
      }
      DT::formatStyle(
        DT::datatable(display, rownames = FALSE, options = list(dom = "t", pageLength = 20, scrollX = TRUE, ordering = FALSE)),
        "Resultado",
        backgroundColor = DT::styleEqual(
          c("Ganho demonstrado", "Ganho não demonstrado", "Piora demonstrada", "IC não estimável"),
          c("#D1E7DD", "#F1F3F5", "#F8D7DA", "#FFF3CD")
        ),
        fontWeight = "bold"
      )
    })

    output$assessment_bottleneck_table = DT::renderDT({
      assessment = assessment_state()
      contributions = assessment$contributions
      shiny::validate(shiny::need(nrow(contributions) > 0L, "Calcule o AHP–TOPSIS (passo 2) para ver o ranking dos sistemas."))
      mentions = p7_mentions_by_alternative(questionnaire_snapshot_for_mode()$questionnaire_factors)
      index = match(contributions$alternative_id, mentions$alternative_id)
      display = data.frame(
        `Posição` = contributions$rank,
        Sistema = paste(contributions$alternative_id, contributions$alternative_name),
        `Prioridade TOPSIS` = round(contributions$coefficient, 3),
        `Critério que mais pesa` = contributions$dominant_criterion_label,
        `Subcritério dominante` = contributions$dominant_subcriterion_label,
        `Citações P7` = mentions$p7_mentions[index],
        `Exemplos P7` = mentions$p7_examples[index],
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::formatStyle(
        DT::datatable(display, rownames = FALSE, options = list(dom = "t", pageLength = 10, scrollX = TRUE)),
        "Prioridade TOPSIS",
        background = DT::styleColorBar(c(0, 1), "#F0B7BE"), backgroundSize = "98% 70%",
        backgroundRepeat = "no-repeat", backgroundPosition = "center"
      )
    })

    output$assessment_confidence_table = DT::renderDT({
      confidence = assessment_state()$confidence
      names(confidence) = c("Item", "Valor", "Situação", "Observação")
      DT::formatStyle(
        DT::datatable(confidence, rownames = FALSE, options = list(dom = "t", ordering = FALSE)),
        "Situação",
        backgroundColor = DT::styleEqual(c("OK", "Atenção"), c("#D1E7DD", "#FFF3CD")), fontWeight = "bold"
      )
    })

    # ------------------------------------------------------------------
    # Teste das melhorias e avaliação completa
    # ------------------------------------------------------------------
    selected_levers = function() {
      catalog = improvement_lever_catalog()
      use = purrr::map_lgl(catalog$lever_id, function(id) !identical(input[[paste0("lever_use_", id)]], FALSE))
      magnitude = purrr::map_dbl(catalog$lever_id, function(id) {
        value = input[[paste0("lever_mag_", id)]]
        if (is.null(value)) return(catalog$default_magnitude[catalog$lever_id == id])
        if (is.numeric(value) && length(value) == 1L && is.finite(value)) value else NA_real_
      })
      levers = data.frame(lever_id = catalog$lever_id[use], magnitude = magnitude[use], stringsAsFactors = FALSE)
      bad = !is.finite(levers$magnitude) | levers$magnitude <= 0
      if (any(bad)) {
        stop(paste0(
          "Informe uma magnitude positiva para: ",
          paste(catalog$label[match(levers$lever_id[bad], catalog$lever_id)], collapse = "; "), "."
        ), call. = FALSE)
      }
      if (nrow(levers) == 0L) stop("Marque ao menos uma melhoria para testar.", call. = FALSE)
      levers
    }

    improvement_reference = function() {
      choice = input$diag_reference %||% "current"
      if (identical(choice, "recommended")) {
        simulation = simulation_state()
        if (is.null(simulation)) {
          stop("Execute a simulação (passo 3) antes de usar a configuração recomendada como referência.", call. = FALSE)
        }
        reference = simulation$recommended
        if (is.null(reference) || nrow(reference) == 0L) {
          reference = simulation$summary[which.max(simulation$summary$ci_lower), , drop = FALSE]
        }
        config = build_base_config_from_inputs(reference$fleet_size[[1]])
        config$diagnostic_teams = as.integer(reference$diagnostic_teams[[1]])
        config$repair_teams = as.integer(reference$repair_teams[[1]])
        config$bays = as.integer(reference$bays[[1]])
        config$initial_stock = as.integer(reference$initial_stock[[1]])
      } else {
        fleet = input$diag_fleet
        if (!is.numeric(fleet) || length(fleet) != 1L || !is.finite(fleet) || fleet < 1) {
          stop("Informe o tamanho da frota da situação atual (painel Ajustar as melhorias).", call. = FALSE)
        }
        config = build_base_config_from_inputs(as.integer(round(fleet)))
      }
      if (config$fleet_size < input$target_ready) {
        stop(paste0(
          "A frota de referência (", config$fleet_size, ") é menor que a meta M de viaturas prontas (",
          input$target_ready, "). Ajuste a frota ou a meta no passo 3."
        ), call. = FALSE)
      }
      if (as.integer(input$initial_unavailable %||% 0) > config$fleet_size) {
        stop("Viaturas indisponíveis no início excedem a frota de referência.", call. = FALSE)
      }
      config
    }

    execute_improvements = function() {
      require_finite_inputs(simulation_numeric_labels)
      levers = selected_levers()
      config = improvement_reference()
      params = build_params_from_inputs()
      validation = validate_simulation_inputs(config, params, input$simulation_horizon, as.integer(input$target_ready))
      if (!validation$valid) stop(paste(validation$errors, collapse = "; "), call. = FALSE)
      result = shiny::withProgress(message = "Testando melhorias na simulação...", value = 0, {
        evaluate_improvement_levers(
          config = config, params = params,
          horizon = input$simulation_horizon,
          target_ready = as.integer(input$target_ready),
          replications = as.integer(input$simulation_replications),
          seed = as.integer(input$simulation_seed),
          levers = levers,
          progress = function(index, total, detail) {
            shiny::setProgress(value = (index - 0.5) / total, detail = detail)
          }
        )
      })
      result$evidence_mode = evidence_mode_state()$mode
      result$input_signature = scenario_signature()
      result$input_settings = scenario_settings()
      improvement_state(result)
      invisible(result)
    }

    shiny::observeEvent(input$run_improvements, {
      tryCatch({
        execute_improvements()
        shiny::showNotification("Melhorias testadas. Consulte os resultados na última aba.", type = "message", duration = 8); navigate_to("resultados")
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 12))
    })

    output$results_stale_banner = shiny::renderUI({
      if (!is.null(improvement_state())) return(NULL)
      shiny::tags$div(
        class = "alert alert-warning",
        shiny::tags$strong("Sem resultado para os parâmetros atuais. "),
        "Ainda não houve execução, ou algum campo foi alterado e o resultado anterior foi retirado para não mostrar números desatualizados. ",
        shiny::actionButton("run_full_assessment_p4", "Executar a avaliação agora", icon = shiny::icon("play"), class = "btn btn-primary btn-sm")
      )
    })

    run_full_assessment_now = function() {
      problems = character()
      note = function(stage, e) problems <<- c(problems, paste0(stage, ": ", conditionMessage(e)))
      shiny::withProgress(message = "Executando a avaliação completa", value = 0, {
        shiny::setProgress(0.05, detail = "1/3 — AHP e TOPSIS")
        tryCatch(recompute_multicriteria(TRUE), error = function(e) note("AHP–TOPSIS", e))
        shiny::setProgress(0.25, detail = "2/3 — simulação da frota")
        tryCatch(execute_simulation(), error = function(e) note("Simulação da frota", e))
        shiny::setProgress(0.6, detail = "3/3 — teste das melhorias")
        tryCatch(execute_improvements(), error = function(e) note("Teste das melhorias", e))
      })
      if (length(problems) > 0L) {
        shiny::showNotification(
          shiny::HTML(paste0("Avaliação concluída com pendências:<br>", paste(problems, collapse = "<br>"))),
          type = "warning", duration = 20
        )
      } else {
        shiny::showNotification(
          "Avaliação concluída. Consulte os resultados na última aba.",
          type = "message", duration = 10
        )
        navigate_to("resultados")
      }
    }
    shiny::observeEvent(input$run_full_assessment, run_full_assessment_now())
    shiny::observeEvent(input$run_full_assessment_p4, run_full_assessment_now())

    # ------------------------------------------------------------------
    # Questionário de demonstração
    # ------------------------------------------------------------------
    shiny::observeEvent(input$load_demo_questionnaire, {
      tryCatch({
        n = load_synthetic_questionnaire_demo(database_path)
        db_version(db_version() + 1L)
        multicriteria_state(NULL)
        mode = evidence_mode_state()$mode
        shiny::showNotification(
          paste0(
            n, " respostas de demonstração (SYNTHETIC) carregadas. Elas entram nos modos SYNTHETIC e HYBRID",
            if (identical(mode, "EMPIRICAL")) " — o modo atual é EMPIRICAL, por isso não aparecem nos resultados." else "."
          ),
          type = "message", duration = 12
        )
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 12))
    })
    shiny::observeEvent(input$remove_demo_questionnaire, {
      tryCatch({
        remove_synthetic_questionnaire_demo(database_path)
        db_version(db_version() + 1L)
        multicriteria_state(NULL)
        shiny::showNotification("Respostas de demonstração removidas.", type = "message")
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 12))
    })

    # ------------------------------------------------------------------
    # Relatório Excel
    # ------------------------------------------------------------------
    report_directory = function() {
      directory = file.path(getwd(), "Relatorios")
      dir.create(directory, showWarnings = FALSE, recursive = TRUE)
      directory
    }

    assemble_report = function() {
      ensure_current_results()
      simulation = simulation_state()
      calibrated = calibrated_state()
      mc = multicriteria_state()
      ram_metrics = if (is.null(calibrated$error)) {
        tryCatch(build_ram_metrics_frame(calibrated), error = function(e) data.frame())
      } else data.frame(status = calibrated$error)
      if (is.data.frame(ram_metrics) && "metric" %in% names(ram_metrics)) {
        names(ram_metrics) = c("Indicador", "Valor", "Unidade", "Definição", "Estimável", "Motivo")[seq_along(names(ram_metrics))]
      }
      observed_downtime = tryCatch({
        d = downtime_observed()
        names(d) = c("Componente", "Horas registradas")
        d
      }, error = function(e) data.frame())
      quality = tryCatch(data_quality_report(analysis_snapshot_state())$summary, error = function(e) data.frame())
      improvements = improvement_state()
      value_or = function(x, default = "—") if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) default else paste(x, collapse = ", ")
      assumptions = data.frame(
        Item = c(
          "Versão do aplicativo", "Modo de evidência", "Banco de dados",
          "Processamento AHP–TOPSIS (auditoria)", "Agregação das notas P6", "AHP: só matrizes com RC < 0,10",
          "Cobertura mínima P6 por célula", "Execução da simulação (auditoria)",
          "Meta M (viaturas prontas)", "Horizonte da missão (h)", "Probabilidade-alvo", "Replicações por configuração",
          "Semente", "Utilização na missão (h uso / h calendário)", "Modelo de falhas",
          "Diagnóstico médio (h)", "Reparo corretivo médio (h)", "Preventiva média (h)", "Teste/liberação médio (h)",
          "CV dos tempos", "Prazo médio de reposição (h)", "Probabilidade de exigir peça (corretiva)",
          "Apoio-base (diagnóstico / reparo / boxes / estoque)", "Grade simulada (frota; D; R; B; S)",
          "Situação de referência das intervenções", "Intervenções testadas (magnitudes)"
        ),
        Valor = c(
          JLTV_MODEL_VERSION, evidence_mode_state()$mode, normalizePath(database_path, winslash = "/", mustWork = FALSE),
          value_or(mc$mc_run_id, "não gravado"), value_or(input$score_aggregation), value_or(isTRUE(input$only_consistent_ahp)),
          paste0(value_or(input$min_cell_coverage), "%"), if (is.null(simulation)) "não executada" else value_or(simulation$run_id),
          value_or(input$target_ready), value_or(input$simulation_horizon), value_or(input$target_probability),
          value_or(input$simulation_replications), value_or(input$simulation_seed), value_or(input$mission_utilization),
          paste0(value_or(input$failure_distribution), " (", value_or(input$failure_parameter_1), "; ", value_or(input$failure_parameter_2), ")"),
          value_or(input$mean_diagnosis), value_or(input$mean_corrective), value_or(input$mean_preventive), value_or(input$mean_test),
          value_or(input$duration_cv), value_or(input$support_lead), value_or(input$support_part_probability),
          paste(value_or(input$support_diag), value_or(input$support_repair), value_or(input$support_bays), value_or(input$support_stock), sep = " / "),
          paste(value_or(input$grid_fleet), value_or(input$grid_diag), value_or(input$grid_repair), value_or(input$grid_bays), value_or(input$grid_stock), sep = "; "),
          if (is.list(improvements)) configuration_id(improvements$config) else "não executado",
          if (is.list(improvements)) paste0(improvements$levers$label, " (", improvements$levers$magnitude, ")", collapse = "; ") else "não executado"
        ),
        stringsAsFactors = FALSE
      )
      assumptions = rbind(assumptions, data.frame(
        Item = c("Tipo de simulação", "Camada de Monte Carlo", "Sistema de filas", "Motor de simulação"),
        Valor = c(
          "Simulação de eventos discretos (SED): dinâmica, discreta por eventos, estocástica e de alta resolução (cada viatura é simulada).",
          "Replicações independentes com sementes comuns entre cenários (comparação pareada) e IC95% das médias.",
          "G/G/c com população finita (m = N viaturas), disciplina FIFO; diagnóstico, reparo (capacidade = menor valor entre equipes e boxes) e peça.",
          "Motor próprio em R (lista de eventos, filas e estoque); o pacote simmer é apenas referência da literatura."
        ), stringsAsFactors = FALSE))
      frozen_settings = if (!is.null(simulation)) simulation$input_settings else if (!is.null(improvements)) improvements$input_settings else scenario_settings()
      settings_rows = data.frame(Item = paste0("Parâmetro técnico: ", names(frozen_settings)),
        Valor = vapply(frozen_settings, value_or, character(1)), stringsAsFactors = FALSE)
      assumptions = rbind(assumptions, settings_rows,
        data.frame(Item = "Assinatura SHA256 das entradas", Valor = scenario_signature(), stringsAsFactors = FALSE))
      p7_summary = questionnaire_summary_state()$p7
      list(
        meta = list(
          version = JLTV_MODEL_VERSION,
          generated_at = format(Sys.time(), "%d/%m/%Y %H:%M"),
          evidence_mode = evidence_mode_state()$mode
        ),
        assessment = assessment_state(),
        multicriteria = mc,
        p7_mentions = p7_mentions_by_alternative(questionnaire_snapshot_for_mode()$questionnaire_factors),
        p7_summary = p7_summary,
        factors = questionnaire_snapshot_for_mode()$questionnaire_factors,
        improvements = improvements,
        simulation = simulation,
        systems_simulation = sys_result_state(),
        ram_metrics = ram_metrics,
        observed_downtime = observed_downtime,
        quality_summary = quality,
        assumptions = assumptions
      )
    }

    generate_and_open_report = function() {
      tryCatch({
        path = file.path(report_directory(), paste0("Relatorio_JLTV_Sustainment_Decision_Lab_", format(Sys.time(), "%Y-%m-%d_%Hh%Mmin%S"), ".xlsx"))
        shiny::withProgress(message = "Gerando relatório Excel...", value = 0.4, {
          write_assessment_workbook(path, assemble_report())
        })
        last_excel_report(path)
        opened = open_file_with_system(path)
        shiny::showNotification(
          paste0(
            "Relatório salvo em ", normalizePath(path, winslash = "\\", mustWork = FALSE),
            if (opened) ". Abrindo no Excel..." else "."
          ),
          type = "message", duration = 15
        )
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 12))
    }
    shiny::observeEvent(input$open_excel_report, generate_and_open_report())
    shiny::observeEvent(input$open_excel_report_step5, generate_and_open_report())

    excel_download = function(prefix) {
      shiny::downloadHandler(
        filename = function() paste0(prefix, format(Sys.time(), "%Y-%m-%d_%Hh%M"), ".xlsx"),
        content = function(file) {
          write_assessment_workbook(file, assemble_report())
          last_excel_report(file)
        },
        contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
      )
    }
    last_word_report = shiny::reactiveVal(NULL)
    write_word_from_inputs = function(path) {
      res = write_assessment_word(
        path, assemble_report(),
        provider = input$word_ai_provider %||% "local",
        api_key = input$word_ai_key %||% "",
        model = input$word_ai_model %||% "",
        meta = list(
          author = input$word_meta_author, title = input$word_meta_title, subtitle = input$word_meta_subtitle,
          nature = input$word_meta_nature, sector = input$word_meta_sector, course = input$word_meta_course,
          advisor = input$word_meta_advisor, city = input$word_meta_city, year = input$word_meta_year,
          max_pages = input$word_max_pages
        )
      )
      last_word_report(res)
      res
    }
    shiny::observeEvent(input$open_word_report, {
      tryCatch({
        path = file.path(report_directory(), paste0("Analise_JLTV_Sustainment_Decision_Lab_", format(Sys.time(), "%Y-%m-%d_%Hh%Mmin%S"), ".docx"))
        res = shiny::withProgress(
          message = if (identical(input$word_ai_provider %||% "local", "local")) "Gerando relatório Word..." else "Gerando relatório Word: a IA está analisando os resultados e redigindo o texto (pode levar de 3 a 8 minutos)...",
          value = 0.4, write_word_from_inputs(path))
        opened = open_file_with_system(path)
        shiny::showNotification(
          paste0("Relatório salvo em ", normalizePath(path, winslash = "\\", mustWork = FALSE),
                 if (opened) ". Abrindo no Word..." else ".",
                 " Corpo estimado em ", format(ceiling(res$body_pages_estimate %||% NA_real_)), " de ", res$max_pages %||% 20, " páginas",
                 if ((res$deferred_objects %||% 0L) > 0L) paste0("; ", res$deferred_objects, " figura(s)/tabela(s) foram para o apêndice.") else ".",
                 if (!isTRUE(res$systems_pie)) " A pizza do tempo parado por sistema não entrou: execute antes a simulação por sistema." else "",
                 if (!is.null(res$note)) paste0(" ", res$note) else ""),
          type = if (is.null(res$note)) "message" else "warning", duration = 15)
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 12))
    })
    output$download_word_report = shiny::downloadHandler(
      filename = function() paste0("Analise_JLTV_Sustainment_Decision_Lab_", format(Sys.time(), "%Y-%m-%d_%Hh%M"), ".docx"),
      content = function(file) write_word_from_inputs(file),
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    )
    output$word_report_status = shiny::renderUI({
      res = last_word_report()
      if (is.null(res)) return(NULL)
      shiny::tags$p(class = "small-note", paste0(
        "Último relatório Word: ", if (res$source == "local") "texto padrão do aplicativo" else paste0("texto redigido por ", res$source),
        if (!is.null(res$body_pages_estimate)) paste0(" — corpo estimado em ", ceiling(res$body_pages_estimate), " de ", res$max_pages, " páginas; ", res$references, " fontes citadas") else "",
        if (!isTRUE(res$systems_pie)) " — sem a pizza do tempo parado por sistema (execute antes a simulação por sistema)" else "",
        if (!is.null(res$note)) paste0(" — ", res$note) else ""))
    })
    output$download_excel_report = excel_download("Relatorio_JLTV_Sustainment_Decision_Lab_")
    output$download_excel_report_step5 = excel_download("Relatorio_JLTV_Sustainment_Decision_Lab_")
    output$excel_report_status = shiny::renderUI({
      path = last_excel_report()
      if (is.null(path)) {
        return(shiny::tags$p(class = "small-note", "Nenhum relatório gerado nesta sessão. Os relatórios abertos pelo botão verde ficam na pasta Relatorios, ao lado do aplicativo."))
      }
      shiny::tags$p(class = "small-note", paste("Último relatório:", normalizePath(path, winslash = "\\", mustWork = FALSE)))
    })
    shiny::observeEvent(input$open_report_folder, {
      opened = open_file_with_system(report_directory())
      if (!opened) shiny::showNotification(paste("Pasta dos relatórios:", report_directory()), type = "message", duration = 12)
    })

    output$download_multicriteria_xlsx = shiny::downloadHandler(
      filename = function() paste0("AHP_TOPSIS_JLTV_", format(Sys.time(), "%Y-%m-%d_%Hh%M"), ".xlsx"),
      content = function(file) {
        result = multicriteria_state()
        if (is.null(result)) stop("Calcule o AHP–TOPSIS antes de exportar.", call. = FALSE)
        sheets = list(
          `Pesos AHP` = result$ahp$subcriteria_weights %||% data.frame(status = result$ahp$error %||% "AHP não estimável"),
          `Consistência` = result$ahp$consistency %||% data.frame(status = "não disponível"),
          `Diagnóstico por avaliador` = result$ahp$individual_diagnostics %||% data.frame(status = "não disponível"),
          `Ranking TOPSIS` = if (isTRUE(result$topsis$estimable)) result$topsis$ranking else data.frame(status = result$topsis$reason %||% "TOPSIS não estimável"),
          `Contribuição por critério` = topsis_criterion_contributions(result),
          `Notas P6 agregadas` = if (is.data.frame(result$aggregated_scores) && nrow(result$aggregated_scores) > 0L) result$aggregated_scores else data.frame(status = "sem notas")
        )
        openxlsx::write.xlsx(sheets, file, headerStyle = openxlsx::createStyle(textDecoration = "bold", fgFill = "#0B1F33", fontColour = "#FFFFFF"), colWidths = "auto", firstRow = TRUE)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

  }

  shiny::shinyApp(ui = ui, server = server)
}


# Abertura direta ao executar este arquivo com Rscript.
if (!identical(Sys.getenv("JLTV_CORE_ONLY"), "1")) {
  .jltv_direct_arg = grep("^--file=", commandArgs(FALSE), value = TRUE)
  .jltv_direct = length(.jltv_direct_arg) > 0L &&
    identical(basename(sub("^--file=", "", .jltv_direct_arg[[1]])),
      "JLTV_Sustainment_Decision_Lab_CODIGO_COMPLETO.R")
  if (.jltv_direct) shiny::runApp(jltv_aplicativo_completo,
    host = "127.0.0.1", launch.browser = TRUE)
  invisible(jltv_aplicativo_completo)
}
