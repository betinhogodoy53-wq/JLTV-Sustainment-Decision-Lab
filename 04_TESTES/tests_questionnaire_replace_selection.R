Sys.setenv(JLTV_CORE_ONLY = "1")
app_dir = normalizePath(getwd(), winslash = "/", mustWork = TRUE)
env = new.env(parent = globalenv())
source("app.R", local = env, chdir = TRUE, encoding = "UTF-8")

source_db = file.path(app_dir, "jltv_cmms_ram_empirical.sqlite")
file_500 = "C:/Users/betin/Desktop/SIMULADOR JLTV AQUI/JLTV_Questionario_500_Respostas_500_Especialistas.xlsx"
stopifnot(file.exists(source_db), file.exists(file_500))

# Captura o conjunto atual de 7 respostas do Forms/Google Sheets.
con = DBI::dbConnect(RSQLite::SQLite(), source_db)
seven = list(
  questionnaire_respondents = DBI::dbGetQuery(con, "SELECT * FROM questionnaire_respondents WHERE data_source = 'EMPIRICAL_GOOGLE_SHEETS'"),
  ahp_pairwise = DBI::dbGetQuery(con, "SELECT * FROM ahp_pairwise WHERE data_source = 'EMPIRICAL_GOOGLE_SHEETS'"),
  topsis_scores = DBI::dbGetQuery(con, "SELECT * FROM topsis_scores WHERE data_source = 'EMPIRICAL_GOOGLE_SHEETS'"),
  questionnaire_factors = DBI::dbGetQuery(con, "SELECT * FROM questionnaire_factors WHERE data_source = 'EMPIRICAL_GOOGLE_SHEETS'")
)
DBI::dbDisconnect(con)
stopifnot(nrow(seven$questionnaire_respondents) == 7L)

# Trabalha apenas em cópia temporária do banco.
test_db = tempfile(fileext = ".sqlite")
file.copy(source_db, test_db, overwrite = TRUE)

# 1) Selecionar arquivo com 500 respostas deve produzir exatamente 500.
r500 = env$sync_questionnaire_file(test_db, file_500, "EMPIRICAL")
stopifnot(r500$imported == 500L, r500$ahp == 7000L, r500$p6 == 50000L)
con = DBI::dbConnect(RSQLite::SQLite(), test_db)
stopifnot(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM questionnaire_respondents")$n[[1]] == 500L)
stopifnot(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM ahp_pairwise")$n[[1]] == 7000L)
stopifnot(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM topsis_scores")$n[[1]] == 50000L)
DBI::dbDisconnect(con)

# 2) Trocar para a planilha do Forms com 7 deve SUBSTITUIR os 500, não somar 507.
env$replace_questionnaire_source_tables(test_db, seven, "_GOOGLE_SHEETS")
con = DBI::dbConnect(RSQLite::SQLite(), test_db)
stopifnot(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM questionnaire_respondents")$n[[1]] == 7L)
stopifnot(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM ahp_pairwise")$n[[1]] == 98L)
stopifnot(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM topsis_scores")$n[[1]] == 700L)
DBI::dbDisconnect(con)

# 3) Voltar ao arquivo de 500 deve novamente substituir tudo e retornar a 500.
r500b = env$sync_questionnaire_file(test_db, file_500, "EMPIRICAL")
stopifnot(r500b$imported == 500L)
con = DBI::dbConnect(RSQLite::SQLite(), test_db)
stopifnot(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM questionnaire_respondents")$n[[1]] == 500L)
stopifnot(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM ahp_pairwise")$n[[1]] == 7000L)
stopifnot(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM topsis_scores")$n[[1]] == 50000L)
DBI::dbDisconnect(con)

cat("PASS: 500 -> 7 -> 500 substitui integralmente o questionario ativo, sem acumulacao.\n")
