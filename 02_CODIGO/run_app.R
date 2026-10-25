required_app_packages = function() {
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

launcher_directory = function(arguments = commandArgs(trailingOnly = FALSE)) {
  file_argument = grep("^--file=", arguments, value = TRUE)
  if (length(file_argument) > 0L) {
    script_path = sub("^--file=", "", file_argument[[1]])
    return(normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

missing_app_packages = function(
  checker = function(package) requireNamespace(package, quietly = TRUE)
) {
  packages = required_app_packages()
  packages[!vapply(packages, checker, logical(1))]
}

# v1.5.0 — o aplicativo abre no Google Chrome (janela de aplicativo); se o
# Chrome não estiver instalado, tenta o Edge e, por fim, o navegador padrão.
browser_candidates = function(browser = c("chrome", "edge")) {
  browser = match.arg(browser)
  environment_paths = unique(c(
    Sys.getenv("ProgramFiles", unset = ""),
    Sys.getenv("ProgramFiles(x86)", unset = ""),
    Sys.getenv("ProgramW6432", unset = ""),
    Sys.getenv("LOCALAPPDATA", unset = "")
  ))
  environment_paths = environment_paths[nzchar(environment_paths)]
  configured_candidates = if (browser == "chrome") {
    c(
      file.path(environment_paths, "Google", "Chrome", "Application", "chrome.exe"),
      unname(Sys.which("chrome.exe")), unname(Sys.which("chrome"))
    )
  } else {
    c(
      file.path(environment_paths, "Microsoft", "Edge", "Application", "msedge.exe"),
      unname(Sys.which("msedge.exe")), unname(Sys.which("msedge"))
    )
  }
  unique(configured_candidates[nzchar(configured_candidates)])
}

locate_browser = function(candidates) {
  existing = candidates[file.exists(candidates)]
  if (length(existing) == 0L) "" else normalizePath(
    existing[[1]], winslash = "/", mustWork = TRUE
  )
}

launch_app_window = function(url) {
  for (browser in c("chrome", "edge")) {
    executable = locate_browser(browser_candidates(browser))
    if (!nzchar(executable)) next
    launched = try(
      system2(
        executable,
        args = c(paste0("--app=", url), "--new-window", "--start-maximized"),
        wait = FALSE,
        stdout = FALSE,
        stderr = FALSE
      ),
      silent = TRUE
    )
    if (!inherits(launched, "try-error")) return(invisible(TRUE))
  }
  utils::browseURL(url)
  invisible(FALSE)
}

write_launcher_log = function(log_path, text, append = TRUE) {
  timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(paste0("[", timestamp, "] ", text, "\n"), file = log_path, append = append)
}

load_application = function(app_directory) {
  app_file = file.path(app_directory, "app.R")
  if (!file.exists(app_file)) {
    stop("app.R não foi localizado na mesma pasta do iniciador.", call. = FALSE)
  }
  application_environment = new.env(parent = globalenv())
  source_result = source(
    app_file,
    local = application_environment,
    chdir = TRUE,
    encoding = "UTF-8"
  )
  application = source_result$value
  if (!inherits(application, "shiny.appobj")) {
    stop(
      paste(
        "app.R foi carregado, mas não devolveu um aplicativo Shiny.",
        "Feche o R/RStudio e use este iniciador."
      ),
      call. = FALSE
    )
  }
  application
}

run_launcher = function() {
  app_directory = launcher_directory()
  setwd(app_directory)
  log_path = file.path(app_directory, "JLTV_SUSTAINMENT_DECISION_LAB_STARTUP.log")
  write_launcher_log(log_path, "Início da abertura do JLTV Sustainment Decision Lab.", append = FALSE)

  tryCatch(
    {
      missing = missing_app_packages()
      if (length(missing) > 0L) {
        stop(
          paste0(
            "Pacotes ausentes: ", paste(missing, collapse = ", "),
            ". Execute primeiro 01_INSTALAR_DEPENDENCIAS.cmd."
          ),
          call. = FALSE
        )
      }

      Sys.unsetenv("JLTV_CORE_ONLY")
      Sys.setenv(JLTV_DESKTOP_MODE = "1")
      database_env = Sys.getenv("JLTV_DB_PATH", unset = "")
      seed_env = Sys.getenv("JLTV_SEED_SYNTHETIC", unset = "1")
      write_launcher_log(
        log_path,
        paste0(
          "Dependências verificadas; banco=",
          if (nzchar(database_env)) database_env else "jltv_cmms_ram.sqlite (padrão)",
          "; seed_sintetico=", seed_env, "; carregando o aplicativo."
        )
      )

      application = load_application(app_directory)
      write_launcher_log(log_path, "Aplicativo carregado; iniciando o servidor local.")

      shiny::runApp(
        application,
        host = "127.0.0.1",
        launch.browser = launch_app_window,
        quiet = TRUE
      )
      write_launcher_log(log_path, "Aplicativo encerrado normalmente.")
      invisible(TRUE)
    },
    error = function(error_condition) {
      error_text = conditionMessage(error_condition)
      write_launcher_log(log_path, paste0("ERRO: ", error_text))
      message("Falha ao abrir o JLTV Sustainment Decision Lab: ", error_text)
      stop(error_text, call. = FALSE)
    }
  )
}

if (!identical(Sys.getenv("JLTV_LAUNCHER_ONLY"), "1")) {
  run_launcher()
}
