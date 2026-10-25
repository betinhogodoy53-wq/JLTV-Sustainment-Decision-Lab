packages = c(
  "shiny", "bslib", "DBI", "RSQLite", "DT", "ggplot2", "plotly",
  "jsonlite", "scales", "purrr", "googlesheets4", "readxl", "digest",
  "openxlsx", "shinyWidgets", "reactable", "data.table", "dplyr",
  "tidyr", "viridisLite", "patchwork", "svglite", "gganimate",
  "gifski", "survival", "readr", "shinycssloaders", "shinyvalidate",
  "pointblank", "ggdist", "ggrepel", "rsvg", "rsample", "binom",
  "thematic", "ggtext", "officer", "flextable"
)

user_library = Sys.getenv("R_LIBS_USER", unset = "")
if (nzchar(user_library)) {
  dir.create(user_library, recursive = TRUE, showWarnings = FALSE)
  .libPaths(unique(c(user_library, .libPaths())))
}

# Modo de verificacao rapida (usado pelo atalho da area de trabalho): 0 = tudo instalado; 10 = faltam pacotes.
if ("--check" %in% commandArgs(trailingOnly = TRUE)) {
  absent = packages[!nzchar(vapply(packages, function(p) system.file(package = p), character(1)))]
  quit(save = "no", status = if (length(absent) == 0L) 0L else 10L)
}

missing = packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) == 0L) {
  cat("Todos os pacotes necessários já estão instalados.\n")
  quit(save = "no", status = 0L)
}
options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages(missing, dependencies = NA)

still_missing = packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing) > 0L) {
  stop(paste0("A instalação não foi concluída para: ", paste(still_missing, collapse = ", ")),
       call. = FALSE)
}
cat("DEPENDÊNCIAS INSTALADAS COM SUCESSO.\n")
