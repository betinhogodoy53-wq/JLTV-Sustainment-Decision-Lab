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
