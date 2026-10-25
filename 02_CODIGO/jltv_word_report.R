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
