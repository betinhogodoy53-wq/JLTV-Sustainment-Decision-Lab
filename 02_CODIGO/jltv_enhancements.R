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
