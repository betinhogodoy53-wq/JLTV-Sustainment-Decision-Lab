source('jltv_systems.R', encoding='UTF-8')
stopifnot(length(jltv_new_package_map()) >= 20L)
stopifnot(all(vapply(names(jltv_new_package_map()), requireNamespace, logical(1), quietly=TRUE)))
html = htmltools::renderTags(sys_ui())$html
stopifnot(all(vapply(c('sys_audit_table','sys_audit_csv','sys_diagnostic_svg',
  'sys_diagnostic_png','sys_animation_gif'),function(id)grepl(paste0('id="',id,'"'),html,fixed=TRUE),logical(1))))
p = sys_demo_profiles()[1:2,]
p$life_distribution = 'fixed'; p$life_h = c(10, 1e6)
p$diagnosis_h = 1; p$repair_h = 2; p$test_h = 1
p$time_cv = 0; p$part_probability = 0
cfg = list(fleet_size=2, diagnostic_teams=1, repair_teams=1, bays=1, utilization=1)
r = sys_compare(p, cfg, 20, 1, c(7, 8), 'component', p$component_id[1], 'repair', 20)
a = jltv_stage_audit(r)
stopifnot(nrow(a) == nrow(r$systems), all(a$consistent),
  isTRUE(all.equal(sum(a$logged_hours), sum(r$baseline$down_h), tolerance=1e-9)))
stopifnot(jltv_quality_agent(r))
stopifnot(jltv_wilson_audit(r))
s = jltv_scenario_overview(r)
stopifnot(nrow(s) == 2L, all(c('Ao_mean','mission_rate') %in% names(s)),
  isTRUE(all.equal(s$Ao_mean[s$scenario=='Referencia'], mean(r$baseline$Ao))))
f = jltv_first_failure_fit(r)
stopifnot(inherits(f, 'survfit'), max(f$time) <= r$horizon)
chart = jltv_diagnostic_chart(r)
stopifnot(inherits(chart, 'patchwork'))
svg = tempfile(fileext='.svg'); jltv_export_svg(r, svg)
stopifnot(file.exists(svg), file.info(svg)$size > 100L)
png = tempfile(fileext='.png'); jltv_export_png(r,png)
stopifnot(file.exists(png), file.info(png)$size > 100L)
stopifnot(all(is.na(jltv_bootstrap_ao(r, times=30L))))
r10 = sys_compare(p,cfg,20,1,1:10,'component',p$component_id[1],'repair',20)
boot = jltv_bootstrap_ao(r10, times=30L)
stopifnot(length(boot)==2L, all(is.finite(boot)), boot[1]<=boot[2])
csv = tempfile(fileext='.csv'); jltv_export_audit_csv(r, csv)
stopifnot(file.exists(csv), nrow(readr::read_csv(csv, show_col_types=FALSE)) == nrow(a))
gif = tempfile(fileext='.gif'); jltv_export_animation(r, gif)
stopifnot(file.exists(gif), file.info(gif)$size > 100L)
shiny::testServer(function(input,output,session) {
  result = sys_server(input,output,session,questionnaire_provider=function()data.frame(),
    multicriteria_provider=function()NULL,snapshot_provider=function()list(work_orders=data.frame()),
    references_provider=function()NULL,storage_dir=tempdir())
}, {
  session$setInputs(sys_mode='SYNTHETIC',sys_load_demo=1,sys_fleet=2,sys_diag=1,sys_repair=1,sys_bays=1,
    sys_util=1,sys_horizon=20,sys_target=1,sys_n=2,sys_seed=7,sys_scope='system',sys_target_id='A3',
    sys_lever='repair',sys_change=20,sys_all=FALSE)
  session$setInputs(sys_run=1)
  stopifnot(grepl('Conciliação aprovada',output$sys_audit_status,fixed=TRUE))
})
cat('PACOTES v1.5.3: auditoria, cenários, censura, SVG, CSV e GIF OK\n')
