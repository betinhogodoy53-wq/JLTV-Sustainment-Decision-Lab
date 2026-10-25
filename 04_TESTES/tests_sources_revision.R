module = Sys.getenv('JLTV_SYSTEMS_FILE', unset='jltv_systems.R')
source(module, encoding='UTF-8')
stopifnot(exists('sys_source_signature'))
q = data.frame(respondent_id='R1',related_system='A1',identification='PN1')
mc = list(mc_run_id='audit1',topsis=list(estimable=TRUE,ranking=data.frame(alternative_id='A1',coefficient=.7)),ahp=list(subcriteria_weights=data.frame(global_weight=.1)))
os = data.frame(work_order_id='OS1',maintenance_type='CORRECTIVE',is_functional_failure=1,detected_at='2026-01-01 00:00:00',released_at='2026-01-01 04:00:00')
mapping = data.frame(work_order_id='OS1',component_id='A1_DEMO',source='OS1')
p = sys_demo_profiles()
src = sys_source_snapshot(q,mc,list(work_orders=os,audit_runs=data.frame(id='audit1')),mapping,p)
sig = sys_source_signature(src)
mc$mc_run_id='audit2'
stopifnot(identical(sig,sys_source_signature(sys_source_snapshot(q,mc,list(work_orders=os,audit_runs=data.frame(id='audit2')),mapping,p))))
q$identification='PN2'
stopifnot(!identical(sig,sys_source_signature(sys_source_snapshot(q,mc,list(work_orders=os),mapping,p))))
q$identification='PN1'; os$released_at='2026-01-01 05:00:00'
stopifnot(!identical(sig,sys_source_signature(sys_source_snapshot(q,mc,list(work_orders=os),mapping,p))))
os$released_at='2026-01-01 04:00:00'; mapping$source='Outro registro'
stopifnot(!identical(sig,sys_source_signature(sys_source_snapshot(q,mc,list(work_orders=os),mapping,p))))
html = htmltools::renderTags(sys_ui())$html
stopifnot(!anyDuplicated(regmatches(html,gregexpr(' id="[^"]+"',html))[[1]]))
stopifnot(all(vapply(c('Resumo','Sistemas e etapas','Intervenções','Confronto'),grepl,logical(1),x=html,fixed=TRUE)))
cat('SYS fontes/UX: assinatura muda com fontes efetivas, preserva audit IDs; quatro saídas e IDs únicos.\n')
shiny::testServer(function(input,output,session) {
  q_state=shiny::reactiveVal(data.frame(related_system='A1',identification='PN1'))
  mc_state=shiny::reactiveVal(list(mc_run_id='audit1'))
  os_state=shiny::reactiveVal(data.frame())
  full_q=shiny::reactiveVal(list(topsis_scores=data.frame(value=5)))
  result=sys_server(input,output,session,questionnaire_provider=function()q_state(),
    multicriteria_provider=function()mc_state(),snapshot_provider=function()list(work_orders=os_state()),
    references_provider=function()NULL,storage_dir=tempfile(),questionnaire_sources_provider=function()full_q())
}, {
  session$setInputs(sys_mode='SYNTHETIC',sys_load_demo=1,sys_fleet=1,sys_diag=1,sys_repair=1,sys_bays=1,
    sys_util=1,sys_horizon=20,sys_target=1,sys_n=2,sys_seed=7,sys_scope='system',sys_target_id='A3',
    sys_lever='repair',sys_change=20,sys_all=FALSE)
  session$setInputs(sys_run=1)
  stopifnot(!is.null(result()),nchar(result()$source_signature)==64,result()$settings$target_ids=='A3')
  signature=result()$source_signature
  mc_state(list(mc_run_id='audit2'));session$flushReact()
  stopifnot(!is.null(result()),identical(signature,result()$source_signature))
  full_q(list(topsis_scores=data.frame(value=6)));session$flushReact()
  stopifnot(is.null(result()))
  session$setInputs(sys_run=2)
  stopifnot(!is.null(result()))
  q_state(data.frame(related_system='A1',identification='PN2'));session$flushReact()
  stopifnot(is.null(result()))
  session$setInputs(sys_run=3)
  stopifnot(!is.null(result()))
  os_state(data.frame(work_order_id='OS_NOVA'));session$flushReact()
  stopifnot(is.null(result()))
})
cat('SYS fontes reativas: audit não invalida; P6, P7 e OS invalidam.\n')
