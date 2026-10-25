source('jltv_systems.R', encoding='UTF-8')
passed = 0L
check = function(x, name) { if (!isTRUE(x)) stop(name); passed <<- passed + 1L }
near = function(x,y) isTRUE(all.equal(x,y,tolerance=1e-9,check.attributes=FALSE))
reject = function(expr) inherits(tryCatch({force(expr); NULL},error=function(e)e),'error')
# Mutations caught: lost attribution, double counting, stock sharing, renewal of healthy item.
check(exists('sys_simulate'), 'motor por componente deve existir')
open_order = data.frame(work_order_id='ABERTA',maintenance_type='CORRECTIVE',is_functional_failure=1,
  detected_at='2026-01-01 00:00:00',released_at='')
open_map = data.frame(work_order_id='ABERTA',component_id='A1_DEMO',source='OS aberta')
open_obs = sys_observed(open_order,open_map,sys_demo_profiles())
check(open_obs$classified==1 && open_obs$invalid_times==1 && nrow(open_obs$by_component)==0, 'OS aberta é lacuna de duração, sem bloquear classificação')

p = sys_demo_profiles()[1:2,]
p$component_id = c('C1','C2'); p$part_id = c('P1','P2')
p$life_distribution = 'fixed'; p$life_h = c(10,1e6)
p$diagnosis_h = 1; p$repair_h = 2; p$test_h = 1; p$time_cv = 0
p$part_probability = 0
cfg = list(fleet_size=1,diagnostic_teams=1,repair_teams=1,bays=1,utilization=1)
a = sys_simulate(p,cfg,20,1,7)
check(near(a$metrics$Ao,0.8), '10h uso + 4h parada + 6h uso: Ao=80%')
check(near(sum(a$stages$hours),4), 'conservação de quatro horas paradas')
check(all(a$stages$component_id == 'C1'), 'somente o componente falho recebe parada')
check(a$metrics$mission_success == FALSE, 'uma viatura parada viola meta contínua')
check(identical(a,sys_simulate(p,cfg,20,1,7)), 'mesma semente reproduz execução')
p$life_h = c(10,12)
b = sys_simulate(p,cfg,20,1,7)
check(near(b$events$time[b$events$event=='FAILURE'],c(10,16)), 'outro relógio retém dois h após reparo, não reinicia')
p$life_h = c(10,1e6); cfg$fleet_size = 2
c = sys_simulate(p,cfg,20,2,7)
check(sum(c$stages$hours[c$stages$stage=='QUEUE_DIAGNOSIS']) > 0, 'equipes compartilhadas geram fila')
p$part_probability = 1; p$initial_stock = 0; p$reorder_point = 0; p$reorder_qty = 1; p$lead_h = 5
cfg$fleet_size = 1
d = sys_simulate(p,cfg,20,1,7)
check(near(sum(d$stages$hours[d$stages$stage=='WAIT_PART']),5), 'falta de PN causa cinco h de espera após solicitação')
p$part_id = 'SHARED'; p$initial_stock = c(0,1)
check(reject(sys_validate_profiles(p)), 'estoque divergente do mesmo PN é rejeitado')
p = sys_demo_profiles()[1:2,]
q = sys_intervene(p,'component',p$component_id[1],'repair',20)
check(near(q$repair_h[1],p$repair_h[1]*0.8) && near(q$repair_h[2],p$repair_h[2]), 'intervenção altera somente alvo')
p$evidence_class = 'SYNTHETIC'
check(reject(sys_validate_profiles(p,'EMPIRICAL')), 'modo empírico rejeita parâmetros sintéticos')
p$life_h[1] = NA
check(reject(sys_validate_profiles(p)), 'parâmetro ausente bloqueia simulação')
# Colisão AB/CA da antiga soma ponderada não pode sincronizar itens independentes.
pair = sys_demo_profiles()[1:2,]; pair$component_id=c('AB','CA');pair$life_h=10
pair$diagnosis_h=0;pair$repair_h=0;pair$test_h=0;pair$part_probability=0
cfg$fleet_size=1
collision = sys_simulate(pair,cfg,100,1,7)
faults=collision$events[collision$events$event=='FAILURE',]
first=tapply(faults$time,faults$component_id,min)
check(length(first)==2 && abs(first[1]-first[2])>1e-9, 'IDs AB/CA não compartilham fluxo aleatório')
check(identical(collision$events,sys_simulate(pair[2:1,],cfg,100,1,7)$events), 'reordenar catálogo conserva os fluxos e eventos')
cat('SISTEMAS:',passed,'verificações aprovadas\n')
# Mutations caught: unpaired deltas, zero-observation omission, altered seeds, missing evidence export.
p = sys_demo_profiles()[1:2,]; p$life_h = c(10,1e6); p$life_distribution='fixed'
p$diagnosis_h=1; p$repair_h=2; p$test_h=1; p$time_cv=0; p$part_probability=0
cfg$fleet_size=1
no_failure = p; no_failure$life_h = 1e6
z = sys_compare(no_failure,cfg,20,1,c(7,8),'component','A1_DEMO','repair',20)
check(near(z$baseline_Ao[1],1) && nrow(z$stage_logs)==0, 'execuções sem panes geram referência de 100% e log vazio')
cmp = sys_compare(p,cfg,20,1,c(7,8),'component','A1_DEMO','repair',20)
check(near(cmp$comparison$delta_Ao,0.02), 'reduzir 2 para 1,6 h dá dois pontos percentuais na missão de 20 h')
check(near(cmp$comparison$lower95,0.02), 'diferença pareada determinística tem intervalo pontual')
check(nrow(cmp$components)==2 && cmp$components$down_h[cmp$components$component_id=='A2_DEMO']==0, 'item sem falha consta com zero')
check(nrow(cmp$replications)==4 && identical(cmp$seeds,c(7,8)), 'baseline e intervenção mantêm mesmas sementes')
mapping = data.frame(work_order_id='OS1',component_id='A1_DEMO',source='Livro OS1')
orders = data.frame(work_order_id=c('OS1','OS2'),maintenance_type='CORRECTIVE',is_functional_failure=1,
  detected_at=c('2026-01-01 00:00:00','2026-01-01 00:00:00'),released_at=c('2026-01-01 04:00:00','2026-01-01 08:00:00'))
obs = sys_observed(orders,mapping,p)
check(near(obs$by_component$down_h,4) && obs$unmapped==1, 'OS sem classificação fica fora da atribuição')
check(reject(sys_observed(orders,rbind(mapping,mapping),p)), 'mesma OS não pode ter duas causas atribuídas')
tmp = tempfile(fileext='.xlsx'); sys_export(cmp,tmp)
sheets = openxlsx::getSheetNames(tmp)
check(all(c('Componentes','Sistemas','Etapas','Intervencoes','Parametros','Replicacoes','Eventos','Referencias') %in% sheets), 'Excel contém decomposição, entradas, eventos e referências')
unlink(tmp)
cat('SISTEMAS TOTAL:',passed,'verificações aprovadas\n')
# Mutations caught: invisible navigation, duplicated IDs, UI run not calling the component engine.
html = htmltools::renderTags(sys_ui())$html
ids = regmatches(html,gregexpr(' id="[^"]+"',html))[[1]]
check(!anyDuplicated(ids), 'controles do modo por sistema têm ids únicos')
shiny::testServer(function(input,output,session) {
  result = sys_server(input,output,session,questionnaire_provider=function()data.frame(),
    multicriteria_provider=function()NULL,snapshot_provider=function()list(work_orders=data.frame()),
    references_provider=function()NULL,storage_dir=tempdir())
}, {
  session$setInputs(sys_mode='SYNTHETIC',sys_load_demo=1,sys_fleet=1,sys_diag=1,sys_repair=1,sys_bays=1,
    sys_util=1,sys_horizon=20,sys_target=1,sys_n=2,sys_seed=7,sys_scope='system',sys_target_id='A3',
    sys_lever='repair',sys_change=20,sys_all=FALSE)
  session$setInputs(sys_run=1)
  check(!is.null(result()) && result()$comparison$target_id=='A3', 'interface calcula cenário direcionado')
  check(nrow(result()$systems)==10, 'interface devolve os dez sistemas do catálogo')
})

# A peça única é consumida uma vez: outro componente aguarda a mesma reposição.
shared=sys_demo_profiles()[1:2,];shared$part_id='PN_COMUM';shared$life_distribution='fixed';shared$life_h=c(10,12)
shared$diagnosis_h=0;shared$repair_h=0;shared$test_h=0;shared$time_cv=0
shared$part_probability=1;shared$initial_stock=1;shared$reorder_point=0;shared$reorder_qty=1;shared$lead_h=100
cfg$fleet_size=1
sh=sys_simulate(shared,cfg,40,1,7)
check(near(sum(sh$stages$hours[sh$stages$stage=='WAIT_PART']),28) && nrow(sh$stocks)==1, 'PN comum tem um estoque e espera de 28 h após segundo consumo')
q=sys_intervene(shared,'system','A1','lead',20)
check(all(q$lead_h==80), 'intervenção em PN comum informa e altera seu alcance em ambos os sistemas')
real=sys_demo_profiles()[1:2,];real$evidence_class='EMPIRICAL';real$source='Registros rastreáveis de teste'
rc=sys_compare(real,cfg,20,1,c(7,8),'component','A1_DEMO','repair',20,mode='EMPIRICAL')
check(rc$parameter_sets[[2]]$evidence_class[1]=='HYPOTHESIS' && rc$parameter_sets[[2]]$evidence_class[2]=='EMPIRICAL', 'intervenção hipotética não é exportada como parâmetro empírico')
cat('SISTEMAS + UI:',passed,'verificações aprovadas\n')
