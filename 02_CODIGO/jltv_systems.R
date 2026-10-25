# JLTV — extensão SYS-1: parâmetros e eventos por componente.
# Implementação própria. Referências externas orientam estrutura, nunca valores JLTV.
JLTV_SYSTEMS_VERSION = 'SYS-1 / JLTV Sustainment Decision Lab 1.5.7 (2026-10-06)'
sys.source(file.path(getwd(),'jltv_enhancements.R'),envir=environment())
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
