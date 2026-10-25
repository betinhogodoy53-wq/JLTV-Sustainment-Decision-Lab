Sys.setenv(JLTV_CORE_ONLY="1")
e <- new.env(parent=globalenv())
source("app.R", local=e, encoding="UTF-8")
check <- function(ok,msg) if(!isTRUE(ok)) stop(msg,call.=FALSE)
failures <- character()
test <- function(name,code) tryCatch({force(code);cat("PASS:",name,"\n")},error=function(x){failures<<-c(failures,name);cat("FAIL:",name,"-",conditionMessage(x),"\n")})
fixture <- function() {
r <- data.frame(priority_rank=1:3,label=c("Aumentar a confiabilidade (MTBF)","Reduzir o tempo de reparo corretivo","Ampliar o estoque de sobressalentes"),
change=c("MTBF de 180,0 h para 216,0 h (+20%)","Reparo de 8,0 h para 6,4 h (−20%)","Estoque de 6 para 9"),
base_availability=.9418,new_availability=c(.9519,.9503,.9418),delta_availability=c(.0101,.0085,0),
delta_ci_lower=c(.006,.005,0),delta_ci_upper=c(.014,.012,0),new_success=c(.5,.48,.32),
demonstrated=c(TRUE,TRUE,FALSE),conclusion=c("Ganho demonstrado","Ganho demonstrado","Ganho não demonstrado"),
criterion_label=c("Falha / Ocorrência","Manutenção e Apoio","Suprimento"),targets_bottleneck="",stringsAsFactors=FALSE)
list(meta=list(evidence_mode="EMPIRICAL",version="1.5.7",generated_at="11/10/2026"),
assessment=list(kpis=list(ao_reference=.9418,ao_improved=.959,ao_gain=.0172,ao_observed=NA_real_,
availability_target=10/12,success_reference=.32,success_improved=.64,improved_label="Pacote",
top_bottleneck="Sistema elétrico",top_bottleneck_driver="Impacto na Disponibilidade",top_process="Reparo corretivo",top_process_share=.7),
bottlenecks=data.frame(rank=1,alternative_name="Sistema elétrico",coefficient=.925,dominant_criterion_label="Impacto na Disponibilidade"),
process=data.frame(component=c("Reparo corretivo","Diagnóstico","Espera por peça"),hours=c(70,30,0),share=c(.7,.3,0)),
improvements=r,package=data.frame(base_availability=.9418,new_availability=.959,delta_availability=.0172,base_success=.32,new_success=.64),
criteria_weights=c(OCCURRENCE=.2,DOWNTIME=.4,SUPPLY=.25,SUPPORT=.15),confidence=data.frame()),
improvements=list(config=list(fleet_size=12,mission_tolerance_hours=0),horizon=168,target_ready=10,replications=50,seed=21),
simulation=list(p_target=.8,summary=data.frame(configuration_id="N12",success_probability=.32,ci_lower=.207,ci_upper=.458,mean_availability=.9418)),
multicriteria=NULL,assumptions=data.frame(Item=c("Probabilidade-alvo","Horizonte da missão (h)","Meta M (viaturas prontas)"),Valor=c(".8","168","10")))
}
report <- fixture()
test("JSON preserves criterion names and labels through serialization",{
f<-e$word_report_facts(report); j<-jsonlite::fromJSON(jsonlite::toJSON(f,auto_unbox=TRUE,na="null",null="null"))
check(is.data.frame(j$pesos_ahp_criterios)&&all(c("criterio","peso","criterio_id")%in%names(j$pesos_ahp_criterios)),"Numeric vector loses names in JSON; use named rows.")
check(j$pesos_ahp_criterios$criterio[j$pesos_ahp_criterios$criterio_id=="DOWNTIME"]=="Impacto na Disponibilidade","Criterion label must correspond to the actual named weight.")
})
test("JSON explains mission success and downtime aggregation",{
f<-e$word_report_facts(report)
check(f$contexto_da_missao$frota==12&&f$contexto_da_missao$meta_viaturas_prontas==10&&f$contexto_da_missao$horizonte_h==168,"Mission N, M and H must be available to interpret probability.")
check(f$contexto_da_missao$probabilidade_alvo==.8,"Probability target is different from M/N.")
check(grepl("viatura",f$definicoes$horas_do_processo)&&grepl("replica",f$definicoes$horas_do_processo),"Hours need their aggregation basis.")
})
test("Conclusion contains quantitative proposals and unmet mission target",{
n<-e$word_report_narrative_local(report)
s<-paste(n$conclusao,collapse=" ")
check(grepl("94,2",s)&&grepl("95,9",s)&&grepl("1,0",s)&&grepl("IC95%",s),"Conclusion must synthesize run-specific effects and uncertainty.")
check(grepl("propõe|prioriza",s)&&grepl("180,0|216,0",s),"Conclusion must state a concrete tested proposal and its magnitude.")
check(grepl("80,0|80%",s)&&grepl("64,0|64%",s),"Package below the mission probability target must be explicit.")
check(!grepl("\\{[cp]:",s),"Conclusion cannot add citations.")
})
test("Missing or nonpositive effects cannot claim full objective fulfillment",{
r<-fixture();r$assessment$improvements<-NULL;r$assessment$package<-NULL
s<-paste(e$word_report_narrative_local(r)$conclusao,collapse=" ")
check(grepl("parcial|não foi",s)&&!grepl("foi cumprido nos limites",s),"Incomplete run cannot claim the general objective was met.")
r<-fixture();r$assessment$improvements$demonstrated<-FALSE;r$assessment$improvements$delta_ci_lower<-NA_real_
s<-paste(e$word_report_narrative_local(r)$conclusao,collapse=" ")
check(!grepl("propõe priorizar",s),"No positive estimable interval means no supported intervention.")
})
test("Generic AI cannot erase the numerical decision core",{
original<-e$word_report_call_ai
e$word_report_call_ai<-function(...)list(conclusao="Recomenda-se melhorar a logística.",melhorias="É preciso investir.",proximos_passos="Estudar mais.")
tryCatch({
 n<-e$word_report_resolve_narrative(report,"claude","test-only")
 s<-paste(n$text$conclusao,collapse=" ")
 check(grepl("94,2",s)&&grepl("180,0|216,0",s),"AI response must preserve factual proposal core.")
 check(any(grepl("IC95%",n$text$melhorias)),"Evidence paragraph must remain in section 4 before conclusion.")
},finally={e$word_report_call_ai<-original})
})

test("Claude still writes a supported conclusion through the API path", {
 original <- e$word_report_call_ai
 expected <- e$word_report_decision_core(report)$conclusao
 e$word_report_call_ai <- function(...) list(conclusao=expected,melhorias="O efeito concentra-se no processo de reparo.")
 tryCatch({
  n <- e$word_report_resolve_narrative(report,"claude","test-only")
  check(identical(n$source,"claude") && identical(n$text$conclusao,expected),"Supported AI conclusion must be used.")
  check(any(grepl("O efeito concentra",n$text$melhorias)),"AI discussion must be retained.")
 },finally={e$word_report_call_ai<-original})
})
test("Proposals specify candidate actions instead of only changing a parameter", {
 s <- paste(e$word_report_narrative_local(report)$conclusao,collapse=" ")
 check(grepl("análise de causa raiz",s)&&grepl("Ferramental",s),"Operational proposals need actions already documented in the lever catalog.")
 check(grepl("teste alterou o parâmetro",s),"Parameter gain must not be reported as demonstrated action effectiveness.")
})

cat("Failures:",length(failures),"\n")
if(length(failures))stop(paste(failures,collapse="; "),call.=FALSE)
