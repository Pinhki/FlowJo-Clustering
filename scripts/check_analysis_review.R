source("Contents/R/global/flowjo_pipeline_helpers.R")
`%||%` <- function(a,b) if(is.null(a)) b else a
locate <- function(n,name) {
 if(!is.call(n)&&!is.expression(n)) return(NULL)
 if(is.call(n)&&identical(n[[1]],quote(`<-`))&&identical(n[[2]],as.name(name))) return(n[[3]])
 for(i in seq_along(n)){if(identical(n[[i]],quote(expr=)))next; x<-locate(n[[i]],name);if(!is.null(x))return(x)}
 NULL
}
code<-parse("Contents/R/global/shared_store.R")
analysis_source_data<-eval(locate(code,"analysis_source_data"))
items<-list(a=list(id="a",sample_id="s",gate_id="g",include=TRUE,label="Wanted"),b=list(id="b",sample_id="s",gate_id="h",include=TRUE,label="Unwanted"))
population_items<-function()items
fcs_registry<-function()list(s=list(name="Sample",df=data.frame(A=1:4,B=5:8)))
gate_tree<-function()list(g=list(name="Gate G",parent_id="root"),h=list(name="Gate H",parent_id="root"))
gate_memberships<-function()list(s=list(g=c(TRUE,TRUE,FALSE,FALSE),h=c(FALSE,FALSE,TRUE,TRUE)))
analysis_numeric_cols<-function(x)names(x)[vapply(x,is.numeric,logical(1))]
original<-items
x<-analysis_source_data("basket",population_ids="a")
stopifnot(nrow(x$data)==2,all(x$data$population_label=="Wanted"),identical(items,original),nrow(x$selection)==1)
stopifnot(inherits(try(analysis_source_data("basket",character()),silent=TRUE),"try-error"))
stopifnot(inherits(try(analysis_source_data("basket","removed"),silent=TRUE),"try-error"))
analysis_create<-eval(locate(code,"analysis_create"))
analysis_unique_name<-identity
analysis_validate_source<-function(x)x$markers
analysis_make_id<-function()"object"
objects<-list();analysis_objects<-function(x){if(missing(x))return(objects);objects<<-x}
active_analysis_id<-function(x)invisible(x)
o<-analysis_create("Reviewed",source="basket",population_ids="a")
stopifnot(identical(o$provenance$population_ids,"a"),o$provenance$selection$gate_path=="root/Gate G")
stopifnot(inherits(try(analysis_create("Wrong",source="batch"),silent=TRUE),"try-error"))
cat("PASS: reviewed subset only, basket unchanged, empty/stale selection blocked, provenance retained, upstream batch bypass blocked.\n")

library(shiny)
source("Contents/R/interface/page_gating_concat.R")
export_code <- parse("Contents/R/interface/page_gating_concat.R")
find_content <- function(n) {
 if(!is.call(n)&&!is.expression(n))return(NULL)
 if(is.call(n)&&identical(n[[1]],as.name("downloadHandler")))return(n$content)
 for(i in seq_along(n)){if(identical(n[[i]],quote(expr=)))next; x<-find_content(n[[i]]);if(!is.null(x))return(x)}
 NULL
}
exported <- NULL
flowjo_export_population_fcs_zip <- function(concat_df,marker_cols,mapping,file) exported <<- list(data=concat_df,mapping=mapping)
dat<-data.frame(A=1:4,B=5:8,sample_id="s",FlowSOM_Cluster=c("1","1","2","2"))
obj<-list(id="o",name="Object",source_id="s",markers=c("A","B"),active_run_id="r",runs=list(r=list(name="Run",result=list(df=dat))))
shared<-list(analysis=list(objects=function()list(o=obj),active_id=function()"o"))
testServer(flowjo_pipeline_page_server,args=list(shared=shared),{
 session$setInputs(export_object="o",export_run="r",export_clusters="2",concat_mode="all")
 content<-eval(find_content(export_code))
 content(tempfile())
 stopifnot(nrow(exported$data)==2,all(exported$data$A==c(3,4)),all(exported$mapping$cluster=="2"))
 session$setInputs(concat_mode="cap",balanced_n=1)
 content(tempfile())
 stopifnot(nrow(exported$data)==1,exported$mapping$events==1)
 session$setInputs(export_clusters=character())
 stopifnot(inherits(try(content(tempfile()),silent=TRUE),"try-error"))
})
cat("PASS: downstream export uses only selected run clusters, preserves marker values, applies optional cap, and rejects empty selection.\n")
