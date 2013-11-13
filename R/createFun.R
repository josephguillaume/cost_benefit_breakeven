createFun <-
function(scen,baseline,ranges,fixed.vals=NULL){
  pars <- getDefaultPars(NPV)
  for(v in ranges$Variable) {
    if(is.null(eval(parse(text=sprintf("pars$%s",v))))) stop(sprintf("Parameter %s not recognised",v))
  }
  ##TODO: don't require fixed values to be strings, don't require quotes in strings
  fixed.vals2 <- paste(sapply(1:length(fixed.vals),function(i) sprintf("pars$%s <- %s",names(fixed.vals)[i],fixed.vals[[i]])),collapse="\n    ")
  if(is.null(fixed.vals)) fixed.vals2 <- ""
  settings <- paste(sapply(1:nrow(ranges),function(i) sprintf("pars$%s <- x[%d]",ranges[i,1],i)),collapse="\n    ")
  eval(parse(text=sprintf("
  function(x){
    x<-as.numeric(x)
    pars <- getDefaultPars(NPV)
    %s
    pars$scen='%s'
    %s
    s=do.call(NPV,pars)
    pars$scen='%s'
    b=do.call(NPV,pars)
    diff=s-b
    stopifnot(!is.na(diff))
    diff
  }",fixed.vals2,scen,settings,baseline)))
}