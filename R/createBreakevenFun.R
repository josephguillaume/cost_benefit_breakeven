createBreakevenFun <- function(scen,baseline,breakeven.factor,ranges,fixed.vals=NULL){
    pars <- getDefaultPars(NPV)
    for(v in ranges$Variable) {
        if(is.null(eval(parse(text=sprintf("pars$%s",v))))) stop(sprintf("Parameter %s not recognised",v))
    }
    ##TODO: don't require fixed values to be strings, don't require quotes in strings
    fixed.vals2 <- paste(sapply(1:length(fixed.vals),function(i) sprintf("pars$%s <- %s",names(fixed.vals)[i],fixed.vals[[i]])),collapse="\n    ")
    if(is.null(fixed.vals)) fixed.vals2 <- ""
    settings <- paste(sapply(1:nrow(ranges),function(i) sprintf("pars$%s <- x[%d]",ranges[i,1],i)),collapse="\n    ")
    if(breakeven.factor=="net.environmental.cost"){
        return(eval(parse(text=sprintf("
        function(x){
            x<-as.numeric(x)
            pars <- getDefaultPars(NPV)
            %s
            pars$scen='%s'
            %s
            annual1=do.call(NPV,pars)
            pars$scen='%s'
            annual2=do.call(NPV,pars)
            delta=annual1-annual2
            return(net.environmental.cost+delta)
        }",fixed.vals2,scen,settings,baseline))))
    }
    if(breakeven.factor=="pump.cost.dollar.per.ml"){
        return(eval(parse(text=sprintf("
        function(x){
            x<-as.numeric(x)
            pars <- getDefaultPars(NPV)
            %s
            pars$scen='%s'
            %s
            pars$state.var=NA
            annual1=do.call(NPV,pars)
            pars$state.var='\"pump.vol.ml\"'
            pumpvol1=do.call(NPV,pars)
            pars$scen='%s'
            pars$state.var=NA
            annual2=do.call(NPV,pars)
            pars$state.var='\"pump.vol.ml\"'
            pumpvol2=do.call(NPV,pars)
            delta=-(annual1-annual2)/(pumpvol2-pumpvol1)
            return(pump.cost.dollar.per.ml+delta)
        }",fixed.vals2,scen,settings,baseline))))
    }##pump.cost.dollar.per.ml
}## createBreakevenFun
