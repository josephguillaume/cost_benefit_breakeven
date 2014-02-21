createBreakevenFun <- function(scen,baseline,breakeven.factor,ranges,fixed.vals=NULL){
    stopifnot(breakeven.factor %in% c("net.environmental.cost","pump.cost.dollar.per.ml"))
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
            pars$state.var='annual.cash.flow'
            pars$scen='%s'
            %s
            annual1=do.call(NPV,pars)
            pars$scen='%s'
            annual2=do.call(NPV,pars)
            delta=annual1-annual2
            return(pars$net.environmental.cost+delta)
        }",fixed.vals2,scen,settings,baseline))))
    }
    if(breakeven.factor=="pump.cost.dollar.per.ml"){
        scen.surface.pump=ifelse(scen=="base",
        "pars$capture.pump.cost.ratio.surface*total.surface.water",
        "pars$capture.pump.cost.ratio.mar*total.surface.water")
        base.surface.pump=ifelse(baseline=="base",
        "pars$capture.pump.cost.ratio.surface*total.surface.water",
        "pars$capture.pump.cost.ratio.mar*total.surface.water")
        return(eval(parse(text=sprintf("
        function(x){
            x<-as.numeric(x)
            pars <- getDefaultPars(NPV)
            %s
            pars$scen='%s'
            %s
            pars$state.var='annual.cash.flow'
            annual1=do.call(NPV,pars)
            pars$state.var='pump.vol.ml'
            pumpvol1=do.call(NPV,pars)
            pars$scen='%s'
            pars$state.var='annual.cash.flow'
            annual2=do.call(NPV,pars)
            pars$state.var='pump.vol.ml'
            pumpvol2=do.call(NPV,pars)
            pars$state.var='total.surface.water'
            total.surface.water=do.call(NPV,pars)
            pumpvol1=pumpvol1+%s
            pumpvol2=pumpvol2+%s
            delta=-(annual1-annual2)/(pumpvol2-pumpvol1)
            return(pars$pump.cost.dollar.per.ml+delta)
        }",fixed.vals2,scen,settings,baseline,scen.surface.pump,base.surface.pump))))
    }##pump.cost.dollar.per.ml
}## createBreakevenFun
