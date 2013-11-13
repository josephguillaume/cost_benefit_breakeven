univariate.breakeven <-
function(ranges,scen,baseline){
    eqs <- sapply(ranges$Variable,function(v){
        ranges0 <- subset(ranges,Variable==v)
        basin_base0 <- createFun(scen,baseline,ranges0)
        tryCatch(return(uniroot(basin_base0,interval=c(ranges0$Min,ranges0$Max))$root),error=function(e) return(NA))
    })
    data.frame(ranges,"break"=eqs)
}
