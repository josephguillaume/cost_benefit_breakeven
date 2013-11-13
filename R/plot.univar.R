plot.univar <-
function(ranges,variable,scen,baseline){
    ranges0 <- subset(ranges,Variable==variable)
    basin_base0 <- createFun(scen,baseline,ranges0)
    ##TODO: deal with small values
    plot(Vectorize(basin_base0),from=ranges0$Min,to=ranges0$Max,
         ##TODO: generalise beyond NPV
         main=variable,xlab=variable,ylab="NPV")
    abline(v=ranges0$Modeled)
    abline(h=0,lty=2)
}
