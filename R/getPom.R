getPom <-
function(ranges,start.pos,scen,baseline){
    netdiff <- createFun(scen,baseline,ranges)
    maxmin=-sign(netdiff(start.pos))
    nn <- nsga2( ## Closest to start.pos in each dimension
                ## Function is minimised
                function(x) abs(x-start.pos),
                idim=nrow(ranges),odim=nrow(ranges),
                ## constraints>=0
                constraints=function(x) maxmin*netdiff(x),cdim=1,
                ## For the specified range
                lower.bounds=ranges$Min,upper.bounds=ranges$Max,
                ## Optimisation settings
                generations=200,popsize=200,mprob=1/nrow(ranges)
                )
    pom <- nn$par[nn$pareto.optimal,]
    if(NROW(pom)>0) colnames(pom) <- ranges[,1]
    pom
}
