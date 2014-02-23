crossoverEquiconcern=function(scen, baseline, ranges){
    basin_base0 <-createFun(scen, baseline, ranges)
    dNPV=function(loc,bound){
        x=ifelse(bound<ranges$Modeled,
        bound+loc*(ranges$Modeled-bound)/100,
        bound-loc*(bound-ranges$Modeled)/100
        )
        basin_base0(x)
    }
    bounds<-do.call(expand.grid,lapply(apply(ranges[,c("Min","Max")],1,as.list),unlist))
    locs=apply(bounds,1,function(bound){
        res=tryCatch(uniroot(dNPV,c(0,100),bound=bound)$root,error=function(e) NA)
    })
    if(all(is.na(locs))) stop("No crossover points found")
    w.max.loc=which.max(locs)
    loc=locs[w.max.loc]
    bound=as.numeric(bounds[w.max.loc,])
    names(bound)<-ranges$Variable
    x=ifelse(bound<ranges$Modeled,
    bound+loc*(ranges$Modeled-bound)/100,
    bound-loc*(bound-ranges$Modeled)/100
    )
    names(x)<-ranges$Variable
    list(loc=loc,
         bound=bound,
         dNPV=dNPV(loc,bound=bound),
         values=x
         )
}
