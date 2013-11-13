bivariate.breakeven <-
function(ranges,scen,baseline,n=100){
  stopifnot(nrow(ranges)==2)
  netdiff <- createFun(scen,baseline,ranges)
  xs<-rep(NA,n)
  ys=seq(ranges$Min[2],ranges$Max[2],length.out=n)
  for(i in 1:n){
    xs[i] <- tryCatch(uniroot(function(x) netdiff(c(x,ys[i])),
                                                  interval=c(ranges$Min[1],ranges$Max[1]))$root,
                      error=function(e) return(NA))
  }
  mm <- cbind(xs,ys)
  colnames(mm) <- ranges$Variable
  mm
}
