getRanges<-function(MODEL=NPV){
    ranges=NULL
    ## Get the default values of the model
    pars=lapply(formals(MODEL),eval)
    ## Only keep the numeric values
    ##  Others can't be analysed
    pars=pars[sapply(pars,is.numeric)]
    for(a in names(pars)){
        if(length(pars[[a]])==1){
            ## If it's a single value, set it
            ranges<-rbind(ranges,
                          data.frame(
                                     Variable=a,
                                     Modeled=eval(pars[[a]]),
                                     stringsAsFactors=FALSE
                                     )
                          )
        } else {
            ## If it's a vector, set of all of them
            ranges<-rbind(ranges,
                          data.frame(
                                     Variable=sprintf("%s$%s",a,names(eval(pars[[a]]))),
                                     Modeled=eval(pars[[a]]),
                                     stringsAsFactors=FALSE
                                     )
                          )
        }
    }
    rownames(ranges)<-NULL
    ## Remove NAs, because they are overridden by other variables
    ranges<-ranges[!is.na(ranges$Modeled),]
    ## Remove infinites because they don't make sense
    ranges<-ranges[is.finite(ranges$Modeled),]
    ## These columns need to exist but don't have sensible defaults
    ranges$Min<-NA
    ranges$Max<-NA
    ranges
}
