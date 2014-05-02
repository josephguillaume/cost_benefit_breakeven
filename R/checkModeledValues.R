## Get default value for each ranges$Variable before overriding
checkModeledValues<-function(ranges,MODEL=get("NPV",envir=.GlobalEnv)){
    pars <- as.list(eval(formals(MODEL)))
    vals=sapply(ranges$Variable,
    function(v) eval(eval(parse(text = sprintf("pars$%s", v)))))
    wanted <- ranges$Modeled!=vals | is.na(vals) | is.na(ranges$Modeled)
    cbind(ranges=ranges$Modeled,fn=vals)[wanted,]
}
