getDefaultPars <-
function(fn) {
  pars <- as.list(eval(formals(fn)))
  for(i in 1:nrow(ranges))
    eval(parse(text=sprintf("pars$%s <- %f",ranges[i,1],ranges$Modeled[i])))
  pars
}
