annualised.capital.cost <-
function(C,r,t){
 annuity.factor = (1-1/(1+r)^t)/r
 C/annuity.factor
}
