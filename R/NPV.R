NPV <-
function(
scen="base",

## Same across scenarios
pump.cost.dollar.per.ml=25,
## cotton = Cotton BT (grown with 5% pigeon pea)
crop.water.requirement.ml.per.ha=c(
 cotton=7.9,
 faba.bean=2.7,
 cultivated.dryland=0
),
                crop.proportion.water.required=c(
                  cotton=0.72,
                  faba.bean=0.28,
                  cultivated.dryland=0
                  ),
water.available=c(
 surface.regular.license.ml=400,
 supplementary=200,
 groundwater=750
),
yield.per.ha=c(
 cotton=9.5,
 faba.bean=5,
 cultivated.dryland=(1.4+1.8)/2
),
price.per.yield=c(
 cotton=538,
 faba.bean=348,
 cultivated.dryland=(348+244)/2
),
variable.cost.per.ha=c(
 cotton=2505,
 faba.bean=565,
 cultivated.dryland=224.6
),
total.overhead.cost=173066,
surface.evap.rate=0.35,
asr.loss.rate=0.15,
discount.rate=.07,
nyears=50,

## Base case cost
surface.evap.distrib=c(
 cotton=0.75,
 faba.bean=0.25,
 cultivated.dryland=0
),

asr.capacity.ml=600,

## Basin infiltration
basin.design.cost=10000,
                ## Cost to achieve volume for 90day recharge period etc.
                basin.capital.cost.per.ml.per.infiltration=264*0.5,
                basin.infiltration.rate=0.5, #m/m2/day
basin.maintenance.rate=0.03,

# ASR injection
asr.design.cost=20000,
asr.capital.cost.per.ml=192,
asr.treatment.capital.cost=50000,
asr.treatment.cost.per.ml=70,
asr.maintenance.rate=0.03,

net.environmental.cost=0,
                breakeven.factor=NA,
         state.var=NA

){

## Between scenarios
if(scen=="base"){
  asr.ml=0
  land.used.ha=c(cotton=105,faba.bean=NA,cultivated.dryland=280)
} else {
 asr.ml=water.available["surface.regular.license.ml"]+water.available["supplementary"] #600
 land.used.ha=c(cotton=NA,faba.bean=NA,cultivated.dryland=280)
}

################################################
## Gross margin for dryland and irrigated cropping

if(scen=="base") {
 net.water.available=sum(water.available)-surface.evap.rate*sum(water.available[c("surface.regular.license.ml","supplementary")])
 pump.vol.ml=water.available["groundwater"]
} else {
  net.water.available=sum(water.available)-asr.loss.rate*asr.ml
  ##c(cotton=906,faba.bean=354) for 1260, see spreadsheet
  wanted.water.applied=net.water.available*crop.proportion.water.required
  pump.vol.ml=water.available["groundwater"]+(1-asr.loss.rate)*asr.ml
}

water.applied.ml=crop.water.requirement.ml.per.ha*land.used.ha

if(is.na(land.used.ha["faba.bean"]) && is.na(land.used.ha["cotton"])){
 land.used.ha["faba.bean"]=wanted.water.applied["faba.bean"]/crop.water.requirement.ml.per.ha["faba.bean"]
 land.used.ha["cotton"]=wanted.water.applied["cotton"]/crop.water.requirement.ml.per.ha["cotton"]
 water.applied.ml=crop.water.requirement.ml.per.ha*land.used.ha
} else if(is.na(land.used.ha["faba.bean"])){
  ## Assume water limiting, not land - Faba bean area expands depending on water availability
  land.used.ha["faba.bean"]=(net.water.available-sum(water.applied.ml,na.rm=T))/crop.water.requirement.ml.per.ha["faba.bean"]
 water.applied.ml=crop.water.requirement.ml.per.ha*land.used.ha
}


gross.value.per.yield=yield.per.ha*price.per.yield
gross.margin.per.ha=gross.value.per.yield-variable.cost.per.ha

gross.margin.per.ml=gross.margin.per.ha/crop.water.requirement.ml.per.ha

total.farm.gross.margin=land.used.ha*gross.margin.per.ha

#total.contrib.farm.income=total.farm.gross.margin/sum(total.farm.gross.margin)
## Fixed for each scenario
total.contrib.farm.income=c(cotton=56.9188118961194,faba.bean=28.4027025937631,cultivated.dryland=14.6784855101176)/100
overhead.cost=total.overhead.cost*total.contrib.farm.income

net.farm.income=total.farm.gross.margin-overhead.cost
net.farm.income.per.ml=net.farm.income/water.applied.ml


## if(exists("details") && isTRUE(details) ) print(cbind(crop.water.requirement.ml.per.ha,land.used.ha,water.applied.ml,yield.per.ha, price.per.yield,gross.value.per.yield,variable.cost.per.ha,gross.margin.per.ha,gross.margin.per.ml,total.farm.gross.margin,overhead.cost,net.farm.income,total.contrib.farm.income,net.farm.income.per.ml))

########################################
## Costs

if(scen=="base"){
 ## Cost of evaporative losses
 total.evap.loss=surface.evap.rate*sum(water.available[c("surface.regular.license.ml","supplementary")])
 evaporation.lost.farm.income=sum(total.evap.loss*surface.evap.distrib*net.farm.income.per.ml,na.rm=T)
 cost=evaporation.lost.farm.income+
  pump.vol.ml*pump.cost.dollar.per.ml ## groundwater pumping
} else if(scen=="basin"){
 ## Basin infiltration
  basin.capital.cost.per.ml=basin.capital.cost.per.ml.per.infiltration/basin.infiltration.rate
  ##print(basin.capital.cost.per.ml)
 capital.cost=
   #Design and investigation cost (one time)
  basin.design.cost+
   #Capital cost  600 ML Recharge (ecl. Temporary storage  and excl. land)
  basin.capital.cost.per.ml*asr.capacity.ml

##print(pump.vol.ml*pump.cost.dollar.per.ml) ## groundwater pumping

 ongoing.cost=basin.maintenance.rate*basin.capital.cost.per.ml*asr.capacity.ml+
  pump.vol.ml*pump.cost.dollar.per.ml ## groundwater pumping

 cost=annualised.capital.cost(capital.cost,discount.rate,nyears)+ongoing.cost
} else if(scen=="injection"){
  ## ASR injection
  capital.cost=asr.design.cost+asr.capital.cost.per.ml*asr.capacity.ml+asr.treatment.capital.cost
  ongoing.cost=asr.treatment.cost.per.ml*asr.ml+asr.maintenance.rate*(asr.capital.cost.per.ml*asr.capacity.ml+asr.treatment.capital.cost)+ pump.vol.ml*pump.cost.dollar.per.ml
  cost=annualised.capital.cost(capital.cost,discount.rate,nyears)+ongoing.cost
} else{
 stop(sprintf("scen '%s' not recognised",scen))
}

## Environmental cost/benefit
if(scen!="base"){
 cost=cost+net.environmental.cost
}

########################################
## Benefits
benefit=sum(net.farm.income)

annual.cash.flow=benefit-cost

## if(exists("details") && isTRUE(details) ){
## print(cost)
## print(benefit)
## print(annual.cash.flow)
## }

## Single scenario breakeven, i.e. obtain NPV of zero
if(!is.na(breakeven.factor) && breakeven.factor=="net.environmental.cost") return(net.environmental.cost+annual.cash.flow)
if(!is.na(breakeven.factor) && breakeven.factor=="pump.cost.dollar.per.ml") return(pump.cost.dollar.per.ml+annual.cash.flow/pump.vol.ml)

## TODO: allow requesting more than one state variable
if(!is.na(state.var)) return(get(state.var))

years.discount.rate <- 1/(1 + discount.rate)^(1:nyears)
sum(annual.cash.flow * years.discount.rate)
}
