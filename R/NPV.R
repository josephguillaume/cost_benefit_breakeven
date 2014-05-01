NPV <-
function(
scen="base",

## Same across scenarios
pump.cost.dollar.per.ml=35,
capture.pump.cost.dollar.per.ml.surface=NA,
capture.pump.cost.dollar.per.ml.mar=NA,
capture.pump.cost.ratio.surface=0.5,
capture.pump.cost.ratio.mar=0.6,
## cotton = Cotton BT (grown with 5% pigeon pea)
crop.water.requirement.ml.per.ha=c(
 cotton=7.9,
 faba.bean=2.7,
 cultivated.dryland=0
),
water.available=c(
 supplementary=200
),
## If NA, is calculated.       
gross.margin.per.ml=c(
 cotton=NA,
 faba.bean=NA,
  cultivated.dryland=Inf
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
 cotton=2505+153.1,
 faba.bean=565,
 cultivated.dryland=224.6
),
total.overhead.cost=0,
surface.evap.rate=0.4,
mar.loss.rate=0.05,
discount.rate=.07,
nyears=c(
 base=30,
 basin=30,
 injection=20
),

## Base case cost
surface.evap.distrib=c(
 cotton=0.75,
 faba.bean=0.25,
 cultivated.dryland=0
),
farmdam.cost.per.ml=1000,
include.farm.dam.capital.cost=0,
farm.dam.maintenance.rate=0.005,
asr.capacity.ml=NA,

## Basin infiltration
basin.cost.temp.storage.per.ml=0,
basin.design.cost=NA,
basin.design.cost.proportion.of.capital.cost=0.1,
## Cost to achieve volume for 90day recharge period etc.
basin.capital.cost.per.ml=NA,
basin.capital.cost.per.ml.at.0.2.m.per.day=363,
basin.infiltration.rate=0.2, #m/day
basin.maintenance.rate=0.1,

# ASR injection
asr.design.cost=NA,
asr.design.cost.proportion.of.capital.cost=0.1,
asr.capital.cost.per.ml=700,
asr.cost.temp.storage.per.ml=0,
asr.treatment.capital.cost=NA, #overrides asr.treatment.capital.cost.per.ml
asr.treatment.capital.cost.per.ml=250,
asr.treatment.cost.per.ml=150,
asr.maintenance.rate=0.07,

net.environmental.cost=0,
                breakeven.factor=NA,
         state.var=NA

){

nyears=nyears[[scen]]
 
## Between scenarios
if(scen=="base"){
  mar.ml=0
} else {
 mar.ml=water.available["supplementary"] #600
}

if(is.na(asr.capacity.ml)) asr.capacity.ml <- mar.ml

################################################
## Gross margin for dryland and irrigated cropping

## TODO: cannot be set directly because needs to be kept consistent with sum(water.available)
total.surface.water=sum(water.available["supplementary"])

if(scen=="base") {
 net.water.available=sum(water.available)-surface.evap.rate*total.surface.water
 pump.vol.ml=0
} else {
  net.water.available=sum(water.available)-mar.loss.rate*mar.ml
  pump.vol.ml=(1-mar.loss.rate)*mar.ml
}

  land.used.ha=c(cotton=NA,faba.bean=NA,cultivated.dryland=0)

if(any(is.na(gross.margin.per.ml))){##ML/ha calculation

  water.applied.ml=crop.water.requirement.ml.per.ha*land.used.ha

  if(is.na(land.used.ha["faba.bean"]) && is.na(land.used.ha["cotton"])){
    ## Total land used throughout year (i.e. 2x actual land needed)
    total.land.used=net.water.available/(crop.water.requirement.ml.per.ha["faba.bean"]+crop.water.requirement.ml.per.ha["cotton"])
    land.used.ha["faba.bean"]=total.land.used
    land.used.ha["cotton"]=total.land.used
    water.applied.ml=crop.water.requirement.ml.per.ha*land.used.ha
  }

  gross.value.per.yield=yield.per.ha*price.per.yield
  gross.margin.per.ha=gross.value.per.yield-variable.cost.per.ha

  gross.margin.per.ml=gross.margin.per.ha/crop.water.requirement.ml.per.ha

  total.farm.gross.margin=land.used.ha*gross.margin.per.ha

} else{
  water.applied.ml=net.water.available*crop.water.requirement.ml.per.ha/sum(crop.water.requirement.ml.per.ha)
  total.farm.gross.margin=water.applied.ml*gross.margin.per.ml
  total.farm.gross.margin["cultivated.dryland"] <- land.used.ha["cultivated.dryland"]*(yield.per.ha["cultivated.dryland"]*price.per.yield["cultivated.dryland"]-variable.cost.per.ha["cultivated.dryland"])
}

##total.contrib.farm.income=total.farm.gross.margin/sum(total.farm.gross.margin)
## Fixed for each scenario
total.contrib.farm.income=c(cotton=56.9188118961194,faba.bean=28.4027025937631,cultivated.dryland=14.6784855101176)/100
overhead.cost=total.overhead.cost*total.contrib.farm.income

net.farm.income=total.farm.gross.margin-overhead.cost
net.farm.income.per.ml=net.farm.income/water.applied.ml

## if(exists("details") && isTRUE(details) ) print(cbind(crop.water.requirement.ml.per.ha,land.used.ha,water.applied.ml,yield.per.ha, price.per.yield,gross.value.per.yield,variable.cost.per.ha,gross.margin.per.ha,gross.margin.per.ml,total.farm.gross.margin,overhead.cost,net.farm.income,total.contrib.farm.income,net.farm.income.per.ml))

########################################
## Costs

if(scen=="base"){
 farm.dam.cost=farmdam.cost.per.ml*total.surface.water
 capital.cost=farm.dam.cost*include.farm.dam.capital.cost

 pump.cost=pump.vol.ml*pump.cost.dollar.per.ml ## groundwater pumping

 if(is.na(capture.pump.cost.dollar.per.ml.surface))  capture.pump.cost.dollar.per.ml.surface<-pump.cost.dollar.per.ml * capture.pump.cost.ratio.surface
 surface.pump.cost=total.surface.water*capture.pump.cost.dollar.per.ml.surface

 farm.dam.maintenance=farm.dam.maintenance.rate*farm.dam.cost

 ongoing.cost=pump.cost+farm.dam.maintenance+surface.pump.cost
 cost=annualised.capital.cost(capital.cost,discount.rate,nyears)+ongoing.cost
} else if(scen=="basin"){
 ## Basin infiltration
  if(is.na(basin.capital.cost.per.ml))  basin.capital.cost.per.ml=basin.capital.cost.per.ml.at.0.2.m.per.day*0.2/basin.infiltration.rate
  basin.capital.cost=basin.capital.cost.per.ml*asr.capacity.ml
  if(is.na(basin.design.cost)) basin.design.cost=basin.capital.cost*basin.design.cost.proportion.of.capital.cost

  temporary.storage.cost= basin.cost.temp.storage.per.ml*asr.capacity.ml

  ##print(basin.capital.cost.per.ml)
 capital.cost=
   #Design and investigation cost (one time)
  basin.design.cost+
  basin.capital.cost+
  temporary.storage.cost

##print(pump.vol.ml*pump.cost.dollar.per.ml) ## groundwater pumping

 if(is.na(capture.pump.cost.dollar.per.ml.mar))  capture.pump.cost.dollar.per.ml.mar<-pump.cost.dollar.per.ml * capture.pump.cost.ratio.mar
 surface.pump.cost=total.surface.water*capture.pump.cost.dollar.per.ml.mar

 ongoing.cost=basin.maintenance.rate*basin.capital.cost.per.ml*asr.capacity.ml+surface.pump.cost+
  pump.vol.ml*pump.cost.dollar.per.ml ## groundwater pumping

 cost=annualised.capital.cost(capital.cost,discount.rate,nyears)+ongoing.cost
} else if(scen=="injection"){
  ## ASR injection

   if(is.na(capture.pump.cost.dollar.per.ml.mar))  capture.pump.cost.dollar.per.ml.mar<-pump.cost.dollar.per.ml * capture.pump.cost.ratio.mar
  surface.pump.cost=total.surface.water*capture.pump.cost.dollar.per.ml.mar

  temporary.storage.cost= asr.cost.temp.storage.per.ml*asr.capacity.ml
  asr.capital.cost=asr.capital.cost.per.ml*asr.capacity.ml
  if(is.na(asr.treatment.capital.cost)) asr.treatment.capital.cost=asr.treatment.capital.cost.per.ml*asr.capacity.ml
  if(is.na(asr.design.cost)) asr.design.cost=(asr.capital.cost+asr.treatment.capital.cost)*asr.design.cost.proportion.of.capital.cost
  
  capital.cost=asr.design.cost+asr.capital.cost+asr.treatment.capital.cost+ temporary.storage.cost
  maintenance.cost=asr.maintenance.rate*(asr.capital.cost+asr.treatment.capital.cost)
  pump.cost=pump.vol.ml*pump.cost.dollar.per.ml
  ongoing.cost=asr.treatment.cost.per.ml*mar.ml+maintenance.cost+ pump.cost+surface.pump.cost
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

#years.discount.rate <- 1/(1 + discount.rate)^(1:nyears)
#sum(annual.cash.flow * years.discount.rate)
## years.discount.rate is a geometric series. sum is: ((1+discount.rate)^(-nyears)-(1+discount.rate)^(-1+1))/(1-(1+discount.rate))
annual.cash.flow * ((1+discount.rate)^(-nyears)-1)/(-discount.rate)

}

