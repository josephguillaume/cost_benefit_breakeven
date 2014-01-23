\name{NPV}
\alias{NPV}
\title{
NPV for Managed Aquifer Recharge scenarios
}
\description{
Calculate NPV for given \code{scen}
}
\usage{
NPV(scen = "base", pump.cost.dollar.per.ml = 25, crop.water.requirement.ml.per.ha = c(cotton = 7.9, faba.bean = 2.7, cultivated.dryland = 0), water.available = c(surface.regular.license.ml = 400, supplementary = 200, groundwater = 750), yield.per.ha = c(cotton = 9.5, faba.bean = 5, cultivated.dryland = (1.4 + 1.8)/2), price.per.yield = c(cotton = 538, faba.bean = 348, cultivated.dryland = (348 + 244)/2), variable.cost.per.ha = c(cotton = 2505, faba.bean = 565, cultivated.dryland = 224.6), total.overhead.cost = 173066, surface.evap.rate = 0.35, asr.loss.rate = 0.15, discount.rate = 0.07, nyears = 50, surface.evap.distrib = c(cotton = 0.75, faba.bean = 0.25, cultivated.dryland = 0), asr.capacity.ml = 600, basin.design.cost = 10000, basin.capital.cost.per.ml.per.infiltration = 264 * 0.5, basin.infiltration.rate = 0.5, basin.maintenance.rate = 0.03, asr.design.cost = 20000, asr.capital.cost.per.ml = 192, asr.treatment.capital.cost = 50000, asr.treatment.cost.per.ml = 70, asr.maintenance.rate = 0.03, net.environmental.cost = 0, breakeven.factor = NA)
}
\arguments{
  \item{scen}{
    One of base,basin,injection
}
  \item{pump.cost.dollar.per.ml}{
%%     ~~Describe \code{pump.cost.dollar.per.ml} here~~
}
  \item{crop.water.requirement.ml.per.ha}{
%%     ~~Describe \code{crop.water.requirement.ml.per.ha} here~~
}
  \item{water.available}{
%%     ~~Describe \code{water.available} here~~
}
  \item{yield.per.ha}{
%%     ~~Describe \code{yield.per.ha} here~~
}
  \item{price.per.yield}{
%%     ~~Describe \code{price.per.yield} here~~
}
  \item{variable.cost.per.ha}{
%%     ~~Describe \code{variable.cost.per.ha} here~~
}
  \item{total.overhead.cost}{
%%     ~~Describe \code{total.overhead.cost} here~~
}
  \item{surface.evap.rate}{
%%     ~~Describe \code{surface.evap.rate} here~~
}
  \item{asr.loss.rate}{
%%     ~~Describe \code{asr.loss.rate} here~~
}
  \item{discount.rate}{
%%     ~~Describe \code{discount.rate} here~~
}
  \item{nyears}{
%%     ~~Describe \code{nyears} here~~
}
  \item{surface.evap.distrib}{
%%     ~~Describe \code{surface.evap.distrib} here~~
}
  \item{asr.capacity.ml}{
%%     ~~Describe \code{asr.capacity.ml} here~~
}
  \item{basin.design.cost}{
%%     ~~Describe \code{basin.design.cost} here~~
}
  \item{basin.capital.cost.per.ml.per.infiltration}{
%%     ~~Describe \code{basin.capital.cost.per.ml.per.infiltration} here~~
}
  \item{basin.infiltration.rate}{
%%     ~~Describe \code{basin.infiltration.rate} here~~
}
  \item{basin.maintenance.rate}{
%%     ~~Describe \code{basin.maintenance.rate} here~~
}
  \item{asr.design.cost}{
%%     ~~Describe \code{asr.design.cost} here~~
}
  \item{asr.capital.cost.per.ml}{
%%     ~~Describe \code{asr.capital.cost.per.ml} here~~
}
  \item{asr.treatment.capital.cost}{
%%     ~~Describe \code{asr.treatment.capital.cost} here~~
}
  \item{asr.treatment.cost.per.ml}{
%%     ~~Describe \code{asr.treatment.cost.per.ml} here~~
}
  \item{asr.maintenance.rate}{
%%     ~~Describe \code{asr.maintenance.rate} here~~
}
  \item{net.environmental.cost}{
%%     ~~Describe \code{net.environmental.cost} here~~
}
  \item{breakeven.factor}{
%%     ~~Describe \code{breakeven.factor} here~~
}
}
\value{
  Net present value for given scenario
}
\author{
Joseph Guillaume based on spreadsheet by Muhammad Arshad.
}
\seealso{
\code{\link{createFun}}
}
\examples{
NPV("basin")
}
