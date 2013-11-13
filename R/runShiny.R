runShiny <-
function(...){
    library(shiny)
    shiny.dir=system.file("shiny1", package = "cost_benefit_breakeven")
    runApp(shiny.dir,...)
}
