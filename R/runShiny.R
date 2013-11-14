runShiny <-
function(...){
    library(shiny)
    shiny.dir=system.file("shiny1", package = "cost.benefit.breakeven")
    runApp(shiny.dir,...)
}
