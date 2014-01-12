runShiny <-
function(...,name="shiny_perctolimit"){
    library(shiny)
    shiny.dir=system.file(name, package = "cost.benefit.breakeven")
    runApp(shiny.dir,...)
}
