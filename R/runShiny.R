runShiny <-
function(...,name="shiny_perctolimit2"){
    library(shiny)
    shiny.dir=system.file(name, package = "cost.benefit.breakeven")
    runApp(shiny.dir,...)
}
