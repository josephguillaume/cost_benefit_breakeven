closest <-
function(){
    cols <- c(rep(1,200),2)
    cols[unique(apply(dist,2,which.min))] <- 3
    glyph_colour(g[1]) <- cols

    library(gWidgets)
    dist <- do.call(cbind,lapply(1:length(start.pos),
                                 function(i) abs(pom[,i]-start.pos[i])))
    colnames(dist) <- ranges[,1]

    closest <- apply(dist,2,which.min)
    win=gwindow(title="Highlight closest?")
    gg <- ggroup(horizontal=FALSE,container=win)
    for(i in 1:length(closest)){
        obj=gcheckbox(names(closest)[i],container=gg,action=closest[i],
        handler=function(h,...) {
            size <- glyph_size(g[1])
            print(h$obj)
            size[h$action] <- ifelse(svalue(h$obj),5,2)
            glyph_size(g[1]) <-size
        })
    }
}
