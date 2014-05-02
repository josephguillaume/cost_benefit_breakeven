library(ggplot2)
library(scales)
library(shiny)
library(hydromad) ##SCEoptim
library(cost.benefit.breakeven)

radioButtonsTable <-
function (inputId, label, data,choices,selected=NULL,titles=list(),concern=NULL)
{
    choices <- shiny:::choicesWithNames(choices)
    if (is.null(selected))
        selected <- names(choices)[[1]]
    inputTags <- list()
    for (i in seq_along(choices)) {
        id <- paste(inputId, i, sep = "")
        name <- names(choices)[[i]]
        value <- choices[[i]]
        inputTag <- tags$input(type = "radio", name = inputId,
                               id = id, value = value)
        if (identical(name, selected))
            inputTag$attribs$checked = "checked"
        ## spanTag <- tags$span(name)
        ## labelTag <- tags$label(class = "radio",inputTag,spanTag)
        inputTags[[length(inputTags) + 1]] <- inputTag
    }
    tags$div(id = inputId, class = "control-group shiny-input-radiogroup",
             tags$label(class = "control-label", `for` = inputId,
                        label),
             tags$table(
                        id = inputId,
                        tags$thead(
                                   ##class = 'hide',
                                   tags$tr(
                                           lapply(names(data), function(name) {
                                               return(tags$th(name,title=titles[[name]]))
                                           }),
                                           tags$th("plot")
                                           )
                                   ),
                        tags$tbody(
                                   lapply(1:nrow(data), function(i) {
                                     tags$tr(
                                             class=concern[i],
                                             lapply(names(data), function(name) {
                                               tags$td(as.character(data[i,name]))
                                             }),
                                             tags$td(inputTags[[i]])
                                             )
                                   })
                                   )
                        )
             )
}

radioButtonsTable2 <-
function (inputId,data,choices,selected=NULL,
          inputId2,choices2,selected2=NULL
          )
{
    ## Choices 1
    choices <- shiny:::choicesWithNames(choices)
    if (is.null(selected))
        selected <- names(choices)[[1]]
    inputTags <- list()
    for (i in seq_along(choices)) {
        id <- paste(inputId, i, sep = "")
        name <- names(choices)[[i]]
        value <- choices[[i]]
        inputTag <- tags$input(type = "radio", name = inputId,
                               id = id, value = value)
        if (identical(name, selected))
            inputTag$attribs$checked = "checked"
        ## spanTag <- tags$span(name)
        ## labelTag <- tags$label(class = "radio",inputTag,spanTag)
        inputTags[[length(inputTags) + 1]] <- inputTag
    }
    ## Choices 2
    choices2 <- shiny:::choicesWithNames(choices2)
    if (is.null(selected2))
        selected2 <- names(choices2)[[1]]
    inputTags2 <- list()
    for (i in seq_along(choices2)) {
        id <- paste(inputId2, i, sep = "")
        name <- names(choices2)[[i]]
        value <- choices2[[i]]
        inputTag <- tags$input(type = "radio", name = inputId2,
                               id = id, value = value)
        if (identical(name, selected2))
            inputTag$attribs$checked = "checked"
        ## spanTag <- tags$span(name)
        ## labelTag <- tags$label(class = "radio",inputTag,spanTag)
        inputTags2[[length(inputTags2) + 1]] <- inputTag
    }
    ## In table
    ## 3 column with names
    ## div(style="position:relative",
    ##     div(lapply(1:nrow(data),function(i) span(data[i,],br())),style="position:absolute;width:90%"),
    ##     div(style="position:absolute;width:10%;right:0",
    ##         div(id=inputId,class = "control-group shiny-input-radiogroup",style="position:absolute;width:50%",
    ##             lapply(inputTags,function(x) span(class="rbtrow",x,br()))),
    ##         div(id=inputId2,class = "control-group shiny-input-radiogroup",style="position:absolute;width:50%;right:0",
    ##             lapply(inputTags2,function(x) span(class="rbtrow",x,br())))
    ##         ),
    ##     div(style="clear:both;height:500px")
    ##     )
    ## 2 column without names
    div(
        div(id=inputId,class = "control-group shiny-input-radiogroup",style="position:absolute;width:50%",
            div(class="rbtrow","Var 1"),
            lapply(inputTags,function(x) div(class="rbtrow",x,br()))),
        div(id=inputId2,class = "control-group shiny-input-radiogroup",style="position:absolute;width:50%;right:0",
            div(class="rbtrow","Var 2"),
            lapply(inputTags2,function(x) div(class="rbtrow",x,br())))
        )


    ## tags$div(id = inputId,
    ##          class = "control-group shiny-input-radiogroup",
    ##          ## tags$label(class = "control-label", `for` = inputId,
    ##          ##            label),
    ##          tags$table(
    ##                     ## id = inputId,
    ##                     tags$thead(
    ##                                ##class = 'hide',
    ##                                tags$tr(
    ##                                        lapply(names(data), function(name) {
    ##                                            tags$th(name)
    ##                                        }),
    ##                                        tags$th("Var1"),tags$th("Var2")
    ##                                        )
    ##                                ),
    ##                     tags$tbody(
    ##                                lapply(1:nrow(data), function(i) {
    ##                                    tags$tr(
    ##                                            lapply(names(data), function(name) {
    ##                                                tags$td(as.character(data[i,name]))
    ##                                            }),
    ##                                            tags$td(inputTags[[i]]),
    ##                                            tags$td(inputTags2[[i]])
    ##                                            )
    ##                                })
    ##                                )
    ##                     )
    ##          )
}

get.normalised <- function(x,start.pos,limit.val1,limit.val2)
    ifelse(abs(x-start.pos)<1e-5,0,
           ifelse(x>start.pos,
                  (x-start.pos)/(limit.val2-start.pos),
                  (start.pos-x)/(start.pos-limit.val1)
                  ))

shinyServer(function(input, output, session) {

    ## uni.bkeven <- reactive({
    ##     ranges
    ## })
    limits <- reactive({
        update.ranges()
        ##input$update_ranges
        user.bounds <- sapply(ranges$Variable,function(v) input[[sprintf("more_slider_%s",v)]])
        if(!is.matrix(user.bounds)) stop("Error in reading user bounds from sliders")
        df <- data.frame(ranges,t(user.bounds))
        rownames(df) <- df$Variable
        df
    })

################################################################################

    this.univariate.breakeven <- reactive({
      update.ranges()
      input$scen
      input$baseline
      uni.bkeven <- univariate.breakeven(ranges,
                                         input$scen,input$baseline)
    })

    uni.bkevenf <- reactive({
      uni.bkeven <- this.univariate.breakeven()
      uni.bkeven$perc.to.limit <- get.normalised(uni.bkeven$"break",limits()$Modeled,limits()$X1,limits()$X2)
      perc.change <- (uni.bkeven$"break"-limits()$Modeled)/limits()$Modeled
      uni.bkeven$"% change of best guess" <- ifelse(is.na(perc.change),"",sprintf("%g%%",round(perc.change,2)*100))
      uni.bkeven$"Level of comfort" <- ifelse(is.na(uni.bkeven$perc.to.limit),"",sprintf("%g%%",round(uni.bkeven$perc.to.limit,2)*100))
      uni.bkeven$"Level of concern" <- ifelse(is.na(uni.bkeven$perc.to.limit),"",sprintf("%g%%",round(1-uni.bkeven$perc.to.limit,2)*100))
      uni.bkeven$NPV <-sapply(limits()$Variable,function(var){
          pars <- as.list(eval(formals(NPV)))
          for (i in 1:nrow(limits())) eval(parse(text = sprintf("pars$%s <- %f",
                                                 limits()$Variable[i], limits()$Modeled[i])))
          pars$scen <- input$scen
          eval(parse(text = sprintf("pars$%s <- %f",
                     var, uni.bkeven$"break"[uni.bkeven$Variable==var])))
          do.call(NPV,pars)
      })
      uni.bkeven$NPV <- ifelse(is.na(uni.bkeven$"break"),"",sprintf("$%gM",round(uni.bkeven$NPV/1e6,2)))
      uni.bkeven$"Value at crossover point" <- round(uni.bkeven$"break",2)
      uni.bkeven
    })

    output$uni_plot_variable<- renderUI({
      update.ranges()
      input$scen
      input$baseline
      uni.bkeven <- uni.bkevenf()
      radioButtonsTable("uni_plot_variable_selected",
                        "Univariate break-even values", uni.bkeven[,c("Level of comfort",
                                                                      "Level of concern",
                                                                      "Value at crossover point",
                                                                      "% change of best guess",
                                                                      "NPV"
                                                                      )],
                        choices=uni.bkeven$Variable,selected=isolate(input$uni_plot_variable_selected),
                        title=list(
                          "Level of comfort"="% distance from best guess",
                          "Level of concern"="% distance from bound"
                        ),
                        concern=ifelse(1-uni.bkeven$perc.to.limit>0.75,"highconcern",
                          ifelse(1-uni.bkeven$perc.to.limit>0.25,"midconcern","lowconcern"))
                        )
    })

    output$uni_plot <- renderPlot({
      update.ranges()
      ##plot.univar(ranges,input$uni_plot_variable_selected,input$scen,input$baseline)
      variable <- input$uni_plot_variable_selected

      ranges0 <- subset(ranges, Variable == variable)
      basin_base0 <- createFun(input$scen, input$baseline, ranges0)
      g <- ggplot(data=data.frame(x=c(ranges0$Min,ranges0$Max)))+
          geom_vline(aes(xintercept=x,linetype=type,size=type,colour=type),
                     data=data.frame(
                     x=c(as.numeric(limits()[variable,"Modeled"]),as.numeric(limits()[variable,c("X1","X2")])),
                     type=c("Best guess","Limits","Limits")),
                     show_guide=TRUE)+
              geom_hline(aes(yintercept=x,linetype="Equal NPV",size="Equal NPV",colour="Equal NPV"),data=data.frame(x=0))+
                stat_function(aes(linetype="Difference in NPV",size="Difference in NPV",colour="Difference in NPV"),fun=Vectorize(basin_base0))+
                  scale_linetype_manual(name="Lines",values=c("Limits"="solid","Best guess"="dashed","Equal NPV"="solid","Difference in NPV"="solid"))+
                    scale_size_manual(name="Lines",values=c("Limits"=0.5,"Best guess"=0.5,"Equal NPV"=1,"Difference in NPV"=1))+
                      scale_colour_manual(name="Lines",values=c("Limits"="black","Best guess"="black","Equal NPV"="grey","Difference in NPV"="black"))+
                        scale_x_continuous(name=variable,limits=range(c(ranges0$Min,ranges0$Max,limits()[variable,"X1"],limits()[variable,"X2"])))+
                          scale_y_continuous(name=sprintf("NPV of %s - NPV of %s",input$scen,input$baseline))+
                              theme(legend.position="top")
      print(g)

    })

################################################################################

    output$bi_plot_variable<- renderUI({
      update.ranges()
      radioButtonsTable2(inputId="bi_var1",
                         ranges[,"Variable",drop=FALSE],
                         choices=ranges$Variable,
                         inputId2="bi_var2",choices2=ranges$Variable
                         )
    })

    do.bivariate.breakeven <- reactive({
      cat("do.bivariate.breakeven\n",file=stderr())
      input$bi_btn_update
      v1 <- isolate(input$bi_var1)
      v2 <- isolate(input$bi_var2)
      if(is.null(v1) || is.null(v2)) stop("Select two different variables and click Update")
      if(v1==v2) stop("Select two different variables and click Update")
      ranges <- subset(ranges,Variable %in% c(v1,v2))
      ##pom <- getPom(ranges,ranges$Modeled,isolate(input$scen),isolate(input$baseline))
      pom <- bivariate.breakeven(ranges,isolate(input$scen),isolate(input$baseline),
                                 n=isolate(input$bi_n_points))
    })

    output$bi_plot <- renderPlot({
      pom <- do.bivariate.breakeven()
      pom<-pom[!apply(pom,1,function(x) any(is.na(x))),]
      if(nrow(pom)==0) stop("No breakeven points found for the variables selected")
      cat("bi_plot\n",file=stderr())
      input$bi_flip
      v1 <- isolate(input$bi_var1)
      v2 <- isolate(input$bi_var2)
      if(input$bi_flip){
        vtemp <- v1
        v1 <- v2
        v2 <- vtemp
      }
      wvars <- match(c(v1,v2),limits()$Variable)
      perc.to.limit <- apply(pom[,c(v1,v2)],1,function(x) max(get.normalised(x,limits()$Modeled[wvars],limits()$X1[wvars],limits()$X2[wvars])))*100
      g <- ggplot()+
        geom_point(aes(x=x1,y=x2,color=level.of.concern),data=data.frame(x1=pom[,v1],x2=pom[,v2],level.of.concern=100-perc.to.limit))+
          scale_x_continuous(name=v1)+scale_y_continuous(name=v2)+
            ##scale_x_continuous(name=v1,limits=range(c(pom[,v1],ranges[ranges$Variable==v1,"Modeled"]),na.rm=TRUE))+
            ##scale_y_continuous(name=v2,limits=range(c(pom[,v2],ranges[ranges$Variable==v2,"Modeled"]),na.rm=TRUE))+
            scale_colour_gradient2(name="Level of concern",limits=c(0,100),low="#008800",mid="#FFA500",high="#FF0000",midpoint=50)+
              ##geom_point(aes(x=X1,y=X2),color="red",data=data.frame(t(limits()$Modeled[wvars])))+
                geom_vline(aes(xintercept=x,linetype="Best guess"),data=data.frame(x=as.numeric(limits()[wvars[1],"Modeled"])),show_guide=TRUE)+
                  geom_hline(aes(yintercept=x,linetype="Best guess"),data=data.frame(x=as.numeric(limits()[wvars[2],"Modeled"])),show_guide=TRUE)+
                    geom_vline(aes(xintercept=x,linetype="Limits"),data=data.frame(x=as.numeric(limits()[wvars[1],c("X1","X2")])),show_guide=TRUE)+
                      geom_hline(aes(yintercept=x,linetype="Limits"),data=data.frame(x=as.numeric(limits()[wvars[2],c("X1","X2")])),show_guide=TRUE)+
                        ##geom_segment(aes(x=x,y=y,xend=xend,yend=yend,linetype="Equidistant lines"),data=data.frame(x=limits()$Modeled[wvars[1]],y=limits()$Modeled[wvars[2]],expand.grid(xend=as.numeric(limits()[wvars[1],c("X1","X2")]),yend=as.numeric(limits()[wvars[2],c("X1","X2")]))))+
                          scale_linetype_manual(name="Values",values=c("Limits"="solid","Best guess"="dashed","Equidistant lines"="dotted"))
      print(g)
    })

    update.ranges <- reactive({
      cat("update.ranges\n",file=stderr())
      input$btn_update_ranges
      ranges.new <- as.data.frame(isolate(input$matrix_ranges),stringsAsFactors=FALSE)
      names(ranges.new) <- c("Variable","Modeled","Min","Max")
      ranges.new$Variable <- as.character(ranges.new$Variable)
      ranges.new$Modeled <- as.numeric(ranges.new$Modeled)
      ranges.new$Min <- as.numeric(ranges.new$Min)
      ranges.new$Max <- as.numeric(ranges.new$Max)
      ranges.new$Variable[ranges.new$Variable==""] <- NA
      ranges.new <- ranges.new[!apply(ranges.new,1,function(x) all(is.na(x))),,drop=FALSE]
      if(nrow(ranges.new)>0) ranges <<- ranges.new
      ranges
    })


    ################################################################################
    ## Notes

    new.notes <- function() if(!exists("notes")) assign("notes",data.frame(Variable=ranges$Variable,
                                                                           bestguess="",
                                                                           bounds="",
                                                                           direction="",
                                                                           is.problem="",
                                                                           stringsAsFactors=FALSE),env=.GlobalEnv)

    ## Base
    observe({
        cat("updating notes bestguess\n",file=stderr())
        handle.load.notes()
        input$notes_which
        input$btn_reset_notes
        new.notes()
        ## Best guess
        val <- notes$bestguess[notes$Variable==input$notes_which]
        if(is.null(val)) val <- ""
        session$sendInputMessage("notes_bestguess", list(value=val))
        ## Bounds
        val <- notes$bounds[notes$Variable==input$notes_which]
        if(is.null(val)) val <- ""
        session$sendInputMessage("notes_bounds", list(value=val))
    })
    observe({
        cat("saving notes bestguess\n",file=stderr())
        new.notes()
        handle.load.notes()
        ## FIXME: intended to prevent overwriting data with blanks, must be more reliable way to do it for all notes
        if(input$btn_save_notes==0) return()
        notes$bestguess[notes$Variable==isolate(input$notes_which)] <<- isolate(input$notes_bestguess)
        notes$bounds[notes$Variable==isolate(input$notes_which)] <<- isolate(input$notes_bounds)
        ##print(notes)
    })

    ## Single variable
    observe({
        cat("updating notes single-var\n",file=stderr())
        handle.load.notes()
        input$uni_plot_variable_selected
        input$btn_reset_notes1
        new.notes()
        ## Best guess
        val <- notes$bestguess[notes$Variable==input$uni_plot_variable_selected]
        if(is.null(val)) val <- ""
        session$sendInputMessage("notes_bestguess1", list(value=val))
        ## Bounds
        val <- notes$bounds[notes$Variable==input$uni_plot_variable_selected]
        if(is.null(val)) val <- ""
        session$sendInputMessage("notes_bounds1", list(value=val))
        ## Direction
        val <- notes$direction[notes$Variable==input$uni_plot_variable_selected]
        if(is.null(val)) val <- ""
        session$sendInputMessage("notes_direction1", list(value=val))
        ## Is.problem
        val <- notes$is.problem[notes$Variable==input$uni_plot_variable_selected]
        if(is.null(val)) val <- ""
        session$sendInputMessage("notes_isproblem1", list(value=val))
    })
    observe({
        cat("saving notes single-var\n",file=stderr())
        handle.load.notes()
        input$btn_save_notes1
        new.notes()
        notes$bestguess[notes$Variable==isolate(input$uni_plot_variable_selected)] <<- isolate(input$notes_bestguess1)
        notes$bounds[notes$Variable==isolate(input$uni_plot_variable_selected)] <<- isolate(input$notes_bounds1)
        notes$direction[notes$Variable==isolate(input$uni_plot_variable_selected)] <<- isolate(input$notes_direction1)
        notes$is.problem[notes$Variable==isolate(input$uni_plot_variable_selected)] <<- isolate(input$notes_isproblem1)
        ##print(notes)
    })

    ## Two variable
    observe({
        cat("updating notes two-var\n",file=stderr())
        handle.load.notes()
        input$bi_var1
        input$bi_var2
        input$btn_reset_notes2
        new.notes()
        ## Best guess
        val <- notes$bestguess[notes$Variable==input$bi_var1]
        if(is.null(val)) val <- ""
        session$sendInputMessage("notes_bestguess2a", list(value=val))
        val <- notes$bestguess[notes$Variable==input$bi_var2]
        if(is.null(val)) val <- ""
        session$sendInputMessage("notes_bestguess2b", list(value=val))
        ## Bounds
        val <- notes$bounds[notes$Variable==input$bi_var1]
        if(is.null(val)) val <- ""
        session$sendInputMessage("notes_bounds2a", list(value=val))
        val <- notes$bounds[notes$Variable==input$bi_var2]
        if(is.null(val)) val <- ""
        session$sendInputMessage("notes_bounds2b", list(value=val))
        ## Direction
        val <- notes$direction[notes$Variable==input$bi_var1]
        if(is.null(val)) val <- ""
        session$sendInputMessage("notes_direction2a", list(value=val))
        val <- notes$direction[notes$Variable==input$bi_var2]
        if(is.null(val)) val <- ""
        session$sendInputMessage("notes_direction2b", list(value=val))
        ## Is.problem
        val <- notes$is.problem[notes$Variable==input$bi_var1]
        if(is.null(val)) val <- ""
        session$sendInputMessage("notes_isproblem2a", list(value=val))
        val <- notes$is.problem[notes$Variable==input$bi_var2]
        if(is.null(val)) val <- ""
        session$sendInputMessage("notes_isproblem2b", list(value=val))
    })
    observe({
        cat("saving notes two-var\n",file=stderr())
        handle.load.notes()
        input$btn_save_notes2
        new.notes()
        notes$bestguess[notes$Variable==isolate(input$bi_var1)] <<- isolate(input$notes_bestguess2a)
        notes$bounds[notes$Variable==isolate(input$bi_var1)] <<- isolate(input$notes_bounds2a)
        notes$direction[notes$Variable==isolate(input$bi_var1)] <<- isolate(input$notes_direction2a)
        notes$is.problem[notes$Variable==isolate(input$bi_var1)] <<- isolate(input$notes_isproblem2a)
        notes$bestguess[notes$Variable==isolate(input$bi_var2)] <<- isolate(input$notes_bestguess2b)
        notes$bounds[notes$Variable==isolate(input$bi_var2)] <<- isolate(input$notes_bounds2b)
        notes$direction[notes$Variable==isolate(input$bi_var2)] <<- isolate(input$notes_direction2b)
        notes$is.problem[notes$Variable==isolate(input$bi_var2)] <<- isolate(input$notes_isproblem2b)
        ##print(notes)
    })

    ################################################################################

    output$bestguess_NPV <- renderText({
        update.ranges()
        pars <- getDefaultPars(NPV)
        npv.scen <- do.call(NPV,modifyList(pars,list(scen=input$scen)))
        npv.baseline <- do.call(NPV,modifyList(pars,list(scen=input$baseline)))
        sprintf("NPV of %s: $%0.2f<br/>
NPV of %s: $%0.2f<br/>
difference: $%0.2f",
                input$scen,npv.scen,
                input$baseline,npv.baseline,
                npv.scen-npv.baseline
                )
    })

    output$uni_isproblem <- renderText({
        sprintf("Given this crossover point exists, can we conclude that %s should be abandoned? Why?",input$scen)
    })
    output$uni_isproblem2 <- renderText({
        sprintf("Given this crossover point exists, can we conclude that %s should be abandoned? Why?",input$scen)
    })

    output$bi_selected_var1 <- renderText({return(input$bi_var1)})
    output$bi_selected_var2 <- renderText({return(input$bi_var2)})

################################################################################
    output$more_results <- renderTable({
        input$more_update
        active_vars <- isolate(input$more)
        if(length(active_vars)==0) stop("Select variables to use")

        if(isolate(input$manyvar_method)=="lynchpin"){
            cat(" Using lynchpin method\n")

            breakeven_factor <- isolate(input$more_bkeven)
            cat("Breakeven factor: ",breakeven_factor,"\n",sep="",file=stderr())
            active_vars<- setdiff(active_vars,breakeven_factor)
            cat("Other factors:",active_vars,"\n",sep=" ",file=stderr())
            which.all <- match(c(active_vars,breakeven_factor),ranges$Variable)
            which.active <- match(active_vars,ranges$Variable)
            limit.val <- sapply(c(active_vars,breakeven_factor),function(v) isolate(input[[sprintf("more_slider_%s",v)]]))
            print(limit.val)
            start.pos <- ranges$Modeled[which.all]

            ## Optimise
            net.environmental.cost <-
                createBreakevenFun(isolate(input$scen),
                                   isolate(input$baseline),
                                   breakeven_factor,
                                   ranges[which.active,])

            get.normalised <- function(x)
                ifelse(abs(x-start.pos)<1e-5,0,
                       ifelse(x>start.pos,
                              (x-start.pos)/(limit.val[2,]-start.pos),
                              (start.pos-x)/(start.pos-limit.val[1,])
                              ))

            ## minimise the complement of the fuzzy membership function
            ## i.e. maximise the fuzzy membership function
            f <- function(x){
                tryCatch(y <- net.environmental.cost(x),error=function(e) browser())
                x <- c(x,y)
                ##updateTextInput(session,"more_status",as.character(max(get.normalised(x)))) ##TODO: doesn't seem to work?
                max(get.normalised(x))
            }

            if(require("compiler")){
                net.environmental.cost <- cmpfun(net.environmental.cost)
                get.normalised <- cmpfun(get.normalised)
                f <- cmpfun(f)
            }

            ##FIXME poorly identified solution, significant parameter interactions, weak sensitivity, very small part of parameter space
            ## optim L-BFGS-B fails
            ## Can't use barrier function with non-gradient-based technique, e.g. stochasticity of SCEoptim -> end up just at Inf
            st <- proc.time()
            opt <- SCEoptim(f,ranges$Modeled[which.active], ##init.pos,
                            ##lower=pmin(limit.val[1:4],start.pos[1:4])*0.99,
                            ##upper=pmax(limit.val[1:4],start.pos[1:4])*1.01,
                            lower=ranges$Min[which.active],
                            upper=ranges$Max[which.active],
                            control=list(ncomplex=20,trace=1) #,returnpop=TRUE)
                            )
            print(proc.time()-st)
            print(str(opt))
            normalised <- get.normalised(c(opt$par,net.environmental.cost(opt$par)))
            print(normalised)
            ## print(which.max(normalised))
            ## net.environmental.cost2 <- createFun(isolate(input$scen),
            ##                                      isolate(input$baseline),
            ##                                      ranges[which.all,])
            ##print(net.environmental.cost2(c(opt$par,net.environmental.cost(opt$par)))) ##should be zero
            df <- isolate(uni.bkevenf())
            return(cbind(df[which.all,c("Variable","Min","Max","break.","Modeled")],
                         ##limit.val,
                         ##pmin(limit.val,start.pos),pmax(limit.val,start.pos),
                         break.even=c(opt$par,net.environmental.cost(opt$par)),
                         change=c(opt$par,net.environmental.cost(opt$par))-start.pos,
                         level.of.concern=(1-normalised)*100
                         ))
        } else if(isolate(input$manyvar_method)=="equiconcern"){
            cat(" Using equiconcern method\n")
            which.active <- match(active_vars,ranges$Variable)
            temp.ranges <- isolate(limits())[which.active,]
            temp.ranges$Min <- temp.ranges$X1
            temp.ranges$Max <- temp.ranges$X2
            ##print(temp.ranges)
            res <- crossoverEquiconcern(isolate(input$scen),
                                        isolate(input$baseline),
                                        temp.ranges)
            df <- isolate(uni.bkevenf())
            return(cbind(df[which.active,c("Variable","Min","Max","break.","Modeled")],
                         ##limit.val,
                         ##pmin(limit.val,start.pos),pmax(limit.val,start.pos),
                         break.even=res$values,
                         change=res$values-temp.ranges$Modeled,
                         level.of.concern=res$loc
                         ))
        }
    })

    ################################################################################

    output$save_notes <-
        downloadHandler(filename = function() {
                            format(Sys.time(),format="notes_%Y%m%d_%H%M.csv")
                        },
                        content = function(con) {
                            write.csv(notes, con,row.names=FALSE)
                        })

    output$save_bounds <-
        downloadHandler(filename = function() {
                            format(Sys.time(),format="bounds_%Y%m%d_%H%M.csv")
                        },
                        content = function(con) {
                            write.csv(limits(), con,row.names=FALSE)
                        })

    handle.load.notes <- reactive({
        cat("Handle load notes\n")
        inFile <- input$load_notes
        if (!is.null(inFile)){
            cat(sprintf(" Loading %s\n",inFile$datapath))
            temp.notes<-read.csv(inFile$datapath,stringsAsFactors=FALSE)
            if(all(c("Variable","bestguess","bounds","direction","is.problem") %in% names(temp.notes))) {
                cat("Saved\n")
                notes<<-temp.notes
            } else {
                cat("Notes csv not in right format\n")
            }
        }
    })

})#shinyServer
