library(shiny)
library(hydromad) ##SCEoptim
library(cost.benefit.breakeven)

radioButtonsTable <-
function (inputId, label, data,choices,selected=NULL)
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
                                               tags$th(name)
                                           }),
                                           tags$th("plot")
                                           )
                                   ),
                        tags$tbody(
                                   lapply(1:nrow(data), function(i) {
                                       tags$tr(
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
    div(style="position:relative",
        div(lapply(1:nrow(data),function(i) span(data[i,],br())),style="position:absolute;width:90%"),
        div(style="position:absolute;width:10%;right:0",
            div(id=inputId,class = "control-group shiny-input-radiogroup",style="position:absolute;width:50%",
                lapply(inputTags,function(x) span(x,br()))),
            div(id=inputId2,class = "control-group shiny-input-radiogroup",style="position:absolute;width:50%;right:0",
                lapply(inputTags2,function(x) span(x,br())))
            ),
        div(style="clear:both;height:500px")
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
        input$update_ranges
        data.frame(ranges,
                   t(sapply(ranges$Variable,function(v) isolate(input[[sprintf("more_slider_%s",v)]])))
                   )
    })

    uni.bkevenf <- reactive({
      update.ranges()
      input$scen
      input$baseline
      uni.bkeven <- univariate.breakeven(ranges,
                                         input$scen,input$baseline)
      perc.to.limit <- get.normalised(uni.bkeven$"break",limits()$Modeled,limits()$X1,limits()$X2)
      uni.bkeven$"% to limit" <- ifelse(is.na(perc.to.limit),"",sprintf("%g%%",round(perc.to.limit,2)*100))
      uni.bkeven$"break" <- round(uni.bkeven$"break",2)
      uni.bkeven
    })

    output$uni_plot_variable<- renderUI({
      update.ranges()
      input$scen
      input$baseline
      uni.bkeven <- uni.bkevenf()
      radioButtonsTable("uni_plot_variable_selected",
                        "Univariate break-even values", uni.bkeven[,c("Variable","Modeled","Min","Max","% to limit","break")],
                        choices=uni.bkeven$Variable
                        )
    })

    output$uni_plot <- renderPlot({
      update.ranges()
      plot.univar(ranges,input$uni_plot_variable_selected,input$scen,input$baseline)
    })

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
      cat("bi_plot\n",file=stderr())
      input$bi_flip
      v1 <- isolate(input$bi_var1)
      v2 <- isolate(input$bi_var2)
      if(NROW(pom)>0) {
        if(input$bi_flip){
          vtemp <- v1
          v1 <- v2
          v2 <- vtemp
        }
        wvars <- match(c(v1,v2),limits()$Variable)
        pom<-pom[!apply(pom,1,function(x) any(is.na(x))),]
        perc.to.limit <- apply(pom,1,function(x) max(get.normalised(x,limits()$Modeled[wvars],limits()$X1[wvars],limits()$X2[wvars])))*100
        library(ggplot2)
        g <- ggplot()+
        geom_point(aes(x=x1,y=x2,color=perc.to.limit),data=data.frame(x1=pom[,v1],x2=pom[,v2],perc.to.limit=perc.to.limit))+
                scale_x_continuous(name=v1)+scale_y_continuous(name=v2)+
                #scale_x_continuous(name=v1,limits=range(c(pom[,v1],ranges[ranges$Variable==v1,"Modeled"]),na.rm=TRUE))+
                #scale_y_continuous(name=v2,limits=range(c(pom[,v2],ranges[ranges$Variable==v2,"Modeled"]),na.rm=TRUE))+
                    scale_colour_gradient(name="% to limit",limits=c(0,100))+
                        geom_point(aes(x=X1,y=X2),color="red",data=data.frame(t(limits()$Modeled[wvars])))+
                            geom_vline(aes(xintercept=x,linetype="Best guess"),data=data.frame(x=as.numeric(limits()[wvars[1],"Modeled"])))+
                                geom_hline(aes(yintercept=x,linetype="Best guess"),data=data.frame(x=as.numeric(limits()[wvars[2],"Modeled"])))+
                                    geom_vline(aes(xintercept=x,linetype="Limits"),data=data.frame(x=as.numeric(limits()[wvars[1],c("X1","X2")])))+
                                        geom_hline(aes(yintercept=x,linetype="Limits"),data=data.frame(x=as.numeric(limits()[wvars[2],c("X1","X2")])))+
                                            geom_segment(aes(x=x,y=y,xend=xend,yend=yend,linetype="Equidistant lines"),data=data.frame(x=limits()$Modeled[wvars[1]],y=limits()$Modeled[wvars[2]],expand.grid(xend=as.numeric(limits()[wvars[1],c("X1","X2")]),yend=as.numeric(limits()[wvars[2],c("X1","X2")]))))+
                                                scale_linetype_manual(name="Lines",values=c("Limits"="solid","Best guess"="dashed","Equidistant lines"="dotted"))


        print(g)
      }
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

    output$more_results <- renderTable({
        input$more_update
        active_vars <- isolate(input$more)
        if(length(active_vars)==0) stop("Select variables to use")
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
        opt <- SCEoptim(f,ranges$Modeled[which.active],##init.pos,
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
        cbind(df[which.all,c("Variable","Min","Max","break.","Modeled")],
              ##limit.val,
              ##pmin(limit.val,start.pos),pmax(limit.val,start.pos),
              break.even=c(opt$par,net.environmental.cost(opt$par)),
              change=c(opt$par,net.environmental.cost(opt$par))-start.pos
              )
    })


})#shinyServer
