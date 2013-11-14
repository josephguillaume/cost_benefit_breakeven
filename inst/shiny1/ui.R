library(shiny)
library(cost.benefit.breakeven)
shinyUI(bootstrapPage(
                      headerPanel('Break-even analysis',
                                  singleton(
                                             tags$head(
                                                       tags$style(HTML("
.control-label {display:inline-block;}
.span8 {width:95%}

#bi_btn_update +div {display:inline-block;width:70%}
#bi_n_points+.jslider {display:inline-block;width:80%}
.checkbox[for='bi_flip']{display:inline-block}

#more>div>div{display:inline-block;width:90%}
#more>div>div>.control-label{width:25%;text-align:right}
#more>div>div>.jslider{display:inline-block;width:70%}
")),
                                                       tags$link(rel = 'stylesheet',
                                                                 type = 'text/css',
                                                                 href = '/tableinput/tableinput.css'),
                                                       tags$script(src = '/tableinput/tableinput.js'),
                                                       ##$(\".control-label[for='more_slider_pump.cost.dollar.per.ml']~span.jslider\").toggle()
                                                       ##$(\".control-label[for='more_slider_water.available$supplementary']~span.jslider\").toggle()
                                                       ##$(\"#more input:checkbox\").on('change',function(){if(!$(this).attr('checked')){$('#more_slider_'+
                                                       ##this.value.replace(/\./g,'\\.').replace(/\$/g,'\\$')+'~.jslider').toggle()}})
                                                       tags$script(HTML("
$( document ).ready(function(){
  $('#more input:checkbox').on('change',function(){$('#more_slider_'+this.value.replace(/\\./g,'\\\\.').replace(/\\$/g,'\\\\$')+'~.jslider').toggle($(this).attr('checked')=='checked')});

$('#more_toggle_all').on('click',function(evt){
 $('#more input:checkbox').attr('checked', ! $('#more input:checkbox').attr('checked'));
 $('#more input:checkbox').trigger('change');
});
                                                                    })"))
                                                       )#head
                                            )#singleton
                                  ),    #headerPanel
                      mainPanel(
                                ##TODO: generalise
                                div(
                                    selectInput("scen","Case 1:",choices=c("base","basin","injection")),
                                    selectInput("baseline"," - Case 2:",choices=c("base","basin","injection"))
                                    ),
                                tabsetPanel(id="tabs",
                                            tabPanel("Change ranges",value="ranges",
                                                     matrixInput("matrix_ranges",
                                                                 "Ranges",
                                                                 ranges[,1:4],
                                                                 types=c("character",rep("numeric",3))),
                                                     actionButton("btn_update_ranges","Update")
                                                     ),
                                            tabPanel("Univariate",value="univariate",
                                                     div(style="position:relative",
                                                         ##TODO: instead just set additional classes for both outputs?
                                                         ## uiOutput("uni_plot_variable"),
                                                         ## plotOutput("uni_plot")
                                                         div(uiOutput("uni_plot_variable"),style="position:absolute;width:50%"),
                                                         div(plotOutput("uni_plot"),style="position:absolute;width:50%;right:0"),
                                                         div(style="clear:both;height:500px") ##so that they are side by side
                                                         )
                                                     ),
                                            tabPanel("Bivariate",value="bivariate",
                                                     div(style="position:relative",
                                                         div(uiOutput("bi_plot_variable"),style="position:absolute;width:25%"),
                                                         div(plotOutput("bi_plot"),
                                                             actionButton("bi_btn_update","Update"),
                                                             sliderInput("bi_n_points","Number of points: ",min=0,max=1000,value=20,step=1,round=0),
                                                             checkboxInput("bi_flip","Flip axes"),
                                                             style="position:absolute;width:70%;right:0"),
                                                         div(style="clear:both;height:500px") ##so that they are side by side
                                                         )
                                                     ## uiOutput("bi_plot_variable"),
                                                     ## plotOutput("bi_plot")
                                                     ),
                                            tabPanel("Normalised multivariate",value="more",
                                                 {
                                                     sliders <- lapply(1:nrow(ranges),function(i) {
                                                         ##frac <- MASS:::.rat((ranges$Modeled[i])/(ranges$Max[i]-ranges$Min[i]),max.denominator = 20)$rat
                                                         frac <-c(round((ranges$Modeled[i]-ranges$Min[i])/(ranges$Max[i]-ranges$Min[i])*15),15)
                                                         ##print(frac)
                                                         ticks <- rep("|",frac[2]+1)
                                                         ticks[frac[1]+1] <- ranges$Modeled[i]
                                                         ##ticks[1] <- ranges$Min[i]
                                                         ##ticks[length(ticks)] <- ranges$Max[i]
                                                         if(ranges$Variable[i]=="net.environmental.cost"){
                                                             check <- ""
                                                         } else {
                                                             check <- tags$input(type = "checkbox", name = "more",
                                                                                 id = paste("more", i, sep = ""), value = ranges$Variable[i])
                                                         }
                                                         div(
                                                             ##ranges$Modeled[i],
                                                             ## TODO: show modeled value and breakeven value on slider?
                                                             ## TODO: select which vars to use and which to calculate
                                                             ## TODO: update when ranges change - can't use updateSliderInput
                                                             check,
                                                             sliderInput(sprintf("more_slider_%s",ranges$Variable[i]),ranges$Variable[i],
                                                                         min=ranges$Min[i],max=ranges$Max[i],
                                                                         value=c(ranges$Min[i],ranges$Max[i]),
                                                                         ticks=ticks
                                                                         )
                                                             )
                                                     })
                                                     ##sliders$id="more"
                                                     ##sliders$class = "control-group shiny-input-checkboxgroup"
                                                     ##do.call(div,sliders)
                                                     div(span("Toggle all",id="more_toggle_all"),sliders,
                                                         id="more",class="control-group shiny-input-checkboxgroup")
                                                 },
                                                     ##uiOutput("more_sliders"),
                                                     actionButton("more_update","Update"),
                                                     ##textInput("more_status","",""),
                                                     tableOutput("more_results")
                                                     )
                                            ) #tabsetPanel
                                )             #mainPanel
                      ))                      #shinyUI
