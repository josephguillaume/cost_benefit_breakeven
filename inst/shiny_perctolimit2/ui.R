library(shiny)
library(cost.benefit.breakeven)
shinyUI(pageWithSidebar(
                        div(
                      headerPanel('Exploring points of indifference',
                                  singleton(
                                             tags$head(
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
$('#more_toggle_all').on('click',function(evt){
 $('#morez input:checkbox').attr('checked', ! $('#morez input:checkbox').attr('checked'));
 $('#morez input:checkbox').trigger('change');
});
                                                                    })")),
                                                       tags$style(HTML("
.control-label {display:inline-block;}

#bi_btn_update +div {display:inline-block;width:70%}
#bi_n_points+.jslider {display:inline-block;width:80%}
.checkbox[for='bi_flip']{display:inline-block}

.side{float:left;width:32%;}
.main {float:left;margin-left:10px;width:62%;}

.sidespacer {
height:127px; /*ul tabs+label+th-border-well. (37+1+20)+(20+5)+(2+62) - 1  - 19 */
vertical-align: middle;
display: table-cell;
}

#scen{width:100px}
#baseline{width:100px}
                                                        
#more>div{
    display:inline-block;
    width:100%;
    height:55px;
    border-bottom-style: solid;
    border-bottom-width: 1px;
    border-bottom-color: grey;
}
#more>div>.control-label{width:50%;text-align:right;padding-right:10px}
#more>div>.jslider{display:inline-block;width:45%;}

.tab-content{overflow:inherit}

/* RANGES */

#matrix_ranges>tbody>tr{height:56px}
#matrix_ranges>thead>tr{height:62px}

/* UNIVARIATE */

#uni_plot_variable_selected>tbody>tr{
    height:56px;
    border-bottom-style: solid;
    border-bottom-width: 1px;
    border-bottom-color: grey;
    text-align:right;
}
#uni_plot_variable_selected>thead>tr{height:62px}

/* BIVARIATE and MULTIVARIATE*/
.bivspacer {height:33px} /*label+th-first row (see .sidespacer) (20+5)+(2+62)-55 */
.rbtrow{
    height:55px;
    line-height:55px; /* necessary for vertical-align http://stackoverflow.com/questions/9249359/text-vertical-align-in-a-div */
    vertical-align:middle;
    border-bottom-style: solid;
    border-bottom-width: 1px;
    border-bottom-color: grey;
}

"))
                                                       )#head
                                            )#singleton
                                  )    #headerPanel
                            ),##header div
                        div(class="side",
                                ##sidebarPanel(
                                         div(class="sidespacer",
                                             ##TODO: generalise
                                             selectInput("scen","Case 1:",choices=c("base","basin","injection")),
                                             selectInput("baseline"," - Case 2:",choices=c("base","basin","injection"))                                            
                                             ),
                                     {
                                         sliders <- lapply(1:nrow(ranges),function(i) {
                                             ##frac <- MASS:::.rat((ranges$Modeled[i])/(ranges$Max[i]-ranges$Min[i]),max.denominator = 20)$rat
                                             frac <-c(round((ranges$Modeled[i]-ranges$Min[i])/(ranges$Max[i]-ranges$Min[i])*15),15)
                                             ##print(frac)
                                             ticks <- rep("|",frac[2]+1)
                                             ticks[frac[1]+1] <- ranges$Modeled[i]
                                             ##ticks[1] <- ranges$Min[i]
                                             ##ticks[length(ticks)] <- ranges$Max[i]
                                             ##ranges$Modeled[i],
                                             ## TODO: show modeled value and breakeven value on slider?
                                             ## TODO: select which vars to use and which to calculate
                                             ## TODO: update when ranges change - can't use updateSliderInput
                                             sliderInput(sprintf("more_slider_%s",ranges$Variable[i]),ranges$Variable[i],
                                                             min=ranges$Min[i],max=ranges$Max[i],
                                                         value=c(ranges$Min[i],ranges$Max[i]),
                                                         ticks=ticks
                                                         )
                                         })
                                         ##sliders$id="more"
                                         ##sliders$class = "control-group shiny-input-checkboxgroup"
                                         ##do.call(div,sliders)
                                         div(class="well",sliders,
                                             id="more",class="control-group shiny-input-checkboxgroup")
                                       }
                            ##uiOutput("more_sliders"),
                            ##actionButton("update_ranges","Update")
                            ),##sidebarPanel
                        div(class="main",##mainPanel(
                                tabsetPanel(id="tabs",
                                            tabPanel("Change ranges",value="ranges",
                                                     matrixInput("matrix_ranges",
                                                                 "Ranges",
                                                                 ranges[,1:4],
                                                                 types=c("character",rep("numeric",3))),
                                                     actionButton("btn_update_ranges","Update")
                                                     ),
                                            tabPanel("Varying one variable",value="univariate",
                                                     div(style="position:relative",
                                                         ##TODO: instead just set additional classes for both outputs?
                                                         ## uiOutput("uni_plot_variable"),
                                                         ## plotOutput("uni_plot")
                                                         div(uiOutput("uni_plot_variable"),style="position:absolute;width:35%"),
                                                         div(plotOutput("uni_plot"),style="position:absolute;width:63%;right:0;padding-left:2%"),
                                                         div(style="clear:both;height:500px") ##so that they are side by side
                                                         )
                                                     ),
                                            tabPanel("Two variables",value="bivariate",
                                                     div(style="position:relative",
                                                         div(class="bivspacer"),
                                                         div(uiOutput("bi_plot_variable"),style="position:absolute;width:10%"),
                                                         div(plotOutput("bi_plot"),
                                                             actionButton("bi_btn_update","Update"),
                                                             sliderInput("bi_n_points","Number of points: ",min=0,max=1000,value=20,step=1,round=0),
                                                             checkboxInput("bi_flip","Flip axes"),
                                                             style="position:absolute;width:80%;right:0;padding-left:5%"),
                                                         div(style="clear:both;height:500px") ##so that they are side by side
                                                         )
                                                     ## uiOutput("bi_plot_variable"),
                                                     ## plotOutput("bi_plot")
                                                     ),
                                            tabPanel("Many variables",value="more",
                                                     div(style="position:relative",
                                                         div(class="bivspacer",
                                                             span("Toggle all",id="more_toggle_all")
                                                             ),
                                                         div(style="position:absolute;width:15%",
                                                             {
                                                               checks <- lapply(1:nrow(ranges),function(i) {
                                                                 if(ranges$Variable[i] %in% c("net.environmental.cost","pump.cost.dollar.per.ml")){
                                                                   radio <- tags$input(type="radio",name="more_bkeven",
                                                                                       id=paste("more_bkeven", i, sep = ""), value = ranges$Variable[i])
                                                                 } else {
                                                                   radio <- ""
                                                                 }
                                                                 check <- tags$input(type = "checkbox", name = "more",
                                                                                     id = paste("more", i, sep = ""), value = ranges$Variable[i])
                                                                 div(class="rbtrow",
                                                                     check,
                                                                     radio
                                                                     )
                                                               })
                                                               div(id="morez",
                                                                   div(class="rbtrow","Vary? (Lynchpin)"),
                                                                   checks)
                                                             }
                                                             ),
                                                         div(style="position:absolute;width:80%;right:0;padding-left:5%",
                                                             p("Tick checkboxes to select the variables to vary."),
                                                             p("Select radiobutton as lynchpin variable (allows the analysis to function)."),
                                                             actionButton("more_update","Update"),
                                                             br(),
                                                             ##textInput("more_status","",""),
                                                             tableOutput("more_results")
                                                             ),
                                                         div(style="clear:both;height:500px") ##so that they are side by side
                                                         )
                                                     ) ##tabPanel many vars
                                            ) #tabsetPanel
                                )             #mainPanel
                      ))                      #shinyUI
