library(shiny)
library(cost.benefit.breakeven)
shinyUI(pageWithSidebar(
                        div(
                      headerPanel('Exploring crossover points',
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

$('#notes_bestguess').on('focus',function(evt){
$('#notes_which').attr('disabled',true);
});
$('#notes_bounds').on('focus',function(evt){
$('#notes_which').attr('disabled',true);
});
$('#btn_save_notes').on('click',function(evt){$
('#notes_which').attr('disabled',false);
});
$('#btn_reset_notes').on('click',function(evt){
$('#notes_which').attr('disabled',false);
});


$('#notes_bestguess1').on('focus',function(evt){
$('#uni_plot_variable_selected input:radio').attr('disabled',true)
});
$('#notes_bounds1').on('focus',function(evt){
$('#uni_plot_variable_selected input:radio').attr('disabled',true)
});
$('#notes_direction1').on('focus',function(evt){
$('#uni_plot_variable_selected input:radio').attr('disabled',true)
});
$('#notes_isproblem1').on('focus',function(evt){
$('#uni_plot_variable_selected input:radio').attr('disabled',true)
});
$('#btn_save_notes1').on('click',function(evt){$
$('#uni_plot_variable_selected input:radio').attr('disabled',false)
});
$('#btn_reset_notes1').on('click',function(evt){
$('#uni_plot_variable_selected input:radio').attr('disabled',false)
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

.highconcern {color:red}
.midconcern {color:orange}
.lowconcern {color:green}

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
                                             selectInput("scen","Case 1:",choices=c("base","basin","injection"),selected="basin"),
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
                                       },
                            div(downloadButton("save_notes","Save notes"),downloadButton("save_bounds","Save bounds"),style="display:inline-block"),p(),
                            fileInput("load_notes","Load notes",accept="")
                            ##fileInput("load_bounds","Load bounds",accept=""),

                            ##uiOutput("more_sliders"),
                            ##actionButton("update_ranges","Update")
                            ),##sidebarPanel
                        div(class="main",##mainPanel(
                                tabsetPanel(id="tabs",
                                            tabPanel("Base analysis",value="ranges",
                                                     div(style="position:relative",
                                                         div(style="position:absolute;width:65%",
                                                             matrixInput("matrix_ranges",
                                                                         "Ranges",
                                                                         ranges[,1:4],
                                                                         types=c("character",rep("numeric",3))),
                                                             actionButton("btn_update_ranges","Update")
                                                             ),
                                                         div(style="position:absolute;width:35%;right:0;padding-left:5%",
                                                             p(htmlOutput("bestguess_NPV")),
                                                             div(class="well",
                                                              selectInput("notes_which","Variable",ranges$Variable,ranges$Variable[1]),br(),
                                                              tags$b("Why is the best guess selected:"),br(),
                                                              tags$textarea(id="notes_bestguess",rows=3,cols=40),br(),
                                                              tags$b("What influences the bounds:"),br(),
tags$li("Are the exploration bounds too narrow? Could other values be of concern?"),
tags$li("Are the minimum and maximum values too wide? Are any values definitely not of concern?"),
                                                              tags$textarea(id="notes_bounds",rows=3,cols=40),
                                                              actionButton("btn_save_notes","Save notes"),
                                                              actionButton("btn_reset_notes","Reset notes")
                                                                )
                                                            )
                                                         )
                                                     ),
                                            tabPanel("Varying one variable",value="univariate",
                                                     div(style="position:relative",
                                                         ##TODO: instead just set additional classes for both outputs?
                                                         ## uiOutput("uni_plot_variable"),
                                                         ## plotOutput("uni_plot")
                                                         div(uiOutput("uni_plot_variable"),style="position:absolute;width:53%"),
                                                         div(style="position:absolute;width:45%;right:0;padding-left:2%",
                                                             plotOutput("uni_plot"),
                                                             div(class="well",
                                                              tags$b("Why is the best guess selected:"),br(),
                                                              tags$textarea(id="notes_bestguess1",rows=3,cols=40),br(),
                                                              tags$b("What influences the bounds:"),br(),
                                                              tags$li("Is this crossover point of concern? Why/why not?"),
                                                              tags$li("What should the bounds be?"),
                                                              tags$li("If there is no crossover point, is this expected? why?"),
                                                              tags$textarea(id="notes_bounds1",rows=3,cols=40),br(),
                                                              tags$b("What direction of change is expected? Does it match the model results?"),br(),
                                                              tags$textarea(id="notes_direction1",rows=3,cols=40),br(),
                                                              tags$b(textOutput("uni_isproblem")),
                                                              tags$li("What further analysis might show the crossover point is not of concern?"),
                                                              tags$li("What can we do to avoid this crossover point?"),
                                                              tags$textarea(id="notes_isproblem1",rows=3,cols=40),br(),
                                                              actionButton("btn_save_notes1","Save notes"),
                                                              actionButton("btn_reset_notes1","Reset notes")
                                                                )
                                                         ),
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
                                                             checkboxInput("bi_flip","Flip axes"),p(),
                                                             div(class="well",
                                                              tags$b("Why is the best guess selected:"),br(),
                                                              div(style="display:inline-block",textOutput("bi_selected_var1"),br(),
                                                              tags$textarea(id="notes_bestguess2a",rows=3,cols=40)),
                                                              div(style="display:inline-block",textOutput("bi_selected_var2"),br(),
                                                              tags$textarea(id="notes_bestguess2b",rows=3,cols=40)),br(),
                                                              tags$b("What influences the bounds:"),br(),
                                                              tags$li("Is this crossover point of concern? Why/why not?"),
                                                              tags$li("What should the bounds be?"),
                                                              tags$li("If there is no crossover point, is this expected? why?"),
                                                              tags$textarea(id="notes_bounds2a",rows=3,cols=40),
                                                              tags$textarea(id="notes_bounds2b",rows=3,cols=40),br(),
                                                              tags$b("What direction of change is expected? Does it match the model results?"),br(),
                                                              tags$textarea(id="notes_direction2a",rows=3,cols=40),
                                                              tags$textarea(id="notes_direction2b",rows=3,cols=40),br(),
                                                              tags$b(textOutput("uni_isproblem2")),
                                                              tags$li("What further analysis might show the crossover point is not of concern?"),
                                                              tags$li("What can we do to avoid this crossover point?"),
                                                              tags$textarea(id="notes_isproblem2a",rows=3,cols=40),
                                                              tags$textarea(id="notes_isproblem2b",rows=3,cols=40),br(),
                                                              actionButton("btn_save_notes2","Save notes"),
                                                              actionButton("btn_reset_notes2","Reset notes")
                                                                ),
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
