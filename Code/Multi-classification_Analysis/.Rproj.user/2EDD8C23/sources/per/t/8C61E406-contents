source('data.R')
source('dependencies.R')

########################
v_box <- 
    fluidRow(offset = 0, style='padding:0px;left: 0re'
        ,column(
            width = 4, style='padding:0px;',align="center",
            div(id = 'lf1', style = "top: 0rem;padding:0px;margin: 0px;",
                div(style = "width: 430px;padding:0px",
                    tabBox(width = 12,id = "tabset1", height = "678px",
                    tabPanel(div(icon("file"), "Course Year",
                                 style = "top: 0rem;font-size:12px;width: 366px;padding:0px;height: 15px"),
                             div(
                                 highchartOutput('line_plot_course', width = 380, height = 250)%>% withLoader(type = "html", loader = "loader6")
                                 ,style = "top: 1rem;padding:1px;margin: 4; align:center"),
                             div(
                                 br(),br(),
                                 DT::dataTableOutput("time_table_course", width = 380)
                                 , style = "font-size:85%"
                             ),
                             div(
                                 style = "position: absolute; left: 20px; bottom: 2em;",
                                 dropdown(
                                     downloadButton(outputId = "d0", label = "Download table"),
                                     size = "xs",
                                     icon = icon("download", class = "opt"),
                                     up = TRUE
                                 )
                             )

                    ),
                    tabPanel(div(icon("calendar"), "Publication Year",
                                 style = "top: 0rem;font-size:12px;width: 366px;padding:0px;height: 15px"),
                             div(
                                 highchartOutput('line_plot_pub', width = 380, height = 250)%>% withLoader(type = "html", loader = "loader6")
                             ,style = "top: 1rem;padding:1px;margin: 4px; align:center"),

                             div(
                                 br(),br(),
                                 DT::dataTableOutput("time_table_pub", width = 380)
                                 , style = "font-size:85%"
                             ),
                             
                             div(
                                 style = "position: absolute; left: 20px; bottom: 2em;",
                                 dropdown(
                                     downloadButton(outputId = "d1", label = "Download table"),
                                     size = "xs",
                                     icon = icon("download", class = "opt"),
                                     up = TRUE
                                 )
                             ),
                             
                    )
            )
                )
            )
        )
        , column(
            width = 4,  style = "padding:0px;",align="center",
            div(id = 'lf2', style = "top: 0rem;padding:0px;margin: 0px;",
                div(style = "width: 430px;padding:1px",
                    tabBox(width = 12,id = "tabset2", height = "680px",
                           tabPanel(div(icon("diamond"), "Word Cloud",
                                        style = "top: 0rem;font-size:12px;width: 366px;padding:0px;height: 15px"),
                                        div(
                                            wordcloud2Output('word_cloud_scientific', width = 380, height = 243)%>% withLoader(type = "html", loader = "loader6")
                                            ,style = "top: 0rem;padding:1px;margin: 4; align:center"
                                        ),
                                        div(
                                            br(),br(),br(),
                                            DT::dataTableOutput("times_table_sci", width = 380)
                                            , style = "font-size:85%"
                                    ),
                                    div(
                                        style = "position: absolute; left: 20px; bottom: 2em;",
                                        dropdown(
                                            downloadButton(outputId = "d2", label = "Download table"),
                                            size = "xs",
                                            icon = icon("download", class = "opt"),
                                            up = TRUE
                                        )
                                    )
                                    
                           ),
                            tabPanel(div(icon("search-plus"), "Distribution",
                                         style = "top: 0rem;font-size:12px;width: 366px;padding:0px;height: 15px"),
                                         div(  radioButtons("rb", "",
                                                      inline = TRUE,
                                                      choiceNames = list(
                                                          div(icon("file"), HTML("<p style='color:black;'><font size='1'><b>Course Year</b></p>"),
                                                              style = "top: 0rem;padding:0px;margin:0; align:right;font-size:12px;"),
                                                          div(icon("calendar"), HTML("<p style='color:black;'><font size='1'><b>Publication Year</b></p>"),
                                                              style = "top: 0rem;padding:0px;margin:0; align:right;font-size:12px;")
                                                      ),
                                                      selected = "Course Year",
                                                      choiceValues = list(
                                                          "Course Year", "Publication Year"
                                                      )
                                                 ),
                                             style = "top: 1rem;width: 366px;font-size:12px;padding:0px;"
                                             ),
                                         div(
                                             plotOutput('time_series_plot_sci', width = 380, height = 180)%>% withLoader(type = "html", loader = "loader6")
                                             ,style = "top: 0rem;padding:0px;margin:0; align:center"
                                         ),
                                         div(br(),br(),
                                     DT::dataTableOutput("times_table_sci_2", width = 380)
                                     , style = "font-size:115%"
                                     )
                            )
                    )))
        ),
        column(
            width = 4, style = "padding:0px;",align="center",
            div(id = 'lf3', style = "top: 0rem;padding:0px;margin: 0;",
                div(style = "width: 430px;padding:1px",
            tabBox( width = 12,id = "tabset3", height = "682px",
                    tabPanel(div(icon("diamond"), "Word Cloud",
                                 style = "top: 0rem;font-size:12px;width: 366px;padding:0px;height: 15px"),
                             fluidRow(
                             div(
                                 wordcloud2Output('word_cloud_concept', width = 380, height = 266)
                                 ,style = "top: 1rem;padding:1px;margin: 4px; align:center"
                             ),
                             div(
                                 br(),br(),
                                 DT::dataTableOutput("times_table_con", width = 350)
                                 , style = "font-size:110%"
                             ),
                             div(
                                 style = "position: absolute; left: 20px; bottom: 2em;",
                                 dropdown(
                                     downloadButton(outputId = "d3", label = "Download table"),
                                     size = "xs",
                                     icon = icon("download", class = "opt"),
                                     up = TRUE
                                 )
                             )
                             )

                    ),
                    tabPanel(div(icon("search-plus"), "Distribution",
                                 style = "top: 0rem;font-size:12px;width: 366px;padding:0px;height: 15px"),
                     # h4("Distribution of scientific fields",align = 'center'),
                     div(radioButtons("rb1", "",
                                      inline = TRUE,
                                      choiceNames = list(
                                          div(icon("file"), HTML("<p style='color:black;'><font size='1'><b>Course Year</b></p>"),
                                              style = "top: 0rem;padding:0px;margin:0; align:right;font-size:12px;"),
                                          div(icon("calendar"), HTML("<p style='color:black;'><font size='1'><b>Publication Year</b></p>"),
                                              style = "top: 0rem;padding:0px;margin:0; align:right;font-size:12px;")
                                      ),selected = "Course Year",
                                      choiceValues = list(
                                          "Course Year", "Publication Year"
                                      )),
                         style = "top: 1rem;width: 366px;font-size:12px;padding:0px;"
                     ),
                     div(
                         plotOutput('rader_plot_sci', width = 380, height = 180)%>% withLoader(type = "html", loader = "loader6")
                         ,style = "top: 0rem;padding:0px;margin:0; align:center"
                     ),
                    div(br(),br(),
                    DT::dataTableOutput("sci_table", width = 350)
                    , style = "font-size:115%"
                    ),
                    div(
                        style = "position: absolute; left: 20px; bottom: 4em;",
                        dropdown(
                            downloadButton(outputId = "d4", label = "Download table"),
                            downloadButton(outputId = "d44", label = "Download table"),
                            size = "xs",
                            icon = icon("download", class = "opt"),
                            up = TRUE
                        )
                    )
                    )
            )))
        )
    )
 

# sidebar ###############################################################
sidebar <- dashboardSidebar(
    
    width = 220,
    introjsUI(),
    
    
    
    # put introduction in the front
    introBox(data.step = 1, id = "tabs",data.intro = intro$text[1], #  intro tour
             div(class = "inlay", style = "height:15px;width:100%;background-color: #fef3e2;"),
             
    
    sidebarMenu(
        id="tabs",
        
        #div(class = "inlay", style = "height:15px;width:100%;background-color: #fef3e2"),
        menuItem("About", tabName = "About", icon = icon("globe"), selected = TRUE ),
        menuItem("SearchingEngine",tabName="SearchingEngine",icon=icon("database")),
        # menuItem("Overview", tabName = "Overview", icon = icon("th")),
        menuItem("Visualization",tabName="Visualization", icon=icon("database")),
        menuItem("Reference",tabName="tab_Ref",icon=icon("globe"))
        )
    )
    
)
    


# body###################################################################
body <- dashboardBody(
    
# add external CSS
    
tags$head(
    
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    ### Styles 
    tags$style(HTML(".small-box {height: 400px}")),
    tags$style(HTML(".tab-content { padding-left: 0px; padding-right: 0px; padding-top:0px; padding-down: 0px}")),
    
    
    ## modify the dashboard's skin color
    tags$style(HTML('
                       /* logo */
                       .skin-yellow-light .main-header .logo {
                       background-color: #f39c12;
                       }
                       .nav-tabs-custom .nav-tabs li.active {
                           border-top-color:  #f39c12;
                       }"
                       "

                    .box.box-solid.box-warning>.box-header {
                        background: #fef3e2;
                        background-color: #f39c12;
                        skin:  #fef3e2
                    }

                    .box.box-solid.box-warning{
                        border-bottom-color: #f39c12;
                        border-left-color: #f39c12;
                        border-right-color: #f39c12;
                        border-top-color: #f39c12;
                        background: #fef3e2;
                    }

                    "
                       /* logo when hovered */
                       .skin-yellow-light .main-header .logo:hover {
                       background-color: #e77d13;
                       }
                       # /* navbar (rest of the header) */
                       # .skin-yellow-light .main-header .navbar {
                       # background-color: #f7da9c;
                       # }
                       /* active selected tab in the sidebarmenu */
                       .skin-yellow-light .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #f39c12;
                       }
                       .sidebar .logo:hover{
                       background-color: #f39c12
                       }
                       
                       /* main sidebar */
                       .skin-yellow-light .main-sidebar {
                       background-color:  #fef3e2;
                    }
                       
                       /* other links in the sidebarmenu */
                       .skin-yellow-light .main-sidebar .sidebar .sidebar-menu a{
                        background-color: #fef3e2;
                        color: #000000;
                       }

                        /* other links in the sidebarmenu when hovered */
                       .skin-yellow-light .main-sidebar .sidebar .sidebar-menu a:hover{
                        background-color: #f39c12;
                                }
                       
                       /* body */
                       .content-wrapper, .right-side {
                        background-color: #fef3e2;
                        }
                       ')
    ),
    
    ## modify icon size in the sub side bar menu
    tags$style(HTML('
                      /* change size of icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview-menu>li>a>.fa {
                      font-size: 13px;
                      }
                      .sidebar .sidebar-menu .treeview-menu>li>a>.glyphicon {
                      font-size: 13px;
                      }
                      /* Hide icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview>a>.fa-angle-left {
                      display: none;
                      } 
                      '
    )
    ),
    
    tags$style( HTML("hr {border-top: 1px solid #000000;}") ),
    
    
    ## to not show error message in shiny
    tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
    tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
    
    ## heand dropdown menu size
    #tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu { width:100px;}'))
    tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu { width:10px; font-size:10px; padding:1px; margin:1px;}')),
    tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > h4 {width:0px; font-size:0px; padding:0px; margin:0px;}')),
    tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > p {width:0px; font-size:0px; padding:0px; margin:0px;}')),
    
    
    
    # custom 
    tags$style(HTML('#b12{width:1200px; font-size:13px; padding:0px; margin:0px;align: center}')),
    tags$style(HTML('#title11{width:1200px; font-size:9px; padding:0px; margin:0px;align: center}')),
    # tags$style( HTML("#lf1{width:400px; height:100px; font-size:12px; padding:0px; margin:0px;align: center}") ),
    # tags$style( HTML("#tabset1{width:400px; height:100px; font-size:12px; padding:0px; margin:0px;align: center}") ),
    # tags$style( HTML("#tabset2{width:400px; height:100px; font-size:12px; padding:0px; margin:0px;align: center}") ),
    # tags$style( HTML("#tabset2{width:400px; height:100px; font-size:12px; padding:0px; margin:0px;align: center}") ),
    tags$style(HTML(".fa{font-size: 12px;}"))
),
useShinyjs(),
introjsUI(),


###########################################################   
# TODO: Layout
# tab items
tabItems(
    ####################### About #####################
    
    ### Overview
    tabItem("About",
            fluidRow(
                column(h2(paste0("Open Statistical Education News Resources"),align = 'center'),
                       width = 12),
                    column(
                        
                        p(
                            # Introduction of this project
                            "The statistical educational news resource project is an open portal
                            which target users include both educators and students. This portal 
                            could be used in the classroom to fetch real world examples which is correlated 
                            to some ", 
                          # Purpose of this project!
                          strong("statistics knowledge"), 
                          ".",
                          style="text-align:justify;color:black;background-color:Moccasin;padding:15px;border-radius:10px"),
                        width=12),
                
                div(id = 'bd1', height = 1, style = 'padding: 0px; margin: 0px',
                    box(id = 'b12',status = "warning", solidHeader = TRUE, title = div(id = 'title11', h3(paste0("Data Collection Process")),align = 'center', style = 'border-radius:4px; padding: 0px; height: 4px; text-align:center'), width = 12,
                        height="400px",
                column(h4(strong("How do we collect the data?"),align = 'left'),
                 p("The news we collected were used by professor Nancy Reid from University of Toronto in both basic
                    and advanced statistics courses. The data used in this application are publicly available on the pages of many",
                 
                   em("Online News Portal"),
                   
                   "such as The Globe & Mail and The New York Times, etc. Not only the basic course, 
                   such as course code and course date, and news information such as Author, Publication date have been automatically gathered, but also statistics concepts 
                    and summaries have been manually filled in the spreadsheet.",
                style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                   width = 12),
    
             column(h4(strong("What We have done so far?"),align = 'left'),
                    div("We have completed taskes including",
                    # p("We have completed taskes such as", strong('Database Construction'), ',', strong('Descriptive Analysis'), 
                    #   ',', strong('Natural Language Process--Text Mining'), ',', strong('Concept Dictionary'),
                    # 'and',strong('Website Building') ,".", 
                    tags$ul(
                        tags$li("Database Construction"), 
                        tags$li("Descriptive Analysis"), 
                        tags$li("Natural Language Process--Text Mining"),
                        tags$li("Concept Dictionary"),
                        tags$li("Website Building")
                    ),
                      style="text-align:justify;color:black;background-color:papayawhip;padding:3px;border-radius:7px"),
                       width = 12),
            
             column(h4(strong("What problems we have met?"),align = 'left'),
                       p("Text auto summary is not accurate, and nlp result also have low accuracy that we 
                         have little confidence to believe its result.", 
                       style="text-align:justify;color:black;padding:5px;border-radius:10px;background-color:papayawhip"),
                       width = 12)
             
                    ),
             
             column(width = 12, br()),
             
             box(id = 'b12',status = "warning", solidHeader = TRUE,title = div(id = 'title11', style="text-align:justify;color:black;padding:0px;border-radius:1px; height: 4px",
                 div(style = 'color: white',h3("How to use this platform?"))
                 
                 ,align = 'center'), width = 12,height="50px",
                 column(width = 12, tags$img(src="student.png",width="60px",height="50px"),
                        strong('Students'),
                        'could use the visualization page to track the most popular field based on the evolution of the statistics
                        concepts appeared in the news.'
                       ),
                 column(width = 12, tags$img(src="teacher.png",width="50px",height="35px"),
                        strong('Instructors') ,
                        'could use the searching engine to find the news related to their teaching topics 
                        by simply filling in some key features, such as headlines and statistics concepts.')
             )
            ))
            ),
    
    #########################################
    
    tabItem("SearchingEngine",
              p('Please Fill In The' , strong("Key Words"),  'Here:',
                     style="text-align:left;color:black;padding:0px;border-radius:10px"),
                 div(id = '111',
                     fluidRow(
                         column(10,
                                conditionalPanel("cp0",
                                                 selectizeInput(
                                                     inputId = "select_title",
                                                     label = "Title:",
                                                     choices = c(levels(as.factor(d3$Headline))),
                                                     multiple = TRUE, selected = NULL, width = "1000px"
                                                 )
                                )),
                         column(2,
                                br(),
                                actionButton('btn_reset_cr',
                                             'Reset',
                                             icon = icon('refresh') ),
                                div(
                                    style = "position: absolute; left: 30px; bottom: 4em; bottom: em;",
                                    dropdown(
                                        downloadButton(outputId = "dt", label = "Download table"),
                                        size = "xs",
                                        icon = icon("download", class = "opt"),
                                        up = FALSE
                                    )
                                )
                                
                         ),
                         column(3,
                            conditionalPanel("cp1",
                                             selectizeInput(
                                                 inputId = "select_author",
                                                 label = "Author:",
                                                 choices = c(levels(as.factor(d3$Author))),
                                                 multiple = TRUE, selected = NULL, width = "200px"
                                             )
                             )),
                         column(3,
                                conditionalPanel("cp2",
                                             selectizeInput(
                                                 inputId = "select_sci_field",
                                                 label = "Scientific Field:",
                                                 choices = c(levels(as.factor(d3$Scientific_Field))),
                                                 multiple = TRUE, selected = NULL, width = "200px"
                                             )
                                )),
                         column(3,
                         conditionalPanel("cp3",
                                             selectizeInput(
                                                 inputId = "select_sourse_year",
                                                 label = "Course Year:",
                                                 choices = c(levels(as.factor(d3$label2))),
                                                 multiple = TRUE, selected = NULL, width = "200px"
                                             )
                         )),
                         column(3,
                        conditionalPanel("cp4",
                                             selectizeInput(
                                                 inputId = "select_news_year",
                                                 label = "Publication Year:",
                                                 choices = c(levels(as.factor(d3$label))),
                                                 options = list(
                                                     onInitialize = I(onInitialize)
                                                 ),
                                                 multiple = TRUE, selected = NULL, width = "200px"
                                             )
                        )),
                                          
                            )),
                    hr(style = "border-top: 0.8px solid #000000;"),
            DT::dataTableOutput("show_table")
            ),
    tabItem("Overview",
            h2('overview')
            ),
    tabItem("tab_Ref",
            # h2('Reference')
            h5("  We provide all the", strong("references"), "below:",
               style = "text-align:left;color:black;padding:0px;border-radius:10px"
            ),
            br(),
            fluidRow(width = 12, 
                         box(
                                id = "b12",status = "warning", solidHeader = TRUE,
                                title = div(
                                        id = "title11",
                                        h3(strong(paste0("Code reference"))), align = "left"
                            ), width = 12, height = '200px',
                            column( '1. Rader - shiny project: We were inspired by ',
                                tags$a(href="https://ceefluz.shinyapps.io/radar/?_ga=2.173093091.1539210521.1625842077-1864172558.1622645983", 
                                       "this shiny project"),
                                ' for constructing our own webpage.',
                                br(),
                                '2. Selectize vs. select: We learnt the difference between selectize and select when writing our code of ',
                                tags$a(href="https://shiny.rstudio.com/gallery/selectize-vs-select.html", 
                                       "this shiny webpage."),
                                br(),
                                '3. Sentence Transformers: We learnt the text summarization from ',
                                tags$a(href="https://github.com/UKPLab/sentence-transformers", 
                                       "this github page."),
                                br(),
                                '4. Text Classification - tokenization: We learnt text classification from ',
                                tags$a(href="https://github.com/kk7nc/Text_Classification#tokenization",
                                       "this github page."),
                                br(),
                                width = 12, style = "overflow-y:scroll;max-height: 250px;background-color:papayawhip;border-radius: 10px"
                            )
                        )
                    ),
            br(),
            # fluidRow(box(
            #     id = "b12", title = div(
            #         id = "title11",
            #         h3(strong(paste0("News reference"))), align = "left"
            #     ), width = 12,
            #     column(
            #         p("1. reference 1"), br(),
            #         p("2. reference 2"), br(),
            #         p("3. reference 3"), br(),
            #         p("4. reference 4"), br(),
            #         width = 12, style = "overflow-y:scroll;max-height: 120px;background-color:lavender;border-radius: 10px"
            #     )
            # )),
            # br(),
            fluidRow(
              box(
                id = "b12", title = div(
                    id = "title11",
                    h3(strong(paste0("Other reference"))), align = "left"
                ), width = 12,height = '200px',status = "warning", solidHeader = TRUE,
                
                column('1. Open Case Studies (OCS) project: We are inspired by ',
                    tags$a(href="https://www.opencasestudies.org/", 
                           "this educational webpage"),
                    ' when designing ours.',
                    br(),
                    '2. Cause Resource Library: We are inspired by ',
                    tags$a(href="https://www.causeweb.org/cause/resources/library?field_material_type_tid=104", 
                           "this statistical topic resources library."),
                    br(),
                    width = 12, style = "overflow-y:scroll;max-height: 250px;background-color:papayawhip;border-radius: 10px"
                )
            )
            )
    ),
    tabItem("Visualization",
            div(id = '333',
            fluidRow(
                column(2,
            div(width= 1,
                    br(), column(2,
                                 actionButton('btn_reset_cr_1',
                                              'Reset',
                                              icon = icon('refresh') ,width = "100px")
                    
            ), style = 'padding:0px; margin: 0px; height: 1px; align: right'
            )
            ),
               column(10,
            div(width = 10,height = 1,
              fluidRow(
                    column(2,
                           conditionalPanel("cp11",
                                            selectizeInput(
                                                inputId = "select_author_1",
                                                label = "Author:",
                                                choices = c(levels(as.factor(d3$Author))),
                                                multiple = TRUE, selected = NULL, width = "150px"
                                            )
                           )),
                    column(2,
                           conditionalPanel("cp22",
                                            selectizeInput(
                                                inputId = "select_sci_field_1",
                                                label = "Scientific Field:",
                                                choices = c(levels(as.factor(d3$Scientific_Field))),
                                                multiple = TRUE, selected = NULL, width = "150px"
                                            )
                           )),
                    column(2,
                           conditionalPanel("cp33",
                                            selectizeInput(
                                                inputId = "select_sourse_year_1",
                                                label = "Course Year:",
                                                choices = c(levels(as.factor(d3$label2))),
                                                multiple = TRUE, selected = NULL, width = "150px"
                                            )
                           )),
                    column(2,
                           conditionalPanel("cp44",
                                            selectizeInput(
                                                inputId = "select_news_year_1",
                                                label = "Publication Year:",
                                                choices = c(levels(as.factor(d3$label))),
                                                options = list(
                                                    onInitialize = I(onInitialize)
                                                ),
                                                multiple = TRUE, selected = NULL, width = "150px"
                                            )
                           )),
                    column(2,
                           conditionalPanel("cp55",
                                            selectizeInput(
                                                inputId = "select_quality_1",
                                                label = "Quality:",
                                                choices = c(levels(as.factor(d3$`Quality (NR) (1 best)`))),
                                                options = list(
                                                    onInitialize = I(onInitialize)
                                                ),
                                                multiple = TRUE, selected = NULL, width = "150px"
                                            )
                           )),
                    column(2,
                           conditionalPanel("cp66",
                                            selectizeInput(
                                                inputId = "select_sophistication_1",
                                                label = "Sophistication:",
                                                choices = c(levels(as.factor(d3$`Statistical Sophistication (1 best)`))),
                                                options = list(
                                                    onInitialize = I(onInitialize)
                                                ),
                                                multiple = TRUE, selected = NULL, width = "150px"
                                            )
                           )),
                    ),
            
            ))),
            
            
            hr(style = "border-top: 0.8px solid #000000;padding:3px; margin:10px")
            ,v_box
            ))
            


 )
)





# header ################################################################

header <- dashboardHeader(
        title = span(img(src = "stats.png", height = 35), 'Stats Education'),
        disable = FALSE, 
        titleWidth  = 240,
        dropdownMenuCustom( type = 'message',
                            customSentence = customSentence,
                            messageItem(
                                from = "@ Stats Educational News Web",
                                message =  '',
                                icon = icon("envelope"),
                                href = "https://github.com/RuyiPan/2021-summer-project-teaching-database.git"
                            ),
                            
                            icon = div(icon('comment'),style = 'color: white')
        ),
        dropdownMenuCustom( type = 'message',
                            customSentence = customSentence_share,
                            icon = div(icon("share-alt"),style = 'color: white'),
                            messageItem(
                                from = 'Twitter',
                                message = "",
                                icon = icon("twitter"),
                                href = "https://twitter.com/intent/tweet"
                            ),
                            messageItem(
                                from = 'Pinterest',
                                message = "",
                                icon = icon("pinterest-p"),
                                href = "http://pinterest.com/pin/create/button/"
                            ))
)
       

#function used to define the dashboard
######################################################################
dashboardPage(
    skin = "yellow-light",
    options = list(sidebarExpandOnHover = TRUE),
    sidebar = sidebar,
    body = body,
    header = header
) 