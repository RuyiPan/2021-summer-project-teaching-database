source('data.R')
source('dependencies.R')

# increase the uploading file size limit to 2000M, now our upload is not just about movebank file, it also include the saved data.
options(shiny.maxRequestSize = 2000*1024^2)
VERIFY_DATA_SYNC <- FALSE

server <- function(input, output, session){
    
    ###################### Event ####################
    observeEvent("", {
        showModal(modalDialog(
            includeHTML("intro_text.html"),
            easyClose = TRUE,
            icon = icon("angle-right"),
            footer = tagList(
                actionButton(inputId = "intro", label = "INTRODUCTION TOUR", icon = icon("info-circle"))
            )
        ))
    })
  
  observeEvent(input$btn_reset_cr, {
    # session$reload()
    shinyjs::reset("111")
  })
  
  observeEvent(input$btn_reset_cr_1, {
    # session$reload()
    shinyjs::reset("333")
  })
  
  observeEvent(input$Ref, {
   
  })
  
    
    observeEvent(input$intro,{
        removeModal()
    })
    
    # show intro tour
    observeEvent(input$intro,
                 introjs(session, options = list("nextLabel" = "Continue",
                                                 "prevLabel" = "Previous",
                                                 "doneLabel" = "Alright. Let's go"))
    )
    
    observeEvent(input$btn_reset_cr, {
        updateButton(
            session, 
            inputId = "btn_reset_cr", 
            label = 'Reset',
            icon = icon('refresh') )
        updateButton(
            session, 
            inputId = "select_title")
    })
    
    
    download_box <- function(exportname, tib) {
      downloadHandler(
        filename = function() {
          paste(exportname, Sys.Date(), '.csv', sep = '')
        },
        content = function(file){
            write.csv(tib,file)
          
        }
      )
    }
    
    
    #### download ####
    output$dt <- download_box("course_year", show_table())
    output$d0 <- download_box("course_year", dd0())
    output$d1 <- download_box("publication_year", dd1())
    output$d2 <- download_box("times_table_sci", tib_sci())
    output$d3 <- download_box("times_table_con", tib_con())
    output$d4 <-  download_box("sci_table_publication",
                                tib_usable_split_con_pub())
    output$d44 <- download_box("sci_table_course",
                               tib_usable_split_con_cou())
    
    
    ###################### Searching Engine ######################
    
    set_base <- reactive({
        hea = input$select_title
        aut = input$select_author
        sci = input$select_sci_field
        syr = input$select_sourse_year
        nyr = input$select_news_year
        q = input$select_quality
        s = input$select_sophistication
         d3 %>%
            filter( if(!is.null(hea)) Headline %in% hea else Headline %in% Headline)%>%
            filter( if(!is.null(syr)) label %in% syr else label %in% label)%>%
            filter( if(!is.null(nyr)) label2 %in%  nyr else label2 %in% label2 )%>%
            filter( if(!is.null(sci)) Scientific_Field %in% sci else Scientific_Field %in% Scientific_Field)%>%
            filter( if(!is.null(aut)) Author %in% aut else Author %in% Author) %>%
            filter( if(!is.null(s)) s %in% `Quality (NR) (1 best)` else `Quality (NR) (1 best)` %in% `Quality (NR) (1 best)`)%>%
            filter( if(!is.null(q)) q %in% `Statistical Sophistication (1 best)` else `Statistical Sophistication (1 best)` %in% `Statistical Sophistication (1 best)`) 
    })
    
    set_base2 <- reactive({
        aut = input$select_author_1
        sci = input$select_sci_field_1
        syr = input$select_sourse_year_1
        nyr = input$select_news_year_1
        q = input$select_quality_1
        s = input$select_sophistication_1
        d3 %>%
            filter( if(!is.null(syr)) label %in% syr else label %in% label)%>%
            filter( if(!is.null(nyr)) label2 %in%  nyr else label2 %in% label2 )%>%
            filter( if(!is.null(sci)) Scientific_Field %in% sci else Scientific_Field %in% Scientific_Field)%>%
            filter( if(!is.null(aut)) Author %in% aut else Author %in% Author) %>%
          filter( if(!is.null(s)) s %in% `Quality (NR) (1 best)` else `Quality (NR) (1 best)` %in% `Quality (NR) (1 best)`)%>%
          filter( if(!is.null(q)) q %in% `Statistical Sophistication (1 best)` else `Statistical Sophistication (1 best)` %in% `Statistical Sophistication (1 best)`)
        
    })

    
    table_data <- reactive({
        set_base()%>%
        dplyr::select(-c(label, label2, group, Course_Date, Publication_Date))
    })
    
    tib <- reactive({
         set_base2() %>%
            group_by(Publication_Year)%>%
            count() %>%
            ungroup()%>%
            mutate(mean = mean(n))
    })
    
    tib2 <- reactive({
        set_base2() %>%
            group_by(Course_Year)%>%
            count() %>%
            ungroup()%>%
            mutate(mean = mean(n))
    })
    
    dat_split_sci <- reactive({
        set_base2() %>%
            dplyr::select(Id, Course_Date, Course_Year, 
                   Publication_Date,Publication_Year,
                   Scientific_Field)%>%
            separate_rows(Scientific_Field, sep = ", ")
    })
    
    
    dat_usable_split_sci <- reactive({
        dat_split_sci()%>%
        ungroup()%>%
        group_by(Id) %>%  slice(1)%>%
        arrange(Id)%>%
        mutate(group = ifelse(Publication_Year < 2006, 1, 
                              ifelse(Publication_Year < 2011, 2,
                                     ifelse(Publication_Year < 2016, 3, 4)
                              )
        )
        )%>%
        mutate(label= ifelse(Publication_Year < 2006, '<= 2005', 
                             ifelse(Publication_Year < 2011,'2006-2010',
                                    ifelse(Publication_Year < 2016, '2011-2015',
                                           '2015+')
                             )
        )
        )%>%
        mutate(label2 = ifelse(Course_Year < 2006, '<= 2005', 
                               ifelse(Course_Year  < 2011,'2006-2010',
                                      ifelse(Course_Year  < 2016, '2011-2015',
                                             '2015+')
                               )
        )
        )
    })
    
    tib_sci_pub <- reactive({
        dat_usable_split_sci()%>%
            ungroup()%>%
            group_by(label, Scientific_Field, group)%>%
            summarise(count_Publication = n())%>%
            arrange(-desc(label)) %>%
            rename(count = count_Publication, word = Scientific_Field)%>%
            ungroup %>%
            group_by(factor(label))%>%
            mutate(s1 = sum(count))%>%
            mutate(group = as.factor(group))%>%
            mutate(label = as.factor(paste0(label, ':  ', s1, ' News')))%>%
            ungroup %>%
            arrange(-desc(group))
    })
    
    tib_sci_course <- reactive({
        dat_usable_split_sci()%>%
            ungroup()%>%
            group_by(label2, Scientific_Field)%>%
            summarise(count_Publication = n())%>%
            arrange(-desc(label2)) %>%
            rename(count = count_Publication, word = Scientific_Field)%>%
            ungroup %>%
            group_by(label2)%>%
            mutate(s1 = sum(count))%>%
            mutate(label2 = paste0(label2, ':  ', s1, ' News'))%>%
            ungroup 
    })
    
    
    dat_split_con <- reactive({
        set_base2()%>%
            separate_rows(Specific_Concept, sep = ",")%>%
            mutate(Specific_Concept = str_trim(Specific_Concept, side = c("both", "left", "right")))
    })
    
    dat_usable_split_con_pub <- reactive({
        dat_split_con()%>%
            ungroup()%>%
            mutate(Specific_Concept = str_to_title(Specific_Concept))%>%
            group_by(Specific_Concept,label)%>%
            summarise(count = n())%>%
            ungroup()%>%
            group_by(label)%>%
            arrange(label)%>%
            mutate(Specific_Concept = ifelse(count <= 1, 'others',
                                             Specific_Concept))%>%
            group_by(Specific_Concept,label)%>%
            summarise(count = sum(count))%>%
            ungroup()%>%
            group_by(label)%>%
            arrange(-desc(label))%>%
            mutate(rate = round(count/sum(count) * 100,2))%>%
            ungroup()%>%
            group_by(label)%>%
            mutate(label = paste0(label, ': ', sum(count), 'News'))%>%
            ungroup()%>%
            group_by(label, Specific_Concept)%>%
            rename(word = Specific_Concept)%>%
            ungroup()%>%
            arrange(desc(count))%>%
            rename('rate(%)' = 'rate')
    })
    
    tib_usable_split_con_pub <- reactive({
      dat_split_con()%>%
        ungroup()%>%
        mutate(Specific_Concept = str_to_title(Specific_Concept))%>%
        group_by(Specific_Concept)%>%
        summarise(count = n())%>%
        ungroup()%>%
        mutate(Specific_Concept = ifelse(count <= 1, 'others',
                                         Specific_Concept))%>%
        group_by(Specific_Concept)%>%
        summarise(count = sum(count))%>%
        arrange(-desc(count))%>%
        mutate(rate = round(count/sum(count) * 100,2))%>%
        arrange(desc(count))%>%
        rename(word = Specific_Concept)%>%
        rename('rate(%)' = 'rate')
    })
    
    dat_usable_split_con_cou <- reactive({
        dat_split_con() %>%
            group_by(Specific_Concept,label2)%>%
            summarise(count = n())%>%
            ungroup()%>%
            group_by(label2)%>%
            arrange(label2)%>%
            mutate(Specific_Concept = ifelse(count <= 1, 'others',
                                             Specific_Concept))%>%
            group_by(Specific_Concept,label2)%>%
            summarise(count = sum(count))%>%
            ungroup()%>%
            group_by(label2)%>%
            arrange(-desc(label2))%>%
            mutate(rate = round(count/sum(count) * 100,2))%>%
            ungroup()%>%
            group_by(label2)%>%
            mutate(label2 = paste0(label2, ': ', sum(count), 'News'))%>%
            ungroup()%>%
            group_by(label2, Specific_Concept)%>%
            rename(word = Specific_Concept)%>%
            ungroup()%>%
            arrange(count)%>%
            rename('rate(%)' = 'rate')
    })
    
    tib_usable_split_con_cou <- reactive({
      dat_split_con() %>%
        group_by(Specific_Concept)%>%
        summarise(count = n())%>%
        ungroup()%>%
        mutate(Specific_Concept = ifelse(count <= 1, 'others',
                                         Specific_Concept))%>%
        ungroup()%>%
        group_by(Specific_Concept) %>%
        mutate(sum_o = n())%>%
        mutate(sum = ifelse(sum_o==1, count, sum_o))%>%
        dplyr::select(Specific_Concept, sum) %>%
        slice(1)%>%
        rename(count = sum)%>%
        ungroup()%>%
        mutate(rate = round(count/sum(count) * 100,2))%>%
        rename(word = Specific_Concept)%>%
        arrange(desc(count))%>%
        rename('rate(%)' = 'rate')
    })
   
    
    #############################tibs###############################
    
    tib_sci <- reactive({
        dat_split_sci() %>%
            ungroup()%>%
            group_by(Scientific_Field)%>%
            summarise(count = n())%>%
            dplyr::select(Scientific_Field, count)%>%
            arrange(desc(count))
    })
    
    
    tib_con <- reactive({
        dat_split_con()%>%
            ungroup()%>%
            mutate(Specific_Concept = str_to_title(Specific_Concept))%>%
            group_by(Specific_Concept)%>%
            summarise(count = n())%>%
            arrange(desc(count))
    })
    
    output$times_table_con = renderDataTable({
        datatable(tib_con(),
                  options = list(pageLength = 5,lengthMenu = c(5, 10, 15, 20), scrollX = T,
                                 autoWidth = FALSE,
                                 columnDefs = list(
                                     list(width = '10px', targets =c(0, 1, 2))
                                 )))
    })

    
    output$show_table = renderDataTable({
        datatable(table_data()[c(1, 2, 16, 17, 3, 4, 5, 6, 7, 8, 9, 10, 14, 15, 11, 12, 13)],
        options = list(pageLength = 5,lengthMenu = c(5, 10, 15, 20), scrollX = T,
                       autoWidth = TRUE,
                       columnDefs = list(
                                         list(width = '900px', targets = c(10)),
                                         list(width = '250px', targets = c(5)),
                                         list(width = '150px', targets = c(15, 17, 16)),
                                         list(width = '100px', targets = c(6, 7, 8)),
                                         list(width = '60px', targets = c(11, 12, 13, 14)),
                                         list(width = '5px', targets = c(1, 2, 3, 4)),
                                         list(width = '3px', targets =c(8, 9)),
                                         list(className = 'dt-center', targets = 0:4)
                                         ))
                                        )
                })
    
    output$sci_table = renderDataTable({
        if(input$rb1 == 'Publication Year'){
            datatable(tib_usable_split_con_pub(),
                      options = list(pageLength = 5,lengthMenu = c(5, 10, 15, 20), scrollX = T,
                                     autoWidth = FALSE,
                                     columnDefs = list(
                                         list(width = '10px', targets =c(0, 1, 2))
                                     )))
        }else{
            datatable(tib_usable_split_con_cou(),
                      options = list(pageLength = 5,lengthMenu = c(5, 10, 15, 20), scrollX = T,
                                     autoWidth = FALSE,
                                     columnDefs = list(
                                         list(width = '10px', targets =c(0, 1, 2))
                                     )))
        }
        
    })
    
    
    output$news_total <- renderText({
        HTML(
            paste("Total number of news:", 
                  strong(
                      unique(
                          paste(set_base()$id) %>% 
                              length()
                      )
                  )
            )
        )
    })
    
    ################### Visualization ######################
    
    output$line_plot_course <- renderHighchart({
        
        highchart() %>%
            hc_add_series(tib(), hcaes(x = Publication_Year, y = n, color = n), type = "line") %>%
            hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Year: <b>{point.x}</b> <br> literacy rate: <b>{point.y}</b>")) %>%
            hc_title(text = "Evolution of number collected statistical educational News by publication year", style = list(fontSize = "16px")) %>%
            hc_subtitle(text = "1992-2020") %>%
            hc_xAxis(title = list(text = "Year")) %>%
            hc_yAxis(title = list(text = "# of News"),
                     allowDecimals = FALSE,
                     plotLines = list(list(
                         color = "black", width = 2, dashStyle = "Dash",
                         value = mean(tib()$mean),
                         label = list(text = "Mean = 4 news/yr",
                                      style = list(color = "black"))))) %>%
            hc_legend(enabled = FALSE) %>%
            hc_add_theme(custom_theme)
    })
    
    dd0 <- reactive(tib() %>% dplyr::select(-mean) %>% arrange(desc(n)))
    
    output$time_table_course = renderDataTable({
      datatable(dd0(),options = list(pageLength = 5,lengthMenu = c(5, 10, 15, 20), scrollX = T,
                                 autoWidth = FALSE,
                  columnDefs = list(
                      list(width = '20px', targets =c(0, 1, 2))
                  )))
    })

    output$line_plot_pub = renderHighchart({
        highchart() %>%
            hc_add_series(tib2(), hcaes(x = Course_Year, y = n, color = n), type = "line") %>%
            hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Year: <b>{point.x}</b> <br> literacy rate: <b>{point.y}</b>")) %>%
            hc_title(text = "Evolution of Number of collected statistical educational News by course year", style = list(fontSize = "16px")) %>%
            hc_subtitle(text = "1992-2020") %>%
            hc_xAxis(title = list(text = "Year")) %>%
            hc_yAxis(title = list(text = "# of News"),
                     allowDecimals = FALSE,
                     plotLines = list(list(
                         color = "black", width = 2, dashStyle = "Dash",
                         value = mean(tib2()$mean),
                         label = list(text = paste0("Mean = 9.14 news/yr"),
                                      style = list(color = "black"))))) %>%
            hc_legend(enabled = FALSE) %>%
            hc_add_theme(custom_theme)
    })
    
    dd1 <- reactive(tib2() %>% dplyr::select(-mean)%>% arrange(desc(n)))
    
    time_table_pub = renderDataTable({
        datatable(dd1(),
                  options = list(pageLength = 5,lengthMenu = c(5, 10, 15, 20), scrollX = T,
                                 autoWidth = FALSE,
                                 columnDefs = list(
                                   list(width = '20px', targets =c(0, 1, 2))
                                 )))
    })
    
    output$time_table_pub <- time_table_pub
    
    output$times_table_sci =  renderDataTable({
        datatable(tib_sci(),
                  options = list(pageLength = 5,lengthMenu = c(5, 10, 15, 20), scrollX = T,
                                 autoWidth = FALSE,
                                 columnDefs = list(
                                     list(width = '10px', targets =c(0, 1, 2))
                                 )))
    })
    
    output$word_cloud_scientific = renderWordcloud2({
        tib_sci() %>%
            wordcloud2a( backgroundColor = "white", size = 3, 
                         color='random-dark', shape = 'pentagon')
    })
    
    output$word_cloud_concept= renderWordcloud2({
        tib_con() %>%
            wordcloud2a( backgroundColor = "white", size = 3, 
                         color = "random-dark")
    })
    
  
    
    output$times_table_sci_2 <- renderDataTable({
        datatable(tib_sci(),
                  options = list(pageLength = 5,lengthMenu = c(5, 10, 15, 20), scrollX = T,
                                 autoWidth = FALSE,
                                 columnDefs = list(
                                     list(width = '20px', targets =c(1))
                                 )))
    })
    
    output$time_series_plot_sci = renderPlot({
        if(input$rb == 'Publication Year'){
            tib_sci_pub()%>%
                ggplot(aes(x = word, y = count, fill = word))+
                geom_bar(stat="identity",position = 'dodge')+
                geom_text(aes(label = count),colour="black",size = 2.6,
                          position = position_stack(vjust = 0.5))+
                facet_wrap(~ factor(label), ncol = 4,strip.position = "top",
                )+
                coord_flip()+
                scale_fill_manual(values = alpha(pnw_palette('Sailboat',7),0.7)) +
                # theme_ipsum(
                #     base_family = "Arial Narrow",
                #     base_size = 6,
                #     plot_title_size = 12,
                #     subtitle_size = 9,
                #     subtitle_margin = 10,
                #     caption_size = 7,
                #     axis_text_size = 8,
                #     plot_margin = margin(2, 2, 2, 2)
                # )+
                theme( legend.position = 'none',
                       legend.justification='top',
                       legend.direction='horizontal',
                       legend.margin=margin(t = -0.2, unit='cm'),
                       plot.subtitle = element_text(hjust = 0.9),
                       axis.title.y = element_text(hjust = 0.1, size = 0),
                       axis.title.x = element_text(hjust = 0.1, size = 0),
                       axis.ticks.x = element_blank(),
                       axis.text.x=element_blank(),
                       panel.grid.minor= element_blank(),
                       strip.placement = "outside",
                       strip.background=element_rect(fill="white", colour = 'grey' ),
                       strip.text = element_text(size = 8),
                       panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                       panel.background = element_rect(fill = "white"),
                       panel.spacing =unit(.05, "lines"),
                       panel.border = element_rect(color = "black", fill = NA, size = 0.4)
                )+
                guides(fill=guide_legend(nrow=1,
                                         byrow=TRUE
                )
                )+
                labs(
                    x=" Scientific Fields", y="Frequency",
                    #title=" Comparison of Distribution of Scienfic Fields of The News Over Publication Years",
                    #subtitle="--- Main Scientific Fields",
                    legend = 'Scientific Fields'
                    #caption= "Data Source: Newspapers and Online Platforms"
                    )
        }else{
            tib_sci_course()%>%
                ggplot(aes(x = word, y = count, fill = word))+
                geom_bar(stat="identity",position = 'dodge')+
                geom_text(aes(label = count),colour="black",size = 2,
                          position = position_stack(vjust = 0.5))+
                facet_wrap(~ label2, ncol = 4,strip.position = "top",
                )+
                coord_flip()+
                scale_fill_manual(values = alpha(pnw_palette('Sailboat',25),0.7)) +
                # theme_ipsum(
                #     base_family = "Arial Narrow",
                #     base_size = 6,
                #     plot_title_size = 12,
                #     subtitle_size = 9,
                #     subtitle_margin = 10,
                #     caption_size = 7,
                #     axis_text_size = 8,
                #     plot_margin = margin(2, 2, 2, 2)
                # )+
                theme( legend.position = 'none',
                       legend.justification='top',
                       legend.direction='horizontal',
                       legend.margin=margin(t = -0.2, unit='cm'),
                       plot.subtitle = element_text(hjust = 0.9),
                       axis.title.y = element_text(hjust = 0.1, size = 0),
                       axis.title.x = element_text(hjust = 0.1, size = 0),
                       axis.ticks.x = element_blank(),
                       axis.text.x=element_blank(),
                       panel.grid.minor= element_blank(),
                       strip.placement = "outside",
                       strip.background=element_rect(fill="white", colour = 'grey' ),
                       strip.text = element_text(size = 8),
                       panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                       panel.background = element_rect(fill = "white"),
                       panel.margin=unit(.0, "lines"),
                       panel.border = element_rect(color = "black", fill = NA, size = 0.4)
                )+
                guides(fill=guide_legend(nrow=1,
                                         byrow=TRUE
                )
                )+
                labs(
                    x=" Scientific Fields", y="Frequency",
                    #title=" Evolution of Specific Concepts In The News Over Course Year",
                    #subtitle="--- News Release Dates From 2002 to 2020",
                    legend = 'Scientific Fields'
                    #caption= "Data Source: Newspapers and Online Platforms"
                    )
        }
    })
    
    
   output$rader_plot_sci = renderPlot({
       if(input$rb1 == 'Publication Year'){
           dat_usable_split_con_pub()%>%
               ggplot(aes(x = word, y = count, fill = word))+
               geom_bar(stat="identity",position = 'dodge')+
               geom_text(aes(label = count),colour="black",size = 2,
                         position = position_stack(vjust = 0.5))+
               facet_wrap(~ label, ncol = 4,strip.position = "top",
               )+
               coord_flip()+
               scale_fill_manual(values = alpha(pnw_palette('Sailboat',20),0.9)) +
               theme_ipsum(
                   base_family = "Arial Narrow",
                   base_size = 6,
                   plot_title_size = 12,
                   subtitle_size = 9,
                   subtitle_margin = 10,
                   caption_size = 7,
                   axis_text_size = 8,
                   plot_margin = margin(2, 2, 2, 2)
               )+
               theme( legend.position = 'none',
                      legend.justification='top',
                      legend.direction='horizontal',
                      legend.margin=margin(t = -0.2, unit='cm'),
                      plot.subtitle = element_text(hjust = 0.9),
                      axis.title.y = element_text(hjust = 0.1, size = 0),
                      axis.title.x = element_text(hjust = 0.1, size = 0),
                      axis.ticks.x = element_blank(),
                      axis.text.x=element_blank(),
                      panel.grid.minor= element_blank(),
                      strip.placement = "outside",
                      strip.background=element_rect(fill="white", colour = 'grey' ),
                      strip.text = element_text(size = 8),
                      panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                      panel.background = element_rect(fill = "white"),
                      panel.margin=unit(.0, "lines"),
                      panel.border = element_rect(color = "black", fill = NA, size = 0.4)
               )+
               guides(fill=guide_legend(nrow=1,
                                        byrow=TRUE
               )
               )+
               labs(
                   x=" Scientific Fields", y="Frequency",
                   #title=" Evolution of Specific Concepts In The News Over Publication Year",
                   #subtitle="--- News Release Dates From 2002 to 2020",
                   legend = 'Scientific Fields'
                   #caption= "Data Source: Newspapers and Online Platforms"
                   )
       }else{
           dat_usable_split_con_cou()%>%
               ggplot(aes(x = word, y = count, fill = word))+
               geom_bar(stat="identity",position = 'dodge')+
               geom_text(aes(label = count),colour="black",size = 2,
                         position = position_stack(vjust = 0.5))+
               facet_wrap(~ label2, ncol = 4,strip.position = "top",
               )+
               coord_flip()+
               scale_fill_manual(values = alpha(pnw_palette('Sailboat',25),0.7)) +
               theme_ipsum(
                   base_family = "Arial Narrow",
                   base_size = 6,
                   plot_title_size = 12,
                   subtitle_size = 9,
                   subtitle_margin = 10,
                   caption_size = 7,
                   axis_text_size = 8,
                   plot_margin = margin(2, 2, 2, 2)
               )+
               theme( legend.position = 'none',
                      legend.justification='top',
                      legend.direction='horizontal',
                      legend.margin=margin(t = -0.2, unit='cm'),
                      plot.subtitle = element_text(hjust = 0.9),
                      axis.title.y = element_text(hjust = 0.1, size = 0),
                      axis.title.x = element_text(hjust = 0.1, size = 0),
                      axis.ticks.x = element_blank(),
                      axis.text.x=element_blank(),
                      panel.grid.minor= element_blank(),
                      strip.placement = "outside",
                      strip.background=element_rect(fill="white", colour = 'grey' ),
                      strip.text = element_text(size = 8),
                      panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                      panel.background = element_rect(fill = "white"),
                      panel.margin=unit(.0, "lines"),
                      panel.border = element_rect(color = "black", fill = NA, size = 0.4)
               )+
               guides(fill=guide_legend(nrow=1,
                                        byrow=TRUE
               )
               )+
               labs(
                   x=" Scientific Fields", y="Frequency",
                   #title=" Evolution of Specific Concepts In The News Over Course Year",
                   #subtitle="--- News Release Dates From 2002 to 2020",
                   legend = 'Scientific Fields'
                   #caption= "Data Source: Newspapers and Online Platforms"
                   )
               
       }
   })
   
}



