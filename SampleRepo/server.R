

server <- function(input, output, session){
    
    
    output$all_chain_samples_dt <- DT::renderDataTable(
        
        all_sites_samples, 
        style = 'bootstrap4',
        options = list(scrollX = TRUE,
                       pageLength = 25))
    
    
    output$all_samples_down <- down_function(df = all_sites,
                                             file_name = "all_chain_samples")
    
    
    all_chain_samples_reactive <- reactive({
        all_samples_time[study == input$select_study & time_point  == input$select_time_point_all & 
                             type_name %in% input$select_sample_type ]
    })
    
    all_chain_samples_reactive_site <- reactive({
        
        all_chain_samples_reactive()[site %in% input$select_site]
        
    })
    
    output$all_chain_bytime <- DT::renderDataTable(
        
        all_chain_samples_reactive_site()%>%
               pivot_wider(names_from = type_name, values_from = samples) %>%
                  adorn_totals(where = c("row", "col")), 
        style = 'bootstrap4',
        options = list(scrollX = TRUE,
                       pageLength = 25))
    
    
    output$all_chain_bytime_all <-  DT::renderDataTable({
        bycol = c("categ_enrol", "time_point", "type_name", "aliquot")

        all_chain_samples_reactive()%>% .[, .(freq = sum(samples)), by = bycol] %>%
            pivot_wider(names_from = type_name, values_from = freq) %>%
            adorn_totals(where = c("row", "col"))}, 
        style = 'bootstrap4',
        options = list(scrollX = TRUE,
                       pageLength = 25))
    
    
    
    
    output$dt_shipped_samples <- DT::renderDataTable(
        
        shipped, 
        style = 'bootstrap4',
        options = list(scrollX = TRUE,
                       pageLength =15))
    
    
    output$down_shipped_samples <- down_function(df = shipped,
                                                 file_name = "shipped_samples_tally")
    
    
    output$mc_samples <- DT::renderDataTable(
        
        main_cohort_samples, 
        style = 'bootstrap4',
        options = list(scrollX = TRUE,
                       pageLength =15))
    
    
    output$mc_down <- down_function(df = main_cohort_samples,
                                                 file_name = "main_cohort_samples")
    
    time_point_mc_react <- reactive({
        
        lab_storage_after2[time_point ==input$select_time_point  & study == "main_cohort"]
        
    })
    
    output$mc_samples_time <- DT::renderDataTable({
        
        
        df = time_point_mc_react()
        df[, .(samples = .N), 
            by = .( type_name, categ_enrol, aliquot)] %>%
            pivot_wider(names_from = type_name, values_from = samples) %>%
            adorn_totals(where = c("row", "col"))
        
        
    },
        
        
        style = 'bootstrap4',
        options = list(scrollX = TRUE,
                       pageLength =15))
    
    
    
    output$mc_samples_time_alive <- DT::renderDataTable({
        
        
        df = time_point_mc_react()
        df = df[died == input$select_alive]
        df[, .(samples = .N), 
           by = .( type_name, categ_enrol, aliquot)] %>%
            pivot_wider(names_from = type_name, values_from = samples) %>%
            adorn_totals(where = c("row", "col"))
        
        
    },
    
    
    style = 'bootstrap4',
    options = list(scrollX = TRUE,
                   pageLength =15))
    
    
    part_mc_samples_dt <- reactive({
        
        sample_types =  input$select_sample_type_mc
        df <- lab_storage_after[study == "main_cohort" & 
                                                      type_name %in% sample_types &
                                                      aliquot == input$select_aliquot_mc,
                                                  .(record_id, site, categ_enrol,died, pk_specimen_id,
                                                    type_name, time_point, aliquot)]
        sample_types  <- df[, unique(type_name)]
        df <- df[time_point == input$select_time_point_mc_part]
        df <- df %>%
            distinct(record_id, type_name, aliquot, .keep_all = T) %>%
            pivot_wider(names_from = type_name, values_from = pk_specimen_id)
        
        setDT(df)
        
       df[, (sample_types) := lapply(.SD,  as.character ),
                .SDcols = sample_types]
       

       df[, (sample_types) := lapply(.SD, function(x) ifelse(is.na(x),
                                                                    "sample_unavailable", x )),
                .SDcols = sample_types]
        df
        
    })
    
    
    output$part_with_samples <- DT::renderDataTable({
        
        part_mc_samples_dt()
        
        
        
    },
    
    
    style = 'bootstrap4',
    options = list(scrollX = TRUE,
                   pageLength =15))
    
    ##NCC
    
    output$ncc_dt <- renderDataTable(
        ncc_part_sum,
        style = 'bootstrap4',
        options = list(scrollX = TRUE,
                       pageLength =15))
    
    output$ncc_dt_available <- renderDataTable(
        ncc_all_samples_sum,
        style = 'bootstrap4',
        options = list(scrollX = TRUE,
                       pageLength =15))
    
    ## Young Infants
    
    
    output$young_infants_dt <- renderDataTable(
        young_infants_samples,
        style = 'bootstrap4',
        options = list(scrollX = TRUE,
                       pageLength =15))
    
    output$young_infants_all_down <- down_function(df = young_infants_samples,
                                    file_name = "young_infants_samples")
    
    #select_time_point_yi
    time_point_mc_react_yi <- reactive({
        
        lab_storage_after2[time_point == input$select_time_point_yi  & study == "Young_infants"]
        
    })
    
    # 
    # output$young_infants_time <- render_dt({
    #     
    #     
    #     df = time_point_mc_react_yi()
    #     df[, .(samples = .N), 
    #        by = .( type_name, categ_enrol, aliquot)] %>%
    #         pivot_wider(names_from = type_name, values_from = samples) %>%
    #         adorn_totals(where = c("row", "col"))
    #     
    #     
    # })
    
    
    output$young_infants_time <- DT::renderDataTable({
        
        
        df = time_point_mc_react_yi()
        df[!is.na(categ_enrol), .(samples = .N), 
           by = .( type_name, categ_enrol, aliquot)] %>%
            pivot_wider(names_from = type_name, values_from = samples) %>%
            adorn_totals(where = c("row", "col"))
        
        
    },
    
    
    
    style = 'bootstrap4',
    options = list(scrollX = TRUE,
                   pageLength =15))
    
    
    output$yi_samples_time_alive <- DT::renderDataTable({
        
        
        df = time_point_mc_react_yi()
        df = df[died == input$select_alive_yi]
        df[, .(samples = .N), 
           by = .( type_name, categ_enrol, aliquot)] %>%
            pivot_wider(names_from = type_name, values_from = samples) %>%
            adorn_totals(where = c("row", "col"))
        
        
    },
    
    
    style = 'bootstrap4',
    options = list(scrollX = TRUE,
                   pageLength =15))
    
    ## BMC
    
    output$bmc_dt <- renderDataTable(
        bmc,
        style = 'bootstrap4',
        options = list(scrollX = TRUE,
                       pageLength =15))
    
    output$bmc_down <- down_function(df = bmc,
                                     file_name = "bmc_samples")
    
    
    
    ## BMC
    
    
    
    
    
    
    ## Freezer Space
    
    output$plot_freezer_space <- renderPlotly(
        
        
        plot_frzr
    )
    
    tray_label_selected <- reactive({
        
        if(input$freezer_select == 42){
            
            tray = input$select_tray_42
            
        }else if(input$freezer_select == 43){
            
            tray = input$select_tray_43
        }else if(input$freezer_select == 49){
            
            tray = input$select_tray_49
        }else if(input$freezer_select == 55){
            
            tray = input$select_tray_55
        }else tray = input$select_tray_59
        
        tray
        
        
        
    })
    
    
    
    output$plot_freezer_tray_box <- renderPlotly({
        df = tab_boxes[ tray_label %in% tray_label_selected()]
        ggplotly(
            ggplot(df, aes(box_number, box_perc_full, fill = box_number))+
                geom_bar(stat = "identity")+
                labs(x = "Freezer Name", y = "% Full")+
                scale_fill_hc(name = "")+
                theme_fivethirtyeight()+
                theme(legend.position = "none")
            
        )

        
    }
    )
    
    boxes_capacity_select_df <- reactive(
        
        tab_boxes[media_name %in% input$freezer_select  & 
                      between(box_perc_full, input$box_select_perc[1], input$box_select_perc[2])]
    )
    
    
    output$choose_trays_boxes_full <- renderDataTable(
        
        boxes_capacity_select_df(),
        style = 'bootstrap4',
        options = list(scrollX = TRUE,
                       pageLength =15))
    
    get_boxes_trays <- reactive({
        
        df =  boxes_capacity_select_df()
        boxes = df[, box_number] %>% unique()
        trays = df[, tray_label]  %>% as.character()
        
       gt_boxes =  list(boxes = boxes, 
             trays = trays)
       
       gt_boxes
       }
        
    )

    output$down_boxes_half_full <- down_function({
        
        df =  boxes_capacity_select_df()
        boxes = df[, box_number] %>% as.character() %>% unique()
        trays = df[, tray_label]  %>% as.character()
    
        freezers_contents[tray_label %in% trays & box_number %in% boxes]
        },
        
        file_name = "samples_from_selected_boxes")
    
    output$down_boxes_half_full_boxes <- down_function({
        
        df =  boxes_capacity_select_df()
        boxes = df[, box_number] %>% as.character() %>% unique()
        trays = df[, tray_label]  %>% as.character()
        
        freezers_contents[tray_label %in% trays & box_number %in% boxes] %>%
            distinct(tray_label, box_number)
    },
    
    file_name = "boxes_of_interest")
    
    
    
    
    
    
    
    
    
    
    


}
