
server <- function(input, output, session){
  source("global.R")

  # source global

  # shiny::reactiveFileReader(
  #   intervalMillis = 300000, # check for changes every 5 mins
  #   session = session,
  #   filePath = "global.R",
  #   readFunc = source
  # )
  # ++ Home tab -------
  output$neon_adm <- renderValueBox({
    valueBox(
      value = overal_adms,
      subtitle = h4('Neonatal admissions'),
      icon = icon("bar-chart-o") ,color = "purple")
  })

  output$blood_cult <- renderValueBox({
    valueBox(
      value = nrow(blood_culture),
      subtitle = h4('Blood cultures done'),
      icon = icon("stats",lib='glyphicon'),
      color = "green")
  })

  output$inpat_deaths <- renderValueBox({
    valueBox(
      value = nrow(inpatdeaths),
      subtitle = h4('Inpatient deaths'),
      icon = icon("bar-chart-o"),
      color = "yellow")
  })


  output$consent_d28 <- renderValueBox({
    valueBox(
      value = table(base_list$consent_day28)[[2]],
      subtitle = h4('D28 consented'),
      icon = icon("stats",lib='glyphicon'),
      color = "light-blue")
  })

  output$alive_d28 <- renderValueBox({
    valueBox(
      value = nrow(d28_success),
      subtitle = h4('Alive at D28 (Kilifi & Mbagathi)'),
      icon = icon("stats",lib='glyphicon'),
      color = "green")
  })


  # demographic table and average weekly
  output$demo_table <- renderDataTable(
    final_demoTable,
    rownames = F,
    colnames = c("", "Kilifi", "Mbagathi", "Kiambu")
  )

  # download handler

  output$demog <- downloadHandler(
    # function for file name
    filename = function(){
      paste0("demographic_characteristics",".csv")
    },

    content = function(file){
      write.csv(final_demoTable, row.names = F, file)
    }
  )



  # weekly culture
  output$wkly_clt <- renderDataTable(
    weekly_per_site,
    rownames = F,
    colnames = c("", "Weekly average", "Weekly median")
  )

  output$wkly_cul <- downloadHandler(
    # function for file name
    filename = function(){
      paste0("weekly_cultures",".csv")
    },

    content = function(file){
      write.csv(weekly_per_site, row.names = F, file)
    }
  )



  # ++ cumulative plot and bar plot section -------
  cum_plot <- reactive({
    if (input$site == "Kilifi"){
      base_list %>%
        select(patient_serial_number, date_collect, site) %>%
        filter(site == "kilifi") %>%
        group_by(date_collect) %>%
        summarise(
          total = n()
        ) %>%
        mutate(
          cum_tot = cumsum(total)
        )
    } else if (input$site == "Mbagathi"){
      base_list %>%
        select(patient_serial_number, date_collect, site) %>%
        filter(site == "mbagathi") %>%
        group_by(date_collect) %>%
        summarise(
          total = n()
        ) %>%
        mutate(
          cum_tot = cumsum(total)
        )
    } else if (input$site == "Kiambu") {
      base_list %>%
        select(patient_serial_number, date_collect, site) %>%
        filter(site == "kiambu") %>%
        group_by(date_collect) %>%
        summarise(
          total = n()
        ) %>%
        mutate(
          cum_tot = cumsum(total)
        )
    } else {
      base_list %>%
        select(patient_serial_number, date_collect, site) %>%
        group_by(date_collect) %>%
        summarise(
          total = n()
        ) %>%
        mutate(
          cum_tot = cumsum(total)
        )
    }
  })


  # reactive plot for download
  render_plot <- reactive({
    if (input$plot_type == 1){
      cum_plot() %>% ggplot(aes(date_collect, cum_tot)) + geom_line(col = "maroon")+
        scale_x_date(date_labels = "%Y-%b", date_breaks = "4 weeks")+
        scale_y_continuous(breaks = seq(0, nrow(base_list), 100))+
        labs(y="Enrolled", x = "",
             subtitle = paste(input$site, "cumulative enrolment curve")) +
        theme_hc() + expand_limits(y = 0) +
        theme(
          axis.ticks.y = element_blank()
        )
    } else if (input$plot_type == 0) { # Multiple lines cumulative plot
      cum_data %>%
        ggplot(aes(date_collect, cum_tot)) +
        geom_line(aes(color = site),
                  na.rm = T,
                  size = 1/2) +
        scale_x_date(date_labels = "%Y-%b",
                     date_breaks = "4 weeks",
                     expand = c(0, 0)) +
        labs(y = "Enrolled",
             title = "Cumulative enrolment curves by site",
             x = "") +
        scale_y_continuous(
          limits = c(0, max(cum_data$cum_tot)),
          # use secondary axis to label lines
          sec.axis = dup_axis(
            breaks = recent_cum$cum_tot,
            labels = recent_cum$site,
            name = NULL
          )
        ) +
        theme_hc() +
        theme(legend.position = "none") +
        scale_color_brewer(palette = "Set1") +
        annotate(geom = "label",
                 label = paste("Kilifi start date: ",start_date[1,],
                               "\nMbagathi start date: ", start_date[2,],
                               "\nKiambu start date: ", start_date[3,],
                               sep = " "),
                 fill = "turquoise1", fontface = "bold",
                 y = 260, x = ymd("2020-11-10"))+
        annotate(geom = "rect",
                 xmin = ymd("2020-12-09"), xmax = ymd("2021-02-07"),
                 ymin = 45, ymax = 70,
                 alpha = .2)+
        annotate(geom = "text",
                 x = ymd("2020-12-31"),
                 y = 76, label = "Health workers' strike at the site",
                 col = "maroon")
    } else {
      base_list %>%
        group_by(site) %>%
        summarise(
          total = n()
        ) %>% ggplot(aes(reorder(site, -total), total, fill = site)) +
        geom_bar(stat = "identity", width = 0.45) +
        scale_fill_brewer(palette = "Dark2") + geom_text(aes(label = total),
                                                         vjust = -0.5)+
        theme_hc()+ labs(x = "", subtitle = "Enrollments by site") +
        theme(legend.position = "none")+
        theme(
          axis.ticks.y = element_blank()
        )
    }
  })


  # actual creation of the plots
  output$cum_plot <- renderPlot(
    render_plot()
  )

  # create a download handler for the plot

  # create download handler for the plot
  output$down_cum_plot <- downloadHandler(
    filename = function(){
      paste0("accrued_numbers", ".png")
    },

    content = function(file){
      ggsave(file, plot = render_plot(),
             width = 12, height = 6, dpi = 180)
    }
  )


  # + Estimated target completion plot ----

  output$est_plot <- renderPlotly(
    ggplotly(ggplot() + geom_line(mapping = aes(date_collect, Enrolled),
                                  data = enrolment_rate, col = "steelblue") +
               geom_line(mapping = aes(date, estimate.number),
                         data = enrol_rate2, col = "red", linetype = "dashed") +
               scale_x_date(date_labels = "%Y-%b",
                            date_breaks = "8 weeks") + theme_hc()+
               labs(x="", y = "Total enrolled",
                    title = "Cumulative enrolments & estimated target completion") +
               annotate(geom = "label",
                        label = paste("Kilifi start date: ",start_date[1,],
                                      "\nMbagathi start date: ", start_date[2,],
                                      "\nKiambu start date: ", start_date[3,],
                                      sep = " "),
                        fill = "turquoise1", fontface = "bold",
                        y = 4500, x = Sys.Date())
    )

  )


  output$plt_scrn_tab <- renderPlot({

    screening_plot

  }, res = 100)

  output$plt_scrn_tab_week <- renderPlot({

    screening_plot_wk

  }, res = 100)





  # ++ Flow charts

  # Kilifi diagram
  output$kilifi_diag <- renderGrViz({
    grViz({
      kilifi_chart
    })
  })

  # Mbagathi diagram
  output$mbagathi_diag <- renderGrViz({
    grViz({
      mbagathi_chart
    })
  })

  # Kiambu diagram
  output$Kiambu_diag <- renderGrViz({
    grViz({
      kiambu_chart
    })
  })

  # overal diagram
  output$overal_diag <- renderGrViz({
    grViz({
      overal_chart
    })
  })


  # abx use section ---------

  output$abx_use <- renderDataTable(
    abx_all %>%
      mutate(
        `%` = scales::percent(Prescribed/sum(abx_all$Prescribed))
      )
  )

  # day 28 list schedule (reactive)
  d28_list <- reactive({
    if (input$d28_site == "Kilifi"){
      d28_final_list %>%
        filter(site == "Kilifi")
    } else if (input$d28_site == "Mbagathi"){
      d28_final_list %>%
        filter(site == "Mbagathi")
    } else{
      d28_final_list
    }
  })

  # ++ Day 28 follow up report and queries
  output$d28_sched <- renderDataTable(
    d28_list()
  )

  # create a download handler for d28 list

  output$d28_fup <- downloadHandler(
    # function for file name
    filename = function(){
      paste0(input$d28_site,"_","d28_schedule_list",".csv")
    },

    content = function(file){
      write.csv(d28_list(), row.names = F, file)
    }

  )

  # + d28 queries

  backlog_data <- reactive({
    if (input$backlog_site == "Kilifi"){
      backlog %>%
        filter(site == "kilifi")
    } else if (input$backlog_site == "Mbagathi") {
      backlog %>% filter(site == "mbagathi")
    } else if (input$backlog_site == "Kiambu"){
      backlog %>% filter(site == "kiambu")
    } else {
      backlog
    }
  })

  output$redcap_backlog <- renderDataTable(
    backlog_data()
  )

  # missing CIN number

  miss_cin_data <- reactive({
    if (input$cin_site == "Kiambu"){
      miss_cin_num %>%
        filter(site == "kiambu")
    } else if (input$cin_site == "Mbagathi") {
      miss_cin_num %>% filter(site == "mbagathi")
    } else {
      miss_cin_num
    }
  })


  output$miss_cin <- renderDataTable(
    miss_cin_data()
  )

  output$missing_cin_down <- downloadHandler(
    # function for file name
    filename = function(){
      paste0("missing_cin",".csv")
    },

    content = function(file){
      write.csv(miss_cin_data(), row.names = F, file)
    }
  )
  # missing abx information

  miss_abx_data <- reactive({
    if (input$abx_site == "Kiambu"){
      miss_abx %>%
        filter(site == "kiambu")
    } else if (input$abx_site == "Mbagathi") {
      miss_abx %>% filter(site == "mbagathi")
    } else {
      miss_abx
    }
  })

  output$miss_abx_info <- renderDataTable(
    miss_abx_data()
  )

  # missing consenting information

  miss_cons_data <- reactive({
    if (input$cons_site == "Kilifi"){
      miss_cons %>%
        filter(site == "kilifi")
    } else if (input$cons_site == "Mbagathi") {
      miss_cons %>% filter(site == "mbagathi")
    } else if (input$cons_site == "Kiambu"){
      miss_cons %>% filter(site == "kiambu")
    } else {
      miss_cons
    }
  })


  output$miss_const <- renderDataTable(
    miss_cons_data()
  )


  # ++ --- CIN Received dataset
  output$cin_genderSite <- renderDataTable(
    cin_siteGender
  )

  # CIN sites REDCap summaries
  output$cin_redcap <- renderDataTable(
    cin_REDCap
  )

  # IDs in REDCap whose CIN data we don't have

  CIN_miss_list <- reactive({
    if (input$miss_IDs == 1){
      miss_cinIDs %>%
        filter(site == "kiambu")
    } else if (input$miss_IDs == 2) {
      miss_cinIDs %>%
        filter(site == "mbagathi")
    } else {
      miss_cinIDs
    }


  })

  # ++ CIN-NeOBAC data check

  # gender
  output$qry_gender <- renderDataTable(

    if (input$site_gender == "All sites"){
      gender_Qry
    } else {
      gender_Qry %>% filter(
        site == input$site_gender
      )
    }

  )

  # admission date
  output$qry_admDate <- renderDataTable(

    if (input$site_adm == "All sites"){
      adm_Qry
    } else {
      adm_Qry %>% filter(
        site == input$site_adm
      )
    }
  )

  # Date of birth
  output$qry_dob <- renderDataTable(


    if (input$site_dob == "All sites"){
      dob_Qry
    } else {
      dob_Qry %>% filter(
        site == input$site_dob
      )
    }
  )

  output$miss_CIN_IDs <- renderDataTable(
    CIN_miss_list(),
    rownames = F
  )

  # make download hander
  output$down_missIDs <- downloadHandler(
    # function for file name
    filename = function(){
      paste0("CIN_missing_IDs",".csv")
    },

    content = function(file){
      write.csv(CIN_miss_list(), row.names = F, file)
    }
  )


  # Summary table of missing CIN clinical data

  output$cin_summary_miss_tab <- renderDataTable(
    miss_cinIDs_sum
  )

  # --- IDs from the paedtriatic database
  paed_dt <- reactive({
    if (input$paed_IDs == 1) {
      cinNeo_paedIDs %>%
        filter(site == "kiambu") %>% select(-c(surgical_burns, age_less1mnth))
    } else if (input$paed_IDs == 2) {
      cinNeo_paedIDs %>%
        filter(site == "mbagathi")%>% select(-c(surgical_burns, age_less1mnth))
    } else {
      cinNeo_paedIDs %>% select(-c(surgical_burns, age_less1mnth))
    }
  })

  # render the above table

  output$paed_ids <- renderDataTable(
    paed_dt(), rownames = F
  )

  # -------------------------------------------------------------------------

  ## Lab report

  ## Lab data

  # output$kidms_samples <- DT::renderDataTable({
  #
  #   kidms_sample
  #
  # })


  output$kidms_samples  <- DT::renderDataTable(
    datatable(
      kidms_sample,
      rownames = TRUE,
      options = list(
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = FALSE,
        dom = 'tB',
        buttons = c('copy')
      ),
      class = "display", #if you want to modify via .css
      extensions = "Buttons"
    ))


  output$conta_rate <- renderPlot({

    gg_conta_rate

  })

  input_isolates <- reactive({

    if(input$isolate_type == "all") isolate_input = c("Pathogen", "Contaminant") else isolate_input = input$isolate_type

  })

  reactive_isolate_site <- reactive({
    if(input$site_isolate_tab == "all") {
      mysite = c("mbagathi", "kilifi", "kiambu")
    }else{
      mysite = input$site_isolate_tab
    }
  })


  isolates_result <- reactive({


    df1 = blood_culture[between(date_collect, input$date_range[1],  input$date_range[2]), .(freq = .N), by = .(site, bac_positive)] %>%
      pivot_wider(names_from = "bac_positive", values_from = "freq")%>%
      adorn_totals(where = c("col", "row"))

    df2 <- all_isolates[between(date_collect, input$date_range[1],  input$date_range[2])&
                          type %in% input_isolates() & site %in% reactive_isolate_site() , ]
    df3 <- AST_sm_wide[between(date_collect, input$date_range[1],  input$date_range[2]) &
                         site %in% reactive_isolate_site(), ]

    isolate_list <- list(bac_pos = df1, all_isolates = df2, asts = df3)
    isolate_list
  })

  # output$bac_table <- DT::renderDataTable(
  #
  #   isolates_result()$bac_pos,  options = list(scrollX = TRUE))
  #

  # output$bac_table  <- DT::renderDataTable(
  #   datatable(
  #     isolates_result()$bac_pos,
  #     rownames = TRUE,
  #     options = list(
  #       fixedColumns = TRUE,
  #       autoWidth = TRUE,
  #       ordering = FALSE,
  #       dom = 'tB',
  #       buttons = c('copy')
  #     ),
  #     class = "display", #if you want to modify via .css
  #     extensions = "Buttons"
  #   ))



  output$bac_table  <- render_dt(isolates_result()$bac_pos)

  output$isolates_table <- DT::renderDataTable(

    isolates_result()$all_isolates,  options = list(scrollX = TRUE))


  output$ast_results <- DT::renderDataTable(

    isolates_result()$asts,  options = list(scrollX = TRUE))

  output$isolate_down <- downloadHandler(
    filename = function() {
      site = paste( input$lab_site, collapse = "_")
      paste('neobac_isolate', '.csv', sep='')
    },
    content = function(con) {
      df = isolates_result()$all_isolates
      write.csv(df, con, row.names = F)

    }
  )

  output$ast_down <- downloadHandler(
    filename = function() {

      paste('Ast neobac',  '.csv', sep='')
    },
    content = function(con) {
      df  = isolates_result()$asts
      write.csv(df, con, row.names = F)

    }
  )
  # Lab Queries



  lab_queries <- reactive({



    miss_bloods <- missing_bloods[site == input$lab_site]
    bld_pending <-  bld_pending_culture[site == input$lab_site]
    bld_missing_results <- bld_missing_results[site == input$lab_site]
    bld_zone <- bld_zone_size_not_tested[site == input$lab_site]
    setDT(culture_storage)
    culture_storage <- culture_storage[site == input$lab_site]
    r2_missing_esbl <-  r2_missing_esbl[site == input$lab_site]

    missing_species_name = missing_species_name[site == input$lab_site]
    lab_queries_list <- list(miss_bloods=miss_bloods, bld_pending = bld_pending,
                             bld_missing_results = bld_missing_results,
                             bld_zone = bld_zone, culture_storage = culture_storage,
                             missing_species_name = missing_species_name,
                             r2_missing_esbl = r2_missing_esbl)
    lab_queries_list


  })

  output$mis_blood_query <- DT::renderDataTable(

    lab_queries()$miss_bloods
    , options = list(scrollX = TRUE))
  output$pnd_culture <- DT::renderDataTable(

    lab_queries()$bld_pending, options = list(scrollX = TRUE))

  output$culture_growth <- DT::renderDataTable(

    lab_queries()$bld_missing_results, options = list(scrollX = TRUE))

  output$culture_ast <- DT::renderDataTable(

    lab_queries()$bld_zone, options = list(scrollX = TRUE))

  output$culture_storage <- DT::renderDataTable(

    lab_queries()$culture_storage, options = list(scrollX = TRUE))

  output$esbl_ast <- DT::renderDataTable(

    lab_queries()$r2_missing_esbl, options = list(scrollX = TRUE))



  output$pnd_culture_down <- downloadHandler(
    filename = function() {
      site = paste( input$lab_site, collapse = "_")
      paste('Pending culture', site, '.csv', sep='')
    },
    content = function(con) {
      df =lab_queries()$bld_pending
      write.csv(df, con, row.names = F)

    }
  )


  output$mis_blood_down <- downloadHandler(
    filename = function() {
      site = paste( input$lab_site, collapse = "_")
      paste('Missing blood sample kidms', site, '.csv', sep='')
    },
    content = function(con) {
      df =lab_queries()$miss_bloods
      write.csv(df, con, row.names = F)

    }
  )



  output$mis_culture_down <- downloadHandler(
    filename = function() {
      site = paste( input$lab_site, collapse = "_")
      paste('Missing culture results', site, '.csv', sep='')
    },
    content = function(con) {
      df =lab_queries()$bld_missing_results
      write.csv(df, con, row.names = F)

    }
  )




  output$mis_ast_down <- downloadHandler(
    filename = function() {
      site = paste( input$lab_site, collapse = "_")
      paste('Missing AST', site, '.csv', sep='')
    },
    content = function(con) {
      df =lab_queries()$bld_zone
      write.csv(df, con, row.names = F)

    }
  )


  output$mis_store_down <- downloadHandler(
    filename = function() {
      site = paste( input$lab_site, collapse = "_")
      paste('Missing Storage LIMS', site, '.csv', sep='')
    },
    content = function(con) {
      df =lab_queries()$culture_storage
      write.csv(df, con, row.names = F)

    }
  )

  output$mis_esbl_down <- downloadHandler(
    filename = function() {
      site = paste( input$lab_site, collapse = "_")
      paste('Missing ESBL Results', site, '.csv', sep='')
    },
    content = function(con) {
      df =lab_queries()$r2_missing_esbl
      write.csv(df, con, row.names = F)

    }
  )

  output$pnd_isolate <- DT::renderDataTable(

    lab_queries()$missing_species_name, options = list(scrollX = TRUE))


  output$mis_isolate_down <- downloadHandler(
    filename = function() {
      site = paste( input$lab_site, collapse = "_")
      paste('Missing isolate', site, '.csv', sep='')
    },
    content = function(con) {
      df =lab_queries()$missing_species_name
      write.csv(df, con, row.names = F)

    }
  )
  ## Report output

  ## previuos code
  #
  output$weekly_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste('neobac_report', sep = '.', switch(
        input$report_format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },

    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).

      tempReport <- file.path(tempdir(), "neobac_weekly.Rmd")
      file.copy("neobac_weekly.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(start_date_rmd = input$date_range[1], end_date_rmd = input$date_range[2])

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        output_format =  switch(
                          input$report_format,
                          PDF = pdf_document(), HTML = html_document(), Word = word_document()
                        ),
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )


  # output$weekly_report <- downloadHandler(
  #   params <- list(start_date_rmd = input$date_range[1], end_date_rmd = input$date_range[2]),
  #
  #   filename = function() {
  #     paste('neobac_report', sep = '.', switch(
  #       input$report_format, PDF = 'pdf', HTML = 'html', Word = 'docx'
  #     ))
  #   },
  #
  #   content = function(file) {
  #     src <- normalizePath('neobac_weekly.Rmd')
  #
  #     # temporarily switch to the temp dir, in case you do not have write
  #     # permission to the current working directory
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  #     file.copy(src, 'neobac_weekly.Rmd', overwrite = TRUE)
  #
  #     library(rmarkdown)
  #     out <- render('neobac_weekly.Rmd', switch(
  #       input$report_format,
  #       PDF = pdf_document(), HTML = html_document(), Word = word_document()
  #     ), params = params)
  #     file.rename(out, file)
  #   }
  # )
  #
  #

  output$miss_perc_cin <- render_dt(miss_cin_perc)
  cin_miss_vars_react <- reactive(({
    miss_cin_df[site == input$miss_cin_site & variable == input$miss_cin_var]
  }))

  output$cin_miss_individual <- renderDataTable(
    cin_miss_vars_react(),
    style = 'bootstrap4',
    options = list(scrollX = TRUE))

  output$missing_cin_perc_down <-  down_function(miss_cin_perc,
                                                 file_name = "cin_missing_perc")

  output$missing_cin_vars_down <-  down_function(cin_miss_vars_react(),
                                                 file_name = "cin_missing_perc")

  age_neg_reactive <- reactive(({
    negative_age[site == input$select_site_age ]
  }))

  output$neg_age_query <- renderDataTable(
    age_neg_reactive(),
    style = 'bootstrap4',
    options = list(scrollX = TRUE))

  output$down_neg_age <-  down_function(age_neg_reactive(),
                                        file_name = "neobac_neg_age")




  # --------Admission treatment CRF----------------------------------

  output$klf_abx <- renderPlot(
    adm_treatment_data %>%
      filter(!is.na(abx_name)) %>%
      group_by(abx_name, redcap_repeat_instance) %>%
      summarise(
        total = n()
      ) %>% ggplot(aes(redcap_repeat_instance,
                       total, fill = abx_name))+
      geom_bar(position = "fill", stat = "identity", col = "white", width = 0.7)+
      scale_fill_brewer(palette = "Set1") + theme_hc() +
      scale_x_continuous(breaks = seq(1, max(adm_treatment_data$redcap_repeat_instance, na.rm = T), 1))+
      theme(legend.position = "right",
            axis.ticks = element_blank()) + labs(x = "Days of admission",
                                                 y = "", fill = "")+
      annotate(
        geom = "text",
        label = paste0("N = ", num_repeat[[2]][1]),
        fontface = "bold",
        y = 1.02, x = 1
      )+
      annotate(
        geom = "text",
        label = paste0("N = ", num_repeat[[2]][2]),
        fontface = "bold",
        y = 1.02, x = 2
      )+
      annotate(
        geom = "text",
        label = paste0("N = ", num_repeat[[2]][3]),
        fontface = "bold",
        y = 1.02, x = 3
      )+
      annotate(
        geom = "text",
        label = paste0("N = ", num_repeat[[2]][4]),
        fontface = "bold",
        y = 1.02, x = 4
      )+
      annotate(
        geom = "text",
        label = paste0("N = ", num_repeat[[2]][5]),
        fontface = "bold",
        y = 1.02, x = 5
      )+
      annotate(
        geom = "text",
        label = paste0("N = ", num_repeat[[2]][6]),
        fontface = "bold",
        y = 1.02, x = 6
      )+
      annotate(
        geom = "text",
        label = paste0("N = ", num_repeat[[2]][7]),
        fontface = "bold",
        y = 1.02, x = 7
      )+
      annotate(
        geom = "text",
        label = paste0("N = ", num_repeat[[2]][8]),
        fontface = "bold",
        y = 1.02, x = 8
      )
  )


  # Missingnes reports (admission treatment CRF)
  treatCRF_missing <- reactive({

    if (input$treat_mis_site == 1){
      miss_abx_name
    } else if (input$treat_mis_site == 2){
      miss_presdate
    } else if (input$treat_mis_site == 3){
      miss_firstadm
    } else if (input$treat_mis_site == 4){
      miss_route
    } else if (input$treat_mis_site == 5){
      miss_dose
    } else if (input$treat_mis_site == 6){
      miss_freq
    } else if (input$treat_mis_site == 7){
      miss_dosecomple
    } else if (input$treat_mis_site == 8){
      miss_datestop
    } else if (input$treat_mis_site == 9){
      miss_otherD
    } else {
      miss_dentryInits
    }

  })

  # display queries
  output$treat_miss <- renderDataTable(
    treatCRF_missing()
  )


  # -----------------Day 28 CRF queries---------------------------------------------

  not_constReac <- reactive({
    if (input$site_d28qry1 == "Kilifi"){
      not_const %>% filter(site == "kilifi")
    } else if (input$site_d28qry1 == "Mbagathi"){
      not_const %>% filter(site == "mbagathi")
    } else {
      not_const
    }
  })

  output$d28_qry1 <- renderDataTable(
    not_constReac()
  )

  # download query 1 for day 28 CRF

  output$down_d28_qry1 <- downloadHandler(
    # function for file name
    filename = function(){
      paste0(input$site_d28qry1,"_day_28query_", ".csv")
    },
    content = function(file){
      write.csv(not_constReac(), row.names = F, file)
    }
  )


  # + Missing variables for day 28

  d28_missVarsReac <- reactive({
    if (input$site_d28qry1 == "Kilifi"){
      d28_missVars %>% filter(site == "kilifi")
    } else if (input$site_d28qry1 == "Mbagathi"){
      d28_missVars %>% filter(site == "mbagathi")
    } else {
      d28_missVars
    }
  })

  output$miss_d28vars <- renderDataTable(
    d28_missVarsReac()
  )

  # download handler
  output$down_miss_d28vars <- downloadHandler(
    # function for file name
    filename = function(){
      paste0(input$site_d28qry1,"missing_d28_queries_", ".csv")
    },
    content = function(file){
      write.csv(d28_missVarsReac(), row.names = F, file)
    }
  )

  # Download admission treatment CRF queries

  output$down_miss_treat <- downloadHandler(
    # function for file name
    filename = function(){
      paste0("admtreat_missing_query_",input$treat_mis_site, ".csv")
    },
    content = function(file){
      write.csv(treatCRF_missing(), row.names = F, file)
    }
  )


  # ++ Download documents

  # --- Kilifi Site
  output$kkl_protocol <- downloadHandler(
    filename = "NeoBAC protocol.docx",
    content = function(file) {
      file.copy("www/Kilifi/NeoBAC protocol V1.1_14  January  2020-Final.docx", file)
    }
  )

  output$kkl_enrolSOP <- downloadHandler(
    filename = "NeoBAC_enrollment_SOP_29-04-2020.docx",
    content = function(file) {
      file.copy("www/Kilifi/NeoBAC_enrollment_SOP_29-04-2020.docx", file)
    }
  )


  output$kkl_mitplan <- downloadHandler(
    filename = "KWTRP COVID-19 Mitigation Plans v1 03Sep2020.pdf",
    content = function(file) {
      file.copy("www/Kilifi/KWTRP COVID-19 Mitigation Plans v1 03Sep2020 signed.PDF", file)
    }
  )


  output$kkl_Homeloc <- downloadHandler(
    filename = "NeoBAC study Home Locator Form.docx",
    content = function(file) {
      file.copy("www/Kilifi/NeoBAC study Home Locator Form.docx", file)
    }
  )


  output$kkl_procflow <- downloadHandler(
    filename = "NEOBAC Process Flow.pdf",
    content = function(file) {
      file.copy("www/Kilifi/NEOBAC Process Flow.pdf", file)
    }
  )


  output$kkl_icf <- downloadHandler(
    filename = "NeoBAC study_ICF_ English.docx",
    content = function(file) {
      file.copy("www/Kilifi/Consent forms/NeoBAC study_ICF_ English.docx", file)
    }
  )


  output$kkl_infoSheet <- downloadHandler(
    filename = "KWTRP Risk information sheet related to COVID-19.pdf",
    content = function(file) {
      file.copy("www/Kilifi/Consent forms/KWTRP Risk information sheet related to COVID-19 final signed.pdf", file)
    }
  )

  output$kkl_d28 <- downloadHandler(
    filename = "NeoBAC day 28 CRF v1.02.docx",
    content = function(file) {
      file.copy("www/Kilifi/CRFs/NeoBAC day 28 CRF v1.02.docx", file)
    }
  )

  output$kkl_adm_crf <- downloadHandler(
    filename = "NeoBAC Admission Treatment v1.01.docx",
    content = function(file) {
      file.copy("www/Kilifi/CRFs/NeoBAC Admission Treatment v1.01.docx", file)
    }
  )


  output$kkl_adm_bldcul <- downloadHandler(
    filename = "Blood Culture Microbiology Request Form_ V1.01.docx",
    content = function(file) {
      file.copy("www/Kilifi/CRFs/Blood Culture Microbiology Request Form_ V1.01.docx", file)
    }
  )

}
