
# -------------------------------------------------------------------------

# source global
source("global.R") 

# shiny::reactiveFileReader(
#   intervalMillis = 10800000, # check for changes every 3 hours
#   session = session,
#   filePath = "global.R",
#   readFunc = source
# )

thematic::thematic_theme()

# dashboard header items
header_contents <- dashboardHeader(
  title = "Neonatal Bacteraemia Study Dashboard",
  dropdownMenu(type = "messages"),
  titleWidth = 400
)


# -------------------------------------------------------------------------


# dashboard side bar items
sidebar_contents <- dashboardSidebar(
  shiny::bootstrapLib(theme = mytheme),
  tags$style(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  sidebarMenu(
    # menu
    menuItem("Home",
             tabName = "home"),

    # NeOBAC-Clinical
    menuItem("Clinical Data",
             # Clinical reports
             tabName = "clinical",

             # flow chart section
             menuSubItem("Flow charts",
                         tabName = "flow_chart"),

             # admission treatment CRF
             menuSubItem("Admission Treatment",
                         tabName = "adm_treat"),

             # day 28 follow up reports
             menuSubItem("D28 follow up",
                         tabName = "d28_fup"),
             # clinical queries (Including REDCap backlog)
             menuSubItem("Queries",
                         tabName = "clinical_queries")
             ),

    # NeOBAC - Lab
    menuItem("Lab. Data",
             tabName = "lab",
             # laboratory reports
             menuSubItem("Lab. Reports",
                         tabName = "lab_reports"),

             # laboratory queries (including KIDMS backlog)
             menuSubItem("Lab. Queries",
                         tabName = "lab_queries")

             ),

    # CIN - Data
    menuItem("CIN-Data",
             tabName = "cin_data",

             # CIN data summary
             menuSubItem("Summary",
                         tabName = "cin_summary"),
             # some data checks vs NeOBAC data
             menuSubItem("CIN-NeOBAC data check",
                         tabName = "cin_neobac_query"),
             # data completeness
             menuSubItem("Data Completeness",
                         tabName = "cin_complete")

             ),
    menuItem("Study documents",
             tabName = "study_documents")

  )
)


# -------------------------------------------------------------------------

# dashboard body items
body_contentds <- dashboardBody(
  shiny::bootstrapLib(theme =  mytheme),


  # Home tab ------
  tabItems(
    tabItem(
      tabName = "home",
    fluidRow(
      valueBoxOutput("neon_adm", width = 3), # neonatal admissions
      valueBoxOutput("blood_cult", width = 2), # blood cultures
      valueBoxOutput("inpat_deaths", width = 2), # inpatient deaths
      valueBoxOutput("consent_d28", width = 2), # consented d28
      valueBoxOutput("alive_d28", width = 3)), # Alive at day 28

    fluidRow(
      tabsetPanel(
             tabPanel("Study accrued numbers",
                      box(width = 10,
                          plotOutput("cum_plot", height = "550px"),
                          title = "Study accrued numbers",status = "primary", solidHeader = T, collapsible = T),
                      box(width = 2,
                          radioButtons("plot_type", "Select plot type:",
                                       choices = c("Single Line Graphs" = 1,
                                                   "Multipe Line Graphs" = 0,
                                                   "Bar plot" = 2)),
                          tags$hr(),

                          radioButtons("site", "Site (single line graph):",
                                       choices = c("All sites" ,"Kilifi", "Mbagathi",
                                                   "Kiambu")),

                          downloadButton("down_cum_plot", "download plot"),
                          status = "primary", solidHeader = T, collapsible = T)
             ),

             tabPanel("Estimated target completion",
                      box(width = 12,
                          plotlyOutput("est_plot", height = "600px"),
                          title = paste("For current", average_daily, "enrolments per day across all sites, ",
                                        "estimated completion date is" , max(enrolment_rate$est_com_date)),
                          status = "primary", solidHeader = T, collapsible = T)
             ),
              tabPanel("Demographics and weekly blood cultures",
                       box(width = 6,
                           dataTableOutput("demo_table", height = 250),
                           title = "Demographic characteristics",
                           status = "primary", solidHeader = T, collapsible = T
                       ),
                       box(width = 2,
                           title = "download tables", status = "primary", solidHeader = T,
                           downloadButton("demog", "demographic table"),

                           tags$hr(),

                           downloadButton("wkly_cul", "culture table")
                           ),

                       box(width = 4,
                           dataTableOutput("wkly_clt", height = 250),
                           title = "Weekly blood cultures per site",
                           status = "primary", solidHeader = T, collapsible = T
                       )
              ),

             tabPanel("Screening Values",
                      box(width = 9,title = "Screening Values up to date",
                          solidHeader = T, status = "primary",
                          plotOutput("plt_scrn_tab")),
                      box(width = 9,title = paste("Screening Values for week",start_date_week, "to", report_date),
                          solidHeader = T, status = "primary",
                          plotOutput("plt_scrn_tab_week"))



             )
      )

    )

  ),

  # Flow charts tab----
  tabItem(tabName = "flow_chart",
          fluidRow(
            tabBox( width = 12,
              title = "Study flow charts", id = "flow_tab", height = "600px",
              tabPanel("Kilifi site",
                       grVizOutput('kilifi_diag', width = "100%", height = "600px")
                       ),
              tabPanel("Mbagathi site",
                       grVizOutput('mbagathi_diag', width = "100%", height = "600px")
                       ),
              tabPanel("Kiambu site",
                       grVizOutput('Kiambu_diag', width = "100%", height = "600px")
                       ),
              tabPanel("All sites",
                       grVizOutput('overal_diag', width = "100%", height = "600px")
                       )
            )
           )
          ),

  # admission treatment reports----
  tabItem(tabName = "adm_treat",
          fluidRow(
            box(width = 4,
                   dataTableOutput("abx_use", height = 500),
                title = "Antibiotic use during admission (all sites)",
                status = "primary", solidHeader = T, collapsible = T
                   ),
            box(width = 8,
                plotOutput("klf_abx", height = 500),
                title = "Kilifi admission antibiotic use",
                status = "primary", solidHeader = T, collapsible = T
                )
          )
          ),

  # day 28 follow up reports----
  tabItem(tabName = "d28_fup",
          fluidRow(
            box(width = 10,
                dataTableOutput("d28_sched"),
                title = "Day 28 scheduled and contact status",
                status = "primary", solidHeader = T, collapsible = T
                ),
            box(width = 2,status = "primary", solidHeader = T, collapsible = T,
                radioButtons("d28_site", label = "Select site to download",
                            choices = c("All sites", "Kilifi", "Mbagathi")),
                tags$hr(),
                downloadButton("d28_fup", "Download report")
                )
          )
          ),

  # Clinical queries reports ----
  tabItem(tabName = "clinical_queries",
          fluidRow(
            tabBox(width = 12,
                    title = "Clinical Queries", id = "dj28_query", height = "600px",

                    # tabPanel("REDCap backlog",
                    #     box(width = 10,
                    #         dataTableOutput("redcap_backlog"),
                    #         status = "primary", solidHeader = T, collapsible = T,
                    #         title = "REDCap Backlog -- Missing in KIDMS"
                    #         ),
                    #     box(width = 2,status = "primary", solidHeader = T, collapsible = T,
                    #         radioButtons("backlog_site", label = "Select site to download",
                    #                      choices = c("All sites", "Kilifi", "Mbagathi", "Kiambu"))
                    # )),

                    tabPanel("Missing CIN number",
                             box(width = 10,
                                 dataTableOutput("miss_cin"),
                                 status = "primary", solidHeader = T, collapsible = T,
                                 title = "Missing CIN number"
                                 ),

                             box(width = 2,status = "primary", solidHeader = T, collapsible = T,
                                 radioButtons("cin_site", label = "Select site to download",
                                              choices = c("All sites", "Kiambu", "Mbagathi"))
                             ),
                             box(width = 2,
                                 downloadButton("missing_cin_down",
                                                label = "Download Missing CIN"))
                    ),
                    tabPanel("Given abx ?",
                             box(width = 10,
                             dataTableOutput("miss_abx_info"),
                             status = "primary", solidHeader = T, collapsible = T,
                             title = "Missing information on whether abx were given"
                             ),
                             box(width = 2,status = "primary", solidHeader = T, collapsible = T,
                                 radioButtons("abx_site", label = "Select site to download",
                                              choices = c("All sites", "Kiambu", "Mbagathi"),
                                              selected = "All sites")
                             )
                    ),
                    tabPanel("d28 consent info",
                             box(width = 10,
                                 dataTableOutput("miss_const"),
                                 status = "primary", solidHeader = T, collapsible = T,
                                 title = "Missing information on d28 consent"
                             ),
                             box(width = 2,status = "primary", solidHeader = T, collapsible = T,
                                 radioButtons("cons_site", label = "Select site to download",
                                              choices = c("All sites", "Kilifi", "Mbagathi", "Kiambu"))
                             )

                    ),
                   tabPanel("Admission treatment CRF",
                            box(width = 9,
                                dataTableOutput("treat_miss"),
                                status = "primary", solidHeader = T, collapsible = T,
                                title = paste("Item missingness for admission treatment CRF")
                            ),
                            box(width = 3,status = "primary", solidHeader = T, collapsible = T,
                                selectInput("treat_mis_site", label = "Select Query & download",
                                             choices = c("Missingness query 1" = 1,
                                                         "Missingness query 2" = 2,
                                                         "Missingness query 3" = 3,
                                                         "Missingness query 4" = 4,
                                                         "Missingness query 5" = 5,
                                                         "Missingness query 6" = 6,
                                                         "Missingness query 7" = 7,
                                                         "Missingness query 8" = 8,
                                                         "Missingness query 9" = 9,
                                                         "Missingness query 10" = 10)),
                                tags$hr(),

                                downloadButton("down_miss_treat", "Download Query"),
                                title = "Missing Queries"
                            )

                   ),

                   # day 28 CRF queries
                   tabPanel("Day 28 CRF Queries",
                            fluidRow(
                              box(width = 10, status = "primary", solidHeader = T, title = "Day 28 follow up Query",
                                  dataTableOutput("d28_qry1")),
                              box(width = 2, status = "primary", solidHeader = T, title = "Site selector",
                                  radioButtons("site_d28qry1", "select site",
                                               choices = c("All sites", "Kilifi", "Mbagathi")),
                                  tags$hr(),
                                  downloadButton("down_d28_qry1", "Download"))
                            ),
                            # missing varibles
                            fluidRow(
                              box(width = 10, status = "primary", solidHeader = T, title = "Missing variables for D28 CRFs",
                                  dataTableOutput("miss_d28vars")),
                              box(width = 2, status = "primary", solidHeader = T,
                                  downloadButton("down_miss_d28vars", "Download"))
                            )

                   ),
                   tabPanel("Age Queries",
                            box(width = 9, status = "primary", solidHeader = T, title = "Neobac Age Queries",
                                dataTableOutput("neg_age_query")),
                            box(width = 3, status = "primary", solidHeader = T,
                                selectInput("select_site_age", label = "Select site", choices = site_id_data, selected = site_id_data[1]),
                                downloadButton("down_neg_age", "Download Neg Age")))
            )
          )
          ),
  # Laboratory reports ----
  tabItem(
    tabName = "lab_reports",
    fluidRow(
      tabBox(width = 12,
        tabPanel("Isolates",
                 box(width = 8,
                     dataTableOutput('bac_table', width = "80%", height = "300px"),
                     solidHeader = T, status = "primary", title = "Number of blood cultures, bac positive yes, no"),

                 br(),br(),

                 box(width = 4,
                   selectInput("isolate_type", label = "Select isolate", choices = c("all","Contaminant", "Pathogen")),
                   selectInput("site_isolate_tab", label = "Select site", choices = c("all","mbagathi", "kilifi", "kiambu")),
                   dateRangeInput(
                     inputId = "date_range",
                     label = "Select date range",
                     start = "2020-10-01" ,
                     end = Sys.Date(),
                     max = Sys.Date())),
                 box(width = 4, title = "Generate weekly report",
                     radioButtons('report_format', 'Document format', c('PDF', 'HTML', 'Word'),
                                  inline = TRUE, selected = "PDF"),
                     downloadButton("weekly_report", "Generate report")),

                 box(width = 10,
                     dataTableOutput('isolates_table', width = "100%", height = "300px"),
                     solidHeader = T, status = "primary", title = "All Isolates"),
                 box(width = 3,
                     downloadButton("isolate_down", label = "Downlaod Isolates")),
                 box(width = 10,
                     dataTableOutput('ast_results', width = "100%", height = "300px"),
                     solidHeader = T, status = "primary", title = "AST Results"),
                 box(width = 3,
                     downloadButton("ast_down", label = "Downlaod Asts"))
                 ),

        tabPanel("Contamination rate and Kidms entries",

      column(width = 12,
             box(width = 10,
                 dataTableOutput('kidms_samples', width = "100%", height = "300px"),
                 solidHeader = T, status = "primary", title = "Number of blood cultures in kidms")

      ),
      column(width = 12,
             box(width = 10,
                 plotOutput('conta_rate', width = "100%", height = "500px"),
                 solidHeader = T, status = "primary", title = "Contamination rate")

      )
    )
      )
    )
  ),

  # Laboratory queries ----

  tabItem(
    tabName = "lab_queries",
    fluidRow(

      column(width = 12,
             box(width = 10,
                 dataTableOutput('mis_blood_query', width = "100%", height = "300px"),
                 solidHeader = T, status = "primary", title ="Samples not received in KIDMS " ),
             box(width = 2,
                 selectInput("lab_site", label = "Select site", choices = c("kilifi", "mbagathi", "kiambu") )),
             box(width = 3,
                 downloadButton("mis_blood_down", label = "Dowload Samples not received"))



      ),
      column(width = 12,
             box(width = 10,
                 dataTableOutput('pnd_culture', width = "100%", height = "300px"),
                 solidHeader = T, status = "primary", title = "Samples pending culture results entries"),
             box(width = 3,

                 downloadButton("pnd_culture_down", label = "Download Pending culture")),

      ),
      column(width = 12,
             box(width = 10,
                 dataTableOutput('culture_growth', width = "100%", height = "300px"),
                 solidHeader = T, status = "primary", title = "Samples without isolate or final negative results"),
             box(width = 3,
                 downloadButton("mis_culture_down", label = "Missing culture results"))

      ),

      column(width = 12,
             box(width = 10,
                 dataTableOutput('pnd_isolate', width = "100%", height = "300px"),
                 solidHeader = T, status = "primary", title = "Samples with bac positive yes  but no isolate"),
             box(width = 3,
                 downloadButton("mis_isolate_down", label = "Missing isolate"))

      ),
      column(width = 12,
             box(width = 10,
                 dataTableOutput('culture_ast', width = "100%", height = "200px"),
                 solidHeader = T, status = "primary", title = "Culture without AST Results"),
             box(width = 3,
                 downloadButton("mis_ast_down", label = "Missing AST results"))

      ),

      column(width = 12,
             box(width = 10,
                 dataTableOutput('esbl_ast', width = "100%", height = "200px"),
                 solidHeader = T, status = "primary", title = "Culture without ESBL Results",
                 downloadButton("mis_esbl_down")),

      ),

      column(width = 12,
             box(width = 10,
                 dataTableOutput('culture_storage', width = "100%", height = "200px"),
                 solidHeader = T, status = "primary", title = "Isolates without LIMS Storage Position"),
             box(width = 3,
                 downloadButton("mis_store_down", label = "Isolate missing storage LIMS"))

      )
    )
  ),

  # CIN data summary -----
  tabItem(tabName = "cin_summary",
          h4("Note: The summary in this section is a mixture of NeoBAC-REDCap and CIN data as received"),
          tags$hr(),

          fluidRow(
            box(width = 6,
                dataTableOutput("cin_genderSite"), status = "primary", solidHeader = T, collapsible = T,
                title = "CIN data by site and gender (received data)"
                ),
            box(width = 6,
                dataTableOutput("cin_redcap"), status = "primary", solidHeader = T, collapsible = T,
                title = "CIN sites REDCap entries (NeOBAC REDCap)")
          ),

          fluidRow(
            box(width = 7,
                dataTableOutput("miss_CIN_IDs"), status = "warning", solidHeader = T, collapsible = T,
                title = "CIN-NeOBAC IDs without corresponding clinical data"
                ),
            box(width = 2,
                radioButtons("miss_IDs", label = "Select site",
                             choices = c("All sites" = 0, "Kiambu" = 1, "Mbagathi" = 2)),
                tags$hr(),
                downloadButton("down_missIDs", "Download IDs")

                ),

            box(width = 3, status = "warning", solidHeader = T, collapsible = F,
                title = "Summary of missing clinical data",
                dataTableOutput("cin_summary_miss_tab")

            )
          ),
          fluidRow(
            box(width = 10,
                dataTableOutput("paed_ids"), status = "success", solidHeader = T, collapsible = T,
                title = "CIN IDs in Paedtriatic Database - Clinical data source is CIN-PAED database"
            ),
            box(width = 2, status = "success", solidHeader = T,
                radioButtons("paed_IDs", label = "Select site",
                             choices = c("All sites" = 0, "Kiambu" = 1, "Mbagathi" = 2))

            )
          )



          ),

  # CIN-NeOBAC data checks
  tabItem(tabName = "cin_neobac_query",
          fluidRow(
            tabsetPanel(
              tabPanel("Query on child's gender",
                       box(width = 10,
                           dataTableOutput("qry_gender"),
                           title = "Differing Child's gender",status = "primary", solidHeader = T, collapsible = T),
                       box(width = 2,
                           radioButtons("site_gender", "Select site:",
                                        choices = c("All sites","mbagathi",
                                                    "kiambu")),
                           tags$hr(),

                           downloadButton("down_qry_gender", "download List"),
                           status = "primary", solidHeader = T, collapsible = T)
              ),


              tabPanel("Admission date query",
                       box(width = 10,
                           dataTableOutput("qry_admDate"),
                           title = "Difering date of admission",
                           status = "primary", solidHeader = T, collapsible = T
                       ),
                       box(width = 2,
                           radioButtons("site_adm", "Select site:",
                                        choices = c("All sites","mbagathi",
                                                    "kiambu")),
                           tags$hr(),

                           downloadButton("down_qry_admDate", "download List"),
                           status = "primary", solidHeader = T, collapsible = T)
              ),
              tabPanel("Date of birth query",
                       box(width = 10,
                           dataTableOutput("qry_dob"),
                           title = "Difering date of birth",
                           status = "primary", solidHeader = T, collapsible = T
                       ),
                       box(width = 2,
                           radioButtons("site_dob", "Select site:",
                                        choices = c("All sites","mbagathi",
                                                    "kiambu")),
                           tags$hr(),

                           downloadButton("down_qry_dob", "download List"),
                           status = "primary", solidHeader = T, collapsible = T)
              )


            )

          )
          ),

  # CIN data completeness -----
  tabItem(tabName = "cin_complete",
          fluidRow(theme = mytheme,
            tabsetPanel(
              tabPanel(title =  h5("CIN data completeness"),
                       box(width = 9,
                           dataTableOutput("miss_perc_cin"),
                           title = "CIN Summary Missing",
                           status = "primary", solidHeader = T, collapsible = F
                       ),

                       box(width = 3,
                           selectInput("miss_cin_site", "Select site", choices = site_cin, selected = site_cin[1]),
                           selectInput("miss_cin_var", "Select Missing variable", choices = miss_vars_cin, selected = miss_vars_cin[1]),
                           downloadButton("missing_cin_perc_down", label = "Download Summary table"),
                           downloadButton("missing_cin_vars_down", label = "Download Missing vars"),
                           title = "CIN Missing",
                           status = "primary", solidHeader = T, collapsible = F
                       ),

                       box(width = 9,
                           dataTableOutput("cin_miss_individual"),
                           title = "CIN Missing",
                           status = "primary", solidHeader = T, collapsible = F
                       ))
            )
          )

          ),

  tabItem(tabName = "study_documents",
              h4("Below you can find and download documents related to NeOBAC studye i.e. Study protocol, CRFs, SOPs etc."),
          tags$hr(),
          fluidRow(

            tabBox( width = 12,
                    title = "Study documents and SOPs", id = "study_docs", height = "600px",
                    tabPanel("Kilifi site",
                             downloadLink("kkl_protocol", "NeoBAC protocol V1.1_14  January  2020-Final"),
                             tags$hr(),
                             downloadLink("kkl_enrolSOP", "NeoBAC_enrollment_SOP_29-04-2020"),
                             tags$hr(),
                             downloadLink("kkl_mitplan", "KWTRP COVID-19 Mitigation Plans v1 03Sep2020"),
                             tags$hr(),
                             downloadLink("kkl_Homeloc", "NeoBAC study Home Locator Form"),
                             tags$hr(),
                             downloadLink("kkl_procflow", "NEOBAC Process Flow"),
                             tags$hr(),
                             downloadLink("kkl_icf", "NeoBAC study_ICF_ English"),
                             tags$hr(),
                             downloadLink("kkl_infoSheet", "KWTRP Risk information sheet related to COVID-19 final"),
                             tags$hr(),
                             downloadLink("kkl_d28", "NeoBAC day 28 CRF v1.02"),
                             tags$hr(),
                             downloadLink("kkl_adm_crf", "NeoBAC Admission Treatment v1.01"),
                             tags$hr(),
                             downloadLink("kkl_adm_bldcul", "Blood Culture Microbiology Request Form_ V1.01"),

                    ),
                    tabPanel("Mbagathi site",

                    ),
                    tabPanel("Kiambu site",

                    ),
                    tabPanel("Data management",

                    )
            )
          )
          )
  )
)

# combine to user interface using dashboard page
ui <- dashboardPage(header_contents,
              sidebar_contents,
              body_contentds,
              controlbar = dashboardControlbar(collapsed = T, skinSelector()),
              footer = dashboardFooter(left = "Data management and reporting tool",
                                       right = " Data sources : NeOBAC-CIN databases")
              )

# -------------------------------------------------------------------------


