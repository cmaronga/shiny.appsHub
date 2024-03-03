## Load required packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(ggthemes)
library(reactable)
library(reactR)
library(ggbeeswarm)

ui <- dashboardPage(skin = "green",
                    
## ++ Shiny dashboard HEADER-------------------------------------------------------------------------------------
dashboardHeader(title = "CHAIN main cohort data Visualization",
                  dropdownMenu(type = "messages",
  messageItem(
    from = "",
    message = ""
  )), titleWidth = 400),
  

## ++ Shiny dashboard SIDEBAR -----------------------------------------------------------------------------------
dashboardSidebar(
    sidebarMenu(
      menuItem(h5("Home"), 
               tabName = "home"),
      
      menuItem(h5("Clinical data"), 
               tabName = 'clinical',
               menuSubItem("Admission illness", 
                           tabName = "adm_illness"),
               
               menuSubItem("Discharge illness",
                           tabName = "disch_illness"),
               
               menuSubItem("Previous health",
                           tabName = "prev_health")),
      
      menuItem(h5("Demographics"), 
               tabName = 'demographs',
               menuSubItem("Anthropometry",
                           tabName = "anthropometry"),
               
               menuSubItem("Dates",
                           tabName = "dates"),
               
               menuSubItem("Demographics",
                           tabName = "demographics"),
               
               menuSubItem("Outcome",
                           tabName = "outcome")
               ),
      menuItem(h5("Social & Household"), tabName = 'soc_hse',
               
               menuSubItem("Caregiver data",
                           tabName = "care_giver")),
      
      menuItem(h5("Lab Data"), tabName = "lab_data",
               
               menuSubItem("CBC and Biochemistry", 
                           tabName = "biochem")),
      
      menuItem(h5("Binary Variables Summary"), tabName = "bin_vars",
               
               menuSubItem("Admission", 
                           tabName = "adm_tpoint"),
               menuSubItem("Discharge", 
                           tabName = "disch_tpoint"),
               menuSubItem("Follow up", 
                           tabName = "fup_tpoint"),
               menuSubItem("Social Variables", 
                           tabName = "soc_tpoint"),
               menuSubItem("Household hwa variables", 
                           tabName = "hwa_tpoint"),
               menuSubItem("Home visit hsurv", 
                           tabName = "hsurv_tpoint")
               ),
      menuItem(h5("Note to file/ QC comments"), tabName = "ntfile")
    )
  ),
  
## ++ Shiny dashboard BODY -------------------------------------------------------------------------------------
dashboardBody(
  ## all the body items go into the tabitems
  tabItems(
    tabItem(tabName = "home",
            fluidRow(box(tags$h5("Welcome to CHAIN Data Curation Visualization Dashboard. CHAIN Curation is a monthly cycle of data cleaning and preparation for analysis"), 
                         width = 12, status = "primary", solidHeader = T, background = "blue")),
            fluidRow(
              tabsetPanel(
                tabPanel("General checks",
                         column(width = 10,
                                box(width = NULL, solidHeader = T, status = "primary", title = "General checks and outcome summary",
                                    dataTableOutput("gen_checks"))),
                         column(width = 2,
                                box(width = NULL, solidHeader = T, status = "primary",
                                    selectInput("general_checks", "View summary by:", choices = c("General", "FUP"),
                                                selected = "General")))),
                tabPanel("anthropometry checks",
                         column(width = 10,
                                box(width = NULL, solidHeader = T, status = "primary", title = "Anthropometry completeness",
                                    dataTableOutput("anthro_checks"))),
                         column(width = 2,
                                box(width = NULL, solidHeader = T, status = "primary",
                                    selectInput("athro_checks", "View summary by:", choices = c("admission", "discharge"),
                                                selected = "admission")))),
                         
                tabPanel("clinical summaries",
                         column(width = 12,
                                box(width = NULL, solidHeader = T, status = "primary", title = "summary of key clinical variables",
                                    dataTableOutput("clin_sum")))),
                tabPanel("CBC/Biochemistry",
                         column(width = 6,
                                box(width = NULL, solidHeader = T, status = "primary",
                                    radioButtons("cbc_biochem", "Select results type:", choices = list("CBC" = 1, "Biochemistry" = 2),
                                                 selected = 1))),
                         column(width = 6,
                                box(width = NULL,solidHeader = T, status = "primary",
                                    selectInput("cbc_timepoint", "select timepoint", 
                                                choices = c("admission", "discharge"), selected = "admission"))),
                         column(width = 12,
                                box(width = NULL, solidHeader = T, status = "primary", title = "CBC and Biochemistry completeness",
                                    dataTableOutput("cb_biochem"))))
              )
            )
            ),
    tabItem(tabName = "adm_illness",
            fluidRow(
              
                    tabsetPanel(
                        tabPanel("General Graphs",
                                 column(width = 10,
                                        box(width = NULL,
                                            plotlyOutput("adm_illness_bar"),solidHeader = T, status = "primary")),
                                 column(width = 2,
                                        box(width = NULL,solidHeader = T, status = "primary",
                                            selectInput("adm_illness_options", "Clinical", choices = nms_clin_adm_illness, selected = "" ),
                                            tags$hr(),
                                            selectInput("adm_illness_options_split", "Split variable", choices =c("categ_enrol", "site", "read_died"), selected = "categ_enrol" )
                                        )),
                                 
                                 column(width = 10,
                                        box(width = NULL,
                                            plotlyOutput("split_bar_adm_illness"), solidHeader = T, status = "primary"))
                                 
                        )
                    )
            )),
    tabItem(tabName = "disch_illness", 
            fluidRow(
                 tabsetPanel(
                    tabPanel("General Graphs",
                             column(width = 10,
                                    box(width = NULL,
                                        plotlyOutput("disch_illness_bar"),solidHeader = T, status = "primary")),
                             column(width = 2,
                                    box(width = NULL,solidHeader = T, status = "primary",
                                        selectInput("disch_illness_options", "Clinical", choices = nms_clin_disch_illness, selected = "" ),
                                        tags$hr(),
                                        selectInput("disch_illness_options_split", "Split variable", choices =c("categ_enrol", "site", "read_died"), selected = "categ_enrol" )
                                    )),
                             
                             column(width = 10,
                                    box(width = NULL,
                                        plotlyOutput("disch_bar2"), solidHeader = T, status = "primary"))
                             
                    )
                )
            )
),
tabItem(tabName = "prev_health", "Previous health vizs",
            fluidRow(
              tabsetPanel(
                tabPanel("General Graphs",
                         column(width = 10,
                                box(width = NULL,
                                    plotlyOutput("prev_gen_bar"),solidHeader = T, status = "primary")),
                         column(width = 2,
                                box(width = NULL,solidHeader = T, status = "primary",
                                    selectInput("prev_options", "Clinical", choices = nms_clini_prev_health, selected = "adm_neuro" ),
                                    tags$hr(),
                                    selectInput("prev_options_split", "Split variable", choices =c("categ_enrol", "site", "read_died"), selected = "categ_enrol" )
                                  )),
                         
                         column(width = 10,
                                box(width = NULL,
                                    plotlyOutput("split_bar_prev_hlth"), solidHeader = T, status = "primary"))
                         
                )
                
              ))),
    tabItem(tabName = "anthropometry",
            fluidRow(
              tabsetPanel(
                tabPanel("General Graphs",
              column(width = 10,
                     box(width = NULL,
                         plotlyOutput("scatter"),solidHeader = T, status = "primary")),
              column(width = 2,
                     box(width = NULL,solidHeader = T, status = "primary",
                         selectInput("muac_x", "X axis variable", choices = c(names(anthro_data))[-1], selected = "muac_adm"),
                         tags$hr(),
                         selectInput("muac_y", "Y axis variable", choices = c(names(anthro_data))[-1], selected = "muac_disch"))),
              
              column(width = 10,
                     box(width = NULL,
                         plotlyOutput("anth_hists"), solidHeader = T, status = "primary")),
              column(width = 2,
                     box(width = NULL, solidHeader = T, status = "primary",
                         selectInput("anth_hist_x", "choose X axis", choices = c(names(anthro_data))[-1], selected = "adm_agemons")))
            ),
            tabPanel("By ABC/site",
            column(width = 9,
                     box(width = NULL,
                         plotlyOutput("abc_anth_box"), solidHeader = T, status = "primary")),
              column(width = 3,
                     box(width = NULL, solidHeader = T, status = "primary",
                         selectInput("abc_anth_y", "choose Y axis", choices = c(names(anthro_data))[-1], 
                                     selected = "adm_agemons"), dataTableOutput("abc_anth_box_sum"))),
              column(width = 9,
                     box(width = NULL,
                         plotlyOutput("site_anth_box", height = "600px"), solidHeader = T, status = "primary")),
              column(width = 3,
                     box(width = NULL, solidHeader = T, status = "primary",
                         selectInput("site_anth_y", "choose Y axis", choices = c(names(anthro_data))[-1], 
                                     selected = "adm_agemons"), dataTableOutput("site_anth_box_sum")))
            ),
            
            tabPanel("By Outcome",
                     
              column(width = 10,
                     box(width = NULL,
                         plotOutput("out_anth_box"), solidHeader = T, status = "primary")),
              column(width = 2,
                     box(width = NULL, solidHeader = T, status = "primary",
                         selectInput("abc_anth_y_out", "choose Y axis", choices = c(names(anthro_data))[-1], 
                                     selected = "adm_agemons"))),
              column(width = 10,
                     box(width = NULL,
                         plotOutput("site_anth_box_out"), solidHeader = T, status = "primary")),
              column(width = 2,
                     box(width = NULL, solidHeader = T, status = "primary",
                         selectInput("site_anth_y_out", "choose Y axis", choices = c(names(anthro_data))[-1], 
                                     selected = "adm_agemons")))
                     ),
            tabPanel("By time point",
             column(width = 10,
                     box(width = NULL,
                         plotOutput("abc_dotplot", height = "600px"), solidHeader = T, status = "primary")),
              column(width = 2,
                     box(width = NULL, solidHeader = T, status = "primary",
                         selectInput("facet_by", "View graph by:", choices = c("categ_enrol","site"), 
                                     selected = "categ_enrol"))))
            
            ))),
    
    ## Visualizing dates data
    tabItem(tabName = "dates", 
            fluidRow(
              column(width = 10,
                     box(width = NULL, solidHeader = T, status = "danger",
                         plotlyOutput("dates_plots"))),
              column(width = 2,
                     box(width = NULL, solidHeader = T, status = "danger",
                         selectInput("date_y_axis", "choose Y axis", choices = c(names(dates))[-c(1,18,19)], 
                                     selected = "date_disch"),
                         tags$hr(),
                         selectInput("date_x_axis", "choose X axis", choices = c(names(dates))[-c(1,18,19)], 
                                     selected = "date_adm"))),
              column(width = 10,
                     box(width = NULL, solidHeader = T, status = "danger",
                         dataTableOutput("dates_sum"))
                     ),
              column(width = 2,
                     box(width = NULL, solidHeader = T, status = "primary",
                      selectInput("sum_dates", "summary by:", choices = c("site","categ_enrol"), 
                                     selected = "site"))))),
    tabItem(tabName = "demographics",
            fluidRow(
              column(width = 10,
                     box(width = NULL, solidHeader = T, status = "primary",
                         plotlyOutput("demogra_stack", height = "600px"))),
              column(width = 2,
                     box(width = NULL, solidHeader = T, status = "primary",
                         selectInput("demograph_vars", "summarise/view by:", 
                                     choices = c("categ_enrol", "site"), selected = "categ_enrol"))),
              column(width = 12,
                     box(width = NULL, solidHeader = T, status = "primary",
                         dataTableOutput("demogra_sumtab")))
            )),
    tabItem(tabName = "outcome",
            fluidRow(
            tabsetPanel(
              tabPanel("Summary tables",
              column(width = 10,
                     box(width = NULL, solidHeader = T, status = "primary",title = "summary table 1 (mortalities and readmissions)",
                         dataTableOutput("outc_sum"))),
              column(width = 2,
                     box(width = NULL, status = "primary", solidHeader = T,
                         selectInput("table_var", "summarise table by:", choices = c("categ_enrol", "site"), selected = "site"))),
              column(width = 10,
                     box(width = NULL, solidHeader = T, status = "primary",title = "summary table 2(lost to follow up & withdrawals)",
                         dataTableOutput("outc_sum2")))
                       ),
              tabPanel("Graphical vizs",
                       fluidRow(
                         column(width = 12,
                                 box(width = NULL, status = "success", solidHeader = T,
                                     plotlyOutput("length_stay", height = "550px")))
                       )),
           tabPanel("Where Died",
              column(width = 10,
                     box(width = NULL,
                         plotlyOutput("whr_died_bar", height = "650px"),solidHeader = T, status = "primary")),
              column(width = 2,
                     box(width = NULL,solidHeader = T, status = "primary",
                         selectInput("plot_typ", "Plot type", choices = c("stack plot" = 1, "Bar plot" = 2), selected = 1),
                         tags$hr(),
                         selectInput("view_by", "View plot by", choices = c("categ_enrol", "site"), selected = "site")))
            )
            
              
            )
)),
    tabItem(tabName = "readm_outcome", "Readmission outcomes"),
    tabItem(tabName = "fup_readm", "Follow up readmissions"),
    tabItem(tabName = "care_giver", 
      fluidRow(
            tabsetPanel(
                  tabPanel("General Graphs",
                           column(width = 10,
                                  box(width = NULL,
                                      plotlyOutput("care_bar"),solidHeader = T, status = "primary")),
                           column(width = 2,
                                  box(width = NULL,solidHeader = T, status = "primary",
                                      selectInput("care_options", "Clinical", choices = nms_care_giver, selected = "" ),
                                      tags$hr(),
                                      selectInput("care_options_split", "Split variable", choices =c("categ_enrol", "site", "read_died"), selected = "categ_enrol" )
                                  )),
                           
                           column(width = 10,
                                  box(width = NULL,
                                      plotlyOutput("care_bar2"), solidHeader = T, status = "primary"))
                           
                  ),
                  tabPanel("PHQ - 9 Scores",
                           column(width = 10,
                           box(width = NULL,
                                      plotlyOutput("phq_scplot", height = "500px"),solidHeader = T, status = "primary")),
                           column(width = 2,
                                  box(width = NULL, solidHeader = T, status = "danger",
                                      selectInput("phq_plot_type", "Select Plot type", choices = list("Scatter plot"=1, "Density plot"=2, "Boxplot"=3), selected = 1)
                                      ),
                                  box(width = NULL,solidHeader = T, status = "primary",
                                      selectInput("phq_scatter", "view scatter/boxplot by:", choices = c("categ_enrol", "site"), 
                                                  selected = "categ_enrol" )
                                  ),
                              box(width = NULL,solidHeader = T, status = "success",
                              selectInput("phq_density", "view density plot by:", choices = list("all sites"= 1, "Kilifi", "Nairobi", "Migori",
                                                                                                 "Kampala", "Bantyre", "Banfore", "Karachi", "Matlab", "Dhaka"), 
                                            selected = "categ_enrol" )
                                  )
                                  )
                  ) ## visualise phq - 9 scores
                )
        
        
)),
    tabItem(tabName = "biochem",
              fluidRow(
              tabsetPanel(
                tabPanel("General Graphs",
                         column(width = 10,
                                box(width = NULL,
                                    plotlyOutput("chem_gen_hist"),solidHeader = T, status = "primary")),
                         column(width = 2,
                                box(width = NULL,solidHeader = T, status = "primary",
                                    selectInput("cbc_chem_select", "Select Biochemistry or CBC", choices = c("cbc", "chem"), selected = "cbc" ),
                                    tags$hr(),
                                    conditionalPanel(
                                      
                                      condition = "input.cbc_chem_select == 'chem'",
                                      
                                      selectInput("time_point_cbc_chem", "Time Point",choices = c("adm", "disch",  "comm"), selected = "adm"),
                                      tags$hr(),
                                      selectInput("chem_options", "Types of Biochemistry",choices = chem_vars, selected = "calcium"
                                                  
                                      )),
                                    conditionalPanel(
                                      
                                      condition = "input.cbc_chem_select == 'cbc'",
                                      
                                      selectInput("time_point_cbc_chem", "Time Point",choices = c("adm", "disch", "readm", "comm","day45", "day90", "day180"), selected = "adm"),
                                      tags$hr(),
                                      selectInput("cbc_options", "CBC Results",choices = cbc_vars, selected = "haemoglobin"
                                                  
                                      )),
                              
                                    tags$hr(),
                                    selectInput("chem_options_split", "Split variable", choices =c("categ_enrol", "site", "read_died"), selected = "categ_enrol" )
                                )),
                         
                         column(width = 10,
                                box(width = NULL,
                                    plotlyOutput("split_hist_prev_chem"), solidHeader = T, status = "primary"))
                         
                )
              ))
            
            
            ),
    tabItem(tabName = "adm_tpoint"),
    tabItem(tabName = "disch_tpoint"),
    tabItem(tabName = "fup_tpoint"),
    tabItem(tabName = "hwa_tpoint"),
    tabItem(tabName = "hwa_tpoint"),
    tabItem(tabName = "hsurv_tpoint"),
    tabItem(tabName = "ntfile",
            fluidRow(
              reactableOutput("notefile")
            ))
  )
))




