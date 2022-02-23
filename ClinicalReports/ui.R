## The required packages -------
library(shiny)
library(DT)
require(shinythemes)
library(leaflet)
library(plotly)

## Starting shiny ui --------
ui <- navbarPage(theme = shinytheme("cerulean"),
  "Clinical Reports",
    tabPanel("Enrollment summaries",
  titlePanel(h4("Recruitment summaries"),windowTitle = "enrollment"),
 sidebarLayout( 
    sidebarPanel(
                 dateRangeInput("date", "Select Date ranges", start = as.Date("2016-11-01"), end = NULL,
                format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                language = "en", separator = "up to ", width = '250px'),
                tags$hr(),
    selectInput("site", "Select site to view",
    choices = list("All sites","Kilifi", "Nairobi", "Migori", "Kampala","Blantyre","Dhaka","Karachi",
                   "Matlab","Banfora"),
    selected = "All sites",width = '200px',selectize = FALSE),
    tags$hr(),
    width = 3),
    
## start of main panel -------
    mainPanel(
      tabsetPanel(type="tabs",
            tabPanel("Summary",
                     br(),
              plotOutput("linegraph"),
              br(),
              downloadButton(
                "summary_table",
                label = ("Download summary table")
              ),
              br(),br(),
              dataTableOutput("results"),
              br(),br(),
              plotOutput("cleveland_dot")
              ),
            tabPanel("ABC Groups",
                  br(),
                 plotOutput("enrol"),
                 tags$hr(),
                 plotOutput("enrol2"),
            tags$hr(),
            h4("Classification of all mortalities based on where the participant died"),
            br(),
            dataTableOutput("mort_sums"),
            br(),
            h4("Summary of events of readmissions across all sites"),
            tags$hr(),
            dataTableOutput("readmit_sums"),
            h4("Fatal readmissions (included with deaths) and non-fatal readmissions across all sites"),
            h5("NOTE: This is a count of readmitted children and NOT readmission events"),
            tags$hr(),
            dataTableOutput("readm_types")
            ),
            tabPanel("Enrolment targets",
             h4("Below are the total enrollments across all sites"),
             br(),
              dataTableOutput("enrolNums"),
             br(),tags$hr(),
             h4("Enrolments checks and target for each nutritional category upto 31-Jan-2019"),
             dataTableOutput("targets_abs")
             ),
  tabPanel("Age distribution",
           br(),
           plotOutput("age_p1"),
           br(),br(),
           dataTableOutput("table1"),
           br(),br(),
           plotOutput("age_p2")),
  
  tabPanel("Monthly Enrollments",
           h4("Sites' overal monthly enrolments"),
           dataTableOutput("monthly"),
           downloadButton(
             "monthly_enrolment",
             label = ("Download table")
             
           ),
tags$hr(),
h4("Overal monthly enrolment by age category"),
           dataTableOutput("age_cat_monthly"),
tags$hr(),
h4("Sites' monthly enrolment stratified by nutritional category"),
dataTableOutput("nutr_monthly")

           )
    ))
    
)
  ), ## End of tabPanel 1 "Enrollment status"

### Tab panel 2 Follow up summaries -----

tabPanel("Follow up status",
         titlePanel(h4("Follow up visits")),
         sidebarLayout( 
           sidebarPanel(
     sliderInput("bins", label = "Select bins for dotplot", min = 0.3, 
                 max = 1, value = 0.7,step = 0.1),
     width = 3),
           mainPanel(
             tabsetPanel(type="tabs",
                tabPanel("Visit summaries",
                          br(),br(),
                         tableOutput("disch_sumary"),
                         br(),
                          dataTableOutput("followup"),
                         tags$hr(),
                         h5(strong("Follow up Visits; where seen")),
                         plotOutput("where_seen"),
                         tags$hr(),
                         plotOutput("where_seen2"),
                         tags$hr(),
                         plotOutput("where_seen3")
                         ),
                tabPanel("Day 180 Grouping",
                         br(),br(), 
                         plotOutput("day180"),
                         br(),br(),
                         h4("Summary of Nutritional status at Study end"),
                         dataTableOutput("day180_nut_grp")),
                tabPanel("Study End status",
                         br(),
                         plotOutput("study_end_status"),
                         tags$hr(),
                         h4("Lost to follow up and withdrawals by site"),
                         tableOutput("lost_patients")),
                tabPanel("Length of hospital stay",
                         br(),
                         h4("Duration of patients' hospital admission in days"),
                         tags$hr(),
                         dataTableOutput("hosp_stay"),
                         br(),
                         h4("Patients still in Hospital"),
                         tags$hr(),
                         plotOutput("still_inhosp"))
)
))
),## End of tabPanel 2 "Follow up summaries"

### Tab panel 3 Growth Indicators -----
tabPanel("Survival curves",
         titlePanel(h4("Survival curves and z scores")),
         sidebarLayout( 
           sidebarPanel(
radioButtons("risk.table", label = "See risk table?",choices = list("Yes"=1,"No"=0), selected = 0),
             tags$hr(),
radioButtons("confi.interv", label = "See Confidence interval?",choices = list("Yes"=1,"No"=0), selected = 0),

width = 2),
           mainPanel(
             h5("Survival curves for all deaths(inpatient and outpatients)"),
             plotOutput("surv_plot_all"),
              tags$hr(),
             h5("Survival curves for post discharge mortalities"),
             plotOutput("surv_plot_pdm")
           )
)
),
### Tab panel 5 General Reports -----
tabPanel("Study visits",
         titlePanel(h4("Future scheduled visits ")),
         sidebarLayout( 
           sidebarPanel(
radioButtons("sched", label = "select period",choices = list("2 weeks"=14,"1 month"=30,"3 months"=90), selected = 14),

tags$hr(),

textInput("record_id", label = "Enter Patient ID", value = "10001001"),

width = 3),
           mainPanel(
             tabsetPanel(type="tabs",
              tabPanel("Scheduled day45",
                       br(),br(),
                    dataTableOutput("day45")),
              tabPanel("Scheduled day90",
                       br(),br(),
                       dataTableOutput("day90")),
              tabPanel("Scheduled day180",
                       br(),br(),
                       dataTableOutput("day_180")),
              tabPanel("Window dates",
                       h5("Type in the patient ID to view all window open, scheduled dates and window close dates"),
                       br(),
                       dataTableOutput("window_dates",width = "75%")),
              tabPanel("Missed visits",
                       dataTableOutput("missed_all",width = "75%"))
             )
           )
         )
), ## End of tabPanel 5 "General Reports"

## Tab panel 6 Lab samples inventory ---------
tabPanel("Data completeness",
         titlePanel(h4("Availability and Completeness")),
         sidebarLayout( 
           sidebarPanel(
             paste("Missing data per CRF"),
             width = 2),
           mainPanel(
             tabsetPanel(type="tabs",
                         tabPanel("Participant Enrolment",
                                  h5("Primary Carers Health Questionnaire"),
                                  dataTableOutput("phq_miss"),
                                  h5("Social Information"),
                                  dataTableOutput("soc_miss"),
                                  h5("Household Nutrition"),
                                  dataTableOutput("nutri_miss"),
                                  h5("Household Wealth Assessment"),
                                  dataTableOutput("hwa_miss")),
                         tabPanel("Anthropometry ")
                        
             )
             
             
           )
         )
), ## End of tabPanel 6 "Lab samples inventory"

### Graphs and Charts ------
tabPanel("Visualizations",
         fluidPage( 
           fluidRow(
             column(12,
             h4("MUAC Distribution"),
             plotOutput("muac_dot")),
             br(),br(),
             h4("Average profile plot"),
             plotOutput("avg_prof_plot")
           )
         )
),

tabPanel("About Dashboard",                          # Information about data availability for this dashboard.
         h4("Datasets are auto updated after every 30 minutes, 24 hrs daily."),
         br(),
         h5("Any questions or comments can be sent to:"),
         h5("1. Narshion Ngao - Senior Data Manager: ") ,
         h5(a("nngao@kemri-wellcome.org", href="nngao@kemri-wellcome.org")),
         tags$hr(),
         h5("2. Christopher Maronga - Clinical Data Manager: ") ,
         h5(a("cmaronga@kemri-wellcome.org", href="cmaronga@kemri-wellcome.org")),
         tags$hr(),
         h5("3. Eric Owino - Lab Data Manager: ") ,
         h5(a("eowino@kemri-wellcome.org", href="cmaronga@kemri-wellcome.org")),
         tags$hr())


# 
# navbarMenu("More tabs with reports",
#            tabPanel("Annual report template",
#                     sidebarLayout( 
#                       sidebarPanel(
#                         dateRangeInput("rpt_date", "Select report date", start = as.Date("2016-11-01"), end = NULL,
#                                        format = "yyyy-mm-dd", startview = "month", weekstart = 0,
#                                        language = "en", separator = "up to ", width = '250px'),
#                         width = 3),
#                       
#                       ## start of main panel -------
#                       mainPanel(
#                         tabsetPanel(type="tabs",
#                                     tabPanel("Annual report slides and summary tables",
#                                              tags$hr(),
#                                              plotOutput("slide2_nutri"),
#                                              h5("CHAIN cohort data Summary"),
#                                              tags$hr(),
#                                              dataTableOutput("chrt_summary"),
#                                              tags$hr(),
#                                              h5("Weekly enrolments"),
#                                              plotOutput("weekly_cum"))
#                         )
#                         
#                       )
#                       
#                     )),
#            
#            tabPanel("Weekly screening info."),
#            
# )

)## End of navigation bar//// No code beyond here







 
