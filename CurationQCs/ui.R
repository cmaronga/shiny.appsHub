## Load required packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(lubridate)

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Data Curation Queries"
    
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(h4("Select Site Here"), tabName = "site_selector"),
      menuItem(h4("Dates"), tabName = "dates"),
      menuItem(h4("Anthropometry"), tabName = "anthro"),
      menuItem(h4("Clinical files"), tabName = "clinical"),
      menuItem(h4("Social/Household"), tabName = "social"),
      menuItem(h4("Follow up data"), tabName = "fup")
    )
    
  ),
  dashboardBody(
tags$head(
tags$style(
HTML('
h5{color:#0000FF;
font-size: 16px;
text-decoration: underline;
  }
')
)
),
    
    tabItems(
      tabItem(tabName = "site_selector",
              fluidRow(
                column(width = 4,
                       box(width = NULL, collapsible = T, status = "primary", solidHeader = T, title = "Select site to view Queries",
    selectInput("site_enrol", "Select site to proceed:",
    choices = c("Kilifi", "Nairobi", "Migori", "Kampala","Blantyre","Dhaka","Karachi","Matlab",
                "Banfora"),selected = "Kampala",width = '200px',selectize = FALSE)
              )))),
    
      tabItem(tabName = "dates",
              fluidRow(
                 h5(strong("Wrong day 45 or day 90 phone follow up dates")),
                 tags$hr(),
                 dataTableOutput("d45_d90Contact"),
                 
                 h5(strong("Wrong day 90 and/or day 180 visit dates")),
                       dataTableOutput("d90_d180visdates"),
                       tags$hr(),
                       
                       h5(strong("Possible wrong dates d180/study conclusion dates that needs checking")),
                       dataTableOutput("d180_last"),
                       tags$hr(),
                       
                       h5(strong("Possible wrong study conclusion dates for mortalities")),
                       dataTableOutput("mort_list"),
                       tags$hr(),
                       
                       h5(strong("Possible wrong dates:: Lost to follow up more than 180 days post discharge")),
                       dataTableOutput("lost_list"),
                       tags$hr(),
                       
                       h5(strong("Participants withdrew after study end (past 180 days since discharge)")),
                       dataTableOutput("withdrew_list"),
                       tags$hr(),
                       
                       h5(strong("Disparity in mortality dates in the verbal autopsy CRF")),
                       dataTableOutput("mort_dates"),
                       tags$hr(),
                       
                       h5(strong("Wrong study conclusion date for mortalities")),
                       dataTableOutput("mort_date_last"),
                       tags$hr(),
                       
                       h5(strong("Participants withdrew same day of discharge, kindly double checks dates")),
                       dataTableOutput("with_dsch"),
                       tags$hr(),
                       
                       h5(strong("Study conclusion completed before day 180 visit/contact date")),
                       dataTableOutput("d180_alive"),
                       tags$hr(),
                       
                       h5(strong("Wrong study conclusion dates for LOST TO FOLLOW UP Participants")),
                       dataTableOutput("wrong_ltfup_dates"),
                       tags$hr(),
                       
                       h5(strong("Participant completed study follow up but MISSING day 180 CRF")),
                       dataTableOutput("mis_d180crf"),
                       tags$hr(),
                       
                       h5(strong("Wrong study conclusion dates for day 180 phone follow ups")),
                       dataTableOutput("d180_phone"),
                 
                       h5(strong("Possible wrong dates:: Participants having less than 166 days of follow up")),
                       dataTableOutput("d180_less"),
                 
                       h5(strong("Missing dates query:: Participants discharge but missing date left hospital")),
                       dataTableOutput("mis_dlefthosp")
              )),
      tabItem(tabName = "anthro",
              fluidRow(
                tabsetPanel(
                  tabPanel("Admission and Discharge",
                 h5(strong("Admission vs discharge Weight:: weight gain/loss of 2 or more Kgs")),
                 dataTableOutput("adm_dischwhgt"),
                tags$hr(),
                 h5(strong("Admission height 1 vs height 2:: height difference 2 or more centimetres")),
                 dataTableOutput("adm_height"),
                 tags$hr(),
                 h5(strong("Admission vs discharge Height:: height gain/loss of 2 or more centimetres")),
                 dataTableOutput("adm_dischhgt"),
                 tags$hr(),
                 h5(strong("Admission muac 1 vs muac 2:: muac differebce of 1 or more centimetres")),
                 dataTableOutput("adm_muacdif"),
                 tags$hr(),
                 h5(strong("Admission vs discharge MUAC:: MUAC gain/loss of 2 or more centimetres")),
                 dataTableOutput("adm_dischmuac"),
                 tags$hr(),
                 h5(strong("Admission vs discharge Head Circumfrence:: Circumfrence gain/loss of 2.5 or more centimetres")),
                 dataTableOutput("adm_headcirc")
                 ),
                  tabPanel("Discharge and Day 45"),
                  tabPanel("Day 45 and Day 90"),
                  tabPanel("Day 90 and Day 180")
                )
              )),
      tabItem(tabName = "clinical"),
      tabItem(tabName = "social"),
      tabItem(tabName = "fup")
      
    )
    
    
  )
)