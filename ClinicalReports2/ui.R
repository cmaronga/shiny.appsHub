## Load required packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

ui <- dashboardPage(skin = "purple",
                    
## ++ Shiny dashboard HEADER-------------------------------------------------------------------------------------
dashboardHeader(title = "CHAIN Study Reports",
                  dropdownMenu(type = "messages",
  messageItem(
    from = "",
    message = ""
  )), titleWidth = 300),
  

## ++ Shiny dashboard SIDEBAR -----------------------------------------------------------------------------------
dashboardSidebar(
    sidebarMenu(
       sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                    label = "Search..."),
      menuItem(h5("Home"), tabName = "home"),
      menuItem(h5("Summary"), tabName = 'summary'),
      menuItem(h5("Sub-studies"), 
               tabName = 'sub-std0',
               menuSubItem("General summaries",
                           tabName = "sub-std"),
               
               menuSubItem("BMC summaries",
                           tabName = "bmc_more"),
               
               menuSubItem("BMC Follow up alerts",
                           tabName = "bmc_fu_more"),
               
               menuSubItem("Immunology summaries",
                           tabName = "immunology_more"),
               
               menuSubItem("Immunology Queries",
                           tabName = "immuno_query"),
               
               menuSubItem("BMC Queries",
                           tabName = "bmc_query")
               ),
      menuItem(h5("Follow UPs"), tabName = 'fups')
    )
  ),
  
## ++ Shiny dashboard BODY -------------------------------------------------------------------------------------
dashboardBody(
   tabItems(
  tabItem(tabName = "home",
          fluidRow(
  valueBoxOutput("phase1", width = 3),
  valueBoxOutput("under6m", width = 3),
  valueBoxOutput("cp", width = 3),
  valueBoxOutput("op", width = 3)),
  
  tags$hr(),
  fluidRow(
    column(width = 7,
           box(width = NULL, dataTableOutput("summary"), title = "Summary of young infants data", solidHeader = T, 
               status = "success", collapsible = T)
      
    ),
    
    column(width = 5,
           box(width = NULL, dataTableOutput("commun"), title = "Summary of young infants outcome (died = yes)", solidHeader = T,
               status = "warning", collapsible = T)),
    column(width = 6,
           box(
             width = NULL, plotOutput("age_distr"), title = "Age distribution in days", solidHeader = T, collapsible = T,
             status = "primary"
           )),
    column(width = 6,
           box(
            width = NULL, plotOutput("age_distr2"), title = "Age distribution in months", solidHeader = T, collapsible = T,
            status = "primary" 
           ))
  )
  
),
tabItem(tabName = "summary",
        fluidRow(
infoBoxOutput("tot_disch", width = 3),
infoBoxOutput("tot_fup", width = 3),
infoBoxOutput("tot_readm", width = 3),
infoBoxOutput("tot_death", width = 3)
        )),

## ++ put extra sub-study summaries here
tabItem(tabName = "bmc_more",
        fluidRow(
                  column(width = 8,
                 box(width = NULL, dataTableOutput("sum_figs"), title = "Summary figures", status = "success", collapsible = T,
                   solidHeader = T
                 )),
                
                column(width = 4,
                 box(width = NULL,
    selectInput("site_id", "Select site to view", choices = c("Mbagathi", "Migori", "Kharadar","Civil"),
    selected = "Migori",width = '200px',selectize = FALSE), title = "Select site to View samples", status = "success", collapsible = T,
                   solidHeader = T
                 ))
        ),
        
        fluidRow(
          column(width = 6,
                 box(width = NULL,
                   dataTableOutput("brst_serum"), title = "Breask milk serum samples", status = "primary", collapsible = T,
                   solidHeader = T
                 )),
                column(width = 6,
                 
                 box(width = NULL,
                   dataTableOutput("whle_serum"), title = "Whole breast milk samples", status = "warning", collapsible = T,
                   solidHeader = T
                 )),
          
                column(width = 6,
                 box(width = NULL,
                   dataTableOutput("cell_pels"), title = "Cell Pellets samples", status = "primary", collapsible = T,
                   solidHeader = T
                 )),
          
                column(width = 6,
                 box(width = NULL,
                   dataTableOutput("stool"), title = "Stool samples", status = "warning", collapsible = T,
                   solidHeader = T
                 ))
        )),

tabItem(tabName = "immunology_more",
                  fluidRow(
valueBoxOutput("im_enrol", width = 3),
valueBoxOutput("im_ward", width = 3),
valueBoxOutput("im_op", width = 3),
valueBoxOutput("im_disch", width = 3)),
fluidRow(
  tabsetPanel(
    tabPanel("Recruitment summary"),
    tabPanel("Discharge summary"),
    tabPanel("Follow up visits"),
    tabPanel("Study Exits"),
    tabPanel("Daily records"),
    tabPanel("CRFs completeness"),
    tabPanel("Visualizations")
    
  )
)
        ),
tabItem(tabName = "immuno_query",
        fluidRow(
          tabsetPanel(
            tabPanel("Participant Enrolment"),
            tabPanel("PHQ 9 CRF"),
            tabPanel("Social information"),
            tabPanel("Daily records"),
            tabPanel("Discharge"),
            tabPanel("Day 45"),
            tabPanel("Day 90"),
            tabPanel("Day 180"),
            tabPanel("Study conclusion")
          )
        )),

tabItem(tabName = "bmc_fu_more",
        fluidRow(
          column(width = 4,
                        box(width = NULL,
                            selectInput("site_id_follow", "Select site to view", choices = c("Mbagathi", "Migori", "Kharadar","Civil"),
                                        selected = "Migori",width = '200px',selectize = FALSE), title = "Select site to view visit dates", status = "success", collapsible = T,
                            solidHeader = T
                        )),
          column(width = 4,
                 box(width = NULL,
                     selectInput("time_point", "Select time point", choices = c("d45", "d90", "d180"),
                                 selected = "d45",width = '200px',selectize = FALSE), title = "Select time point to view visit dates", status = "success", collapsible = T,
                     solidHeader = T
                 )),
          column(width = 12,
                 box(width = NULL,
                     dataTableOutput("bmc_alert"), title = "Follow up alerts", status = "primary", collapsible = T, solidHeader = T
                 )
        ),
        column(width = 12,
               box(width = NULL,
                   dataTableOutput("bmc_alert_follo"), title = "Weekly Follow up", status = "primary", collapsible = T, solidHeader = T
               )
        ))),


tabItem(tabName = "bmc_query",
        fluidRow(
          tabsetPanel(
            tabPanel("CHAIN BMC "),
            tabPanel("Daily records"),
            tabPanel("Discharge"),
            tabPanel("Day 45"),
            tabPanel("Day 90"),
            tabPanel("Day 180"),
            tabPanel("Study conclusion"),
            tabPanel("Verbal autopsy")
            
          )
        )),

tabItem(tabName = "fups",
        fluidRow(
          box(dataTableOutput("fup_sum"), title = "Follow up visits summary", status = "primary", collapsible = T,
              solidHeader = T)
        )),

  tabItem(tabName = "sub-std", 
          fluidRow(
infoBoxOutput("bmc", width = 3),
infoBoxOutput("immunology", width = 3),
infoBoxOutput("nutrition", width = 3),
infoBoxOutput("sugar", width = 3)),
tags$hr(),
fluidRow(
  column(width = 5,
         box(width = NULL,
           dataTableOutput("bmc_sum"), title = "BMC summaries", status = "primary", collapsible = T, solidHeader = T
         )
    
  ),
  
  column(width = 7,
         box(width = NULL,
             dataTableOutput("bmc_categ"), title = "BMC nutritional groups", status = "primary", collapsible = T, solidHeader = T)
         )
)
)
  )
))





