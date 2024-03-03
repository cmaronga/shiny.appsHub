require(shiny)
require(dplyr)
library(lubridate)
require(readr)
require(shinythemes)
require(tidyr)
require(ggplot2)
library(scales)
library(RColorBrewer)
library(stringr)

ui= navbarPage(theme = shinytheme("cerulean"),"Chain Network Lab Queries",
               
               tabPanel("Lab Data Queries",
                        
                        tabsetPanel(type = "tabs",
                                    tabPanel("REDCAP and KIDMS BACKLOGS ",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 selectInput("site",h3("Select  site:"), choices = c("Kilifi","Nairobi","Migori","Kampala","Blantyre","Karachi","Dhaka","Matlab","Banfora")
                                                 ),
                                                 
                                                 dateRangeInput(
                                                   inputId = "date_range",
                                                   label = h3("Date Range"),
                                                   start = "2016-11-01" ,
                                                   end = Sys.Date(),
                                                   max = Sys.Date()),
                                                 downloadButton(
                                                   "pend",label = ("Redcap BackLogs")),br(),br(),br(),
                                                 
                                                 downloadButton(
                                                   "pend1",label = ("KIDMS BackLogs"))
                                               ),
                                               mainPanel(
                                                 textOutput("text"),tags$head(tags$style("#text{color: blue;
                                                                                         font-size: 30px;
                                                                                         font-style: bold;
                                                                                         }")), br(),br(),
                                DT::dataTableOutput("backlog"),br(),
                                textOutput("text1"),tags$head(tags$style("#text1{color: blue;
                                                                         font-size: 30px;
                                                                         font-style: bold;
                                                                         }")),br(),
                                DT::dataTableOutput("backlog1")
                                                 )
                                
                                
                                
                                               )
                                
                                
                                
                        ),
                        tabPanel("Sample Processing Queries",
                                 sidebarLayout(
                                   sidebarPanel(
                                     downloadButton(
                                       "pend2",label = ("Require Aliquoting")),
                                     br(),br(),br(),
                                     downloadButton(
                                       "pend3",label = ("wrong Aliquots"))
                                   ),
                                   mainPanel(
                                     textOutput("text2"),tags$head(tags$style("#text2{color: blue;
                                                                              font-size: 30px;
                                                                              font-style: bold;
                                                                              }")), br(),br(),
                                DT::dataTableOutput("mis_aliquot"),br(),
                                
                                textOutput("text3"),tags$head(tags$style("#text3{color: blue;
                                                                         font-size: 30px;
                                                                         font-style: bold;
                                                                         }")), br(),br(),
                         DT::dataTableOutput("wrong_aliquot"),br()
                                )
                                )),
                        # tabPanel("Sample Date and Time Queries",
                        #          sidebarLayout(
                        #            sidebarPanel(
                        #              downloadButton(
                        #                "pend4",label = ("date collect query")),
                        #              br(),br(),br(),
                        #              downloadButton(
                        #                "pend5",label = ("time collect query"))
                        #            ),
                        #            mainPanel(
                        #              textOutput("text4"),tags$head(tags$style("#text4{color: blue;
                        #                                                       font-size: 30px;
                        #                                                       font-style: bold;
                        #                                                       }")), br(),br(),
                        #         DT::dataTableOutput("date_collect"),br(),
                        #         
                        #         textOutput("text5"),tags$head(tags$style("#text5{color: blue;
                        #                                                  font-size: 30px;
                        #                                                  font-style: bold;
                        #                                                  }")), br(),br(),
                        #  DT::dataTableOutput("time_collect"),br()
                        #         )
                        #         )),
                        
                        tabPanel("Sample Duration Queries",
                                 sidebarLayout(
                                   sidebarPanel(
                                     downloadButton(
                                       "negstore_dwn",label = ("Negative Duration to store ")),
                                     br(),br(),br(),
                                     downloadButton(
                                       "longstore_dwn",label = ("Longer time to store")),
                                     br(),br(),br(),
                                     downloadButton(
                                       "negrecp_dwn",label=("Negative Duration to reception")
                                     ),
                                     br(),br(),br(),
                                     downloadButton(
                                       "longrecp_dwn",label=("More than 120mins to reception")
                                     ),
                                     br(),br(),br(),
                                     downloadButton(
                                       "negproc_dwn",label=("Negative Duration to process")
                                     ),
                                     br(),br(),br(),
                                     downloadButton(
                                       "longproc_dwn",label=("More than 120mins to process")
                                     )
                                   ),
                                   mainPanel(
                                     textOutput("text_negstore"),tags$head(tags$style("#text_negstore{color: blue;
                                                                                      font-size: 30px;
                                                                                      font-style: bold;
                                                                                      }")), br(),br(),
                                DT::dataTableOutput("negstore"),br(),
                                
                                textOutput("text_longstore"),tags$head(tags$style("#text_longstore{color: blue;
                                                                                  font-size: 30px;
                                                                                  font-style: bold;
                                                                                  }")), br(),br(),
                         DT::dataTableOutput("longstore"),br(),
                         textOutput("text_negrecp"),tags$head(tags$style("#text_negrecp{color: blue;
                                                                         font-size: 30px;
                                                                         font-style: bold;
                                                                         }")), br(),br(),
                         DT::dataTableOutput("negrecp"),br(),
                         textOutput("text_longrecp"),tags$head(tags$style("#text_longrecp{color: blue;
                                                                          font-size: 30px;
                                                                          font-style: bold;
                                                                          }")), br(),br(),
                         DT::dataTableOutput("longrecp"),br(),
                         textOutput("text_negproc"),tags$head(tags$style("#text_negproc{color: blue;
                                                                         font-size: 30px;
                                                                         font-style: bold;
                                                                         }")), br(),br(),
                         DT::dataTableOutput("negproc"),br(),
                         textOutput("text_longproc"),tags$head(tags$style("#text_longproc{color: blue;
                                                                          font-size: 30px;
                                                                          font-style: bold;
                                                                          }")), br(),br(),
                         DT::dataTableOutput("longproc"),br()
                                )
                         ))
                        
                        
                         )
                        
                         ),
               #We define interface for haematology missing results
               tabPanel("Haematology CRF Query",
                        tabsetPanel(type="tabs",
                                    #we define table for haematology crfs
                                    tabPanel("CBC QUERIES",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 downloadButton("mis_cbc_dwn",label = ("CBC Missing Results")),br(),
                                                 downloadButton("cbc_status_dwn",label = ("CBC Status not indicated"))
                                               ),
                                               mainPanel(
                                                 textOutput("text_mis_cbc_results"),
                                                 tags$head(tags$style("#text_mis_cbc_results{color: blue;
                                                                      font-size: 30px;
                                                                      font-style: bold;}")), 
                                                 br(),br(),
                                                 DT::dataTableOutput("mis_cbc_results"),
                                                 textOutput("text_cbc_status"),
                                                 tags$head(tags$style("#text_cbc_status{color: blue;
                                                                      font-size: 30px;
                                                                      font-style: bold;}")),
                                                 br(),br(),
                                                 DT::dataTableOutput("cbc_status")
                                                 ))),
                                    tabPanel("Clinical Chemistry Queries",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 downloadButton("mis_chem_dwn",label = ("Chemistry Missing Results")),br(),
                                                 downloadButton("serum_status_dwn",label = ("Serum status not indicated"))),
                                               mainPanel(
                                                 textOutput("text_mis_chem_results"),
                                                 tags$head(tags$style("#text_mis_chem_results{color: blue;
                                                                      font-size: 30px;
                                                                      font-style: bold;}")),
                                                 br(),br(),
                                                 DT::dataTableOutput("mis_chem_results"),
                                                 br(),
                                                 textOutput("text_serum_status"),
                                                 tags$head(tags$style("#text_serum_status{color: blue;
                                                                      font-size: 30px;
                                                                      font-style: bold;}")),
                                                 br(),br(),
                                                 DT::dataTableOutput("serum_status")
                                                 )
                                                 )
                                               )
                                    ,
                                    tabPanel("BLOOD GAS QUERIES",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 downloadButton("mis_gas_dwn",label = ("Blood Gas Missing Results")),br(),
                                                 downloadButton("gas_status_dwn",label = ("Gas status not indicated"))
                                               ),
                                               mainPanel(
                                                 textOutput("text_mis_gas_results"),
                                                 tags$head(tags$style("#text_mis_gas_results{color: blue;
                                                                      font-size: 30px;
                                                                      font-style: bold;}")), 
                                                 br(),br(),
                                                 DT::dataTableOutput("miss_gas_results"),br(),
                                                 textOutput("text_gas_status"),
                                                 tags$head(tags$style("#text_gas_status{color: blue;
                                                                      font-size: 30px;
                                                                      font-style: bold;}")), 
                                                 br(),br(),
                                                 DT::dataTableOutput("gas_status"),br()
                                                 )
                                                 )
                                               ),
                                    tabPanel("Outlier Values",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 downloadButton("outlier_cbc_dwn",label = ("CBC Outliers")),
                                                 br(),br(),
                                                 downloadButton("outlier_chem_dwn",label = ("Chemistry Outliers")),
                                                 br(),br(),
                                                 downloadButton("outlier_gas_dwn",label = ("GAS Outliers"))),
                                               mainPanel(
                                                 textOutput("text_outlier_cbc_results"),
                                                 tags$head(tags$style("#text_outlier_cbc_results{color: blue;
                                                                      font-size: 30px;
                                                                      font-style: bold;}")), 
                                                 br(),br(),
                                                 DT::dataTableOutput("outlier_cbc_results"),br(),
                                                 
                                                 textOutput("text_outlier_chem_results"),
                                                 tags$head(tags$style("#text_outlier_chem_results{color: blue;
                                                                      font-size: 30px;
                                                                      font-style: bold;}")), 
                                                 br(),br(),
                                                 DT::dataTableOutput("outlier_chem_results"),br(),
                                                 
                                                 textOutput("text_outlier_gas_results"),
                                                 tags$head(tags$style("#text_outlier_gas_results{color: blue;
                                                                      font-size: 30px;
                                                                      font-style: bold;}")), 
                                                 br(),br(),
                                                 DT::dataTableOutput("outlier_gas_results"),br()
                                                 )
                                                 )
                                             
                                                 ),
                                    tabPanel("Results Entered as Characters",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 downloadButton("char_cbc_dwn",label = ("Character CBC results")),
                                                 br(),br(),
                                                 downloadButton("char_chem_dwn",label = ("Character CHEM results"))),
                                               mainPanel(
                                                 textOutput("text_char_cbc_results"),
                                                 tags$head(tags$style("#text_char_cbc_results{color: blue;
                                                                      font-size: 30px;
                                                                      font-style: bold;}")), 
                                                 br(),br(),
                                                 DT::dataTableOutput("char_cbc_results"),br(),
                                                 
                                                 textOutput("text_char_chem_results"),
                                                 tags$head(tags$style("#text_char_chem_results{color: blue;
                                                                      font-size: 30px;
                                                                      font-style: bold;}")), 
                                                 br(),br(),
                                                 DT::dataTableOutput("char_chem_results"),br()
                                                 
                                                 )
                                                 )
                                             
                                               )
                                    
                                             ) 
                                                 ),
              #We define the interface for lims sections
              tabPanel("LIMS Queries",
                        tabsetPanel(type = "tabs",
                                    
                                    #define table for lims query   
                                    tabPanel("Sample Not Stored Queries",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 downloadButton(
                                                   "notstored_dwn",label = ("Samples Not Stored query")),
                                                 br(),br(),br(),
                                                 downloadButton(
                                                   "wronglystored_dwn",label = ("Samples wrongly stored"))
                                               ),
                                               mainPanel(
                                                 textOutput("text_notstored"),tags$head(tags$style("#text_notstored{color: blue;
                                                                                                   font-size: 30px;
                                                                                                   font-style: bold;
                                                                                                   }")), br(),br(),
                                DT::dataTableOutput("notstored"),br(),
                                
                                textOutput("text_wronglystored"),tags$head(tags$style("#text_wronglystored{color: blue;
                                                                                      font-size: 30px;
                                                                                      font-style: bold;
                                                                                      }")), br(),br(),
                         DT::dataTableOutput("wronglystored"),br()
                                )
                                )),
                         #define isolate lims storage query
                         tabPanel("Isolates ",
                                  sidebarLayout(
                                    sidebarPanel(
                                      downloadButton(
                                        "isolatestorage_dwn",label = ("BloodIsolate_Miss_DateFrozen ")
                                      ),
                                      br(),br(),br(),
                                      downloadButton(
                                        "isolatestorage_rc_dwn",label = ("RectalIsolate_miss_DateFrozen")
                                      ) 
                                      
                                    ),
                                    mainPanel(
                                      textOutput("text_isolatestorage"),tags$head(tags$style("#text_isolatestorage{color: blue;
                                                                                                   font-size: 30px;
                                                                                                   font-style: bold;
                                                                                                   }")), br(),br(),
                                      DT::dataTableOutput("isolatestorage"),br(),
                                      
                                      textOutput("text_isolatestorage_rc"),tags$head(tags$style("#text_isolatestorage_rc{color: blue;
                                                                                                   font-size: 30px;
                                                                                                   font-style: bold;
                                                                                                   }")), br(),br(),
                                      DT::dataTableOutput("isolatestorage_rc"),br()
                                    )
                                    
                                  )
                                  
                         ),
                         
                         #define isolate lims storage query
                         tabPanel("Isolates Storage ",
                                  sidebarLayout(
                                    sidebarPanel(
                                      downloadButton(
                                        "isolatestorage_dwn1",label = ("BloodIsolate not stored")
                                      ),
                                      br(),br(),br(),
                                      downloadButton(
                                        "isolatestorage_rc_dwn1",label = ("RectalIsolate not stored")
                                      ) 
                                      
                                    ),
                                    mainPanel(
                                      textOutput("text_isolatestorage1"),tags$head(tags$style("#text_isolatestorage1{color: blue;
                                                                                                   font-size: 30px;
                                                                                                   font-style: bold;
                                                                                                   }")), br(),br(),
                                      DT::dataTableOutput("isolatestorage1"),br(),
                                      
                                      textOutput("text_isolatestorage_rc1"),tags$head(tags$style("#text_isolatestorage_rc1{color: blue;
                                                                                                   font-size: 30px;
                                                                                                   font-style: bold;
                                                                                                   }")), br(),br(),
                                      DT::dataTableOutput("isolatestorage_rc1"),br()
                                    )
                                    
                                  )
                                  
                         )
                         
                        )       
                        
                        ),
               
               ###define user interface section of isolates tabs
               tabPanel("ISOLATE Queries",
                        tabsetPanel(type = "tabs",
                                    
                                    #define table for isolate query   
                                    tabPanel("Swab Culture Query",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 downloadButton(
                                                   "R2_not_cultured_dwn",label = ("R2 not cultured")),
                                                 br(),br(),br(),
                                                 downloadButton(
                                                   "R2_miss_culture_dwn",label = ("R2 missing culture")),
                                                 br(),br(),br(),
                                                 downloadButton(
                                                   "R2_miss_esbl_dwn",label = ("Missing ESBL Results")),
                                                 br(),br(),br(),
                                                 downloadButton(
                                                   "R2_drug_dwn",label = ("Missing zone size")),
                                                 br(),br(),br(),
                                                 downloadButton(
                                                   "R2_sense_dwn",label = ("Missing Sensitivity"))
                                               ),
                                               mainPanel(
                                                 textOutput("text_R2_not_cultured"),tags$head(tags$style("#text_R2_not_cultured{color: blue;
                                                                                                         font-size: 30px;
                                                                                                         font-style: bold;
                                                                                                         }")), br(),br(),
                                
                                                 DT::dataTableOutput("R2_not_cultured"),br(),
                                                 
                                                 textOutput("text_R2_miss_culture"),tags$head(tags$style("#text_R2_miss_culture{color: blue;
                                                                                                         font-size: 30px;
                                                                                                         font-style: bold;
                                                                                                         }")), br(),br(),
                         
                                                 DT::dataTableOutput("R2_miss_culture"),br(),
                                                 
                                                 textOutput("text_R2_miss_esbl"),tags$head(tags$style("#text_R2_miss_esbl{color: blue;
                                                                                                      font-size: 30px;
                                                                                                      font-style: bold;
                                                                                                      }")), br(),br(),
                         
                                                 DT::dataTableOutput("R2_miss_esbl"),br(),
                                                 
                                                 textOutput("text_R2_drug"),tags$head(tags$style("#text_R2_drug{color: blue;
                                                                                                 font-size: 30px;
                                                                                                 font-style: bold;
                                                                                                 }")), br(),br(),
                         
                                                 DT::dataTableOutput("R2_drug"),br(),
                                                 
                                                 textOutput("text_R2_sense"),tags$head(tags$style("#text_R2_sense{color: blue;
                                                                                                  font-size: 30px;
                                                                                                  font-style: bold;
                                                                                                  }")), br(),br(),
                         
                                                 DT::dataTableOutput("R2_sense"),br()
                                                 
                                                 )
                                                 )
                                    ),
                                    tabPanel("R2 zone size",
                                       sidebarLayout(
                                        sidebarPanel(
                                          downloadButton(
                                            "R2_zone_size_char",label = ("R2 Zone size as character")),
                                          br(),br(),br()
                                          )
                                        ,
                                        mainPanel(
                                          textOutput("text_R2_txt"),tags$head(tags$style("#text_R2_txt{color: blue;
                                                                                                         font-size: 30px;
                                                                                                  font-style: bold;
                                                                                                  }")), br(),br(),
                                
                                          DT::dataTableOutput("text_R2_tbl"),br()
                                        )
                                         
                                       ) 
                                         
                                         
                                       )      
                                             
                                             
                                             
                                    
                                 )
                                    ),
               ###define user interface section of Participants to be replaced tabs
               tabPanel("Participants Missing Samples",
                        tabsetPanel(type = "tabs",
                                    #define table for Participant Replacement   
                                    tabPanel("Community Participants Missing Samples",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 downloadButton(
                                                   "replace_dwn",label = ("cp missing sample")),
                                                 br(),br(),br(),
                                                 downloadButton(
                                                   "replace_aliq2dwn",label = ("cp miss aliquot2")),
                                                 br(),br(),br(),
                                                 downloadButton(
                                                   "replace_aliq3_dwn",label = ("cp miss aliquot3")),
                                                 br(),br(),br()),
                                               mainPanel(
                                                 textOutput("text_replace"),tags$head(tags$style("#text_replace{color: blue;
                                                                                                 font-size: 30px;
                                                                                                 font-style: bold;
                                                                                                 }")), br(),br(),
                                
                                                 DT::dataTableOutput("replace"),br(),
                                                 
                                                 textOutput("text_replace_aliq2"),tags$head(tags$style("#text_replace_aliq2{color: blue;
                                                                                                       font-size: 30px;
                                                                                                       font-style: bold;
                                                                                                       }")), br(),br(),
                         
            DT::dataTableOutput("replace_aliq2"),br(),
            
            textOutput("text_replace_aliq3"),tags$head(tags$style("#text_replace_aliq3{color: blue;
                                                                  font-size: 30px;
                                                                  font-style: bold;
                                                                  }")), br(),br(),
                         
            DT::dataTableOutput("replace_aliq3"),br()
     
           
)
)
),
#define interface for other time point participants missing samples
tabPanel("Participants Missing Samples",
         sidebarLayout(
           sidebarPanel(
             downloadButton(
               "replace_part_dwn",label = ("missing_samples")),
             br(),br(),br()
               ),
           mainPanel(
             textOutput("text_replace_part"),tags$head(tags$style("#text_replace_part{color: blue;
                                                                  font-size: 30px;
                                                                  font-style: bold;
                                                                  }")), br(),br(),
               
             DT::dataTableOutput("replace_part"),br()
               )
           
           )
           )
)
)
)
