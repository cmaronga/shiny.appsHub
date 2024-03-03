## The required packages 
library(shiny)
library(DT)
require(shinythemes)
library(stringr)

## Starting shiny server 
ui <- navbarPage(theme = shinytheme("united"),
  "Clinical QC Reports",
## 1. Participant Enrollment event ---------
    tabPanel("Participant Enrolment",
  titlePanel(h4("Participant Enrolment CRFs "),windowTitle = "enrollment"),
 sidebarLayout( 
    sidebarPanel(
    selectInput("site_enrol", "Select site:",
    choices = c("Kilifi", "Nairobi", "Migori", "Kampala","Blantyre","Dhaka","Karachi","Matlab",
                "Banfora"),selected = "Kampala",width = '200px',selectize = FALSE),
    width = 2),
    
## start of main panel 
    mainPanel(
      tabsetPanel(type="tabs",
            tabPanel("Participant Enrollment",
h5(strong("MISSING date of admission::needs update")),
dataTableOutput("admDate_missing"),  

tags$hr(),
h5(strong("Incomplete/missing enrolment anthropometry")),
dataTableOutput("enrol_missing"),

tags$hr(),
h5(strong("Missing temperature,respiration rate and/heart rate")),
dataTableOutput("missingVals"),

tags$hr(),
h5(strong("Improbable/impossible values for Heart rate")),
dataTableOutput("heart_rate"),

tags$hr(),
h5(strong("Improbable/impossible values for respiratory rate")),
dataTableOutput("resprate_rate"),

tags$hr(),
h5(strong("Improbable/impossible values for initial temperature value")),
dataTableOutput("temp_val"),

tags$hr(),
h5(strong("Improbable low oxygen saturation values")),
dataTableOutput("oxy_sat"),

tags$hr(),
h5(strong('Wrong date of birth_see calculated age')),
dataTableOutput("Age_b"),

tags$hr(),
h5(strong('Missing entry on D.O.B')),
dataTableOutput("admDOB"),

tags$hr(),
h5(strong('Missing initials for anthropometry measurer')),
dataTableOutput("anthropo"),

tags$hr(),
h5(strong('Missing value for the field "Previously admitted to hospital"')),
dataTableOutput("prevAdmin"),

tags$hr(),
h5(strong('Missing value for the field "Urine volume in last 24hrs?"')),
dataTableOutput("urineVol")

),
tabPanel("PHQ-9 Questionnaire",
         h5(strong("MISSING PHQ - 9 CRF at admission (phq date missing)")),
         dataTableOutput("mis_phq_adm"),
         tags$hr(),
         h5(strong("MISSING PHQ - 9 CRF at follow up (phq date missing)")),
         dataTableOutput("mis_phq_fup"),
         tags$hr(),
         h5(strong("PHQ - 9 CRF done before date of enrolment/admission")),
         dataTableOutput("wrong_phq_adm")),
tabPanel("Social Information",
         h5(strong("Patients with missing social information CRF and/or date interviwed")),
         dataTableOutput("mis_soc"),
         h5(strong("Missing caregiver information(muac,weight,height and/or both)")),
         dataTableOutput("caregiver_misanth")
         ),
  tabPanel("Hsehld Nutrition",
           h5(strong("Patients with missing household nutrition CRF/information")),
           dataTableOutput("mis_nutri")),
  tabPanel("Wealth Assessment",
           h5(strong("Patients with missing householdhold wealth assesment CRF/information")),
           dataTableOutput("mis_hwa"))
            
    ))
    
)
  ), ## End of tabPanel 1 "Participant enrollment event"

## 2. Daily review events -------
tabPanel("Daily Reviews",
         titlePanel(h4("Daily records")),
         sidebarLayout( 
           sidebarPanel(
             paste("daily record queries")
             ,width = 2),
           mainPanel(
             tabsetPanel(type="tabs",
                         tabPanel("Daily records date Queries",
                          h4("Introduction"),
                          h5("In this section, focus is put on dates of consective events of daily review and/or admission date. A difference of more than 36 hrs\n
                             , 0 hrs and a negative difference implies dates of one of the event is WRONG and needs checking/update"),
                          h4("Admission vs Daily review 1; Wrong dates"),
                          dataTableOutput("rev_1"),
                          downloadButton(
                            "downloadData1",
                            label = ("Download Daily Record QC")
                            
                          ),
                          h4("Daily review 1 vs Daily review 2; Wrong dates"),
                          dataTableOutput("rev_2"),
                          downloadButton(
                            "downloadData2",
                            label = ("Download Daily Record QC")
                            
                          ),
                          h4("Daily review 2 vs Daily review 3; Wrong dates"),
                          dataTableOutput("rev_3"),
                          downloadButton(
                            "downloadData3",
                            label = ("Download Daily Record QC")
                            
                          ),
                          h4("Daily review 3 vs Daily review 4; Wrong dates"),
                          dataTableOutput("rev_4"),
                          downloadButton(
                            "downloadData4",
                            label = ("Download Daily Record QC")
                            
                          ),
                          h4("Daily review 4 vs Daily review 5; Wrong dates"),
                          dataTableOutput("rev_5"),
                          downloadButton(
                            "downloadData5",
                            label = ("Download Daily Record QC")
                            
                          ),
                          h4("Daily review 5 vs Daily review 6; Wrong dates"),
                          dataTableOutput("rev_6"),
                          downloadButton(
                            "downloadData6",
                            label = ("Download Daily Record QC")
                            
                          ),
                          h4("Daily review 6 vs Daily review 7; Wrong dates"),
                          dataTableOutput("rev_7"),
                          downloadButton(
                            "downloadData7",
                            label = ("Download Daily Record QC")
                            
                          ),
                          h4("Daily review 7 vs Daily review 8; Wrong dates"),
                          dataTableOutput("rev_8"),
                          downloadButton(
                            "downloadData8",
                            label = ("Download Daily Record QC")
                            
                          ),
                          h4("Daily review 8 vs Daily review 9; Wrong dates"),
                          dataTableOutput("rev_9"),
                          downloadButton(
                            "downloadData9",
                            label = ("Download Daily Record QC")
                            
                          ),
                          h4("Daily review 9 vs Daily review 10; Wrong dates"),
                          dataTableOutput("rev_10"),
                          downloadButton(
                            "downloadData10",
                            label = ("Download Daily Record QC")
                            
                          ),
                          
                          h4("Daily review 10 vs Daily review 11; Wrong dates"),
                          dataTableOutput("rev_11"),
                          downloadButton(
                            "downloadData11",
                            label = ("Download Daily Record QC")
                            
                          ),
                          h4("Daily review 11 vs Daily review 12; Wrong dates"),
                          dataTableOutput("rev_12"),
                          downloadButton(
                            "downloadData12",
                            label = ("Download Daily Record QC")
                            
                          ),
                          h4("Daily review 12 vs Daily review 13; Wrong dates"),
                          dataTableOutput("rev_13"),
                          downloadButton(
                            "downloadData13",
                            label = ("Download Daily Record QC")
                            
                          ),
                          h4("Daily review 13 vs Daily review 14; Wrong dates"),
                          dataTableOutput("rev_14"),
                          downloadButton(
                            "downloadData14",
                            label = ("Download Daily Record QC")
                            
                          ),
                          h4("Daily review 14 vs Daily review 15; Wrong dates"),
                          dataTableOutput("rev_15"),
                          downloadButton(
                            "downloadData15",
                            label = ("Download Daily Record QC")
                            
                          ),
                          h4("Daily review 15 vs Daily review 16; Wrong dates"),
                          dataTableOutput("rev_16"),
                          downloadButton(
                            "downloadData16",
                            label = ("Download Daily Record QC")
                            
                          ),
                          h4("Daily review 16 vs Daily review 17; Wrong dates"),
                          dataTableOutput("rev_17"),
                          downloadButton(
                            "downloadData17",
                            label = ("Download Daily Record QC")
                            
                          ),
                          h4("Daily review 17 vs Daily review 18; Wrong dates"),
                          dataTableOutput("rev_18"),
                          downloadButton(
                            "downloadData18",
                            label = ("Download Daily Record QC")
                            
                          ),
                          h4("Daily review 18 vs Daily review 19; Wrong dates"),
                          dataTableOutput("rev_19"),
                          downloadButton(
                            "downloadData19",
                            label = ("Download Daily Record QC")
                            
                          ),
                          h4("Daily review 19 vs Daily review 20; Wrong dates"),
                          dataTableOutput("rev_20"),
                          downloadButton(
                            "downloadData20",
                            label = ("Download Daily Record QC")
                          )
                          
                            ),
                         tabPanel("Daily records other Queries",
                                  h5(strong("Daily review time seen--missing value")),
                                  dataTableOutput("time_seen"),
                                  
                                  tags$hr(),
                                  h5(strong("Missing field -- Oedema now, kindly update")),
                                  dataTableOutput("oedema_now"),
                                  
                                  tags$hr(),
                                  h5(strong("Missing field -- Oedema improving?, kindly update")),
                                  dataTableOutput("oedema_improve"),
                                  
                                  tags$hr(),
                                  h5(strong("Temperature >38 ° C in last 24h-- missing entry")),
                                  dataTableOutput("temp_mis"),
                                  
                                  tags$hr(),
                                  h5(strong("Temperature < 36° C in last 24h-- missing entry")),
                                  dataTableOutput("temp_mis2"),
                                  
                                  tags$hr(),
                                  h5(strong("NG tube in last 24h-- field missing entry")),
                                  dataTableOutput("ng_tube"),
                                  
                                  tags$hr(),
                                  h5(strong("Any EBM or breastfeeding in 24h-- field missing entry")),
                                  dataTableOutput("ebm_drev"),
                                  
                                  tags$hr(),
                                  h5(strong("ReSoMal in last 24h-- field missing entry")),
                                  dataTableOutput("resomal"),
                                  
                                  tags$hr(),
                                  h5(strong("ORS in last 24h-- field missing entry")),
                                  dataTableOutput("ors"),
                                  
                                  tags$hr(),
                                  h5(strong("IV fluids given in last 24h-- field missing entry")),
                                  dataTableOutput("iv_fluids"),
                                  
                                  tags$hr(),
                                  h5(strong("Blood transfusion given in last 24h-- field missing entry")),
                                  dataTableOutput("blood_trans")
                                  
                                  )
            )
             
           )
         )
),


### 3. Participant Discharge -----

tabPanel("Discharge & V.A",
         titlePanel(h4("Participant Discharge")),
         sidebarLayout( 
           sidebarPanel(
             paste("discharge CRF queries")
           ,width = 2),
           mainPanel(
             tabsetPanel(type="tabs",
                tabPanel("Participant discharge Queries",
                         h5(strong("Wrong discharge date and/or discharge date left hospital")),
                         dataTableOutput("dtlft_hosp"),
                         h5(strong("Absconded patients with MISSING date of absconding(kindly populate Date observations done in the database)")),
                         dataTableOutput("abs2"),
                         h5(strong("Patients indicated to have absconded, yet they have a discharge date")),
                         dataTableOutput("abs"),
                         h5(strong("Patients discharged same day of admission yet they have daily review data")),
                         dataTableOutput("disch_3"),
                         h5(strong("Incomplete/missing Discharge anthropometry")),
                         dataTableOutput("disch_athrop"),
                         h5(strong("Missing field entries for SaO2")),
                         dataTableOutput("sa02_miss",width = "75%")
                         ),
                tabPanel("Verbal autopsy queries",
                         h5(strong("Database - misclassified verbal autopsy CRFs")),
                         dataTableOutput("va_miscla"))
)
))
),## End of tabPanel Participant Discharge"


## 4. Participant follow up ------
tabPanel("Follow up",
         titlePanel(h4("D45,D90 and D180 follow up ")),
         sidebarLayout( 
           sidebarPanel(
             paste("follow up CRFs queries")
             ,width = 3),
           mainPanel(
             tabsetPanel(type="tabs",
                         tabPanel("Study Conclusion",
                                  h5(strong("Attended day 180, but missing study conclusion")),
                                  dataTableOutput("day180"),
                                  h5(strong("Participant did not complete follow up with missing reason")),
                                  dataTableOutput("rsn_noncomp"),
                                  h5(strong("Study conclusion with missing last vital status date")),
                                  dataTableOutput("v_status_date"),
                                  h5(strong("Study window CLOSED,complete study conclusion")),
                                  dataTableOutput("windo_w"),
                                  h5(strong("Children seen/contacted outside window close date")),
                                  dataTableOutput("vital"),
                                  h5(strong("Study conclusion WITHOUT Day 180 CRF filled")),
                                  dataTableOutput("study_conc"),
                                  h5(strong("For all MORTALITIES, death of death MUST be same as last vital status date")),
                                  dataTableOutput("death_vital"),
                                  h5(strong("Verbal Autopsy indicated as completed but missing in V.A data/information")),
                                  dataTableOutput("mis_va_info"),
                                  h5(strong("Mortalities with missing date of death")),
                                  dataTableOutput("msng_deathdate")
                                  ),
              tabPanel("Day 45 Follow Up",
                       h5(strong("Day 45 follow up date must be before study end")),
                       dataTableOutput("wrong_date"),
                       tags$hr(),
                       
                       h5(strong("Missing day 45 anthropometry")),
                       dataTableOutput("mis_45anthro"),
                       tags$hr(),
                       
                       h5(strong("Attended/seen day 45 but missing date seen")),
                       dataTableOutput("mis_dte_seen"),
                       tags$hr(),
                       
                       h5(strong("Length Measurement 1 and 2 differ greatly")),
                       dataTableOutput("height_ms"),
                       tags$hr(),
                       
                       h5(strong("Head Circ Measurement 1 and 2 differ greatly")),
                       dataTableOutput("headcirc"),
                       tags$hr(),
                       
                       h5(strong("Child in usual state of health?, 'No', missing length of illness")),
                       dataTableOutput("lngth_illness")
                       ),
              tabPanel("Day 90 Follow Up",
                       h5(strong("Missing day 90 anthropometry")),
                       dataTableOutput("mis_90anthro")),
              tabPanel("Day 180 Follow Up",
                       h5(strong("Day 180 follow up date must be before or same as study end")),
                       dataTableOutput("wrong_date_d180"),
                       tags$hr(),
                       h5(strong("Missing day 180 anthropometry")),
                       dataTableOutput("mis_180anthro"))
             )
           )
         )
         ),


### 5. Home Visit(Household survey) -----
tabPanel("Home Visits",
         titlePanel(h4("Household Survey")),
         sidebarLayout( 
           sidebarPanel(
             paste("Home visits CRF queries")
             ,width = 3),
           mainPanel(
             h5("Patients with missing home visit CRF"),
             dataTableOutput("household"),
             h5("Absconded patients with missing home visit CRF"),
             dataTableOutput("household_abs"),
             h5("Patients with incomplete GPS information(C.P and Ward)"),
             dataTableOutput("incop"),
             h5("Home visits done after study completion date::kindly check dates"),
             dataTableOutput("hme_vistend"),
             h5("Home visits done before discharge date::kindly check dates"),
             dataTableOutput("hme_vis_disch"),
              h5("provider took more than 10hsr to reach household, confirm value"),
             dataTableOutput("hme_prov_durat"),
             h5("How long it took to reach househol in minutes:: 60min equal 1 hr"),
             dataTableOutput("durat_mins"),
            h5("Highest level of education indicates primary, yet not able to read"),
             dataTableOutput("educ_level"),
            h5("Caregiver changed but did not indicate the previous"),
             dataTableOutput("care_giver_chang"),
            h5("Livestock ownership in the household"),
             dataTableOutput("lvstock")
           )
           
         )
), ## End of tabPanel Home Visit(Household survey)"

## 6. Other CHAIN CRFs ---------
tabPanel("Screening log",
         titlePanel(h4("Queries on CHAIN Screeding data")),
         sidebarLayout( 
           sidebarPanel(
             paste("screening log queries")
             ,width = 3),
           mainPanel(
             h5("Data Backlog for retrospective screening data upload into REDCap"),
             h6("NOTE:: Assumption is that there is no 'blank' week for all sites"),
             
             tags$hr(),
             dataTableOutput("all_wks_data"),
             
             tags$hr(),
             h5("Record IDs whose date interval does not follow the agreed format:: Monday to Sunday; aprox. 6 days interval"),
             dataTableOutput("date_intervals"),
            
              tags$hr(),
             h5("The sum of ELIGIBLE and NOT ELIGIBLE should be EQUAL to TOTAL SCREENED, the below violates that logic"),
             dataTableOutput("sum_eligs"),
             
             tags$hr(),
             h5("ELIGIBLE minus ENROLLED should equal to ELIGIBLE but NOT ENROLLED , the below violates that logic"),
             dataTableOutput("sum_notligs")
           )
         )
), ## End of tabPanel Other CHAIN CRFs"

## 7. Missed visits ------
tabPanel("Missed visits",
         titlePanel(h4("Follow up missed visits ")),
         sidebarLayout( 
           sidebarPanel(
             paste("Missed visits")
             ,width = 3),
           mainPanel(
             tabsetPanel(type="tabs",
              tabPanel("Patients about to miss visits",
              h5(strong("CHAIN follow up visits:: Participants MISSING VISITS in Less than 14 days")),
              tags$hr(),
              dataTableOutput("about_to_miss")),
              
              tabPanel("Day 45 missed",
                       h5("Day 45 follow up:: missed visits/unreachable/unable to contact"),
                       dataTableOutput("mis_d45")),
              tabPanel("Day 90 missed",
                       h5("Day 90 follow up::  missed visits/unreachable/unable to contact"),
                       dataTableOutput("mis_d90")),
              tabPanel("Day 180 missed",
                       h5("Day 180 follow up::  missed visits/unreachable/unable to contact"),
                       dataTableOutput("mis_d180"))
             )
           )
         )
),


### 8.Data curation queries-----

tabPanel("Curation Queries",
 h4("CLICK the LINK below to access the Data Curation Queries"), 
 tags$hr(), br(),
         
               h5(a( "Data Curation Queries LINK",
               href= "https://production.chainnetwork.org/reports/curation_queries/"))

         )

)## End of navigation bar//// No code beyond here





 
