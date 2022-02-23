## Load required packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(lubridate)
library(haven)

## ++ Data curatio ID's (filter data based on curation record ID's)
filtered_data <- read_csv("master_recordIDs_list_v6.csv")

## start of server function
server <- function(input, output, session){

  chaindata <- reactiveFileReader(
  intervalMillis = 600000, # check for changes every 10 mins
  session = session,
  filePath = "chaindata.csv",
  readFunc = readr::read_csv
)
  
curation_data <- reactive({
chaindata() %>% 
  filter(record_id %in% c(filtered_data$record_id))  ## create curation data (filtered) 
})
  

## Read in Athropometry data
anthro_data <- reactiveFileReader(
  intervalMillis = 600000, # check for changes every 10 mins
  session = session,
  filePath = "CHAIN_anthropometry.dta",
  readFunc = haven::read_stata
)

## small file processing
anthropometry <- reactive({
anthro_data() %>%
  select(-site) %>%
  left_join(mutate_at(chaindata(), vars(record_id), as.numeric) %>% 
              select(record_id, site, length_stay), by = "record_id")  ## I had no other choice but to have it here
})


## DATES ------
output$d45_d90Contact <- renderDataTable({
  curation_data() %>% 
  filter(as_date(fu90_contactdate) <= as_date(fu45_contactdate), site == input$site_enrol) %>% 
  select(record_id, site, disch_date,fu45_contactdate, fu90_contactdate) %>%   
  mutate(
    comments = str_c("Participant contacted for d90 before d45?")
  )
}, rownames = F)
  


output$d90_d180visdates <- renderDataTable({
curation_data() %>% 
  mutate(
    dif_dates = as_date(fu180_visdate) - as_date(fu90_visdate)
  ) %>% 
  select(record_id, site, fu90_visdate, fu180_visdate, dif_dates) %>% 
  filter(dif_dates <= 0, site == input$site_enrol) %>%  
  select(-dif_dates) %>% 
  mutate(
    comments = str_c("came for d180 before attending d90, check/correct the dates")
  )
}, rownames = F)


output$d180_last <- renderDataTable({
curation_data() %>% 
  filter(!is.na(fu180_visdate)) %>% 
  filter(as_date(fu180_visdate) != as_date(sc_lastvitalstat_date), site == input$site_enrol) %>% 
  select(record_id, site, fu180_visdate, sc_lastvitalstat_date) %>%   
  mutate(
    comments = str_c("Date seen during d180 visit SHOULD be equal to the last date on study conclusion")
  )
  
}, rownames = F)

## possible wrong dates for mortalities
output$mort_list <- renderDataTable({
curation_data() %>% 
  filter(sc_nofucomp_resn == 3) %>% 
  select(record_id, site, adm_date, disch_date, fu45_visdate, fu90_visdate, 
         fu180_visdate, fu180_contactdate,sc_lastvitalstat_date, va_death_date, va_deathdate) %>% 
  mutate(
    pd_time = difftime(as_date(sc_lastvitalstat_date), as_date(disch_date), units = c("days")),
    comments = str_c("Participant died more than 180 days post - discharge? Kindly confirm last date, and key in date participant died")
  ) %>% 
  filter(pd_time > 180, site == input$site_enrol) %>% 
    select(-c(pd_time, fu45_visdate,fu90_visdate, va_death_date, va_deathdate))
}, rownames = F)

## Lost more than 180 days post discharge
output$lost_list <- renderDataTable({
  curation_data() %>% 
  filter(sc_nofucomp_resn == 1)%>% 
  select(record_id, site, adm_date, disch_date, fu45_visdate, fu90_visdate, 
         fu180_visdate, fu180_contactdate,sc_lastvitalstat_date) %>% 
  mutate(
    pd_time = difftime(as_date(sc_lastvitalstat_date), as_date(disch_date), units = c("days")),
    comments = str_c("lost to follow up more than 180 days post - discharge? Kindly confirm participants last date")
  ) %>% 
  filter(pd_time > 180, site == input$site_enrol) %>% select(-c(pd_time, fu45_visdate, fu90_visdate))
}, rownames = F)

## Participants withdrew after study end
output$withdrew_list <- renderDataTable({
curation_data() %>% 
  filter(sc_nofucomp_resn == 4)%>% 
  select(record_id, site, adm_date, disch_date, fu45_visdate, fu90_visdate, 
         sc_lastvitalstat_date) %>% 
  mutate(
    pd_time = difftime(as_date(sc_lastvitalstat_date), as_date(disch_date), units = c("days")),
    comments = str_c("Participant withdrew after study end (180 days)")
  ) %>% 
  filter(pd_time > 180, site == input$site_enrol) %>% select(-pd_time)
}, rownames = F)

## verbal autopsy dates not tallying
output$mort_dates <- renderDataTable({
  curation_data() %>% 
    filter(sc_nofucomp_resn == 3) %>% 
  filter(!is.na(va_death_date), !is.na(va_deathdate), site == input$site_enrol) %>% 
  filter(va_death_date != va_deathdate) %>% 
  select(record_id, site,va_death_date, va_deathdate) %>% 
  mutate(
    comments = str_c("'Date of death' is NOT same as 'What was the date of death' for the same participant (VA CRF)")
  )
}, rownames = F)

## Study conclusion date must be equal to date of death
output$mort_date_last <- renderDataTable({
curation_data() %>% 
    filter(sc_nofucomp_resn == 3, site == input$site_enrol) %>% 
  filter(sc_lastvitalstat_date != va_death_date|sc_lastvitalstat_date != va_deathdate) %>% 
  select(record_id, site, adm_date, disch_date, sc_lastvitalstat_date, va_death_date, va_deathdate) %>% 
  mutate(
    comments = str_c("Last study date MUST be equal to date of death for all mortalities")
  )
}, rownames = F)

## participants withdrew same day of discharge
output$with_dsch <- renderDataTable({
curation_data() %>% 
  filter(sc_nofucomp_resn == 4, site == input$site_enrol)%>% 
  select(record_id, site, adm_date, disch_date, fu45_visdate, fu45_contactdate, fu90_visdate,fu90_contactdate, 
         fu180_visdate, fu180_contactdate,sc_lastvitalstat_date) %>% 
  mutate(
    pd_time = difftime(as_date(sc_lastvitalstat_date), as_date(disch_date), units = c("days"))
  ) %>% 
  filter(pd_time == 0) %>%select(-c(pd_time,fu90_visdate,fu90_contactdate,fu180_visdate)) %>%  
  mutate(
    comments = str_c("Patients withdrew same day of discharge, kindy note this to avoid future flaging")
  )
}, rownames = F)


## Study conclusion done before day 180  visit/contact data
output$d180_alive <- renderDataTable({
curation_data() %>% 
  filter(sc_part_fucomplete == 1, site == input$site_enrol) %>% 
  select(record_id, site, adm_date, disch_date, fu45_visdate, fu45_contactdate, fu90_visdate, fu90_contactdate,
         fu180_visdate, fu180_contactdate, sc_lastvitalstat_date) %>% 
  mutate(
    dif_lastdateVis = difftime(as_date(sc_lastvitalstat_date), as_date(fu180_visdate), units = c("days")),
    dif_lastdatePhone = difftime(as_date(sc_lastvitalstat_date), as_date(fu180_contactdate), units = c("days"))
  ) %>% 
  filter(dif_lastdateVis < 0| dif_lastdatePhone < 0) %>% 
  select(record_id, site, adm_date, disch_date,fu180_visdate, fu180_contactdate,sc_lastvitalstat_date) %>% 
  mutate(
    comments = str_c("Study conlusion Done before day 180 visit/contacted")
  )
}, rownames = F)

## Study conclusion date must be the most recent vist for lost to follow up
output$wrong_ltfup_dates <- renderDataTable({
  curation_data() %>%  
  select(record_id, site, adm_date, disch_date, disch_date_left_hosp, fu45_visdate, fu45_contactdate,
         fu90_visdate, fu90_contactdate, fu180_visdate, fu180_contactdate,
         sc_lastvitalstat_date, sc_part_fucomplete, sc_nofucomp_resn) %>% 
  filter(sc_nofucomp_resn == 1, site == input$site_enrol, record_id != 10001179) %>% ## Kilifi used home visit date
  rowwise() %>% 
  mutate(
    recent_visit = max(adm_date, disch_date, disch_date_left_hosp, fu45_visdate, fu45_contactdate,fu90_visdate, 
                       fu90_contactdate, fu180_visdate, fu180_contactdate, na.rm = T),
    recentVslastD = recent_visit == sc_lastvitalstat_date
  ) %>% 
  filter(recentVslastD == FALSE) %>% 
  select(record_id, site, adm_date, disch_date, disch_date_left_hosp, fu45_visdate, fu45_contactdate,
         fu90_visdate, fu90_contactdate,
         sc_lastvitalstat_date) %>% 
  mutate(
    comments = str_c("Lost to follow up study conclusion date MUST be the most recent visit attend/telephone call")
  )
}, rownames = F)

## Participant completed day 180 but missing day 180 CRFS
output$mis_d180crf <- renderDataTable({
curation_data() %>% 
  filter(sc_part_fucomplete == 1, is.na(fu180_visdate), is.na(fu180_contactdate), site == input$site_enrol) %>% 
  select(record_id, site, adm_date, fu180_visdate, fu180_contactdate, sc_part_fucomplete,fu180_lstknown_status,
         sc_lastvitalstat_date) %>% 
  mutate(
    comments = str_c("Participant completed study follow up but missing day 180 CRF details")
  )
}, rownames = F)


## day 180 phone follow ups
output$d180_phone <- renderDataTable({
  curation_data() %>% 
  filter(sc_part_fucomplete == 1, !is.na(fu180_contactdate), site == input$site_enrol) %>% 
  select(record_id, site, adm_date, disch_date, disch_date_left_hosp, fu45_visdate, fu45_contactdate,
         fu90_visdate, fu90_contactdate, fu180_visdate, fu180_contactdate, fu180_lstknown_status, sc_lastvitalstat_date, sc_nofucomp_resn) %>% 
  rowwise() %>% 
  mutate(
    last_date = max(adm_date, disch_date, disch_date_left_hosp, fu45_visdate, fu45_contactdate,
         fu90_visdate, fu90_contactdate, fu180_visdate, fu180_contactdate, na.rm = T)
  ) %>% 
  filter(as_date(last_date) != as_date(sc_lastvitalstat_date)) %>% select(-c(disch_date_left_hosp, fu45_visdate, fu45_contactdate,
         fu90_visdate, fu90_contactdate, fu180_visdate, last_date,sc_nofucomp_resn)) %>% 
  mutate(
    comments = str_c("Date contacted to confirm vital status SHOULD BE the last vitals date in the study conclusion")
  )
}, rownames = F)
  

## ++ Participants seen before day 180 window opens
output$d180_less <- renderDataTable({
curation_data() %>% 
  filter(sc_part_fucomplete == 1, site == input$site_enrol) %>% 
  select(record_id, site, disch_date, fu180_visdate, fu180_contactdate,sc_lastvitalstat_date) %>% 
  mutate(
    study_time = difftime(as_date(sc_lastvitalstat_date), as_date(disch_date), units = c("days"))
  ) %>% filter(study_time < 166) %>% 
  filter(as_date(fu180_contactdate) < ymd("2019-05-31")|as_date(fu180_visdate)< ymd("2019-05-31")) %>%   ## why participants contacted/seen before d 180 open
  mutate(
    comments = str_c("Participants seen/contacted before day 180 window open, check study dates")
  )
}, rownames = F)


## ++ Participants missing date left hospital
output$mis_dlefthosp <- renderDataTable({
curation_data() %>% 
  filter(!is.na(disch_date), is.na(disch_date_left_hosp), site == input$site_enrol) %>% 
  select(record_id, site, disch_date, disch_date_left_hosp, sc_nofucomp_resn) %>% 
    mutate(
      comments = str_c("missing date left hospital")
    )
}, rownames = F)


### ++ Anthropometry dataset queries
output$adm_dischwhgt <- renderDataTable({
curation_data() %>% 
    mutate(
    delta_weight = disch_weight - adm_weight
  ) %>% 
  filter(abs(delta_weight) >= 1.9, site == input$site_enrol) %>% 
  select(record_id, site, adm_weight, disch_weight, delta_weight, length_stay) %>% 
    mutate_at(vars(adm_weight, disch_weight, delta_weight), ~round(., 3)) %>% 
  mutate(
    comments = str_c("Weight gain/loss of 2kgs+")
  )
}, rownames = F)  ## Weight


## Heights
output$adm_height <- renderDataTable({
curation_data() %>% 
  mutate(
    height_diff = adm_height1 - adm_height2
  ) %>% 
  filter(abs(height_diff) > 2, site == input$site_enrol) %>% 
  select(record_id, site, adm_agemons, adm_height1, adm_height2, height_diff) %>% 
    mutate(
      comments = str_c("difference of more than 2 cm, double check entries")
    )
}, rownames = F)


output$adm_dischhgt <- renderDataTable({
  curation_data() %>% 
    mutate(
    delta_height = height_disch - height_enrol
  ) %>% 
  filter(abs(delta_height) >= 1.9, site == input$site_enrol) %>% 
  select(record_id, site, height_enrol, height_disch, delta_height, length_stay) %>%
    mutate_at(vars(height_enrol, height_disch, delta_height), ~round(., 3)) %>% 
  mutate(
    comments = str_c("Height gain/loss of 2cm+, check entries")
  )
}, rownames = F)

## MUACs (admission vs discharge)
output$adm_dischmuac <- renderDataTable({
curation_data() %>% 
  mutate(
    delta_muac = muac_enrol - muac_disch
  ) %>% 
  filter(abs(delta_muac) > 1.9, site == input$site_enrol) %>% 
  select(record_id, site, muac_enrol, muac_disch, delta_muac, length_stay) %>% 
  mutate(
    comments = str_c("MUAC gain/loss of 2cm+, kindly double check both entries")
  )
}, rownames = F)

## muac 1 and muac 2
output$adm_muacdif <- renderDataTable({
curation_data() %>% 
  mutate(
    muac_diff = adm_muac1 - adm_muac2
  ) %>% 
  filter(abs(muac_diff) > 1) %>% 
  select(record_id, site, adm_agemons, adm_muac1, adm_muac2, muac_diff) %>% 
  mutate(
    comments = str_c("muac difference of 1cm or more, kindly double check entries")
  )
})


## Head Circumfrence (Admission vs Discharge)
output$adm_headcirc <- renderDataTable({
  curation_data() %>% 
  mutate(
    delta_headcir = headcirc_enrol - headcirc_disch
  ) %>% 
  filter(abs(delta_headcir) > 2.45, site == input$site_enrol) %>% 
  select(record_id, site, headcirc_enrol, headcirc_disch, delta_headcir, length_stay) %>%
  mutate(
    comments = str_c("Improbable head circufrence gain/loss, double check entries")
  )
}, rowname = F)



}




