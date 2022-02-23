## Loading required packages --------
library(tidyverse)
library(shiny)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(DT)
library(data.table)
library(RColorBrewer)
library(janitor)
library(stringr)
library(lubridate)


server= function(input,output){
  
## Iporting and preparation of the dataset for use in the app -----------
  Daily.Records <- fread("daily.records.csv")%>% 
    filter(!record_id %in% c(20001004,20001013,20001019,20001031,20001032,20001036,20001043,20001044,
                             20001049,20001051,20001060,20001082,20001084,20001089,20001102,20001128,
                             20001130,20001148,20001154,20001160,20001162,20001165,20001175,20001176,
                             20001178))  # import data for daily records
  
  chaindata <- fread("chaindata.csv") # importing processed data
  
  screening_data <- fread("screening_data.csv")%>% 
    arrange(record_id, redcap_repeat_instance) %>% 
    slice(-25)   ## read in dataset ------
  
  daily_data <- fread("daily_data.csv") %>% 
    filter(!record_id %in% c(20001004,20001013,20001019,20001031,20001032,20001036,20001043,20001044,
                             20001049,20001051,20001060,20001082,20001084,20001089,20001102,20001128,
                             20001130,20001148,20001154,20001160,20001162,20001165,20001175,20001176,
                             20001178))  ## exclude some record IDs
  
  note_to_file <- fread("note_to_file.csv")
  FUP_data <- fread("FUP_data.csv")
  Verbal_autopsy <- fread("Verbal_autopsy.csv")
  

## ++ Preparation for note to file intergration dataframes +++ ---
## Variables not measured -----
var_not_measured <- note_to_file %>%
  filter(ntf_description == 2) %>% 
  select(record_id)


## +++ How many number of weeks of data per site so far ++++ -------
wk_data1 <- screening_data %>%
    filter(!is.na(scrnlog_site)) %>% 
    group_by(scrnlog_site) %>% 
    summarise(
      data_points=n()
    )

## +++ How many weeks of data do we expected from all sites to date? ---------
wk_data2 <- screening_data %>% 
    filter(!is.na(scrnlog_site)) %>% 
    distinct(scrnlog_site , .keep_all = T) %>%  # Deleting duplicating
    select(scrnlog_site,wks_enrlng)

week_dataAll <- merge(wk_data1,wk_data2,by="scrnlog_site",all = T) %>% 
  mutate(data_left=wks_enrlng - data_points)  ## last coloumn shows data items still awaiting entry into the database


## Patients incmplete gps(data processing)------
  
  ward <- chaindata %>% 
    filter(adm_parttype==1,!is.na(disch_date)) %>% 
    filter(is.na(hsurv_hmlat)|is.na(hsurv_hmlong)) %>% 
    select(record_id,site,adm_parttype,disch_date)
  
  com <- chaindata %>% 
    filter(adm_parttype==2) %>% 
    filter(is.na(adm_comm_lat)|is.na(adm_comm_long)) %>% 
    select(record_id,site,adm_parttype)
  
  incop_gps <- merge(ward,com,by=c('record_id','site','adm_parttype'),all = T)
  
  
  
  chaindata$adm_sex <- factor(chaindata $adm_sex, levels=c(1,2),
                              labels=c("Male", "Female")) 
  chaindata$adm_parttype <- factor(chaindata$adm_parttype, levels=c(1,2),
                                   labels=c("Ward", "Community")) # labelling participant type
  
  chaindata$Oedema <- as.factor(chaindata$Oedema)
  chaindata$region <- as.factor(chaindata$region)

##+++++++++++++++++++++++++++Participant enrolment event codes -------
## Participant enrolemnt -------
output$admDate_missing <- renderDataTable({
chaindata %>% 
    filter(is.na(adm_date)) %>%
    filter(site==input$site_enrol,!record_id %in% c(var_not_measured$record_id)) %>% 
    select(record_id,site,adm_date,adm_muac1,adm_muac2,adm_parttype)## missing admission date
})
  

output$enrol_missing <- renderDataTable({
chaindata %>% 
    filter(site==input$site_enrol,!record_id %in% c(var_not_measured$record_id)) %>% 
    filter(is.na(adm_weight)|is.na(adm_height1)|is.na(adm_height2)|
             is.na(adm_muac1)|is.na(adm_muac2)|is.na(adm_head_circ1)|is.na(adm_head_circ2)|is.na(adm_oedema)) %>% 
    select(record_id,site,adm_weight,adm_height1,adm_height2,adm_muac1,adm_muac2,adm_head_circ1,adm_head_circ2,adm_oedema)
},rownames=F)
  
output$Age_b <- renderDataTable({
  chaindata %>% 
    filter(site==input$site_enrol) %>% 
    mutate(ageb=trunc(difftime(adm_date,adm_dob,units = c("days"))/30.43,2)) %>% 
    select(record_id,site,adm_agemons,ageb,adm_dob,adm_date) %>% 
    filter(ageb<0|ageb>24)
},rowname=F)

output$admDOB <- renderDataTable({
  chaindata %>% 
    filter(site==input$site_enrol) %>% 
    filter(is.na(adm_dob)) %>% 
    select(record_id,site,adm_dob,adm_agemons)
},rowname=F)

output$missingVals <- renderDataTable({
chaindata %>% 
    filter(site==input$site_enrol,!record_id %in% c(var_not_measured$record_id)) %>% 
    filter(is.na(adm_init_temp)|is.na(adm_init_resprate)|is.na(adm_init_heartrate)) %>% 
    select(record_id,site,adm_init_temp,adm_init_resprate,adm_init_heartrate) ## missing field
})

output$anthropo <- renderDataTable({
chaindata %>% 
    filter(site==input$site_enrol,!record_id %in% c(var_not_measured$record_id)) %>% 
    filter(is.na(adm_anthropo_init1)|is.na(adm_anthropo_init2)) %>% 
    select(record_id,site,adm_anthropo_init1,adm_anthropo_init2,muac_enrol,height_enrol,adm_parttype) ## missing initials
})

output$prevAdmin <- renderDataTable({
chaindata %>% 
    filter(site==input$site_enrol,!record_id %in% c(var_not_measured$record_id)) %>% 
    filter(is.na(adm_prev_admhosp)) %>% 
    select(record_id,site,adm_prev_admhosp,adm_parttype,adm_date)## missing field for previous admission
})

output$urineVol <- renderDataTable({
chaindata %>% 
    filter(site==input$site_enrol,!record_id %in% c(var_not_measured$record_id)) %>%
    filter(is.na(adm_urinevol)) %>% 
    select(record_id,site,adm_urinevol,adm_parttype) 
})

### Heart  range checks
high_hrtRate <- chaindata %>% 
  filter(as.numeric(adm_init_heartrate) > 200) %>%  ## improbable and impossible upper limit
  select(record_id,site,adm_date,adm_init_heartrate)

low_hrtRate <- chaindata %>% 
  filter(as.numeric(adm_init_heartrate) < 75) %>%  ## improbable and impossible upper limit
  select(record_id,site,adm_date,adm_init_heartrate)

impro_vals <- note_to_file %>% 
  filter(ntf_description == 7) %>% 
  select(record_id)   ## bote to file improbable values

output$heart_rate <- renderDataTable({
rbind(high_hrtRate,low_hrtRate) %>% 
    filter(site == input$site_enrol,!record_id %in% c(impro_vals$record_id)) %>% 
    mutate(
      comment = ifelse(adm_init_heartrate <50,"impossible low value","Improbable low value") # combine heart rate QC
    )
},rownames=F)


## Respiratory rate limit checks -----
high_rspRate <- chaindata %>% 
  filter(as.integer(adm_init_resprate) > 80,!record_id %in% c(impro_vals$record_id)) %>%  ## improbable and impossible upper limit
  select(record_id,site,adm_date,adm_init_resprate) %>% 
  mutate(
    comment = ifelse(adm_init_resprate > 110,"immpossible high value","improbable high value")
  )

low_rspRate <- chaindata %>% 
  filter(adm_init_resprate < 18,!record_id %in% c(impro_vals$record_id)) %>%  ## improbable and impossible lower limit
  select(record_id,site,adm_date,adm_init_resprate) %>% 
  mutate(
    comment = ifelse(adm_init_resprate <= 10,"impossible low value","improbable low value")
  )

output$resprate_rate <- renderDataTable({
rbind(high_rspRate,low_rspRate) %>% 
    filter(site == input$site_enrol)
},rownames=F)

## Temperature limit checks -----
high_temp <- chaindata %>%
  filter(as.integer(adm_init_temp) > 41,!record_id %in% c(impro_vals$record_id)) %>%  ## improbable and impossible upper limit
  select(record_id,site,adm_init_temp) %>%
  mutate(
    comment = ifelse(adm_init_temp > 41,"immpossible high value","improbable high value")
  )


low_temp <- chaindata %>%
  filter(as.integer(adm_init_temp) < 34,!record_id %in% c(impro_vals$record_id)) %>%  ## improbable and impossible lower limit
  select(record_id,site,adm_init_temp) %>%
  mutate(
    comment = ifelse(adm_init_temp <= 32,"impossible low value","improbable low value")
  )

output$temp_val <- renderDataTable({
  rbind(high_temp,low_temp) %>%  ## combined temperature QC
    filter(site == input$site_enrol)  
},rownames=F)

## Oxygen saturation ------
output$oxy_sat <- renderDataTable({
chaindata %>% 
    filter(adm_init_oxysat_taken == 1|adm_init_oxysat_taken == 2) %>%
    filter(adm_init_oxysat < 75,site == input$site_enrol,!record_id %in% c(impro_vals$record_id)) %>% 
    select(record_id,site,adm_init_oxysat) %>% 
    mutate(
      comment = paste("improbable low oxygen saturation value")
    )
})


## PHQ - 9 CRF -------

## Note to file missing PHQ -9 
phq9_missing <- note_to_file %>% 
  filter(ntf_description ==1,ntf_crfs_not_filled___phq_9enrol==1) %>% 
  select(record_id)

output$mis_phq_adm <- renderDataTable({
chaindata %>% 
    filter(site==input$site_enrol,!record_id %in% c(phq9_missing$record_id)) %>%
    filter(!is.na(adm_date),is.na(phq_date)) %>% 
    select(record_id,site,adm_date,phq_date) %>% 
    mutate(
      comment = paste("Missing PHQ-9 admission CRF") ## admission PHQ -9 missing
    )
})

miss_phfup <- note_to_file %>% 
  filter(ntf_description == 1,ntf_crfs_not_filled___phq_9fup ==1) %>% 
  select(record_id)  ## ntf, missing phq 9 at follow up

output$mis_phq_fup <- renderDataTable({
FUP_data %>% 
    filter(!is.na(fu45_visdate)|!is.na(fu90_visdate)|!is.na(fu180_visdate)) %>%  ## filter those who attended atleast a follow up visit
    filter(is.na(phq_date),!record_id %in% c(miss_phfup$record_id)) %>% 
    left_join(select(chaindata,record_id,site,adm_date),by="record_id") %>% 
    filter(!record_id %in% c(10001258,10001262, 10001263, 10001265)) %>% ## exclude immunology participants
    select(record_id,adm_date,'site'='site.y',fu45_visdate,fu90_visdate,fu180_visdate) %>% 
    mutate(
      comment = paste("Missing PHQ-9 CRF at follow up")  ## missing PHQ - 9 CRF at follow up
    ) %>% 
    filter(site==input$site_enrol)
})


output$wrong_phq_adm <- renderDataTable({
chaindata %>% 
    filter(adm_date > phq_date,site==input$site_enrol) %>% 
    select(record_id,site,adm_date,disch_date,phq_date) %>% 
    mutate(
      comment = paste("PHQ done before admission/enrolment date")
    )
})


## Social information CRF -------
## Missing social information CRF
soc_missing <- note_to_file %>% 
  filter(ntf_description ==1,ntf_crfs_not_filled___social_information==1) %>% 
  select(record_id)

output$mis_soc <- renderDataTable({
chaindata %>%
    filter(is.na(soc_interview_date),site==input$site_enrol,!record_id %in% c(soc_missing$record_id)) %>% 
    select(record_id,adm_date,disch_date,site,soc_interview_date,soc_interviewer) %>% 
    mutate(
      comment=paste("missing social information CRF/date interv.")
    )
},rownames=F)

## ++ missing caregiver anthropometry
soc_mis_vars <- note_to_file %>%  ## social information variables not measured
  filter(ntf_description == 2,ntf_crfs_not_filled___social_information ==1) ## note to file data intergration

output$caregiver_misanth <- renderDataTable({
  chaindata %>% 
    filter(soc_presadm == 1|soc_presappment == 1,site==input$site_enrol,!record_id %in% c(soc_mis_vars$record_id)) %>% 
    filter_at(vars(soc_pcarer_weight,soc_pcarer_muac,soc_pcarer_height),any_vars(is.na(.))) %>% 
    select(record_id,adm_date,site,soc_pcarer_weight,soc_pcarer_muac,soc_pcarer_height) %>% 
    mutate(
      comments = str_c("Missing caregiver information(muac,weight or height)")
    )
},rownames=F)

## Household nutrition CRF -------
## nutrition CRF missing
nutri_missing <- note_to_file %>% 
  filter(ntf_description ==1,ntf_crfs_not_filled___household_nutrition==1) %>% 
  select(record_id)

output$mis_nutri <- renderDataTable({
chaindata %>%
    filter(is.na(nutr_missedmeal_last7days),site==input$site_enrol,!record_id %in% c(nutri_missing$record_id)) %>% 
    select(record_id,adm_date,disch_date,site,nutr_missedmeal_last7days,nutr_4wk_fdworry) %>% 
    mutate(
      comment = paste("missing CRF")
    )
},rownames=F)

## Household Wealth assesment CRF -------

## note to file hwa_missing
hwa_missing <- note_to_file %>% 
  filter(ntf_description ==1,ntf_crfs_not_filled___wealth_assesment==1) %>% 
  select(record_id)

output$mis_hwa <- renderDataTable({
  chaindata %>%
    filter(is.na(hwa_drink_water_src),site==input$site_enrol,!record_id %in% c(hwa_missing$record_id)) %>% 
    select(record_id,adm_date,disch_date,site,hwa_drink_water_src) %>% 
    mutate(
      comment = paste("missing CRF")
    )
},rownames=F)



##+++++++++++++++++++++++++++Daily reviews -------
## Daily records day 1 -------
drev1 = reactive({Daily.Records %>% 
    filter(adm_parttype==1, !record_id %in% c(40001053, 40001054, 40001055)) %>% 
    filter(site==input$site_enrol) %>% 
    select(record_id,site,adm_date,drev_date1) %>% 
    mutate(rev1_diff = difftime(drev_date1,adm_date,units = c('hours'))) %>% 
    filter(rev1_diff>24|rev1_diff<0)})

output$rev_1 <- renderDataTable({
  drev1()
  
},rownames=F,colnames=c('Hours difference'='rev1_diff'))

#We write the control statements based on user selections
output$downloadData1 <- downloadHandler(
  filename = function() {
    paste("daily_review1",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev1(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 2 -------
drev2 = reactive({
  Daily.Records %>% 
    filter(adm_parttype==1,!record_id %in% c(50002141, 10003146)) %>% 
    filter(site==input$site_enrol) %>% 
    select(record_id,site,adm_date,drev_date1,drev_date2) %>% 
    mutate(rev2_diff = difftime(drev_date2,drev_date1,units = c('hours'))) %>% 
    filter(rev2_diff>24|rev2_diff<24)
  
})

output$rev_2 <- renderDataTable({
  drev2()
},rownames=F,colnames=c('Hours difference'='rev2_diff'))

#We write the control statements based on user selections
output$downloadData2 <- downloadHandler(
  filename = function() {
    paste("daily_review2",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev2(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 3 -------
drev3 = reactive({
  Daily.Records %>% 
    filter(adm_parttype==1,record_id!=10001002,record_id!=50002011,record_id!=50002012) %>% 
    filter(site==input$site_enrol,record_id!=50001021,record_id!=50002147) %>% 
    select(record_id,site,adm_date,drev_date2,drev_date3) %>% 
    mutate(rev3_diff = difftime(drev_date3,drev_date2,units = c('hours'))) %>% 
    filter(rev3_diff>24|rev3_diff<24)
  
})
output$rev_3 <- renderDataTable({
  drev3()
},rownames=F,colnames=c('Hours difference'='rev3_diff'))

#We write the control statements based on user selections
output$downloadData3 <- downloadHandler(
  filename = function() {
    paste("daily_review3",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev3(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 4 -------
drev4 = reactive({
  Daily.Records %>% 
    filter(adm_parttype==1,record_id!=50002083) %>% 
    filter(site==input$site_enrol) %>% 
    select(record_id,site,adm_date,drev_date3,drev_date4) %>% 
    mutate(rev4_diff = difftime(drev_date4,drev_date3,units = c('hours'))) %>% 
    filter(rev4_diff>24|rev4_diff<24)
})
output$rev_4 <- renderDataTable({
  drev4()
},rownames=F,colnames=c('Hours difference'='rev4_diff'))

#We write the control statements based on user selections
output$downloadData4 <- downloadHandler(
  filename = function() {
    paste("daily_review4",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev4(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 5 -------
drev5 = reactive({
  Daily.Records %>% 
    filter(adm_parttype==1) %>% 
    filter(site==input$site_enrol,record_id!=50001021) %>% 
    select(record_id,site,adm_date,drev_date4,drev_date5) %>% 
    mutate(rev5_diff = difftime(drev_date5,drev_date4,units = c('hours'))) %>% 
    filter(rev5_diff>24|rev5_diff<24)
  
})

output$rev_5 <- renderDataTable({
  drev5()
},rownames=F,colnames=c('Hours difference'='rev5_diff'))


#We write the control statements based on user selections
output$downloadData5 <- downloadHandler(
  filename = function() {
    paste("daily_review5",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev5(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 6 -------
drev6= reactive({
  Daily.Records %>% 
    filter(adm_parttype==1) %>% 
    filter(site==input$site_enrol) %>% 
    select(record_id,site,adm_date,drev_date5,drev_date6) %>% 
    mutate(rev6_diff = difftime(drev_date6,drev_date5,units = c('hours'))) %>% 
    filter(rev6_diff>24|rev6_diff<24)
})
output$rev_6 <- renderDataTable({
  drev6()
},rownames=F,colnames=c('Hours difference'='rev6_diff'))

#We write the control statements based on user selections
output$downloadData6 <- downloadHandler(
  filename = function() {
    paste("daily_review6",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev6(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 7 -------
drev7 = reactive({
  Daily.Records %>% 
    filter(adm_parttype==1) %>%
    filter(site==input$site_enrol) %>% 
    select(record_id,site,adm_date,drev_date6,drev_date7) %>% 
    mutate(rev7_diff = difftime(drev_date7,drev_date6,units = c('hours'))) %>% 
    filter(rev7_diff>24|rev7_diff<24)
})
output$rev_7 <- renderDataTable({
  drev7()
},rownames=F,colnames=c('Hours difference'='rev7_diff'))

#We write the control statements based on user selections
output$downloadData7 <- downloadHandler(
  filename = function() {
    paste("daily_review7",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev7(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 8 -------
drev8 = reactive({
  Daily.Records %>% 
    filter(adm_parttype==1) %>% 
    filter(site==input$site_enrol) %>% 
    select(record_id,site,adm_date,drev_date7,drev_date8) %>% 
    mutate(rev8_diff = difftime(drev_date8,drev_date7,units = c('hours'))) %>% 
    filter(rev8_diff>24|rev8_diff<24)
})

output$rev_8 <- renderDataTable({
  drev8()
},rownames=F,colnames=c('Hours difference'='rev8_diff'))

#We write the control statements based on user selections
output$downloadData8 <- downloadHandler(
  filename = function() {
    paste("daily_review8",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev8(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 9 -------
drev9 = reactive({
  Daily.Records %>% 
    filter(adm_parttype==1) %>% 
    filter(site==input$site_enrol) %>% 
    select(record_id,site,adm_date,drev_date8,drev_date9) %>% 
    mutate(rev9_diff = difftime(drev_date9,drev_date8,units = c('hours'))) %>% 
    filter(rev9_diff>24|rev9_diff<24)
})

output$rev_9 <- renderDataTable({
  drev9()
},rownames=F,colnames=c('Hours difference'='rev9_diff'))

#We write the control statements based on user selections
output$downloadData9 <- downloadHandler(
  filename = function() {
    paste("daily_review9",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev9(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 10 -------
drev10 = reactive({
  
  Daily.Records %>% 
    filter(adm_parttype==1) %>% 
    filter(site==input$site_enrol) %>% 
    select(record_id,site,adm_date,drev_date9,drev_date10) %>% 
    mutate(rev10_diff = difftime(drev_date10,drev_date9,units = c('hours'))) %>% 
    filter(rev10_diff>24|rev10_diff<24)
})
output$rev_10 <- renderDataTable({
  drev10()
},rownames=F,colnames=c('Hours difference'='rev10_diff'))

#We write the control statements based on user selections
output$downloadData10 <- downloadHandler(
  filename = function() {
    paste("daily_review10",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev10(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 11 -------
drev11 = reactive({Daily.Records %>% 
    filter(adm_parttype==1) %>% 
    filter(site==input$site_enrol) %>% 
    select(record_id,site,adm_date,drev_date10,drev_date11) %>% 
    mutate(rev11_diff = difftime(drev_date11,drev_date10,units = c('hours'))) %>% 
    filter(rev11_diff>24|rev11_diff<24)})

output$rev_11 <- renderDataTable({
  drev11()
  
},rownames=F,colnames=c('Hours difference'='rev11_diff'))

#We write the control statements based on user selections
output$downloadData11 <- downloadHandler(
  filename = function() {
    paste("daily_review11",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev11(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 12 -------
drev12 = reactive({
  Daily.Records %>% 
    filter(adm_parttype==1) %>% 
    filter(site==input$site_enrol) %>% 
    select(record_id,site,adm_date,drev_date11,drev_date12) %>% 
    mutate(rev12_diff = difftime(drev_date12,drev_date11,units = c('hours'))) %>% 
    filter(rev12_diff>24|rev12_diff<24)
})

output$rev_12 <- renderDataTable({
  drev12()
},rownames=F,colnames=c('Hours difference'='rev12_diff'))

#We write the control statements based on user selections
output$downloadData12 <- downloadHandler(
  filename = function() {
    paste("daily_review12",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev12(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 13 -------
drev13 = reactive({
  Daily.Records %>% 
    filter(adm_parttype==1) %>% 
    filter(site==input$site_enrol) %>% 
    select(record_id,site,adm_date,drev_date12,drev_date13) %>% 
    mutate(rev13_diff = difftime(drev_date13,drev_date12,units = c('hours'))) %>% 
    filter(rev13_diff>24|rev13_diff<24)
  
})
output$rev_13 <- renderDataTable({
  drev13()
},rownames=F,colnames=c('Hours difference'='rev13_diff'))

#We write the control statements based on user selections
output$downloadData13 <- downloadHandler(
  filename = function() {
    paste("daily_review13",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev13(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 14 -------
drev14 = reactive({
  Daily.Records %>% 
    filter(adm_parttype==1) %>% 
    filter(site==input$site_enrol) %>% 
    select(record_id,site,adm_date,drev_date13,drev_date14) %>% 
    mutate(rev14_diff = difftime(drev_date14,drev_date13,units = c('hours'))) %>% 
    filter(rev14_diff>24|rev14_diff<24)
})
output$rev_14 <- renderDataTable({
  drev14()
},rownames=F,colnames=c('Hours difference'='rev14_diff'))

#We write the control statements based on user selections
output$downloadData14 <- downloadHandler(
  filename = function() {
    paste("daily_review14",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev14(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 15 -------
drev15 = reactive({
  Daily.Records %>% 
    filter(adm_parttype==1) %>% 
    filter(site==input$site_enrol) %>% 
    select(record_id,site,adm_date,drev_date14,drev_date15) %>% 
    mutate(rev15_diff = difftime(drev_date15,drev_date14,units = c('hours'))) %>% 
    filter(rev15_diff>24|rev15_diff<24)
  
})

output$rev_15 <- renderDataTable({
  drev15()
},rownames=F,colnames=c('Hours difference'='rev15_diff'))

#We write the control statements based on user selections
output$downloadData15 <- downloadHandler(
  filename = function() {
    paste("daily_review15",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev15(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 16 -------
drev16 = reactive({
  Daily.Records %>% 
    filter(adm_parttype==1) %>% 
    filter(site==input$site_enrol) %>% 
    select(record_id,site,adm_date,drev_date15,drev_date16) %>% 
    mutate(rev16_diff = difftime(drev_date16,drev_date15,units = c('hours'))) %>% 
    filter(rev16_diff>24|rev16_diff<24)
  
})

output$rev_16 <- renderDataTable({
  drev16()
},rownames=F,colnames=c('Hours difference'='rev16_diff'))

#We write the control statements based on user selections
output$downloadData16 <- downloadHandler(
  filename = function() {
    paste("daily_review16",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev16(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 17 -------
drev17 = reactive({
  Daily.Records %>% 
    filter(adm_parttype==1) %>% 
    filter(site==input$site_enrol) %>% 
    select(record_id,site,adm_date,drev_date16,drev_date17) %>% 
    mutate(rev17_diff = difftime(drev_date17,drev_date16,units = c('hours'))) %>% 
    filter(rev17_diff>24|rev17_diff<24)
  
})

output$rev_17 <- renderDataTable({
  drev17()
},rownames=F,colnames=c('Hours difference'='rev17_diff'))

#We write the control statements based on user selections
output$downloadData17 <- downloadHandler(
  filename = function() {
    paste("daily_review17",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev17(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 18 -------
drev18 = reactive({
  Daily.Records %>% 
    filter(adm_parttype==1) %>% 
    filter(site==input$site_enrol) %>% 
    select(record_id,site,adm_date,drev_date17,drev_date18) %>% 
    mutate(rev18_diff = difftime(drev_date18,drev_date17,units = c('hours'))) %>% 
    filter(rev18_diff>24|rev18_diff<24)
  
})

output$rev_18 <- renderDataTable({
  drev18()
},rownames=F,colnames=c('Hours difference'='rev18_diff'))

#We write the control statements based on user selections
output$downloadData18 <- downloadHandler(
  filename = function() {
    paste("daily_review18",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev18(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 19 -------
drev19 = reactive({
  Daily.Records %>% 
    filter(adm_parttype==1) %>%
    filter(site==input$site_enrol) %>% 
    select(record_id,site,adm_date,drev_date18,drev_date19) %>% 
    mutate(rev19_diff = difftime(drev_date19,drev_date18,units = c('hours'))) %>% 
    filter(rev19_diff>24|rev19_diff<24)
})

output$rev_19 <- renderDataTable({
  drev19()
},rownames=F,colnames=c('Hours difference'='rev19_diff'))

#We write the control statements based on user selections
output$downloadData19 <- downloadHandler(
  filename = function() {
    paste("daily_review19",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev19(), file,row.names = FALSE,na= ".")
  }
)

## Daily records day 20 -------
drev20 = reactive({
  Daily.Records %>% 
    filter(adm_parttype==1) %>% 
    filter(site==input$site_enrol) %>% 
    select(record_id,site,adm_date,drev_date19,drev_date20) %>% 
    mutate(rev20_diff = difftime(drev_date20,drev_date19,units = c('hours'))) %>% 
    filter(rev20_diff>24|rev20_diff<24)
})

output$rev_20 <- renderDataTable({
  drev20()
},rownames=F,colnames=c('Hours difference'='rev20_diff'))

#We write the control statements based on user selections
output$downloadData20 <- downloadHandler(
  filename = function() {
    paste("daily_review20",".csv", sep="")
  },
  content = function(file) {
    write.csv(drev20(), file,row.names = FALSE,na= ".")
  }
)
 
### other daily records queries -----
output$time_seen <- renderDataTable({
daily_data %>% 
    filter(site==input$site_enrol) %>% 
    filter(!is.na(drev_date),is.na(drev_time)) %>% 
    select(record_id,site,drev_time,drev_date,drev_oedema_now,time_point)  ## missing field for time seen
},colname=c('Daily record No.'='time_point'))


output$oedema_now <- renderDataTable({
daily_data %>% 
    filter(site==input$site_enrol) %>% 
    filter(!is.na(drev_date),is.na(drev_oedema_now)) %>% 
    select(record_id,site,drev_date,drev_time,drev_oedema_now,time_point)  ## missing field for 
  
},colname=c('Daily record No.'='time_point'))


output$oedema_improve <- renderDataTable({
daily_data %>%
    filter(site==input$site_enrol) %>% 
    filter(drev_oedema_now %in% c(1,2,3)) %>% 
    filter(is.na(drev_imp_oedema)) %>% 
    select(record_id,site,drev_date,drev_time,drev_imp_oedema,drev_oedema_now,time_point)  ## missing field for 
},colname=c('Daily record No.'='time_point'))

output$temp_mis <- renderDataTable({
daily_data %>% 
    filter(site==input$site_enrol) %>% 
    filter(!is.na(drev_date),is.na(drev_temp_gt38)) %>% 
    select(record_id,site,drev_date,drev_time,drev_temp_gt38,time_point)  ## missing field for 
},colname=c('Daily record No.'='time_point'))


output$temp_mis2 <- renderDataTable({
daily_data %>% 
    filter(site==input$site_enrol) %>% 
    filter(!is.na(drev_date),is.na(drev_temp_lt36)) %>% 
    select(record_id,site,drev_date,drev_time,drev_temp_lt36,time_point)  ## missing field for 
},colname=c('Daily record No.'='time_point'))


output$ng_tube <- renderDataTable({
daily_data %>% 
    filter(site==input$site_enrol) %>% 
    filter(!is.na(drev_date),is.na(drev_ngtube)) %>% 
    select(record_id,site,drev_date,drev_time,drev_ngtube,time_point)  ## missing field 
},colname=c('Daily record No.'='time_point'))


output$ebm_drev <- renderDataTable({
daily_data %>% 
    filter(site==input$site_enrol) %>% 
    filter(!is.na(drev_date),is.na(drev_ebm_or_bfeed)) %>% 
    select(record_id,site,drev_date,drev_time,drev_ebm_or_bfeed,time_point)  ## missing field
},colname=c('Daily record No.'='time_point'))


output$resomal <- renderDataTable({
daily_data %>%
    filter(site==input$site_enrol) %>%
    filter(!is.na(drev_date),is.na(drev_resomal)) %>% 
    select(record_id,site,drev_date,drev_time,drev_resomal,time_point)  ## missing field 
},colname=c('Daily record No.'='time_point'))


output$ors <- renderDataTable({
daily_data %>% 
    filter(site==input$site_enrol) %>%
    filter(!is.na(drev_date),is.na(drev_ors)) %>% 
    select(record_id,site,drev_date,drev_time,drev_ors,time_point)  ## missing field 
},colname=c('Daily record No.'='time_point'))


output$iv_fluids <- renderDataTable({
daily_data %>%
    filter(site==input$site_enrol) %>%
    filter(!is.na(drev_date),is.na(drev_ivfluids)) %>% 
    select(record_id,site,drev_date,drev_time,drev_ivfluids,time_point)  ## missing field 
},colname=c('Daily record No.'='time_point'))


output$blood_trans <- renderDataTable({
daily_data %>% 
    filter(site==input$site_enrol) %>%
    filter(!is.na(drev_date),is.na(drev_bldtransfuse)) %>% 
    select(record_id,site,drev_date,drev_time,drev_bldtransfuse,time_point)  ## missing field 
},colname=c('Daily record No.'='time_point'))



##+++++++++++++++++++++++++++Participant discharge event -------
disch_misng <- note_to_file %>% 
  filter(ntf_description ==2, ntf_crfs_not_filled___participant_discharge ==1) %>% 
  select(record_id)  ## note to file missing variables at discharge

## wrong date left hospital (before admission or long after discharge) -----
output$dtlft_hosp <- renderDataTable({
  chaindata %>% 
    mutate(
      dlft_hos = difftime(disch_date_left_hosp,disch_date,units = c("days"))
    ) %>% 
    select(record_id,site,disch_date,disch_date_left_hosp,dlft_hos) %>% 
    filter(dlft_hos > 38|dlft_hos < 0,site==input$site_enrol) %>% 
    mutate(comments = case_when(
      dlft_hos > 38 ~ paste("Date left hosp. is",dlft_hos,"days after discharge"),
      sign(dlft_hos) < 0 ~ paste("Date left hospital is before disch date",dlft_hos,"days")
    )
    ) %>% 
    select(-dlft_hos)
},rownames=F)

## Absconded patients with MISSING date of absconding -----
dish_ntfs_absc <- note_to_file %>% 
  filter(ntf_description == 2, ntf_crfs_not_filled___participant_discharge == 1) %>% 
  select(record_id)

output$abs2 <-renderDataTable({
  chaindata %>% 
    filter(site==input$site_enrol,!record_id %in% c(dish_ntfs_absc$record_id)) %>%
    filter(is.na(disch_abscond_date),disch_absconded==1) %>% 
    select(record_id,site,disch_date,disch_absconded,disch_abscond_date)
},rownames=F)

## Discharged, yet absconded ----
output$abs <-renderDataTable({
  chaindata %>% 
    filter(site==input$site_enrol,record_id!= 50001328) %>%
    filter(!is.na(disch_date),disch_absconded==1) %>% 
    select(record_id,site,disch_date,disch_absconded,disch_date_left_hosp) ## discharged yet absconded
},rownames=F)


## admitted same day, yet they have daily review data------
output$disch_3 <- renderDataTable({
Daily.Records %>% 
    filter(site==input$site_enrol) %>% 
    filter(!is.na(disch_date)) %>% 
    mutate(days_admmited=difftime(disch_date,adm_date,units = c("days"))) %>% 
    filter(days_admmited==0) %>% 
    select(record_id,site,adm_date,disch_date,days_admmited,drev_date1,drev_date2,drev_date3) %>% 
    filter(!is.na(drev_date1)|!is.na(drev_date1)|!is.na(drev_date1))
},rownames=F)


## Missing discharge athropometry 
output$disch_athrop <-renderDataTable({
  chaindata %>% 
    filter(site==input$site_enrol,!record_id %in% c(disch_misng$record_id)) %>%
    filter(!is.na(disch_date)) %>% 
    filter(is.na(disch_weight)|is.na(disch_height1)|is.na(disch_height2)|is.na(disch_muac1)|
             is.na(disch_muac2)|is.na(disch_head_circ1)|is.na(disch_head_circ2)|is.na(disch_oedema)) %>% 
    select(record_id,disch_date,site,disch_weight,disch_height1,disch_height2,disch_muac1,disch_muac2,disch_head_circ1,disch_head_circ2,disch_oedema)
},rownames=F)

## SaO2 missing
output$sa02_miss <-renderDataTable({
chaindata %>% 
    filter(site==input$site_enrol,!record_id %in% c(disch_misng$record_id)) %>%
    filter(!is.na(disch_date),is.na(disch_oxysat_taken)) %>%
    select(record_id,site,disch_date,disch_oxysat_taken)
},rownames=F)


## Verbal autopsy queries ------
output$va_miscla <- renderDataTable({
Verbal_autopsy %>% 
    mutate(
      vatype = ifelse(redcap_event_name == "inpatient_death_arm_1",1,2)
    ) %>% 
    left_join(select(chaindata,record_id,site,sc_nofucomp_resn,adm_date,sc_lastvitalstat_date),by="record_id") %>% 
    filter(va_type!=vatype) %>% 
    select(record_id,'site'='site.y',va_type,vatype,adm_date,sc_lastvitalstat_date) %>% 
    mutate(
      comment = ifelse(va_type == 1,paste("inpatient death filled under outpatient death event"),
                       paste("outpatient death filled under inpatient death event"))) %>% 
    select(-c(vatype,va_type)) %>% 
    filter(site==input$site_enrol)
},rownames=F)


##+++++++++++++++++++++++++++Follow up tabpanel code ------
# Study conclusion ------
output$day180 <-renderDataTable({
  chaindata %>% 
    filter(!is.na(fu180_visdate)) %>% 
    filter(site==input$site_enrol) %>%
    select(record_id,site,fu180_visdate,d180_close,sc_part_fucomplete,sc_lastvitalstat_date) %>%
    filter(is.na(sc_part_fucomplete))
},rownames=F)


output$rsn_noncomp <- renderDataTable({
  chaindata %>% ## missing reason for non-completion of study
    filter(sc_part_fucomplete==0,is.na(sc_nofucomp_resn),site==input$site_enrol) %>%
    select(record_id,site,sc_part_fucomplete,sc_nofucomp_resn) %>% 
    mutate(
      comment = str_c("Missing reason for non-completion of study")
    )
},rownames = F)


output$v_status_date <-renderDataTable({
  chaindata %>% 
    filter(site==input$site_enrol) %>%
    filter(!is.na(sc_part_fucomplete),is.na(sc_lastvitalstat_date),adm_parttype=="Ward") %>% 
    select(record_id,site,sc_part_fucomplete,fu180_visdate,fu180_contactdate,d180_close,sc_lastvitalstat_date)
},rownames=F)

output$windo_w <-renderDataTable({
  chaindata %>% 
    filter(site==input$site_enrol) %>%
    filter(is.na(sc_part_fucomplete),(d180_close<=Sys.Date())) %>% 
    select(record_id,site,d180_close,sc_lastvitalstat_date,sc_nofucomp_resn)
},rownames=F)

output$vital <-renderDataTable({
  chaindata %>% 
    filter(site==input$site_enrol) %>%
    filter(!is.na(disch_date)) %>%  ## consider all discharged paatients
    select(record_id,site,disch_date,fu180_contactdate,fu180_visdate,d180_close,sc_lastvitalstat_date) %>% 
    filter(sc_lastvitalstat_date>d180_close)
},rownames=F)

output$study_conc <-renderDataTable({
  chaindata %>% 
    filter(site==input$site_enrol) %>%
    filter(sc_part_fucomplete==1,adm_parttype=="Ward") %>% 
    filter(is.na(fu180_visdate),is.na(fu180_contactdate)) %>% 
    select(record_id,site,fu180_visdate,fu180_contactdate,sc_part_fucomplete,sc_lastvitalstat_date,sc_crfcompl_date)
},rownames=F)

output$death_vital <-renderDataTable({
  chaindata %>% 
    filter(site==input$site_enrol) %>%
    filter(sc_nofucomp_resn==3) %>% 
    select(record_id,site,sc_part_fucomplete,sc_nofucomp_resn,va_death_date,va_deathdate,
           sc_lastvitalstat_date) %>% 
    filter(as.Date(va_death_date)!=as.Date(sc_lastvitalstat_date))## mortalities
  ## last vital status date should be equall to date of death!
},rownames=F)

## missing verbal autopsy CRF---note to file data
ver_miss <- note_to_file %>% 
  filter(ntf_description == 1, ntf_crfs_not_filled___autopsy ==1) %>% 
  select(record_id)


output$mis_va_info <- renderDataTable({
  chaindata %>% 
    filter(sc_vautopsy_compl == 1,site==input$site_enrol) %>% 
    select(record_id,site,sc_part_fucomplete,sc_nofucomp_resn,sc_vautopsy_compl) %>%  ## verbal autopsy as per study conclusion
    anti_join(Verbal_autopsy,by="record_id") %>% 
    mutate(
      comment = str_c("verbal autopsy indicated was NOT filled (missing VA data)")
    )
})

mort_date_mis <- note_to_file %>% 
  filter((ntf_description == 2 & ntf_crfs_not_filled___autopsy ==1)| (ntf_description == 1 & ntf_crfs_not_filled___autopsy == 1)) %>% 
  select(record_id)  ## mortalities without date of death

output$msng_deathdate <-renderDataTable({
  chaindata %>% 
    filter(site==input$site_enrol,!record_id %in% mort_date_mis$record_id) %>%
    filter(sc_nofucomp_resn==3) %>% 
    select(record_id,site,sc_part_fucomplete,sc_nofucomp_resn,va_death_date,va_deathdate,
           sc_lastvitalstat_date) %>% 
    filter(is.na(va_death_date),is.na(va_deathdate),record_id != 10002192) ## Mortalities without death of death(va_death_date)
},rownames=F)


## Day 45 queries ------
output$wrong_date <- renderDataTable({
  chaindata %>% 
    filter(fu45_where_seen %in% c(1,2),!is.na(fu45_visdate),site == input$site_enrol) %>% 
    filter(as.Date(fu45_visdate)>as.Date(sc_lastvitalstat_date)) %>% ### fu45 follow up date must be before study end
    select(record_id,site,'Day 45 date'='fu45_visdate','Study end date'='sc_lastvitalstat_date') %>% 
    mutate(
      comment = paste("Study end happened before day 45")
    )
},rownames=F)  ## wrong follow up date


muac_d45 <- note_to_file %>% 
  filter(ntf_description == 2, ntf_crfs_not_filled___day_45 == 1) %>% 
  select(record_id)

output$mis_45anthro <- renderDataTable({
chaindata %>%
    filter(!is.na(fu45_visdate),fu45_where_seen==1,site == input$site_enrol) %>%  # exclude those seen in community/contacted
    filter(is.na(fu45_weight)|is.na(fu45_height1)|is.na(fu45_height2)|is.na(fu45_muac1)|
             is.na(fu45_muac2)|is.na(fu45_head_circ1)|is.na(fu45_head_circ2)|is.na(fu45_oedema)) %>% 
    select(record_id,fu45_visdate,site,fu45_weight,fu45_height1,fu45_height2,fu45_muac1,fu45_muac2,fu45_head_circ1,fu45_head_circ2,fu45_oedema) %>% 
    filter(!record_id %in% muac_d45$record_id)
  
},rownames=F)


## attended day 45 but missing visit date
output$mis_dte_seen <- renderDataTable({
chaindata %>% 
  filter(fu45_where_seen %in% c(1,2), is.na(fu45_visdate), site == input$site_enrol) %>%   ## Date must not miss at all
  select(record_id, adm_date,site, fu45_where_seen, fu45_visdate) %>%
  mutate(
    comments = str_c("attended d45 but missing visit date")
  )
}, rownames = F)


## height 1 and height 2 measurement
output$height_ms <- renderDataTable({
chaindata %>% 
  filter(abs(fu45_height1 - fu45_height2) > 2, site == input$site_enrol) %>% 
  select(record_id, site, fu45_height1, fu45_height2) %>% 
  mutate(
    comments = str_c("height 1 and height 2 differ by more than 2cm")
  )
}, rownames = F)


## headcirc 1 and head circ 2
output$headcirc <- renderDataTable({
  chaindata %>% 
  filter(abs(fu45_head_circ1 - fu45_head_circ2) > 2, site == input$site_enrol) %>% 
  select(record_id, site, fu45_head_circ1, fu45_head_circ2) %>% 
  mutate(
    comments = str_c("head_circ1 and head_circ2 differ by more than 2cm")
  )
}, rownames = F)


## missing length of illness
output$lngth_illness <- renderDataTable({
chaindata %>% 
  filter(fu45_normal_health==0, is.na(fu45_illness_length), site == input$site_enrol) %>% 
  select(record_id,site,fu45_normal_health,fu45_illness_length) %>% 
  mutate(
    comments = str_c("missing length of illness")
  )
}, rownames = F)





## Day 90 queries ------
miss_anthrd90 <- note_to_file %>% 
  filter(ntf_description ==2, ntf_crfs_not_filled___day_90==1) %>% 
  select(record_id)  ## variables not measured for day 90


output$mis_90anthro <- renderDataTable({
chaindata %>%
    filter(!is.na(fu90_visdate),fu90_where_seen==1,site == input$site_enrol) %>%  # exclude those seen in community/contacted
    filter(is.na(fu90_weight)|is.na(fu90_height1)|is.na(fu90_height2)|is.na(fu90_muac1)|
             is.na(fu90_muac2)|is.na(fu90_head_circ1)|is.na(fu90_head_circ2)|is.na(fu90_oedema)) %>% 
    select(record_id,fu90_visdate,site,fu90_weight,fu90_height1,fu90_height2,
           fu90_muac1,fu90_muac2,fu90_head_circ1,fu90_head_circ2,fu90_oedema) %>% 
    filter(!record_id %in% miss_anthrd90$record_id)
},rownames=F)


## Day 180 queries ------
output$wrong_date_d180 <- renderDataTable({
chaindata %>% 
    filter(fu180_where_seen %in% c(1,2),!is.na(fu180_visdate),site == input$site_enrol) %>% 
    filter(as.Date(fu180_visdate)>as.Date(sc_lastvitalstat_date)) %>% ### fu180 follow up date must be before study end
    select(record_id,site,'Day 180 date'='fu180_visdate','Study end date'='sc_lastvitalstat_date') %>% 
    mutate(
      comment = paste("Study end happened before day 180")
    )
},rownames=F)  ## wrong follow up date


output$mis_180anthro <- renderDataTable({
chaindata %>%
    filter(!is.na(fu180_visdate),fu180_where_seen==1,site == input$site_enrol) %>%  # exclude those seen in community/contacted
    filter(is.na(fu180_weight)|is.na(fu180_height1)|is.na(fu180_height2)|is.na(fu180_muac1)|
             is.na(fu180_muac2)|is.na(fu180_head_circ1)|is.na(fu180_head_circ2)|is.na(fu180_oedema)) %>% 
    select(record_id,fu180_visdate,site,fu180_weight,fu180_height1,fu180_height2,fu180_muac1,fu180_muac2,fu180_head_circ1,fu180_head_circ2,fu180_oedema)
  
  
})

## ++++++++++++++++ Home Visits QC ----------
crf_mis <- note_to_file %>% 
  filter(ntf_description == 1,ntf_crfs_not_filled___household_survey ==1) %>% 
  select(record_id)  ## missing home visit CRFs

output$household <-renderDataTable({
chaindata %>%
    filter(!c(site== "Blantyre" & adm_date > ymd("2019-01-31"))) %>% 
    filter(site==input$site_enrol,!record_id %in% crf_mis$record_id) %>%  ## intergrate note to file for missing CRFs
    filter(is.na(hsurv_dateofvisit),adm_parttype=="Ward",!is.na(disch_date)) %>%
    mutate(HomeDays=difftime(Sys.Date(),disch_date,units = c("days"))) %>%
    select(record_id,site,adm_date,disch_date,fu45_visdate,sc_lastvitalstat_date)
})

output$household_abs <-renderDataTable({
chaindata %>%
    filter(site==input$site_enrol,!record_id %in% crf_mis$record_id) %>% ## intergrate note to file for missing CRFs
    filter(is.na(hsurv_dateofvisit),adm_parttype=="Ward",disch_absconded==1) %>% 
    mutate(HomeDays=difftime(Sys.Date(),disch_date_left_hosp,units = c("days"))) %>%
    select(record_id,site,adm_date,disch_absconded,disch_date_left_hosp,sc_lastvitalstat_date) 
})



## Incomplete GPS information
gps_notDone <- note_to_file %>% 
  filter(ntf_description == 2) %>% 
  filter(ntf_crfs_not_filled___household_survey ==1 | ntf_crfs_not_filled___participant_enrolment == 1) %>% 
  filter(ntf_hsurv_var_mis___hsurv_hmlong == 1|ntf_hsurv_var_mis___hsurv_hmlat==1 |
           ntf_enrol_vars___adm_comm_lat==1|ntf_enrol_vars___adm_comm_long) %>% 
  select(record_id)

output$incop <-renderDataTable({
  incop_gps %>% 
    filter(site==input$site_enrol,!record_id %in% gps_notDone$record_id) %>% 
    filter(!record_id %in% crf_mis$record_id)## those with no household CRFs should be excluded as well
})

output$hme_vistend <- renderDataTable({
chaindata %>% 
  filter(as_date(hsurv_dateofvisit) >= as_date(sc_lastvitalstat_date), sc_nofucomp_resn !=3) %>% 
  select(record_id, site, hsurv_dateofvisit, sc_lastvitalstat_date, sc_nofucomp_resn) %>%  ## home visits done after study end
  mutate(
    comments = case_when(
      sc_nofucomp_resn == 4 ~ "home visit done after patient withdrew",
      sc_nofucomp_resn == 1 ~ "home visit done after patient was lost to follow up"
    )
  ) %>% filter(site==input$site_enrol)
}, rownames = F)


output$hme_vis_disch <- renderDataTable({
chaindata %>% 
  filter(as_date(hsurv_dateofvisit) < as_date(disch_date)) %>% 
  select(record_id, site, hsurv_dateofvisit, disch_date, sc_nofucomp_resn) %>% 
  mutate(
    comments = str_c("home visit done before discharge")
  ) %>% filter(site==input$site_enrol)
}, rownames = F)


output$hme_prov_durat <- renderDataTable({
chaindata %>%
  select(record_id, site, hsurv_traveltimehr, hsurv_timeofvisit) %>% 
  filter(hsurv_traveltimehr > 10) %>% 
  arrange(desc(hsurv_traveltimehr)) %>% 
    mutate(
      comments = str_c("took more than 10Hrs to reach hsehold,time and duration")
    )%>% filter(site==input$site_enrol)
}, rownames = F)

output$durat_mins <- renderDataTable({
chaindata %>%
  select(record_id, site, hsurv_traveltimemin, hsurv_traveltimehr) %>% 
  filter(hsurv_traveltimemin >= 60) %>% 
  arrange(desc(hsurv_traveltimemin)) %>% 
  mutate(
    comments = str_c("travel time in mins:: 60 mins equals 1 hr")
  )%>% filter(site==input$site_enrol)
}, rownames = F)

hme_visits <- note_to_file %>% 
  filter(ntf_description == 1 | ntf_description == 2) %>% 
  filter(ntf_crfs_not_filled___household_survey == 1) %>% 
  select(record_id)


output$educ_level <- renderDataTable({
chaindata %>% 
  filter(hsurv_pcarer_educ %in% c(1, 2, 3), hsurv_able_to_read %in% c(0, 88)) %>% 
  select(record_id, site, hsurv_pcarer_educ, hsurv_able_to_read) %>% 
  mutate(
    comments = str_c("highest level of education `Primary` and not able to read")
  )%>% filter(site==input$site_enrol, !record_id %in% c(hme_visits$record_id))
}, rownames = F)


output$care_giver_chang <- renderDataTable({
chaindata %>% 
  filter(hsurv_pcarer_changed == 1, is.na(hsurv_prev_pcarer)) %>% 
  select(record_id, site, hsurv_pcarer_changed, hsurv_prev_pcarer) %>% 
  mutate(
    comments = str_c("caregiver changed, did not indicate the previous")
  )%>% filter(site==input$site_enrol)
}, rownames = F)

output$lvstock <- renderDataTable({
chaindata %>% 
  filter(hsurv_has_livestock == 1) %>% 
  select(record_id, site, hsurv_num_cows:hsurv_num_other) %>% 
  filter_at(vars(hsurv_num_cows:hsurv_num_other), all_vars(is.na(.))) %>%
  mutate(
    comments = str_c("specify livestock owned by household")
  )%>% filter(site==input$site_enrol)
}, rownames = F)





## +++++++++++++++++ Screening data/CRF codes------------
output$all_wks_data <- renderDataTable({
week_dataAll
},rownames=F,colnames=c('Study site'='scrnlog_site','Data entered'='data_points','Weeks enrolling'='wks_enrlng',
                        'No.wks left'='data_left'))


#Do the screening week interval uniform?? i.e. from Monday to Sunday(6 days interval) ---------
output$date_intervals <- renderDataTable({
screening_data %>% 
    mutate(
      date_intervl=difftime(scrnlog_weekendate,scrnlog_weekstartdate,units = c("days"))
    ) %>% 
    select(record_id,scrnlog_site,redcap_repeat_instance,scrnlog_weekstartdate,scrnlog_weekendate,date_intervl) %>% 
    filter(date_intervl!=6,scrnlog_site==input$site_enrol)  ## Anyone whose data interval does not follow the agreed format of monday to sunday
},rownames=F,colnames=c('ID'='record_id','repeat instance'='redcap_repeat_instance','Site'='scrnlog_site','wk start date'='scrnlog_weekstartdate',
                        'wk end date'='scrnlog_weekendate','days interval'='date_intervl'))

## Sum of eligible and sum of not eligible should be equal to total screened------
output$sum_eligs <- renderDataTable({
  screening_data %>% 
    mutate(
      elig_notElig=scrnlog_num_eligible+scrnlog_not_elig
    ) %>% 
    select(record_id,redcap_repeat_instance,scrnlog_site,scrnlog_num_scrnd,scrnlog_num_eligible,scrnlog_not_elig,elig_notElig) %>%  
    filter(elig_notElig!=scrnlog_num_scrnd,scrnlog_site==input$site_enrol) %>% 
    select(record_id,scrnlog_site,redcap_repeat_instance,scrnlog_num_eligible,scrnlog_not_elig,scrnlog_num_scrnd)
},rownames=F,colnames=c('ID'='record_id','repeat instance'='redcap_repeat_instance','Site'='scrnlog_site','ELIGIBLE'='scrnlog_num_eligible',
                        'NOT ELIGIBLE'='scrnlog_not_elig','TOTAL SCREENED'='scrnlog_num_scrnd'))

## The difference between total eligible and total enroled should be equal to Number eligible BUT NOT enrolled -----
output$sum_notligs <- renderDataTable({
screening_data %>% 
    mutate(
      elig_notEnrld=scrnlog_num_eligible - scrnlog_num_enrld
    ) %>% 
    select(record_id,redcap_repeat_instance,scrnlog_site,scrnlog_num_scrnd,scrnlog_num_eligible,scrnlog_num_enrld,scrnlog_elignot_enroled,elig_notEnrld) %>% 
    filter(elig_notEnrld!=scrnlog_elignot_enroled,scrnlog_site==input$site_enrol) %>%
    select(record_id,scrnlog_site,redcap_repeat_instance,scrnlog_num_eligible,scrnlog_num_enrld,scrnlog_elignot_enroled)
},rownames=F,colnames=c('ID'='record_id','repeat instance'='redcap_repeat_instance','site'='scrnlog_site',
                        'ELIGIBLE'='scrnlog_num_eligible','ENROLLED'='scrnlog_num_enrld','ELIGIBLE_not_ENROLLED'='scrnlog_elignot_enroled'))


## +++++++++++++++++ Missed Visits tab------------

## Patients about to miss ------
missing45 <- chaindata %>% 
  filter(!is.na(disch_date),adm_parttype=="Ward") %>% 
  filter(is.na(fu45_visdate),is.na(fu45_contactdate)) %>% 
  filter(sc_nofucomp_resn %in% !c(3,4)|is.na(sc_nofucomp_resn)) %>%
  mutate(
    d45_left=difftime(d45_close,Sys.Date(),units = c("days")),
    time_point=paste("Day 45")
  ) %>% 
  filter(d45_left>=0 & d45_left<=14) %>% 
  select(record_id,site,adm_date,time_point,d45_open,d45_close,d45_left) %>% 
  rename('Window Open'='d45_open','Window Close'='d45_close','Days Left'='d45_left')



missing90 <- chaindata %>% 
  filter(!is.na(disch_date),adm_parttype=="Ward") %>% 
  filter(is.na(fu90_visdate),is.na(fu90_contactdate)) %>% 
  filter(sc_nofucomp_resn %in% !c(3,4)|is.na(sc_nofucomp_resn)) %>%
  mutate(
    d90_left=difftime(d90_close,Sys.Date(),units = c("days")),
    time_point=paste("Day 90")
  ) %>% 
  filter(d90_left>=0 & d90_left<=14) %>% 
  select(record_id,site,adm_date,time_point,d90_open,d90_close,d90_left) %>% 
  rename('Window Open'='d90_open','Window Close'='d90_close','Days Left'='d90_left')


missing180 <- chaindata %>% 
  filter(!is.na(disch_date),adm_parttype=="Ward",record_id != 50001078) %>% 
  filter(is.na(fu180_visdate),is.na(fu180_contactdate)) %>% 
  filter(sc_nofucomp_resn %in% !c(3,4)|is.na(sc_nofucomp_resn)) %>%
  mutate(
    d180_left=difftime(d180_close,Sys.Date(),units = c("days")),
    time_point=paste("Day 180")
  ) %>% 
  filter(d180_left>=0 & d180_left<=14) %>% 
  select(record_id,site,adm_date,time_point,d180_open,d180_close,d180_left) %>% 
  rename('Window Open'='d180_open','Window Close'='d180_close','Days Left'='d180_left')


output$about_to_miss <- renderDataTable({
rbind(missing45,missing90,missing180) %>% 
    filter(site==input$site_enrol)
})


output$mis_d45 <- renderDataTable({
chaindata %>% 
    filter(site==input$site_enrol) %>% 
    filter(!is.na(disch_date),(sc_lastvitalstat_date>d45_close|is.na(sc_lastvitalstat_date))) %>% ## remove deaths/withrawals before day 45
    filter(as.Date(d45_close)<Sys.Date()) %>% ## scheduled date has matured per system date
    filter(is.na(fu45_visdate),is.na(fu45_contactdate)) %>% 
    select(record_id,site,disch_date,d45_scheduled,d45_close,fu45_visdate,fu45_contactdate,sc_lastvitalstat_date)
  },rownames=F)


output$mis_d90 <- renderDataTable({
  chaindata %>% 
    filter(site==input$site_enrol) %>% 
    filter(!is.na(disch_date),(sc_lastvitalstat_date>d90_close|is.na(sc_lastvitalstat_date))) %>% ## remove deaths/withrawals before day 90
    filter(as.Date(d90_close)<Sys.Date()) %>% ## scheduled date has matured per system date
    filter(is.na(fu90_visdate),is.na(fu90_contactdate)) %>% 
    select(record_id,site,disch_date,d90_scheduled,d90_close,fu90_visdate,fu90_contactdate,sc_lastvitalstat_date)
},rownames=F)


output$mis_d180 <- renderDataTable({
chaindata %>%
    filter(site==input$site_enrol) %>% 
    filter(as.Date(d180_close)<Sys.Date()|sc_nofucomp_resn==1) %>% 
    filter(is.na(fu180_visdate),is.na(fu180_contactdate)) %>% 
    filter(sc_nofucomp_resn==1|is.na(sc_nofucomp_resn)) %>% 
    select(record_id,site,disch_date,d180_scheduled,d180_close,fu180_visdate,fu180_contactdate,sc_lastvitalstat_date)
},rownames=F)

} ## End of server function tag



