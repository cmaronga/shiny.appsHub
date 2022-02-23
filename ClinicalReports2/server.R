## Load required packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)


server <- function(input, output, session) { 
# Load datasets -----------------------------------------------------------
  
#chaindata() <- read_csv("chaindata().csv")  ## ## old date read method
chaindata <- reactiveFileReader(
  intervalMillis = 600000, # check for changes every 10 mins
  session = session,
  filePath = "chaindata.csv",
  readFunc = readr::read_csv
)

Immunology_Data <- reactiveFileReader(
  intervalMillis = 600000, # check for changes every 10 mins
  session = session,
  filePath = "Immunology_Data.csv",
  readFunc = readr::read_csv
)

# BMC_Data <- read.csv("BMC_Data.csv") ## old date read method
BMC_Data <- reactiveFileReader(
  intervalMillis = 600000, # check for changes every 10 mins
  session = session,
  filePath = "BMC_Data.csv",
  readFunc = readr::read_csv
)

# BMC follow up data
follow_up_data2<- reactiveFileReader(
  intervalMillis = 600000, # check for changes every 10 mins
  session = session,
  filePath = "follow_up_data2.csv",
  readFunc = readr::read_csv
)


# Follow up  <- read.csv("BMC_Data.csv") ## old date read method
follow_up_data <- reactiveFileReader(
  intervalMillis = 600000, # check for changes every 10 mins
  session = session,
  filePath = "follow_up_data.csv",
  readFunc = readr::read_csv
)


#Nutrition_Data <- read_csv("Nutrition_Data.csv")  ## old date read method
Nutrition_Data <- reactiveFileReader(
  intervalMillis = 600000, # check for changes every 10 mins
  session = session,
  filePath = "Nutrition_Data.csv",
  readFunc = readr::read_csv
)


#Sugar_Data <- read_csv("Sugar_Data.csv") ## old date read method
Sugar_Data <- reactiveFileReader(
  intervalMillis = 600000, # check for changes every 10 mins
  session = session,
  filePath = "Sugar_Data.csv",
  readFunc = readr::read_csv
)


# Reading breast milk samples
Breastmilk_Serum <- reactiveFileReader(
  intervalMillis = 600000, # check for changes every 10 mins
  session = session,
  filePath = "Breastmilk_Serum.csv",
  readFunc = readr::read_csv
)


# Reading whole milk samples
Whole_Breastmilk <- reactiveFileReader(
  intervalMillis = 600000, # check for changes every 10 mins
  session = session,
  filePath = "Whole_Breastmilk.csv",
  readFunc = readr::read_csv
)


# Reading Cell_Pellets samples
Cell_Pellets <- reactiveFileReader(
  intervalMillis = 600000, # check for changes every 10 mins
  session = session,
  filePath = "Cell_Pellets.csv",
  readFunc = readr::read_csv
)


# Reading Cell_Pellets samples
stool_samples <- reactiveFileReader(
  intervalMillis = 600000, # check for changes every 10 mins
  session = session,
  filePath = "stool_samples.csv",
  readFunc = readr::read_csv
)



# Reading Young infants datasets ------------------------------------------

# BMC young_infants
bmc_young <- read_csv("BMC_young.csv") %>% 
  filter(record_id != 10002304)

# chain 1 youngs
chain_young_1 <- read_csv("chain_young.csv")

# chain2 youngs
chain_young_2 <- read_csv("young_inf_data.csv")

## merge young infants data
young_infants_data <- reactive({
  reduce(
    list(bmc_young, chain_young_1, chain_young_2),
    plyr::rbind.fill
  )
})


# Little bit of data processing -------------------------------------------

## Summary tab figure computations -------
summary_tab <- reactive({chaindata() %>% 
  filter(cohort == "Phase 2") %>% 
  summarise(
    `Total discharged` = length(which(!is.na(disch_date))),
    `Total readmitted` = length(which(!is.na(readm_date))),
    `Total mortalities`= length(which(sc_nofucomp_resn == 3)),
    d45fups = length(which(!is.na(fu45_visdate)|!is.na(fu45_contactdate))),
    d90fups = length(which(!is.na(fu90_visdate)|!is.na(fu90_contactdate))),
    d180fups = length(which(!is.na(fu180_visdate)|!is.na(fu180_contactdate))),
    `total fups` = d45fups + d90fups + d180fups
  )})


# ++Start fo server side code creation ++ -----------------------------------

output$phase1 <- renderValueBox({
    valueBox(
      value = nrow(young_infants_data()),
      subtitle = h4('All Young Infants Data'),
      icon = icon("stats",lib='glyphicon') ,color = "purple")  
  })
    
output$under6m <- renderValueBox({
    valueBox(
      value = nrow(Immunology_Data()),
      subtitle = h4('Immunology Enrollments'),
      icon = icon("stats",lib='glyphicon'),
      color = "green")  
  })
    
output$cp <- renderValueBox({
    valueBox(
      value = nrow(BMC_Data()),
      subtitle = h4('BMC Enrolments'),
      icon = icon("stats",lib='glyphicon'),
      color = "yellow")  
  })


output$op <- renderValueBox({
    valueBox(
      value = nrow(Immunology_Data() %>% filter(!is.na(adm_date) & categ_enrol =="OP-SAM")),
      subtitle = h4('Total Outpatient SAM'),
      icon = icon("stats",lib='glyphicon'),
      color = "aqua")  
  })
     

# summary of young infants enrolled participants ----------------------------------------------------------
output$summary <- renderDataTable({
young_infants_data() %>% filter(!is.na(categ_enrol)) %>% 
  group_by(categ_enrol) %>% 
  summarise(
    `< 2 months` = length(which(adm_agemons < 2)),
    `2 - 6 months` = length(which(adm_agemons >= 2 & adm_agemons <= 6)),
    `Total Enroled` = `< 2 months` + `2 - 6 months`)
}, rownames = F)

# Summary of young infants outcomes ---------------------------------------
output$commun <- renderDataTable({
young_infants_data() %>% 
    filter(!is.na(categ_enrol), categ_enrol!= "Community") %>% 
  group_by(categ_enrol) %>% 
  summarise(
    `< 2 months` = length(which(adm_agemons < 2)),
    died1 = length(which(adm_agemons < 2 & sc_nofucomp_resn == 3)),
    `2 - 6 months` = length(which(adm_agemons >= 2 & adm_agemons <= 6)),
    died2 = length(which(adm_agemons >= 2 & adm_agemons <= 6 & sc_nofucomp_resn ==3))
  ) %>% 
    janitor::adorn_totals("row") %>% 
    mutate(
      `< 2 months` = paste(died1, "(", round(died1/`< 2 months` * 100, 2), "%", ")", sep = ""),
      `2 - 6 months`= paste(died2, "(", round(died2/`2 - 6 months` * 100, 2), "%", ")", sep = "")
    ) %>% 
    select(-c(died1,died2))
}, rownames = F)


## Summary tab (second tab) -------------
  output$tot_disch <- renderInfoBox({
    infoBox(
      "Total discharged", 
      summary_tab()[1], 
      icon = icon("list"),
      color = "olive"
    )  
  })

output$tot_fup <- renderInfoBox({
    infoBox(
      "Total follow ups", 
      summary_tab()[7], 
      icon = icon("list"),
      color = "navy"
    )  
  })
  
output$tot_readm <- renderInfoBox({
    infoBox(
      "Readmitted", 
      summary_tab()[2], 
      icon = icon("list"),
      color = "fuchsia"
    )  
  })
    
    
output$tot_death <- renderInfoBox({
    infoBox(
      "Mortalities", 
      summary_tab()[3], 
      icon = icon("list"),
      color = "yellow"
    )  
  })

  output$bmc <- renderInfoBox({
    infoBox(
      "BMC ", 
      paste("Enrolled:", nrow(BMC_Data())), 
      icon = icon("list"),
      color = "yellow"
    )  
  })
  
output$immunology <- renderInfoBox({
    infoBox(
      "Immunology ", 
      value = paste("Enrolled:", nrow(Immunology_Data())), 
      icon = icon("list"),
      color = "yellow"
    )  
  })
    
output$nutrition <- renderInfoBox({
    infoBox(
      "Nutrition", 
      paste("Enrolled:", Nutrition_Data() %>% filter(redcap_event_name == "participant_enrolm_arm_1") %>% nrow()), 
      icon = icon("list"),
      color = "yellow"
    )  
  })
      
output$sugar <- renderInfoBox({
    infoBox(
      "Sugar ", 
      paste("Enrolled:", nrow(Sugar_Data())), 
      icon = icon("list"),
      color = "yellow"
    )  
  })

  
 ## ++ Summary of follow up visits -----------
 output$fup_sum <- renderDataTable({
  chaindata() %>% 
  filter(cohort == "Phase 2", !site %in% c("Matlab", "Nairobi")) %>% 
  group_by(site) %>% 
    summarise(
      `day 45` = length(which(!is.na(fu45_visdate)|!is.na(fu45_contactdate))),
      `day 90` = length(which(!is.na(fu90_visdate)|!is.na(fu90_contactdate))),
      `day 180` = length(which(!is.na(fu180_visdate)|!is.na(fu180_contactdate)))
    )

 }, rownames = F)
 
 
## Age distribution plot
output$age_distr <- renderPlot({
chaindata() %>% 
  filter(cohort == "Phase 2", adm_parttype == 1, adm_agemons <= 6) %>%
  ggplot(aes(site,adm_agedays))+geom_boxplot(aes(fill = site)) + theme_minimal()+
   theme(legend.position = "none", axis.text.x = element_text(face = "bold", size = 13)) + labs(y= "age in days", x= "")

})
 
 
output$age_distr2 <- renderPlot({
chaindata() %>% 
  filter(cohort == "Phase 2", adm_parttype == 1, adm_agemons <= 6) %>%
  ggplot(aes(site,adm_agemons))+geom_boxplot(aes(fill = site)) + theme_minimal()+
   theme(legend.position = "none", axis.text.x = element_text(face = "bold", size = 13)) + labs(y= "age in months", x= "")

})

## BMC summaries -----
output$bmc_sum <- renderDataTable({
BMC_Data() %>% 
  group_by(site) %>% 
  summarise(
    Hospitalised = length(which(participant_type == "Hospitalised")),
    Community = length(which(participant_type == "Community")),
    `Total` = Hospitalised + Community
  ) %>% janitor::adorn_totals("row")
}, rownames = F)


## BMC Nutritional groups
output$bmc_categ <- renderDataTable({
  BMC_Data()%>% 
    filter(!is.na(categ_enrol)) %>% 
  group_by(categ_enrol, site) %>% 
  summarise(
    total = n()
  ) %>% spread(categ_enrol, total) %>% janitor::adorn_totals("row")
}, rownames = F)


## BMC follow up visit alerts
output$bmc_alert <- renderDataTable({
  df  = follow_up_data2() %>% 
    group_by(site) %>% filter(!is.na(disch_date)  & site == input$site_id_follow & day == input$time_point & is.na(followup_date)) %>%
    mutate(missed = ifelse(as.Date(Sys.time()) - as.Date(close) > 0, "Yes", "No")) %>% select(-followup_date, -contactdate)}, rownames = F,
  options = list(scrollX = TRUE))


output$bmc_alert_follo <- renderDataTable({
  df  = follow_up_data2() %>% 
    group_by(site) %>% filter(!is.na(disch_date)  & site == input$site_id_follow & day == input$time_point & is.na(followup_date)) %>%
    mutate(missed = ifelse(as.Date(Sys.time()) - as.Date(close) > 0, "Yes", "No")) %>% select(-followup_date, -contactdate) %>%
    filter(missed == "No") %>% filter(between(as.Date(scheduled),as.Date(Sys.time()),  as.Date(Sys.time()) + 7))
  }, rownames = F,
  options = list(scrollX = TRUE))




### +++ BMC Samples reporting

output$brst_serum <- renderDataTable({
  Breastmilk_Serum() %>% 
  group_by(site, aliquot, time_point) %>% 
  summarise(
    total = n()
  ) %>% 
  spread(time_point,total) %>% 
    filter(site == input$site_id)
}, rownames = F)


output$whle_serum <- renderDataTable({
  Whole_Breastmilk() %>% 
  group_by(site, aliquot, time_point) %>% 
  summarise(
    total = n()
  ) %>% 
  spread(time_point,total)%>% 
    filter(site == input$site_id)
}, rownames = F)

output$cell_pels <- renderDataTable({
Cell_Pellets() %>% 
  group_by(site, time_point) %>% 
  summarise(
    total = n()
  ) %>% 
  spread(time_point,total)%>% 
    filter(site == input$site_id)

}, rownames = F)

output$stool <- renderDataTable({
stool_samples() %>% 
  group_by(site, aliquot, time_point) %>% 
  summarise(
    total = n()
  ) %>% 
  spread(time_point,total)%>% 
    filter(site == input$site_id)

}, rownames = F)


output$sum_figs <- renderDataTable({
BMC_Data() %>% 
  group_by(site) %>% 
  summarise(
    Ward = length(which(participant_type == "Hospitalised")),
    CP = length(which(participant_type == "Community")),
    d45 = length(which(!is.na(bmc_supplements)))
    )%>% 
    filter(site == input$site_id)
}, rownames = F)


### IMMUNOLOGY STUDY REPORTS --------
output$im_enrol <- renderValueBox({
    valueBox(
      value = nrow(Immunology_Data() %>% filter(!is.na(adm_date))), ## total enrolled
      subtitle = paste('Total Enrolled'),
      icon = icon("stats",lib='glyphicon') ,color = "purple")  
  })

output$im_ward <- renderValueBox({
    valueBox(
      value = nrow(Immunology_Data() %>% filter(!is.na(adm_date) & adm_parttype ==1)), ## Ward participants
      subtitle = paste('Ward participants enrolled'),
      icon = icon("stats",lib='glyphicon'),
      color = "green")  
  })
    
output$im_op <- renderValueBox({
    valueBox(
      value = nrow(Immunology_Data() %>% filter(!is.na(adm_date) & categ_enrol =="OP-SAM")), # total OP
      subtitle = paste('Total Outpatient SAM'),
      icon = icon("stats",lib='glyphicon'),
      color = "yellow")  
  })

output$im_disch <- renderValueBox({
    valueBox(
      value = nrow(Immunology_Data() %>% filter(categ_enrol == "Community")), ## total discharged
      subtitle = paste('Community Participants'),
      icon = icon("stats",lib='glyphicon'),
      color = "aqua")  
  })

  }