library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(ggthemes)
library(ggbeeswarm)
library(data.table)
library(reactable)
library(reactR)

# Load datasets -----------------------------------------------------------

## ++ Read  chain data
chaindata <- read_csv("chaindata.csv")

## Read in master record ID's for filtering
master_ids <- fread("master_recordIDs_list_v7.csv")

chaindata <- chaindata %>% 
    filter(record_id %in% c(master_ids$record_id))

## read data for note to file/comments
note_to_file <- readxl::read_excel("notetofile_comments_template.xlsx") ## query comments from sites

## read phq all data
phq_data <- read_csv("phq_all_data.csv")%>% 
    filter(record_id %in% c(master_ids$record_id)) %>% 
  left_join(select(chaindata, record_id, categ_enrol, adm_parttype), by = "record_id")

## phq long format data
phq_data_long <- phq_data %>% 
  pivot_longer(cols = c(adm_score, fup_score), names_to = "time_point", values_to = "phq_score")


### ++ read in data for summaries (Landing page) -- Data cleaning
clinical_sum <- read_csv("clinical_variables.csv")

general_clinsum <- read_csv("general_clinical_outcome.csv") 

anthropo_complete <- fread("anthropo_completeness.csv")

cbc_chemistry <- fread("cbc_chem.csv")

## Athropometry data
anthro_data <- fread("anthropometry.csv", na="")

anthro_data <- anthro_data %>% 
  left_join(select(chaindata, record_id, adm_agemons, categ_enrol, site), by = "record_id")

## read outcome data
outcome <- fread("outcome.csv", na.strings  = ".") %>% 
  left_join(select(chaindata, record_id, categ_enrol, mort_type, site), by = "record_id")

# where died as labelled factor
outcome$sc_wherepart_died <- factor(outcome$sc_wherepart_died,
                                    levels = c(1,2,3,99),
                                    labels = c("Study Hosp", "Other Hosp", "Community", "Unknown"))

## ++ manually convert into factors
outcome$dead <- as.factor(outcome$dead)
outcome$dead <- as.factor(outcome$dead)
outcome$adm_dead <- as.factor(outcome$adm_dead)
outcome$readmission <- as.factor(outcome$readmission)


anthro_data <- anthro_data %>% 
  left_join(select(outcome, record_id, dead, adm_dead, readmission), by = "record_id")

## read dates data
dates <- read_csv("dates.csv") %>% 
  left_join(select(chaindata, record_id, site, categ_enrol), by ="record_id")

## readmission outcomes
readm_outcm <- fread("readm_outcome_dates.csv") %>% 
  filter(record_id %in% c(master_ids$record_id)) %>% 
  left_join(select(chaindata, record_id, site, categ_enrol), by = "record_id")



## demographics
demographics <- fread("demographics.csv") %>% 
  select(-site) %>% 
  left_join(outcome, by = "record_id")

## create age group
setnames( demographics, "age_adm", "agemons_adm")
demographics <- demographics %>% 
  mutate(
     age_group = case_when(
      agemons_adm < 6 ~ "<6 months",
      agemons_adm >= 6 & agemons_adm < 12 ~ "6 - 12",
      agemons_adm >= 12 ~ "over 12 months"
    )
  )


# start global previous health conditions
clin_prev_hlth_dat <-  fread("clinical_prev_health_new.csv", na.strings = c(".", "NA"))


nms_clini_prev_health <- c( "adm_neuro" , "adm_sickle_cell_disease", "adm_thalassaemia" ,"adm_visualprob", "adm_weight_change",      
                            "adm_known_tb", "adm_cough_gt14days", "adm_hhold_tbcontact", "adm_suspect_tb", "adm_hivstatus",         
                            "adm_onart" , "adm_onarvs", "adm_hivrdt_now", "adm_pitc_offered", "adm_mother_pmtct",  "hivexposed" ,"hiv_stat")

## end global previous health conditions

# start global haemetology data
chem_cbc_for_viz <- fread("chem_cbc_numeric.csv") %>% setDT()
chem_cbc_for_viz[adm_parttype == 2, event := "comm"]
chem_cbc_for_viz[, event := factor(event, levels = c("adm", "day2", "disch", "day45", "day90", "day180", "readm", "comm"))]
chem_cbc_for_viz <- chem_cbc_for_viz[!is.na(event)]
cbc_vars <-  c("haemoglobin", "haemoglobin_adj", "neutrophils", "rbc", "lymphocytes", "wbc",
               "osinophils", "platelets", "monocytes",  "basophils")


chem_vars <- c("sodium", "alt", "i_phosphate", "potassium", "alk_phosphate", "magnesium",
               "calcium", "urea", "calcium_adj", "albumin", "creatinine","bilirubin")

# end global haemetology data 

## ++ Narshion
#clinical previous history
clin_prev_hlth_dat <-  fread("clinical_prev_health_new.csv", na.strings = c(".", "NA"))


nms_clini_prev_health <- c( "adm_neuro" , "adm_sickle_cell_disease", "adm_thalassaemia" ,"adm_visualprob", "adm_weight_change",      
                            "adm_known_tb", "adm_cough_gt14days", "adm_hhold_tbcontact", "adm_suspect_tb", "adm_hivstatus",         
                            "adm_onart" , "adm_onarvs", "adm_hivrdt_now", "adm_pitc_offered", "adm_mother_pmtct",  "hivexposed" ,"hiv_stat")

#clinical illness at admission
clin_adm_illness_dat <-  fread("clin_adm_illness_data.csv", na.strings   =  c("", ".","NA"))
nms_clin_adm_illness <- names(clin_adm_illness_dat)[!names(clin_adm_illness_dat) %in% c("site","record_id","temp_adm","resprate_adm","heartrate_adm")]

#clinical illness at discharge
clin_disch_illness_dat <-  fread("clin_illness_at_discharge_data.csv", na.strings  =  c("", ".","NA"))
clin_disch_illness_dat[clin_disch_illness_dat == "."] = NA
names(clin_disch_illness_dat) <- gsub("_ ","_" ,names(clin_disch_illness_dat) )
nms_clin_disch_illness <- names(clin_disch_illness_dat)[!names(clin_disch_illness_dat) %in% c("site", "record_id" ,"temp_disch","airway_disch" ,
                                                                                              "absconded_disch", "abscond_date_disch", "resp_rate_disch",                         
                                                                                              "heart_rate_disch", "oxysat_taken_disch", "oxysat_disch")]




clin_disch_illness_dat <- left_join(chem_cbc_for_viz[, .(record_id, categ_enrol, read_died)], clin_disch_illness_dat, by = "record_id")

clin_disch_illness_dat <- distinct(clin_disch_illness_dat, record_id, .keep_all = T)
setDT(clin_disch_illness_dat)
clin_disch_illness_dat[, (nms_clin_disch_illness) := lapply(.SD, factor), .SDcols = nms_clin_disch_illness]
#care giver
care_giver_dat <-  fread("caregiver_data.csv", na.strings = c(".", "NA"))
nms_care_giver <- names(care_giver_dat)[!names(care_giver_dat) %in% c("site","record_id", "hosptravel_cost", "mumage_firstpreg_soc","mother_age_soc",
                                                                      "mum_total_live_births_soc","primary_carer_weight_soc",
                                                                      "primary_carer_muac_soc","primary_carer_height_soc")]



##stacked plot function

stacked_bar_plot <- function(my_df, var_split, var_interest, ...){
  my_df <- my_df[, c(var_split, var_interest)]
  my_df <- my_df %>% na.omit()
  df2 <-  my_df %>% group_by_(var_split, var_interest)  %>%
    summarise(freq= n()) %>%
    mutate(perc = round(freq/sum(freq) * 100, 2)) %>% setDT()

  df2[, df_text := paste("N =", freq, ",", "% =", perc )]
  ggplotly(ggplot(df2,
                  aes_string(x = var_split, y = "perc", fill = var_interest,
                             text ="df_text"))+
             geom_bar(stat = "identity",width = 0.8)+
             scale_fill_economist()+
             theme_minimal()+
             labs(x=  var_split , y="Percentage", fill=""))
}


##single plot function



single_bar_plot <- function(my_df,var_interest, ...){
  df2 <-  my_df %>%tidyr::drop_na(var_interest) %>%
    group_by_(var_interest)  %>%
    summarise(freq= n()) %>%
    mutate(perc = round(freq/sum(freq) * 100, 2)) %>% setDT()
  
  df2[, df_text := paste("N =", freq, ",", "% =", perc )]
  ggplotly(ggplot(df2,
                  aes_string(x = var_interest, y = "perc", fill = var_interest,
                             text ="df_text"))+
             geom_bar(stat = "identity",width = 0.8)+
             scale_fill_economist()+
             theme_minimal()+
             labs(x=  var_interest , y="Percentage", fill=""))
}



