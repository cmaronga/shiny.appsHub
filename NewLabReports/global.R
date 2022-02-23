# load packages
library(shinydashboard)
library(shiny)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(plotly)
library(ggthemes)
library(ggbeeswarm)
library(here)
library(naniar)
library(tidyselect)
library(lubridate)
library(DiagrammeR)
library(reactable)
library(data.table)
library(scales)
library(janitor)
library(reactable)
library(tictoc)
library(shinythemes)
library(bslib)
down_function <- function(df, file_name){
    
    downloadHandler(
        # function for file name
        filename = function(){
            paste0(file_name,".csv")
        },
        
        content = function(file){
            write.csv(df, row.names = F, na = "",file)
        }
    )
}


load("data/lab_report_samples.rda")
list2env(lab_report_samples, .GlobalEnv)


setDT(lab_storage_after)
type_name_s <- c("RECTAL SWAB", "PLASMA", "SERUM", "STOOL", "BLOOD EDTA")
lab_storage_after2 <- lab_storage_after[type_name %in% type_name_s]
lab_storage_after2 <- lab_storage_after2[!is.na(categ_enrol)]