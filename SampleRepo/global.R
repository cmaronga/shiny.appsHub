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
library(showtext)
library(ggsci)
library(ggthemes)
library(shinyWidgets)
library(ggsci)
library(ggthemes)
## percentage of available samples ncc, main cohort etc

down_function <- function(df, file_name){
    
    downloadHandler(
        # function for file name
        filename = function(){
            paste0(file_name,".csv")
        },
        
        content = function(file){
            write.csv(df, row.names = F, na = "", file)
        }
    )
}


#load("lab_report/data/lab_report_samples.rda")
load("data/lab_report_samples.rda")
list2env(lab_report_samples, .GlobalEnv)


setDT(lab_storage_after)
lab_storage_after <- lab_storage_after[!is.na(categ_enrol)]
type_name_s <- c("RECTAL SWAB", "PLASMA", "SERUM", "STOOL", "BLOOD EDTA")
setorder(lab_storage_after, categ_enrol)
lab_storage_after <- lab_storage_after[aliquot < 3]
lab_storage_after2 <- lab_storage_after[type_name %in% type_name_s]
lab_storage_after <- lab_storage_after[!is.na(categ_enrol)]
lab_storage_after[, aliquot := ifelse(aliquot == 0, 1, aliquot)]
lab_storage_after[, time_point := ifelse(time_point == "CP", "A0", time_point)]

lab_storage_after <- lab_storage_after[aliquot < 3]

time_points_vec <- c("A0", "D0", "D45", 
                     "readm", "D90", "D180")

all_timepoints <-  lab_storage_after[, unique(time_point)]
all_typename <-  lab_storage_after[, unique(type_name)]
all_studies <-  lab_storage_after[, unique(study)]
all_sites <-  lab_storage_after[, unique(site)]

deaths_select <-c("Died", "Alive")
# fg = "white", primary = "black",
#,
mytheme <-  bs_theme(bootswatch = "lumen",
                     # Controls the default grayscale palette
                    #bg = "#202123", fg = "#B8BCC2",
                     # Controls the accent (e.g., hyperlink, button, etc) colors
                     primary = "#EA80FC", secondary = "#48DAC6",
                     base_font = c("Grandstander", "sans-serif"),
                     code_font = c("Courier", "monospace"),
                     heading_font = "'Helvetica Neue', Helvetica, sans-serif",
                     # Can also add lower-level customization
                     "input-border-color" = "#EA80FC")


# mytheme <- bs_theme(
#     # Controls the default grayscale palette
#     bg = "#202123", fg = "#B8BCC2",
#     # Controls the accent (e.g., hyperlink, button, etc) colors
#     primary = "#EA80FC", secondary = "#48DAC6",
#     base_font = c("Grandstander", "sans-serif"),
#     code_font = c("Courier", "monospace"),
#     heading_font = "'Helvetica Neue', Helvetica, sans-serif",
#     # Can also add lower-level customization
#     "input-border-color" = "#EA80FC"
# )

#load("lab_report/data/lab_freezer.rda")
load("data/lab_freezer.rda")
list2env(lab_freezer_space, .GlobalEnv)
rm(lab_freezer_space)


render_dt <-  function(dt, pageLength =15 ){
    
    renderDataTable(
    dt,
    style = 'bootstrap4',
    options = list(scrollX = TRUE,
                   pageLength = pageLength))
    
    }

