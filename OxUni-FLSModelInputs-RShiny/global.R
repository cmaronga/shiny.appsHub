# load packages
library(shinydashboard)
library(shiny)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(plotly)
library(ggbeeswarm)
library(tidyselect)
library(lubridate)
library(data.table)
library(scales)
library(janitor)
library(shinythemes)
library(bslib)
library(showtext)
library(ggsci)
library(ggthemes)
library(shinyWidgets)
library(DBI)
library(RSQLite)
library(here)
library(stringr)
library(googlesheets4)
library(foreach)
library(reactable)


## percentage of available samples ncc, main cohort etc



down_df <- function(df, file_name){
  
  downloadHandler(
    
    # function for file name (name of the dataset on file)
    filename = function(){
      paste0(file_name,".csv")
    },
    
    # Handling how to export the dataset
    content = function(file){
      write.csv(df, row.names = F, na = "", file)
    }
  )
}


mytheme <-  bs_theme(bootswatch = "lumen",
                     # Controls the default grayscale palette
                     #bg = "#202123", fg = "#B8BCC2",
                     # Controls the accent (e.g., hyperlink, button, etc) colors
                     primary = "#EA80FC", 
                     secondary = "#48DAC6",
                     base_font = c("Grandstander", "sans-serif"),
                     code_font = c("Courier", "monospace"),
                     heading_font = "'Helvetica Neue', Helvetica, sans-serif",
                     # Can also add lower-level customization
                     "input-border-color" = "#EA80FC")


# # Function needed for reading inputs
# CollectGsheetData <- function(connection = NULL,
#                               table_name = NULL){
#   
#   # create a connection
#   tbl_conn <- dbConnect(SQLite(), 
#                         paste0("inputs_databases/",connection, ".db"))
#   # glance at the data
#   tbl(tbl_conn, table_name)
#   
# }


# Function to list fields and get data
CollectGsheetData <- function(connection = "Genericcountry",
                              table_name = "FLS_Basecase",
                              na.rm.value = TRUE){
  # store scenario name on the global enviroment
  scenarioName <<- table_name
  
  # create a connection
  tbl_conn <- dbConnect(SQLite(),
                        here("inputs_databases",
                             paste0(connection, ".db")))
  # glance at the data
  if(na.rm.value == TRUE){
    tbl(tbl_conn, table_name) %>% filter(!is.na(Value))
  } else {
    tbl(tbl_conn, table_name)
  }

}



active_working.dir<- getwd()











