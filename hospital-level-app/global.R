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


# 1. Function to transform a dataframe (pivot_longer, then pivot wider)

df_transform <- function(d_frame, names_col = "metric"){
  
  d_frame %>% 
    pivot_longer(cols = -1, names_to = {{names_col}}, values_to = "vals") %>% 
    pivot_wider(names_from = 1, values_from = 3)
}

# 2. Function to help print nice numeric output
nice_numOut <- function(dframe){
  dframe %>% 
    mutate_if(is.numeric, ~ comma(.)) %>% 
    mutate_at(vars(ncol(dframe)), ~ sub("\\.\\d+$", "", .)) # just to make sure
  # last column has no decimal points
}


# mytheme <-  bs_theme(bootswatch = "minty",
#                      # Controls the default grayscale palette
#                      bg = "#202123", fg = "#B8BCC2",
#                      # Controls the accent (e.g., hyperlink, button, etc) colors
#                      primary = "#EA80FC", secondary = "#48DAC6",
#                      base_font = c("Grandstander", "sans-serif"),
#                      code_font = c("Courier", "monospace"),
#                      heading_font = "'Helvetica Neue', Helvetica, sans-serif",
#                      # Can also add lower-level customization
#                      "input-border-color" = "#EA80FC")


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

























