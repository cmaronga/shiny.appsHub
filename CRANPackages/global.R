# load packages
library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(rvest)


# Webscraping date of publication into CRAN -------------------------------

#Specifying the url for desired website to be scraped
url <- 'https://cran.r-project.org/web/packages/available_packages_by_date.html'

# Reading the HTML code from the website
webpage <- read_html(url)


# Date published ----------
date_published <- html_nodes(webpage,'td:nth-child(1)')

# Converting the ranking data to text
date_published_data <- html_text(date_published)

# Name of package ----------
pkg_name <- html_nodes(webpage, "a")


# pkg name data to text
pkg_name_data <- html_text(pkg_name)


# Title of package ----------
pkg_title <- html_nodes(webpage, "td~ td+ td")


# pkg name data to text
pkg_title_data <- html_text(pkg_title)


# Create a data frame -----------------------------------------------------

cran_pkg_date <- data.frame(
  date_published = date_published_data,
  Name = pkg_name_data,
  Title = pkg_title_data
) %>% mutate(
  date_published = lubridate::ymd(date_published)
)

# + Available packages

available_pks <- available.packages(#repos = "http://cran.us.r-project.org", 
                                    repos = "https://cran.r-project.org/")[, c("Version",
                         "Depends",
                         "Repository", 
                         "NeedsCompilation",
                         "License")] %>% 
  as.data.frame()%>% tibble::rownames_to_column(var = "pkg_name")


# Merge with data with dates ----------------------------------------------

CRAN_pkgs <- available_pks %>% 
  left_join(cran_pkg_date, 
            by = c("pkg_name" = "Name")) %>% 
  mutate(
    year_published = lubridate::year(date_published)
  )











