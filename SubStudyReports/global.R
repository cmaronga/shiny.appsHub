# load packages
library(shinydashboard)
library(shinydashboardPlus)
library(shiny)
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
library(rmarkdown)
library(bslib)

# -------------------------------------------------------------------------

# Loading data section (Load and process all data) ------------------------------------------------

# ++++ Clinical datasets +++ ---------------------------------

load("clinical_datasets/neobac_data.RDA",
     envir = .GlobalEnv)
load("clinical_datasets/weekly_screening_list.RDA",
     envir = .GlobalEnv)

setDT(miss_cin_num)

miss_cin_num[, adm_date := as.Date(adm_date)]
query_date <- Sys.Date() - 21
miss_cin_num <- miss_cin_num[adm_date < query_date]
setDF(miss_cin_num)
list2env(weekly_screening_list, .GlobalEnv)
rm(weekly_screening_list)
start_date_week <- report_date - 6
miss_abx_name <- left_join(miss_abx_name,
                           kilifi_metrics[, c("patient_serial_number", "date_disch")],
                           by = "patient_serial_number")

miss_abx_name <- miss_abx_name %>%
   filter(!is.na(date_disch))


# ++++ Lab datasets +++ ---------------------------------

load("lab_datasets/neobac_lab_queries.RDA")

blood_culture[, date_collect := as.Date(date_collect)]

# weekly culture by site
blood_culture[, week_number := lubridate::week(date_collect)]

weekly_per_site <- blood_culture[, .(number_per_week = .N), by = .(site, week_number)] %>%
   .[, .(average_per_week = round(mean(number_per_week)),
         median_per_week = round(median(number_per_week))), by = site] %>%
   arrange(desc(average_per_week))

# End of data reading and processing --------------------------------------
# + For the flow chart, avoid index referencing for when something might change

Klf_inpat <- table(all_paeds$disch_type)[["died"]]

overal_adms <- sum(table(base_list$site)[c("kiambu", "mbagathi")]) + nrow(all_paeds)



# Flow charts section -------------------------------------------------------------

# Kilifi
kilifi_chart <- "digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, style = filled, fillcolor = Linen]
      neon_adm [label = '@@1', fillcolor = MistyRose]
      blood_cult [label = '@@2']
      d28_consent [label = '@@3']
      inpat_death [label = '@@4', shape = cds, fillcolor = Gold]
      d28success [label = '@@5']
      d28died_prior [label = '@@6 ']
      d28ltfup [label = '@@7']
      d28withdrew [label = '@@8']

      # edge definitions with the node IDs
      neon_adm -> blood_cult -> d28_consent-> d28success;

      blood_cult -> inpat_death;
      d28_consent -> d28died_prior;
      d28_consent -> d28ltfup;
      d28_consent -> d28withdrew;
      }
      [1]: paste0('Neonatal admissions \\n ( N = ', nrow(all_paeds), ')')
      [2]: paste0('Blood cultures done \\n ( n = ', table(blood_culture$site)[[2]], ')')
      [3]: paste0('Consented for day 28 \\n ( n = ', table(base_list$site, base_list$consent_day28)[[2,2]], ')')
      [4]: paste0('Died in hospital \\n ( n = ', Klf_inpat, ')')
      [5]: paste0('Alive at day 28 \\n ( n = ', table(d28_success$site)[[1]], ')')
      [6]: paste0('Died prior to day 28 \\n ( n = ', table(diedb4_d28$site)[[1]], ')')
      [7]: paste0('Lost to follow up \\n ( n = ', table(outcome_ltfup$site)[[1]], ')')
      [8]: paste0('Withdrawals \\n ( n = ', table(outcome_withdrew$site)[[1]], ')')
      "

# mbagathi
mbagathi_chart <- "digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, style = filled, fillcolor = Linen]
      neon_adm1 [label = '@@1', fillcolor = MistyRose]
      blood_cult1 [label = '@@2']
      d28_consent1 [label = '@@3']
      inpat_death1 [label = '@@4', shape = cds, fillcolor = Gold]
      d28success1 [label = '@@5']
      d28died_prior1 [label = '@@6 ']
      d28ltfup1 [label = '@@7']
      d28withdrew1 [label = '@@8']

      # edge definitions with the node IDs
      neon_adm1 -> blood_cult1 -> d28_consent1-> d28success1;

      blood_cult1 -> inpat_death1;
      d28_consent1 -> d28died_prior1;
      d28_consent1 -> d28ltfup1;
      d28_consent1 -> d28withdrew1;
      }
      [1]: paste0('Neonatal admissions \\n ( N = ', table(base_list$site)[[3]], ')')
      [2]: paste0('Blood cultures done \\n ( n = ', table(blood_culture$site)[[3]], ')')
      [3]: paste0('Consented for day 28 \\n ( n = ', table(base_list$site, base_list$consent_day28)[[3,2]], ')')
      [4]: paste0('Died in hospital \\n ( n = ', nrow(mbagathi_inpat), ')')
      [5]: paste0('Alive at day 28 \\n ( n = ', table(d28_success$site)[[2]], ')')
      [6]: paste0('Died prior to day 28 \\n ( n = ', table(diedb4_d28$site)[[2]], ')')
      [7]: paste0('Lost to follow up \\n ( n = ', table(outcome_ltfup$site)[[2]], ')')
      [8]: paste0('Withdrawals \\n ( n = ', table(outcome_withdrew$site)[[2]], ')')
      "

# Kiambu diagram
kiambu_chart <- "digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, style = filled, fillcolor = Linen]
      neon_admkiambu [label = '@@1', fillcolor = MistyRose]
      blood_cultkiambu [label = '@@2']
      inpat_deathkiambu [label = '@@3', shape = cds, fillcolor = Gold]

      # edge definitions with the node IDs
      neon_admkiambu -> blood_cultkiambu;
      blood_cultkiambu -> inpat_deathkiambu


      }
      [1]: paste0('Neonatal admissions \\n ( N = ', table(base_list$site)[[1]], ')')
      [2]: paste0('Blood cultures done \\n ( n = ', table(blood_culture$site)[[1]], ')')
      [3]: paste0('Died in hospital \\n ( n = ', nrow(kiambu_inpat), ')')
      "

# Overal flow CHART

overal_chart <- "digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, style = filled, fillcolor = Linen]
      neon_adm [label = '@@1', fillcolor = MistyRose]
      bld_done [label = '@@2', fillcolor = MistyRose]
      inpat_deaths [label = '@@3', shape = cds, fillcolor = Gold]
      kil_mbagath [label = '@@4']
      d28_consent [label = '@@5']
      refused_d28 [label = '@@6']
      d28success [label = '@@7']
      d28died_prior [label = '@@8 ']
      d28ltfup [label = '@@9']
      d28withdrew [label = '@@10']

      # edge definitions with the node IDs

      neon_adm -> kil_mbagath;
      neon_adm -> bld_done;


      kil_mbagath -> d28_consent-> d28success;
      kil_mbagath -> refused_d28;

      d28_consent -> d28died_prior;
      d28_consent -> d28ltfup;
      d28_consent -> d28withdrew;
      }
      [1]: paste0('Neonatal admissions \\n ( N = ', overal_adms, ')')
      [2]: paste0('Blood Cultures done \\n ( N = ', nrow(blood_culture), ')')
      [3]: paste0('Inpatient deaths \\n', nrow(inpatdeaths),'(', percent(nrow(inpatdeaths)/overal_adms), ')')
      [4]: paste0('Kilifi & Mbagathi \\n ( N = ', table(base_list$site)[[2]]+table(base_list$site)[[3]], ')')
      [5]: paste0('Consented for day 28 \\n ( n = ', table(base_list$consent_day28)[[2]], ')')
      [6]: paste0('Refused day 28 consent \\n', table(base_list$consent_day28)[[1]], '(', percent(table(base_list$consent_day28)[[1]]/(table(base_list$site)[[2]]+table(base_list$site)[[3]])), ')')
      [7]: paste0('Alive at day 28 \\n ( n = ', nrow(d28_success), ')')
      [8]: paste0('Post-discharge deaths \\n ( n = ', nrow(diedb4_d28), ')')
      [9]: paste0('Lost to follow up \\n ( n = ', nrow(outcome_ltfup), ')')
      [10]: paste0('Withdrawals \\n ( n = ', nrow(outcome_withdrew), ')')
      "



## For neobac Report Data Sets
blood_culture_report <- fread("lab_datasets/blood_culture.csv")

ast_report <- fread("lab_datasets/AST_results.csv")

neobac_ids_report <- fread("lab_datasets/neobac_ids.csv")


blood_culture_report[is.na(bac_positive), bac_positive := ifelse(neg_result == "no growth", "no", bac_positive)]
blood_culture_report[is.na(bac_positive), bac_positive := ifelse(neg_result == "false positive", "no", bac_positive)]
blood_culture_report[is.na(bac_positive), bac_positive := "missing"]
blood_culture_report[, bac_positive := factor(bac_positive, levels = c("no", "yes", "missing"))]
neobac_ids_report[, adm_date := as.Date(adm_date)]
setorder(blood_culture_report, serial_study_id, adm_date)
blood_culture_report <- blood_culture_report %>%
   distinct(serial_study_id, .keep_all = T)



neobac_ids_report <- neobac_ids_report %>%
   distinct(patient_serial_number, .keep_all = T)

neobac_ids_report[, site := NULL]
nms_ids <- names(neobac_ids_report)

blood_culture_report[, (nms_ids) := NULL]
blood_culture_report <- left_join(blood_culture_report,
                              neobac_ids_report,
                              by = c("serial_study_id" = "patient_serial_number" )) %>%
   setDT()


blood_culture_report <- blood_culture_report %>%
   distinct(serial_study_id, .keep_all = T)


blood_culture_report[, receive_antibio_adm_b4_bld := factor(receive_antibio_adm_b4_bld,
                                                         levels = c("No", "Yes"),
                                                         labels = c("Before antibiotics", "After antibiotics"))]

blood_culture_report[, receive_antibio_adm_b4_bld :=
                     fct_explicit_na(receive_antibio_adm_b4_bld,
                                     na_level = "Missing")]



## Chart for Report


# 1. Make a play graph
tmp = DiagrammeR::grViz(overal_chart )

# 2. Convert to SVG, then save as png
tmp = DiagrammeRsvg::export_svg(tmp)
tmp = charToRaw(tmp) # flatten
rsvg::rsvg_png(tmp, "overall_chart.png")


mwd<- getwd()



theme_plot <- function() {
   theme_minimal() +
      theme(
         text = element_text(family = "Bookman", color = "gray25"),
         plot.subtitle = element_text(size = 12),
         plot.caption = element_text(color = "gray30"),
         plot.background = element_rect(fill = "gray95"),
         plot.margin = unit(c(5, 10, 5, 10), units = "mm")
      )
}

mytheme <-  bs_theme(bootswatch = "lumen", version = 3,
                     # Controls the default grayscale palette
                     #bg = "gray",# fg = "#B8BCC2",
                     # Controls the accent (e.g., hyperlink, button, etc) colors
                     #primary = "#EA80FC", secondary = "#48DAC6",
                     base_font = c("Grandstander", "sans-serif"),
                     code_font = c("Courier", "monospace"),
                     heading_font = "'Helvetica Neue', Helvetica, sans-serif",
                     # Can also add lower-level customization
                     "input-border-color" = "#EA80FC")


render_dt <-  function(expr, pageLength =15, ...){

   renderDataTable(expr,
      style = 'bootstrap4',
      options = list(scrollX = TRUE,
                     pageLength = pageLength), ...)

}

# Function to dowload files/tables from UI end
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



## CIN Report

load("clinical_datasets/cin_report.rda")

list2env(cin_report, .GlobalEnv)

rm(cin_report)
