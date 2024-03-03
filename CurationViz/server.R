## Load required packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(ggthemes)
library(ggbeeswarm)
library(data.table)
library(here)

source("global.R", local = TRUE)

server <- function(input, output) {
  
# Start of visualizations -----------------------------------------------------------
  
## ++ Visualizing anthropomentry dataset ------

## ++ scatterplot of all numeric variables
output$scatter <- renderPlotly({
  ggplotly(ggplot(anthro_data,aes_string(input$muac_x, input$muac_y, col = "categ_enrol"))+geom_point(size = 0.8) + 
             theme_minimal() + labs(col = "") + scale_color_brewer(palette = "Dark2")
           )
})


## ++ Histogrames by ABC for all numeric variables
output$anth_hists <- renderPlotly({
  ggplotly(
    ggplot(anthro_data, aes_string(input$anth_hist_x))+geom_histogram(binwidth = 1, col = "white",
                                                   fill="steelblue", aes(y=..density..))+
  theme_hc() + facet_wrap(~categ_enrol)
  )
})

## ++ Box plot for ABC vs all numeric variables
output$abc_anth_box <- renderPlotly({
  ggplotly(
    ggplot(anthro_data,aes_string("categ_enrol", input$abc_anth_y))+geom_boxplot(fill="steelblue") + 
             theme_minimal() + labs(col = "") + scale_color_brewer(palette = "Dark2")+
  labs(x="")
  )
})

## ++ summary table of boxplot by ABC of numeric variables
output$abc_anth_box_sum <- renderDataTable({
  anthro_data %>% group_by(categ_enrol) %>% 
  summarise(
    Mean = round(mean(get(input$abc_anth_y), na.rm = T),3)
  )
}, rownames = F)


## ++ Box plot of all numeric variables by site
output$site_anth_box <- renderPlotly({
  ggplotly(
    ggplot(anthro_data,aes_string("site", input$site_anth_y))+geom_boxplot(fill="steelblue") + 
             theme_minimal() + labs(col = "") + scale_color_brewer(palette = "Dark2")+
  labs(x="")
  )
})

## summary table of box plot of numerical variables by site
output$site_anth_box_sum <- renderDataTable({
  anthro_data %>% group_by(site) %>% 
  summarise(
    Mean = round(mean(get(input$site_anth_y), na.rm = T),3)
  )
}, rownames = F)


## ++ Box plot of numerical variables vs outcome by ABC
output$out_anth_box <- renderPlot({
    ggplot(filter(anthro_data, categ_enrol != "Community"),aes_string("categ_enrol", input$abc_anth_y_out, fill="dead"))+geom_boxplot() + 
             theme_minimal() + labs(col = "") + scale_color_brewer(palette = "Dark2")+
  labs(x="") + scale_fill_brewer(palette = "Dark2")

})

## ++ Box plot of numerical variables vs outcome by site
output$site_anth_box_out <- renderPlot({
    ggplot(filter(anthro_data, categ_enrol != "Community"),aes_string("site", input$site_anth_y_out, fill="dead"))+geom_boxplot() + 
             theme_minimal() + labs(col = "") + scale_color_brewer(palette = "Dark2")+
  labs(x="") + scale_fill_brewer(palette = "Dark2")

})



## Visualizing dates data
output$dates_plots <- renderPlotly({
  
  ggplotly(
    ggplot(dates, aes_string(input$date_x_axis, input$date_y_axis))+geom_point(size=0.8) + theme_minimal()
  )
})

## summary table of dates dataset
output$dates_sum <- renderDataTable({
dates %>% 
  group_by(get(input$sum_dates)) %>% 
  summarise(
    `discharged` = length(which(!is.na(date_disch))),
    `d45 visit` = length(which(!is.na(visdate_fu45))),
    `d90 visit` = length(which(!is.na(visdate_fu90))),
    `d180 visit` = length(which(!is.na(visdate_fu180))),
    `readm1`= length(which(!is.na(date_readm))),
    `readm2` = length(which(!is.na(date2_readm))),
    `readm3` = length(which(!is.na(date3_readm))),
    `readm4` = length(which(!is.na(date4_readm)))
  ) %>% janitor::adorn_totals("row")
})

## ABC dotplots
output$abc_dotplot <- renderPlot({
chaindata %>% 
    filter(adm_parttype== 1) %>% 
    select(record_id,site,categ_enrol,muac_enrol,muac_disch,muac_d45,muac_d90,muac_d180) %>% 
    rename('Admission'='muac_enrol','Discharge'='muac_disch','Day 45'='muac_d45','Day 90'='muac_d90','Day 180'='muac_d180') %>% 
    tidyr::gather("timepoint","muac",-c(record_id,site,categ_enrol)) %>% 
    filter(muac < 20) %>% 
    ggplot(aes(timepoint,muac))+
    geom_quasirandom(method = "smiley",size=1.5,col="darkblue")+
    stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                 geom = "crossbar", width = 0.5, color="Red")+theme_hc()+
    labs(caption="source:chain Network data",y="MUAC (cm)",title=paste("MUAC distribution across timepoints"))+
    scale_x_discrete(name ="Time point", 
                     limits=c("Admission","Discharge","Day 45","Day 90","Day 180"))+
    scale_y_continuous(breaks = seq(4,20,2)) + facet_wrap(~get(input$facet_by))
})

## Demographic dataset
output$demogra_stack <- renderPlotly({
ggplotly(
ggplot(demographics, aes_string("age_group", fill = input$demograph_vars))+geom_bar(width = 0.8, position = "fill")+
 scale_fill_economist()+theme_minimal()+labs(x="age bracket", y="", fill="")+
   scale_y_continuous(labels = scales::percent)
 ) 
})

## summary table for demographics
output$demogra_sumtab <- renderDataTable({
demographics %>% 
  group_by(get(input$demograph_vars)) %>% 
  summarise(
    Male = length(which(sex_adm == 1)),
    Female = length(which(sex_adm == 2)),
    `mean age` = round(mean(agemons_adm, na.rm = T),2),
    `meadian age` = round(median(agemons_adm, na.rm = T),2),
    readmitted = length(which(readmission == 1)),
    `lost/withdrew` = length(which(ltfu == 1)),
    Absconded = length(which(absconded == 1)),
    ward = length(which(parttype_adm == 1)),
    CP = length(which(parttype_adm == 2))
  )
}, rownames = F)




##previous health


output$prev_gen_bar <- renderPlotly({
  setDF(clin_prev_hlth_dat)
  ggplotly(single_bar_plot(my_df =clin_prev_hlth_dat, var_interest =  input$prev_options))
})


output$split_bar_prev_hlth <- renderPlotly({
  
  setDF(clin_prev_hlth_dat)
  ggplotly(stacked_bar_plot(my_df =clin_prev_hlth_dat,var_split =  input$prev_options_split,
                            var_interest =   input$prev_options))
})
### ++ Start of OUtcome dataset ------

## summary table 1
output$outc_sum <- renderDataTable({
outcome %>% 
  group_by(get(input$table_var)) %>% 
  summarise(
    `inpatient deaths` = length(which(mort_type == "inpatient_death")),
    `post disch deaths` = length(which(mort_type == "outpatient_death")),
    `ltfu/withdrew` = length(which(ltfu == 1)),
    readmitted = length(which(readmission == 1)),
    `fatal readmissions` = length(which(dead == 1 & readmission == 1)),
    `readmitted or died` = length(which(read_died == 1))
  ) %>% janitor::adorn_totals("row")
}, rownames = F)


output$outc_sum2 <- renderDataTable({
outcome %>% 
  group_by(get(input$table_var)) %>% 
  summarise(
    `withdrew before disch` = length(which(wltfu_type == 1)),
    `withdrew after disch` = length(which(wltfu_type == 2)),
    `ltfu before disch` = length(which(wltfu_type == 3)),
    `ltfu after disch` = length(which(wltfu_type == 4))
  ) %>% janitor::adorn_totals("row")
}, rownames = F)


## ++ Length of stay
output$length_stay <- renderPlotly({
ggplotly(ggplot(filter(chaindata, length_stay <=60), aes(length_stay))+stat_ecdf(geom = "point", aes(col=categ_enrol))+
  scale_x_continuous(breaks = seq(0,70,10),expand = c(0,0)) + theme_minimal() + labs(y="proportion", 
                                                                                     x="length of hospital stay", title = "Empirical Cumulative Density Function")+
    scale_color_brewer(palette = "Dark2"))
})


## Where participant died
output$whr_died_bar <- renderPlotly({
  if (input$plot_typ == 1){
ggplotly(ggplot(outcome %>% 
  filter(!is.na(sc_wherepart_died)),aes_string(input$view_by, fill = "sc_wherepart_died"))+geom_bar(position = "fill")+
  labs(title = "Where did participant die?", y="proportion", fill = "") + theme_minimal() +
  scale_fill_brewer(palette = "Dark2"))
  } 
  else{
 ggplotly(ggplot(outcome %>% 
  filter(!is.na(sc_wherepart_died)) %>% 
  group_by(sc_wherepart_died, site, categ_enrol) %>% 
  summarise(
    total = n()
  ) ,aes(reorder(sc_wherepart_died, -total), total)) + geom_bar(stat = "identity", 
                                                                        fill = "steelblue",
                                                                        width = 0.7)+
  labs(x="Where Died", y="Total", title = "Where did participant die?") + theme_grey() + facet_wrap(~get(input$view_by)))
  }
})


## General checks tab 1
output$gen_checks <- renderDataTable({
  general_clinsum %>% 
    filter(sum_type == input$general_checks) %>% 
    select(-sum_type) %>% print()
}, rownames = F)

## Tab 2 athropometry checks
output$anthro_checks <- renderDataTable({
  anthropo_complete %>% 
    filter(time_point == input$athro_checks) %>% 
    select(-time_point) %>% print()
}, rownames = F)


## Clinical summaries (tab 3)
output$clin_sum <- renderDataTable({
  clinical_sum
}, rownames = F)


## CBC biochemistry (tab 4)
output$cb_biochem <- renderDataTable({
  if (input$cbc_biochem == 1){
data <- cbc_chemistry %>% 
      select(site, haemoglobin,rbc,wbc,platelets,
neutrophils,lymphocytes,osinophils,monocytes,basophils, time_point) %>% 
  filter(time_point == input$cbc_timepoint) %>% 
  select(-time_point)
  }else {
data <- cbc_chemistry %>% 
  select(site,sodium,potassium,calcium,albumin,
alt,alk_phosphate,urea,creatinine,bilirubin,
i_phosphate,magnesium,calcium_adj, time_point)%>% 
  filter(time_point == input$cbc_timepoint) %>% 
  select(-time_point)
  }
}, rownames = F)


## +++ CBC and Biochemitry -----

## Start of biochemistry

data_cbc_chem <- reactive({
  if(input$cbc_chem_select == "cbc"){
  df <- chem_cbc_for_viz[variable %in% input$cbc_options & type == "cbc"]
  df <- df[, value := as.numeric(value)]

  }else{
      df <- chem_cbc_for_viz[variable == input$chem_options & type == "chem"]
      df <- df[, value := as.numeric(value)]
  
  }
  
  if(input$cbc_chem_select == "cbc"){
    df2 <- chem_cbc_for_viz[variable %in% input$cbc_options & type == "cbc" &  event == input$time_point_cbc_chem]
    df2 <- df2[, value := as.numeric(value)]

  }else{
    df2 <- chem_cbc_for_viz[variable == input$chem_options & type == "chem" &  event == input$time_point_cbc_chem]
    df2 <- df2[, value := as.numeric(value)]

  }
  
  my_list <- list(df = df, df2 = df2)
  my_list

})

chem_cbc_options <- reactive({
  
 if(input$cbc_chem_select == "cbc"){ x = input$cbc_options} else x = input$chem_options
  x


})

  
output$chem_gen_hist <- renderPlotly({
  ggplotly(ggplot(data_cbc_chem()$df, aes_string(x ="event", y = "value", fill = "event"))+
             geom_boxplot()+
             scale_fill_economist()+
             theme_minimal()+
             labs(x= chem_cbc_options(), y="Value", fill=""))
})

output$split_hist_prev_chem <- renderPlotly({
  ggplotly(ggplot(data_cbc_chem()$df2, aes_string(x =input$chem_options_split, y = "value", fill = input$chem_options_split))+
             geom_boxplot()+
             scale_fill_economist()+
             theme_minimal()+
             labs(x= input$chem_options_split, y="Value", fill=""))
})


## end of biochemistry

### +++ Narshion

#clinical illness at adm

output$adm_illness_bar <- renderPlotly({
  setDF(clin_adm_illness_dat)
  ggplotly(single_bar_plot(my_df =clin_adm_illness_dat, var_interest =  input$adm_illness_options ))
})


output$split_bar_adm_illness <- renderPlotly({

  setDF(clin_adm_illness_dat)
  ggplotly(stacked_bar_plot(my_df =clin_adm_illness_dat,var_split =  input$adm_illness_options_split,
                   var_interest =  input$adm_illness_options ))
})

#clinical illness at discharge

output$disch_illness_bar <- renderPlotly({
  setDF(clin_disch_illness_dat)
  ggplotly(single_bar_plot(my_df =clin_disch_illness_dat, var_interest =input$disch_illness_options ))
})


output$disch_bar2 <- renderPlotly({
  setDF(clin_disch_illness_dat)
  ggplotly(stacked_bar_plot(my_df = clin_disch_illness_dat,var_split =  input$disch_illness_options_split,
                            var_interest =  input$disch_illness_options ))
})


#care giver

output$care_bar <- renderPlotly({
 
  setDF(care_giver_dat)
  ggplotly(
  ggplotly(single_bar_plot(my_df =care_giver_dat, var_interest =input$care_options))
  )
})


output$care_bar2 <- renderPlotly({
  setDF(care_giver_dat)
  ggplotly(stacked_bar_plot(my_df = care_giver_dat,var_split =  input$care_options_split,
                            var_interest =  input$care_options ))
})


## PQH 9 scores 
output$phq_scplot <- renderPlotly({
  
  if(input$phq_plot_type == 1){
ggplotly(ggplot(phq_data, aes_string("adm_score", "fup_score", col = input$phq_scatter)) + geom_point(size = 1) +
  scale_x_continuous(breaks = seq(0,27,3)) + scale_y_continuous(breaks = seq(0,27,3)) +
  labs(x = "admission score", y= "follow up score", title = "Admission vs Follow up PHQ score", col = "") +
  theme_minimal()) 
  }
  else if(input$phq_plot_type == 2){
    
    if (input$phq_density == 1){
  ggplotly(ggplot(phq_data_long %>% filter(adm_parttype == 1), aes(phq_score))+ 
  stat_density(geom = "line", adjust = 2, aes(col = time_point), size = 0.5) + theme_minimal() + 
  labs(title = "Distribution of PHQ score for all sites", x="PHQ score", col = ""))
    } else{
 ggplotly(ggplot(phq_data_long %>% filter(adm_parttype == 1, site == input$phq_density), aes(phq_score))+ 
  stat_density(geom = "line", adjust = 2, aes(col = time_point), size = 0.5) + theme_minimal() + 
  labs(title = paste("Distribution of PHQ score for", input$phq_density, "site", sep = " "), x="PHQ score", col = ""))
    }
  } 
  else{
ggplotly(ggplot(phq_data_long, aes_string(input$phq_scatter, "phq_score", fill = input$phq_scatter)) + geom_boxplot(outlier.alpha = 1/2)+
           theme_grey() + theme(legend.position = "none") + 
           labs(title = paste("PHQ 9 scores by", input$phq_scatter, sep = " "), y = "PHQ score") +
           scale_y_continuous(breaks = seq(0,27,3)) + facet_wrap(~time_point)
         )

  }
})



### Note to file and comments
output$notefile <- renderReactable({
 note_to_file %>% 
  reactable(groupBy = c("site","CRF_name"),
            bordered = T, highlight = T,
            defaultColDef = colDef(
    headerStyle = list(background = "steelblue")
  ),
columns = list(
    record_id = colDef(align = "center"),
    variable = colDef(align = "center")
  ),
searchable = TRUE, striped = TRUE
  
  )
  })



  }