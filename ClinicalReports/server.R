## Loading required packages --------
library(tidyverse)
library(shiny)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(DT)
library(RColorBrewer)
library(janitor)
library(survminer)
library(survival)
library(lubridate)
library(viridis)
library(ggbeeswarm)
library(ggsci)
library(leaflet)
library(data.table)

##### SERVER SIDE FUNCTION STARTS HERE ---------------
server= function(input,output){
## Small preparation of the dataset -----------


  Daily.Records <- fread("daily.records.csv")
  chaindata <- fread("chaindata.csv") # importing processed data
  surv_data <- fread("surv_data.csv") # importing processed data
  readmissions <- fread("readmissiondata.csv")# readmission data
  
  ## Fit the survival curve for all mortalities 
  fit_all <- do.call(survfit,list(formula=Surv(ftime_all,status) ~ categ_enrol,data = filter(surv_data,ftime_all > 0 & ftime_all <=200)))
  
  ## Fit the survival curve for all mortalities 
  fit_pdm <- do.call(survfit,list(formula=Surv(ftime_pdm,status) ~ categ_enrol,data = filter(surv_data,ftime_pdm > 0 & ftime_pdm <=180)))
 
  chaindata$adm_sex <- factor(chaindata $adm_sex, levels=c(1,2),
                              labels=c("Male", "Female")) 
  
  chaindata$adm_parttype <- factor(chaindata$adm_parttype, levels=c(1,2),
                                   labels=c("Ward", "Community")) # labelling participant type
  
  chaindata$fu45_where_seen <- factor(chaindata$fu45_where_seen, levels=c(0,1,2),
                                      labels=c("Not seen/vital status", "Hospital/clinic","Seen in community")) 
  
  chaindata$fu90_where_seen <- factor(chaindata$fu90_where_seen, levels=c(0,1,2),
                                      labels=c("Not seen/vital status", "Hospital/clinic","Seen in community")) 
  
  chaindata$fu180_where_seen <- factor(chaindata$fu180_where_seen, levels=c(0,1,2),
                                       labels=c("Not seen/vital status", "Hospital/clinic","Seen in community")) 
  
  ## Follow up visits_missing visits -----
  d45 <- chaindata %>% 
    filter(!is.na(disch_date),(sc_lastvitalstat_date>d45_close|is.na(sc_lastvitalstat_date))) %>% ## remove deaths/withrawals before day 45
    select(record_id,site,d45_close,d45_scheduled,sc_lastvitalstat_date,sc_nofucomp_resn,fu45_visdate,fu45_contactdate) %>%
    filter(as.Date(d45_close)<Sys.Date()) %>% ## scheduled date has matured per system date
    filter(is.na(fu45_visdate),is.na(fu45_contactdate)) %>% 
    group_by(site) %>% 
    summarise(mis_d45=n())
  
  d90 <- chaindata %>%
    filter(!is.na(disch_date),(sc_lastvitalstat_date>d90_close|is.na(sc_lastvitalstat_date))) %>% ## remove deaths/withrawals before day 90
    select(record_id,site,d90_close,d90_scheduled,sc_lastvitalstat_date,sc_nofucomp_resn,fu90_visdate,fu90_contactdate) %>%
    filter(as.Date(d90_close)<Sys.Date()) %>% ## scheduled date has matured per system date
    filter(is.na(fu90_visdate),is.na(fu90_contactdate)) %>% 
    group_by(site) %>% 
    summarise(mis_d90=n())
  
  
  ## missing day 180
  d180 <- chaindata %>%
    filter(as.Date(d180_close)<Sys.Date()|sc_nofucomp_resn==1) %>% 
    filter(is.na(fu180_visdate),is.na(fu180_contactdate)) %>% 
    filter(sc_nofucomp_resn==1|is.na(sc_nofucomp_resn)) %>% 
    select(record_id,site,disch_date,d180_close,sc_lastvitalstat_date,sc_nofucomp_resn,fu180_visdate,fu180_contactdate) %>% 
    group_by(site) %>% 
    summarise(
      mis_d180=n()
    )
  
  missd_visits<-plyr::join_all(list(d45,d90,d180),by='site',type='full') ## missed visits all merged
  
  ## Patients still in hospital --------
  Still_Hospitalised <- chaindata %>% 
    filter(is.na(disch_date),adm_parttype=="Ward",is.na(sc_lastvitalstat_date),is.na(disch_absconded),
           is.na(disch_date_left_hosp)) %>% 
    select(record_id,site,adm_date,disch_date,disch_absconded) %>% 
    mutate(days_in_hospt=round(difftime(Sys.Date(),adm_date,units = c('days')),0)) %>% 
    group_by(site) %>% 
    summarise(total=n())
  
  
  chaindata$Oedema <- as.factor(chaindata$Oedema)
  
  chaindata$region <- as.factor(chaindata$region)
  
  ## Processing Mortalities dataset across all site -------
  ## pre-discharge mortalities ------
  pre_disch <- chaindata %>% 
    filter(!is.na(adm_date),is.na(disch_date),sc_nofucomp_resn==3) %>%  ## all enrollments without discharge date
    filter(is.na(disch_absconded)) %>%              ## and no absconded information filled
    select(record_id,site,adm_date,va_death_date,categ_enrol) %>% 
    mutate(Type=paste("Pre-Discharge"))
  
  ## post_discharge -------
  post_disch <- chaindata %>% 
    filter(!is.na(disch_date)|disch_absconded==1) %>%  ## all enrollment discharged properly and a live
    filter(sc_nofucomp_resn==3) %>%                   ## or they absconded but later died
    select(record_id,site,adm_date,va_death_date,categ_enrol) %>% 
    mutate(Type=paste("Post-Discharge"))
  
  ## Mortalities across sites combined -------
  mortalities <- rbind(pre_disch,post_disch)  ## combine dataset for future use
  
  
  ### Processing data for FOLLOW UP VISITS ------------
  ### chain cohort summary
  cohort <- chaindata %>% 
    group_by(site) %>% 
    summarise(
      Hsp.enrld=length(which(adm_parttype=="Ward")),
      CP.enrld=length(which(adm_parttype=="Community")),
      Withdrawal=length(which(sc_nofucomp_resn==4)),
      Readmssn=length(which(!is.na(readm_date))),
      Death=length(which(sc_nofucomp_resn==3)),
      D.45=length(which(!is.na(fu45_visdate)|!is.na(fu45_contactdate))),
      D.90=length(which(!is.na(fu90_visdate)|!is.na(fu90_contactdate))),
      D.180=length(which(!is.na(fu180_visdate)|!is.na(fu180_contactdate))
      ))
  
  ## Follow up expected visits -----
  expectedd45 <- chaindata %>% 
    filter(!is.na(disch_date),(sc_lastvitalstat_date>d45_close|is.na(sc_lastvitalstat_date))) %>% ## remove deaths/withrawals before day 45
    select(record_id,site,d45_close,d45_scheduled,sc_lastvitalstat_date,sc_nofucomp_resn) %>%
    filter(d45_scheduled<=Sys.Date()) %>% ## scheduled date has matured per system date
    group_by(site) %>% 
    summarise(d45_expected=n())
  
  expectedd90 <- chaindata %>% 
    filter(!is.na(disch_date),(sc_lastvitalstat_date>d90_close|is.na(sc_lastvitalstat_date))) %>% ## remove deaths/withrawals before day 90
    select(record_id,site,d90_close,d90_scheduled,sc_lastvitalstat_date,sc_nofucomp_resn) %>%
    filter(d90_scheduled<=Sys.Date()) %>% ## scheduled date has matured per system date
    group_by(site) %>% 
    summarise(d90_expected=n())
  
  expectedd180 <- chaindata %>% 
    filter(!is.na(disch_date),(sc_lastvitalstat_date<=d180_close|is.na(sc_lastvitalstat_date))) %>% ## non attended but not death/withdrawal
    select(record_id,site,d180_close,d180_scheduled,sc_lastvitalstat_date,sc_nofucomp_resn) %>%
    filter(d180_scheduled<=Sys.Date(),is.na(sc_nofucomp_resn)) %>% ## REMOVE deaths,lost to follow ups and withrawal from pending list
    group_by(site) %>% 
    summarise(d180_expected=n())
  
  
  ## merge datasets to compute percentages(%)----- 
  dframe1 <- merge(cohort,expectedd45,by="site")
  dframe2 <- merge(dframe1,expectedd90,by="site")
  dframe3 <- merge(dframe2,expectedd180,by="site",all = T)
  
  
  ## compute %
  dframe_percent <- dframe3 %>% 
    mutate(
      Day.45=paste(D.45, 
                   " (", ifelse(round(100*(D.45/d45_expected), 1)<100,round(100*(D.45/d45_expected), 1),100), "%", ") ", sep=""),
      Day.90=paste(D.90, 
                   " (", ifelse(round(100*(D.90/d90_expected), 1)<100,round(100*(D.90/d90_expected), 1),100), "%", ") ", sep=""),
      Day.180=paste(D.180, 
                    " (", ifelse(round(100*(D.180/d180_expected), 1)<100,round(100*(D.180/d180_expected), 1),100), "%", ") ", sep="")
    )
  dframe_percent$D.45 = NULL
  dframe_percent$d45_expected = NULL
  
  dframe_percent$D.90 = NULL
  dframe_percent$d90_expected = NULL
  
  dframe_percent$D.180 = NULL
  dframe_percent$d180_expected = NULL
  
## Summary summaries --------
summaryFigs = reactive({
chaindata %>%
    group_by(site) %>% 
    filter(adm_date>=input$date[1] & adm_date<=input$date[2]) %>% 
    summarise(Ward=length(which(adm_parttype=="Ward")),
              CP=length(which(adm_parttype=="Community")),
              in_death=length(which(mort_type=="inpatient_death")),
              disch=length(which((!is.na(disch_date) & 
                                         disch_date>=input$date[1] & disch_date<=input$date[2]))),
              out_death=length(which(mort_type=="outpatient_death")),
              LTFU=length(which(sc_nofucomp_resn==1)),
              Withdrew=length(which(sc_nofucomp_resn==4)))%>% 
    adorn_totals("row")
})
output$results <- renderDataTable({
  summaryFigs()
},rownames=F,colnames=c('pre_DM'='in_death','post_DM'='out_death'))

#We write the control statements based on user selections
output$summary_table <- downloadHandler(
  filename = function() {
    paste("CHAIN Recruitment summaries",".csv", sep="")
  },
  content = function(file) {
    write.csv(summaryFigs(), file,row.names = FALSE,na= ".")
  }
)
### Enrollment line graph---------

selection <- reactive({
  if (input$site == "Kilifi") {
    
   c("Kilifi")
    
  }
  
 else if (input$site == "All sites") {
    
    c("Kilifi","Nairobi","Migori","Kampala","Blantyre","Karachi","Dhaka","Matlab","Banfora")
    
 }
  
else if (input$site == "Nairobi") {
    
    c("Nairobi")
    
}
  else if (input$site == "Migori") {
    
    c("Migori")
    
  }  
else if (input$site == "Kampala") {
    
    c("Kampala")
    
} 
  
else if (input$site == "Blantyre") {
    
    c("Blantyre")
    
} 
  else if (input$site == "Karachi") {
    
    c("Karachi")
    
  } 

  else if (input$site == "Dhaka") {
    
    c("Dhaka")
    
  } 
  
  else if (input$site == "Matlab") {
    
    c("Matlab")
    
  } 
  else if (input$site == "Banfora") {
    
    c("Banfora")
    
  }  
})


plot <- reactive({
chaindata %>% 
    filter(site %in% selection(),!is.na(adm_date)) %>% 
    group_by(year_month) %>% 
    summarise(enroled=n()) %>% 
    mutate(numbers=cumsum(enroled)) 
})

output$linegraph <- renderPlot({
  ggplot(plot(),aes(year_month,numbers,group=1))+geom_point()+geom_line(col="purple")+
    theme_minimal()+labs(y="enrolled",x="year",title=paste(input$site,"enrollment curve"))+
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
})

output$cleveland_dot <- renderPlot({
chaindata %>%
    count(site) %>% 
    ggplot(aes(n,fct_reorder(site,n)))+geom_point(aes(size=n),col="blue")+theme_minimal()+
    theme(legend.position = "none",axis.text = element_text(face = "bold"),axis.line = element_line(color = "blue"))+
    labs(x="Total enrolled",y="",title="CHAIN Enrolments",subtitle="Sites' ward and Community participants")
})


## End of what Goes to tab pabel 1 under the name summary-------


## Nutritional grouping barplot ---------
output$enrol <- renderPlot({
chaindata %>% 
    filter(!is.na(categ_enrol),site %in% selection()) %>% 
    group_by(categ_enrol) %>% 
    summarise(
      total=n()
    ) %>% 
    ggplot(aes(categ_enrol,
               total,fill=categ_enrol))+
    geom_bar(stat = "identity",width = 0.6)+theme_minimal()+theme(legend.position = "none")+
    geom_text(aes(label=total,vjust=-0.5,size=3))+
    labs(x="",y="number enroled",title="Total Enrolments",subtitle="Numbers by Nutritional groups",
         caption="source:https://production.chainnetwork.org/")}) 


output$enrol2 <- renderPlot({
  slide.1 <-chaindata%>% 
    group_by(site) %>% 
    summarise(A=length(which(categ_enrol=="Acute A" & adm_parttype=="Ward")),
              B=length(which(categ_enrol=="Acute B" & adm_parttype=="Ward")),
              C=length(which(categ_enrol=="Acute C" & adm_parttype=="Ward")),
              Community=length(which(adm_parttype=="Community")))
  slide.1b <- gather(slide.1,key="Type",value = "Total",2:5)
  slide.1b <- as.data.frame(slide.1b)
  ggplot(slide.1b,aes(site,Total,fill=Type))+geom_col(position = "stack")+
    theme_classic()+labs(x="",y="",fill="")+ scale_fill_brewer(palette = "Set3")
})


##  tab pabel 1 under the name, nutrutional grouping

## Other summaries ----------

output$enrolNums <- renderDataTable({
  chaindata %>% 
    filter(!is.na(adm_date)) %>% 
    summarise(Kilifi=length(which(site=="Kilifi")),
              Nairobi=length(which(site=="Nairobi")),
              Migori=length(which(site=="Migori")),
              Kampala=length(which(site=="Kampala")),
              Blantyre=length(which(site=="Blantyre")),
              Karachi=length(which(site=="Karachi")),
              Dhaka=length(which(site=="Dhaka")),
              Matlab=length(which(site=="Matlab")),
              Banfora=length(which(site=="Banfora")))
},rownames = FALSE)

## Mortalities across all site, both pre and post ----
output$mort_sums <- renderDataTable({
  chaindata %>%
    filter(sc_nofucomp_resn == 3) %>% 
    group_by(site) %>% 
    summarise(
      Hospital = length(which(sc_wherepart_died ==1|sc_wherepart_died==2)),
      Community = length(which(sc_wherepart_died == 3)),
      Unknown = length(which(sc_wherepart_died == 99)),
      Missing = length(which(is.na(sc_wherepart_died)))
    ) %>% 
    adorn_totals("row")
},rownames=F,colnames=c('Any Hospital'="Hospital"))


## Summary of readmissions episodes by site --------
output$readmit_sums <- renderDataTable({
readmissions %>% 
    group_by(site) %>%  
    summarise(
      First.readm=length(which(!is.na(readm_date1))),
      Second.readm=length(which(!is.na(readm_date2))),
      Third.readm=length(which(!is.na(readm_date3))),
      Fourth.readm=length(which(!is.na(readm_date4))),
      Total.readmsions=(First.readm+Second.readm+Third.readm+Fourth.readm)
    ) %>% 
adorn_totals("row")
},rownames=F)

## Fatal vs non - fatal readmissions -----
output$readm_types <- renderDataTable({
chaindata %>% 
    filter(!is.na(readm_type)) %>% 
    group_by(readm_type) %>% 
    summarise(
      Kilifi=length(which(site=="Kilifi")),
      Nairobi = length(which(site == "Nairobi")),
      Migori= length(which(site == "Migori")),
      Kampala = length(which(site == "Kampala")),
      Blantyre = length(which(site == "Blantyre")),
      Karachi = length(which(site == "Karachi")),
      Dhaka = length(which(site == "Dhaka")),
      Matlab = length(which(site == "Matlab")),
      Banfora = length(which(site == "Banfora")))
},rownames = F,colnames = c(' '='readm_type'))

## Age distribution plots
output$age_p1 <- renderPlot({
ggplot(filter(chaindata,!is.na(adm_parttype)),aes(site,adm_agemons,fill=patient_type))+
    geom_boxplot(outlier.colour = "red")+theme_minimal()+
    scale_fill_brewer(palette = "Dark2")+
    labs(x="",y="Admission age (months)",fill="",title="Age Distribution of Total Enrolled Children")+
    scale_y_continuous(breaks = seq(0,24,2))+theme(
      legend.position = "top",axis.text.x = element_text(face = "bold",size = 11),
      axis.title.y = element_text(face = "bold")
    )
  
})

output$table1<- renderDataTable({
chaindata %>% 
    filter(adm_parttype=="Ward") %>%
    group_by(site) %>% 
    summarise(age_6mon=length(which(adm_agemons>=0 & adm_agemons<6)),
              age_12mon=length(which(adm_agemons>=6 & adm_agemons<12)),
              age_23mon=length(which(adm_agemons>=12 & adm_agemons<24)),
              Total=n())
},rownames=F,colnames = c('2=< Age <6' = 'age_6mon','6=< Age < 12'='age_12mon','12=< Age < 24'='age_23mon'),
caption = 'Age Categories of Enrolled Children – acute participants')

output$age_p2 <- renderPlot({
  ggplot(filter(chaindata,adm_parttype=="Ward",!is.na(categ_enrol)),aes(adm_agemons))+
    geom_histogram(aes(fill=categ_enrol),binwidth = 2,position = "stack",col="white")+
    scale_x_continuous(breaks = seq(0,24,2))+theme_classic()+scale_fill_brewer(palette = "Dark2")+
    labs(x="Age",y="",col="",fill="") +
    labs(title="Age Distribution of Enrolled Children – acute participants")
})

## Monthly enrollments across all sites -------

monthlyEnrol = reactive({
  chaindata %>% 
    filter(!is.na(adm_date)) %>% 
    group_by(year_month) %>%
    summarise(Kilifi=length(which(site=="Kilifi")),
              Nairobi=length(which(site=="Nairobi")),
              Migori=length(which(site=="Migori")),
              Kampala=length(which(site=="Kampala")),
              Blantyre=length(which(site=="Blantyre")),
              Karachi=length(which(site=="Karachi")),
              Dhaka=length(which(site=="Dhaka")),
              Matlab=length(which(site=="Matlab")),
              Banfora=length(which(site=="Banfora")))
})
output$monthly <- renderDataTable({
  monthlyEnrol()
},rownames=F,colnames = c('Month' = 'year_month'))

#We write the control statements based on user selections
output$monthly_enrolment <- downloadHandler(
  filename = function() {
    paste("CHAIN monthly enrolments",".csv", sep="")
  },
  content = function(file) {
    write.csv(monthlyEnrol(), file,row.names = FALSE,na= ".")
  }
)

## monthly enrolment by nutrional groupings ------
output$nutr_monthly <- renderDataTable({
chaindata %>% 
    filter(!is.na(categ_enrol),site==input$site) %>% 
    group_by(year_month,categ_enrol,site) %>%
    summarise(
      total = n()
    ) %>% 
    tidyr::spread(categ_enrol,total,fill=0) %>% 
    rename('Month' = 'year_month')
},rownames=F)


## monthly enrolment by age category ------
output$age_cat_monthly <- renderDataTable({
chaindata %>% 
    filter(!is.na(adm_agemons),adm_parttype == "Ward") %>% 
    group_by(year_month,age_cat) %>% 
    summarise(
      total =n()
    ) %>% 
    tidyr::spread(age_cat,total,fil=0) %>%
    adorn_totals("row") %>% 
    select('Month'='year_month',`Under 6 months`,`Under 12 months`,`Under 24 months`)
},rownames=F)


output$targets_abs <- renderDataTable({
chaindata %>% 
    filter(!is.na(categ_enrol),site==input$site) %>% 
    select(record_id,site,categ_enrol) %>% 
    group_by(site,categ_enrol) %>% 
    summarise(
      enrld=n()
    ) %>% 
    mutate(
      target=if_else(categ_enrol %in% c("Acute A","Acute B"),paste(200),
                     if_else(categ_enrol=="Acute C",paste(100),paste(125))),
      Left_toEnrol=as.numeric(target)-enrld,
      wks_left=round(difftime(ymd("2019-01-31"),ymd("2019-01-31"),units = c("weeks"))),
      TargetPerWeek=str_c("NULL")
    ) %>% 
    mutate(
      Left_toEnrol=replace(Left_toEnrol,which(Left_toEnrol<0),"Target reached"),
      TargetPerWeek=replace(TargetPerWeek,which(Left_toEnrol=="Target reached"),"Target reached"),
      TargetPerWeek=replace(TargetPerWeek,which(TargetPerWeek==0),"a few left")
    )
},rownames=F,colnames=c('Enrolled'='enrld','Target'='target','Enrloments left'='Left_toEnrol',
                        'aprox. wks left'='wks_left','weekly target'='TargetPerWeek','abc group'='categ_enrol'))


#### Graphs/Charts --------
output$muac_dot <- renderPlot({
chaindata %>% 
    filter(adm_parttype== "Ward") %>% 
    select(record_id,site,categ_enrol,muac_enrol,muac_disch,muac_d45,muac_d90,muac_d180) %>% 
    rename('Admission'='muac_enrol','Discharge'='muac_disch','Day 45'='muac_d45','Day 90'='muac_d90','Day 180'='muac_d180') %>% 
    tidyr::gather("timepoint","muac",-c(record_id,site,categ_enrol)) %>% 
    filter(muac < 20,site %in% selection()) %>% 
    ggplot(aes(timepoint,muac))+
    geom_quasirandom(method = "smiley",size=1.5,col="darkblue")+
    stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                 geom = "crossbar", width = 0.5, color="Red")+theme_minimal()+
    labs(caption="source:chain Network data",y="MUAC (cm)",title=paste(input$site,"MUAC distribution across timepoints"))+
    scale_x_discrete(name ="Time point", 
                     limits=c("Admission","Discharge","Day 45","Day 90","Day 180"))+
    scale_y_continuous(breaks = seq(4,20,2))
})


output$avg_prof_plot <- renderPlot({
chaindata %>% 
    filter(!is.na(categ_enrol),adm_parttype== "Ward") %>% 
    select(record_id,site,categ_enrol,muac_enrol,muac_disch,muac_d45,muac_d90,muac_d180) %>% 
    rename('Admission'='muac_enrol','Discharge'='muac_disch','Day 45'='muac_d45','Day 90'='muac_d90','Day 180'='muac_d180') %>% 
    tidyr::gather("timepoint","muac",-c(record_id,site,categ_enrol)) %>% 
    group_by(timepoint,categ_enrol) %>% 
    summarise(
      avg_muac=mean(muac,na.rm = T)
    ) %>% 
    ggplot(aes(timepoint,avg_muac,group=categ_enrol,col=categ_enrol))+geom_point(aes(size=avg_muac),show.legend = F)+
    geom_line()+theme_minimal()+
    scale_x_discrete(name ="Time point", 
                     limits=c("Admission","Discharge","Day 45","Day 90","Day 180"))+
    theme(legend.position = c(0.05,0.90))+labs(col="",title="Average profile plots",subtitle="MUAC by Nutritional status over time",
                                               y="Avergae MUAC",caption="dot size increases with increase in MUAC average")
})


## Visit summaries table ------------

## discharge summaries
output$disch_sumary <- renderTable({
chaindata %>% 
    group_by(site) %>% 
    summarise(
      Normal_discharge=length(which(!is.na(disch_date))),
      Absconded=length(which(disch_absconded==1)),
      Against_advice=length(which(disch_againstadvice==1))
    )
})

output$followup <- renderDataTable({
  chaindata %>% 
    filter(!is.na(disch_date)) %>% 
    group_by(site) %>% 
    summarise(
      D45.attended=length(which(!is.na(fu45_visdate))),
      D45.vitalsOnly=length(which(!is.na(fu45_contactdate))),
      D90.attended=length(which(!is.na(fu90_visdate))),
      D90.vitalsOnly=length(which(!is.na(fu90_contactdate))),
      D180.attended=length(which(!is.na(fu180_visdate))),
      D180.vitalsOnly=length(which(!is.na(fu180_contactdate)))) %>% 
    adorn_totals("row")
},rownames=F)

output$where_seen <- renderPlot({
ggplot(filter(chaindata,!is.na(fu45_where_seen)),aes(site,fill=fu45_where_seen))+geom_bar(position = "dodge")+
    theme(legend.position = c(0.85, 0.85))+labs(fill="Seen at:",y="No. seen/contacted",title="Day 45 follow up visits")  
})


output$where_seen2 <- renderPlot({
  ggplot(filter(chaindata,!is.na(fu90_where_seen)),aes(site,fill=fu90_where_seen))+geom_bar(position = "dodge")+
    theme(legend.position = c(0.85, 0.85))+labs(fill="Seen at:",y="No. seen/contacted",title="Day 90 follow up visits")  
})

output$where_seen3 <- renderPlot({
  ggplot(filter(chaindata,!is.na(fu180_where_seen)),aes(site,fill=fu180_where_seen))+geom_bar(position = "dodge")+
    theme(legend.position = c(0.85, 0.85))+labs(fill="Seen at:",y="No. seen/contacted",title="Day 180 follow up visits")  
})


## Nutritional grouping at day 180 --------
output$day180 <- renderPlot({
  chaindata %>% 
    group_by(site) %>% 
    summarise(Acute_A=length(which(categ_d180=="Acute A")),
              Acute_B=length(which(categ_d180=="Acute B")),
              Acute_C=length(which(categ_d180=="Acute C"))) %>% 
    gather(key = "type",value = "numbers",2:4) %>% 
    filter(site==input$site) %>% 
    ggplot(aes(type,numbers,fill=type))+geom_col()+theme_minimal()+
    labs(title="",x="",y="",fill="")+ scale_fill_brewer(palette = "Dark2")
})

## table of nutritional status at study exit -------
output$day180_nut_grp <- renderDataTable({
  chaindata %>%
    filter(!is.na(fu180_visdate),(fu180_where_seen=="Hospital/clinic"|fu180_where_seen=="Seen in community"),!is.na(categ_d180)) %>% 
    group_by(categ_d180) %>% 
    summarise(
      Kilifi=length(which(site=="Kilifi")),
      Nairobi=length(which(site=="Nairobi")),
      Migori=length(which(site=="Migori")),
      Kampala=length(which(site=="Kampala")),
      Blantyre=length(which(site=="Blantyre")),
      Karachi=length(which(site=="Karachi")),
      Dhaka=length(which(site=="Dhaka")),
      Matlab=length(which(site=="Matlab")),
      Banfora=length(which(site=="Banfora")),
      Total=n()
    )
},rownames=F,colnames=c('Category_d180'='categ_d180'))




## Reasons for un-successful study conclusion-------
output$study_end_status <- renderPlot({
chaindata %>% 
    filter(!is.na(EndStatus)) %>% 
    count(EndStatus) %>% 
    ggplot(aes(reorder(EndStatus,n),n))+geom_bar(stat = "identity",fill="steelblue",col="black",width = 0.5)+
    labs(title="Current overal study-end status",x="",y="")+theme_minimal()+
    theme(axis.text.y = element_blank(),axis.text.x = element_text(face = "bold",size = 12),
          panel.grid = element_blank())+
    geom_text(aes(label=n,vjust=-0.5),size=4)
})


output$lost_patients <- renderTable({
chaindata %>% 
    group_by(site) %>% 
    summarise(
      Lost=length(which(sc_nofucomp_resn==1)),
      Withrawal=length(which(sc_nofucomp_resn==4)) 
    )%>% 
adorn_totals(c("row"))
})

### Hospital stay in days summary-------
output$hosp_stay <- renderDataTable({
chaindata %>% 
    filter(!is.na(length_stay),site %in% selection()) %>% 
    group_by(site,length_stay) %>% 
    summarise(
      number_of_patients = n()
    ) %>% 
    arrange(length_stay)
},colnames=c('Hospital stay (days)'='length_stay','Number of patients'='number_of_patients'))

### Patients in hospital -------
output$still_inhosp <- renderPlot({
  ggplot(Still_Hospitalised,aes(reorder(site,total),total))+geom_col(fill="steelblue")+
    labs(title="Patients still in hospital",x="",y="No. of patients")+theme_minimal()
})

## Survival curve; all mortalities -----
surv_plot <- reactive(ggsurvplot(fit_all,
                        fun = "cumhaz",
                        censor=F,
                        palette = "npg",
                        xlim=c(0,200),ylim=c(0,0.35),
                        break.time.by=14,break.y.by=0.05,
                        xlab="Time since admission (days)",
                        ylab = "all deaths cum.hazard",
                        legend=c(0.1, 0.90),legend.labs=c("Acute A","Acute B","Acute C"),
                        legend.title="Nutritional groupings",
                        ggtheme = theme_minimal()))


output$surv_plot_all <- renderPlot({
  surv_plot()$plot+theme(panel.grid.minor.y = element_blank(),
                       panel.grid.minor.x = element_blank(),
                       axis.line = element_line())
})

## Survival curve; post discharge mortalities -----

surv_plot2 <- ggsurvplot(fit_pdm,
                         fun = "cumhaz",
                         censor=F,
                         palette = "npg",
                         xlim=c(0,180),ylim=c(0,0.35),
                         break.time.by=20,break.y.by=0.05,
                         xlab="Time since discharge (days)",
                         ylab = "Post-discharge deaths cum.hazard",
                         legend=c(0.1, 0.90),legend.labs=c("Acute A","Acute B","Acute C"),
                         legend.title="Nutritional groupings",
                         ggtheme = theme_minimal())

output$surv_plot_pdm <- renderPlot({surv_plot2$plot+theme(panel.grid.minor.y = element_blank(),
                      panel.grid.minor.x = element_blank(),
                      axis.line = element_line())})


### CRF completenes for enrollment ----------
output$phq_miss <- renderDataTable({
chaindata %>%
    group_by(site) %>% 
    summarise(Enrolled=n(),
              CRFs_Done=length(which(!is.na(phq_date))),
              CRFs_missing=length(which(is.na(phq_date)))) %>% 
    mutate(Completeness=paste(round(100*(CRFs_Done/Enrolled),1),"%")) 
},rownames=F)

output$soc_miss <- renderDataTable({
  chaindata %>%
    group_by(site) %>% 
    summarise(Enrolled=n(),
              CRFs_Done=length(which(!is.na(soc_interview_date))),
              CRFs_missing=length(which(is.na(soc_interview_date)))) %>% 
    mutate(Completeness=paste(round(100*(CRFs_Done/Enrolled),1),"%"))
},rownames=F)


output$nutri_miss <- renderDataTable({
  chaindata %>%
    group_by(site) %>% 
    summarise(Enrolled=n(),
              CRFs_Done=length(which(!is.na(nutr_missedmeal_last7days))),
              CRFs_missing=length(which(is.na(nutr_missedmeal_last7days)))) %>% 
    mutate(Completeness=paste(round(100*(CRFs_Done/Enrolled),1),"%"))
},rownames=F)

output$hwa_miss <- renderDataTable({
  chaindata %>%
    group_by(site) %>% 
    summarise(Enrolled=n(),
              CRFs_Done=length(which(!is.na(hwa_drink_water_src))),
              CRFs_missing=length(which(is.na(hwa_drink_water_src)))) %>% 
    mutate(Completeness=paste(round(100*(CRFs_Done/Enrolled),1),"%"))
},rownames=F)



## Reports for expected follow up visits ---------
output$day45 <- renderDataTable({
  chaindata %>% 
    filter(site==input$site) %>% 
    filter(is.na(sc_lastvitalstat_date),!is.na(disch_date)) %>% 
    filter(d45_scheduled >= Sys.Date() ,d45_scheduled <= Sys.Date() + days(as.numeric(input$sched))) %>% 
    select(record_id,site,adm_date,disch_date,d45_open,d45_scheduled,d45_close)
},rownames=F)

output$day90 <- renderDataTable({
  chaindata %>% 
    filter(site==input$site) %>% 
    filter(is.na(sc_lastvitalstat_date),!is.na(disch_date)) %>% 
    filter(d90_scheduled >= Sys.Date() ,d90_scheduled <= Sys.Date() + days(as.numeric(input$sched))) %>% 
    select(record_id,site,adm_date,disch_date,d90_open,d90_scheduled,d90_close)
},rownames=F)


output$day_180 <- renderDataTable({
  chaindata %>% 
    filter(site==input$site) %>% 
    filter(is.na(sc_lastvitalstat_date),!is.na(disch_date),is.na(fu180_visdate),is.na(fu180_contactdate)) %>% 
    filter(d180_scheduled >= Sys.Date() ,d180_scheduled <= Sys.Date() + days(as.numeric(input$sched))) %>% 
    select(record_id,site,adm_date,disch_date,d180_open,d180_scheduled,d180_close)
},rownames=F)

output$window_dates <- renderDataTable({
  window_open <- chaindata %>% 
    filter(record_id==as.numeric(input$record_id)) %>% 
    select(adm_date,disch_date,d45_open,d45_scheduled,d45_close,
           d90_open,d90_scheduled,d90_close,d180_open,d180_scheduled,d180_close) %>% 
    rename('Date enroled'='adm_date','Date discharged'='disch_date','Day 45 window open'='d45_open',
           'Day 45 scheduled date'='d45_scheduled','Day 45 window close date'='d45_close','Day 90 window open'='d90_open',
           'Day 90 scheduled date'='d90_scheduled','Day 90 window close date'='d90_close','Day 180 window open'='d180_open',
           'Day 180 scheduled date'='d180_scheduled','Day 180 window close date'='d180_close')
  dd <- as.data.frame(t(window_open))
  dd <- rename(dd,'Window Dates'='V1')
  dd
},options = list(pageLength = 11))

##+++++++++ Missed visits ------
output$missed_all <- renderDataTable({
  missd_visits
},rownames=F,colnames=c('Missed day 45'='mis_d45','Missed day 90'='mis_d90','Missed day 180'='mis_d180'))


#### Annual reports template -------------
# output$slide2_nutri <- renderPlot({
# chaindata%>% 
#     filter(adm_date>=input$rpt_date[1] & adm_date<=input$rpt_date[2]) %>%
#     group_by(site) %>% 
#     summarise(Acute.A=length(which(categ_enrol=="Acute A")),
#               Acute.B=length(which(categ_enrol=="Acute B")),
#               Acute.C=length(which(categ_enrol=="Acute C")),
#               Community=length(which(categ_enrol=="Community"))) %>%
#     tidyr::gather(key="Type",value = "Total",2:5) %>% 
#     ggplot(aes(site,Total,fill=Type))+geom_col(position = "stack")+
#     theme_classic()+labs(x="",y="",fill="",title="Nutritional Status Groups Enrolled")+ 
#     theme(legend.position = "bottom",legend.text = element_text(colour="blue"))+
#     scale_fill_discrete(name = "",labels = c("Acute A: MUAC <11.5cm or <11cm under 6 mo, or kwashiorkor",
#                                              "Acute B: MUAC <12.5cm & ≥ 11.5cm or <12cm & ≥ 11 cm under 6 mo",
#                                              "Acute C: MUAC ≥12.5cm or ≥12cm under 6 mo","Community participants"))
# })
# 
# output$chrt_summary <- renderDataTable({
#   dframe_percent
# },rownames=F)
# 
# output$weekly_cum <- renderPlot({
# chaindata %>%
#     filter(!is.na(adm_date)) %>% 
#     group_by(enrol_week) %>% 
#     summarise(count=n()) %>%
#     mutate(week_enrolcum=cumsum(count)) %>% 
#     ggplot(aes(as.factor(enrol_week),week_enrolcum))+geom_col(fill="steelblue")+
#     theme_minimal()+labs(title="Cumulative weekly enrolment",x="week",y="No. enroled")
# })



} ## End of server function tag



