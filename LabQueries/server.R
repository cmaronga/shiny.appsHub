require(shiny)
require(dplyr)
library(lubridate)
require(readr)
require(shinythemes)
require(tidyr)
require(ggplot2)
library(scales)
library(RColorBrewer)
library(stringr)
library(data.table)




#source("reading_data.R")
server= function(input,output){
    pending_entries <- read_csv("pending_entries.csv")
    pending_kidms <- read_csv("pending_kidms.csv")
    datetime_query <- read.csv("datetime_query.csv")  
    setDT(datetime_query)
    datetime_query[, site1 := as.character(site1)]
    chain_non_aliquoted <- read_csv("chain_non_aliquoted.csv")
    parent_without_child <- read_csv( "parent_without_child.csv")
    isolates_results <- fread("isolates_results.csv") %>% setDT()
    chem_haem <- fread("chem_haem.csv")
    participant_replacements <- fread("particpants_replacements.csv")
    isolate_storage <- fread( "isolate_storage1.csv")
    
    redcap_backlog= reactive({
        
        pending_entries = filter(pending_entries,site ==  input$site)
        
        pending_entries = filter(pending_entries, date_collect <= input$date_range[2],date_collect >=input$date_range[1])
        
        
        
        pending_kidms = filter(pending_kidms,site ==  input$site)
        pending_kidms$date= dmy(pending_kidms$date)
        pending_kidms = filter(pending_kidms, date<= input$date_range[2],date>=input$date_range[1])
        
        #return list
        back_logs = list(red_cap=pending_entries ,kidms_base = pending_kidms)
        back_logs
    })
    #REDCAP BACKLOGS
    output$text <- renderText({"REDCAP BACKLOGS"})
    output$backlog = DT::renderDataTable( redcap_backlog()$red_cap)
    #KIDMS backlogs
    output$text1 <- renderText({"KIDMS BACKLOGS"})
    output$backlog1 = DT::renderDataTable( redcap_backlog()$kidms_base)
    #We write the control statements based on user selections
    output$pend <- downloadHandler(
        filename = function() {
            paste( input$site,"_redcap_backlog.csv", sep="")
        },
        content = function(file) {
            write.csv(redcap_backlog()$red_cap, file,row.names = FALSE,na= ".")
        }
    )
    
    output$pend1 <- downloadHandler(
        filename = function() {
            paste(input$site,"_kidms_backlog.csv", sep="")
        },
        content = function(file) {
            write.csv(redcap_backlog()$kidms_base, file,row.names = FALSE,na= ".")
        }
    )
    
    ###The server section below selects sample that have not been aliquoted yet
    sample_aliquating = reactive({   
        ## We shall select relevant variables from our imported dataset
        
        #filter by site and date range
        parent_without_child = filter(parent_without_child,site == input$site)
        parent_without_child = filter(parent_without_child, date_collect <= input$date_range[2],date_collect >=input$date_range[1])
        
        ### We obtain samples which should not have been aliquoted
        
        chain_non_aliquoted = filter(chain_non_aliquoted,site == input$site)
        chain_non_aliquoted = filter(chain_non_aliquoted, date_collect <= input$date_range[2],date_collect >=input$date_range[1])
        
        aliquoting_issues = list(alqt= parent_without_child,nonalqt= chain_non_aliquoted)
        aliquoting_issues
    })
    
    #Samples due for aliquoting
    output$text2 <- renderText({"Samples That Have Not Been Aliquoted"})
    output$mis_aliquot = DT::renderDataTable( sample_aliquating()$alqt)
    #We write the control statements based on user selections
    output$pend2 <- downloadHandler(
        filename = function() {
            paste(input$site,"_samples_to_aliquot.csv", sep="")
        },
        content = function(file) {
            write.csv(sample_aliquating()$alqt, file,row.names = FALSE,na= ".")
        }
    )
    
    output$text3 <- renderText({"Samples that Should not Have been Aliquoted"})
    output$wrong_aliquot = DT::renderDataTable( sample_aliquating()$nonalqt)
    
    output$pend3 <- downloadHandler(
        filename = function() {
            paste(input$site,"_samples_not_for_aliquot.csv", sep="")
        },
        content = function(file) {
            write.csv(sample_aliquating()$nonalqt, file,row.names = FALSE,na= ".")
        })
    # incase the appe does not work insert 
    
    ####We query the samples stored before reception
    sample_storage_date= reactive({
        
        
        #datetime_query = filter(datetime_query, date_collect <= input$date_range[2],date_collect >=input$date_range[1])
        datetime_query=filter(datetime_query,site1 == input$site)
        setDT(datetime_query)
        
        sample_storage_neg <-  datetime_query[query_type == "sample_storage_neg", .(Serial_study_id,type_name,time_point,
                                                                                    date_brought,time_brought,date_frozen,time_frozen,pk_specimen_id)]
        
        sample_storage_longer <- datetime_query[query_type ==  "sample_storage_longer",
                                                .(Serial_study_id,type_name,time_point,
                                                  date_brought,time_brought,
                                                  date_frozen,time_frozen,pk_specimen_id,specimen_type_id)]
        
        sample_reception_long <- datetime_query[query_type =="sample_reception_long", 
                                                .(Serial_study_id,type_name,time_point,
                                                  date_collect,time_collect,date_brought,
                                                  time_brought,pk_specimen_id)]
        
        sample_reception_neg <-  datetime_query[query_type == "sample_reception_neg",
                                                .(Serial_study_id,type_name,time_point,date_collect,
                                                  time_collect,date_brought,time_brought,pk_specimen_id)]
        
        
        sample_process_long <- datetime_query[query_type == "sample_process_long",
                                              .(Serial_study_id,type_name,time_point,date_brought,
                                                time_brought,date_processed,time_processed,
                                                pk_specimen_id,specimen_type_id)]
        
        sample_process_neg <- datetime_query[query_type == "sample_process_long",
                                             .(Serial_study_id,type_name,time_point,date_brought,
                                               time_brought,date_processed,time_processed,pk_specimen_id)]
        
        samples_not_stored <- datetime_query[query_type == "samples_not_stored",
                                             .(Serial_study_id,type_name,time_point,date_collect,time_collect,
                                               date_brought,time_brought,date_frozen,time_frozen,pk_specimen_id)]
        
        samples_wrongly_stored <- datetime_query[query_type == "samples_wrongly_stored",
                                                 .(Serial_study_id,type_name,time_point,date_collect,
                                                   time_collect,date_brought,time_brought,date_frozen,
                                                   time_frozen,pk_specimen_id)]
        
        
        datetimequery= list(sample_storage_neg=sample_storage_neg,sample_storage_longer=sample_storage_longer,
                            sample_reception_long=sample_reception_long,sample_reception_neg=sample_reception_neg,
                            sample_process_long=sample_process_long,sample_process_neg=sample_process_neg,
                            samples_not_stored=samples_not_stored,samples_wrongly_stored=samples_wrongly_stored)
        
        datetimequery
    })   
    #output sample_storage_neg
    #wrong date collect
    output$text_negstore <- renderText({"Date and Time Stored less than Date and Time Recieved \n
        (This samples Appears to Have been Stored Before Receiving in Lab)"})
    output$negstore = DT::renderDataTable( sample_storage_date()$sample_storage_neg)
    
    output$negstore_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_datetime_storage_neg.csv", sep="")
        },
        content = function(file) {
            write.csv(sample_storage_date()$sample_storage_neg, file,row.names = FALSE,na= ".")
        }
    )
    #output sample_storage_long
    output$text_longstore <- renderText({"Samples taking longer to store"})
    output$longstore = DT::renderDataTable( sample_storage_date()$sample_storage_longer)
    
    output$longstore_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_datetime_storage_longer.csv", sep="")
        },
        content = function(file) {
            write.csv(sample_storage_date()$sample_storage_longer, file,row.names = FALSE,na= ".")
        }
    )
    #output sample reception negative
    output$text_negrecp <- renderText({"Date and Time Received less than Date and Time Collect \n
        (This samples Appears to Have been received before collected)"})
    output$negrecp = DT::renderDataTable( sample_storage_date()$sample_reception_neg)
    
    output$negrecp_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_datetime_received_neg.csv", sep="")
        },
        content = function(file) {
            write.csv(sample_storage_date()$sample_reception_neg, file,row.names = FALSE,na= ".")
        }
    )
    #output sample reception longer
    output$text_longrecp <- renderText({"Samples taking more than 120mins to reception in Lab"})
    output$longrecp = DT::renderDataTable( sample_storage_date()$sample_reception_long)
    
    output$longrecp_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_datetime_received_longer.csv", sep="")
        },
        content = function(file) {
            write.csv(sample_storage_date()$sample_reception_long, file,row.names = FALSE,na= ".")
        }
    )
    #output sample processing negative
    output$text_negproc <- renderText({"Date and Time Process less than Date and Time Received \n
        (This samples Appears to Have been Processed before Reception)"})
    output$negproc = DT::renderDataTable( sample_storage_date()$sample_process_neg)
    
    output$negproc_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_datetime_process_neg.csv", sep="")
        },
        content = function(file) {
            write.csv(sample_storage_date()$sample_process_neg, file,row.names = FALSE,na= ".")
        }
    )
    
    #output sample processing longer
    output$text_longproc <- renderText({"Date and Time Process longer than 120mins"})
    output$longproc = DT::renderDataTable( sample_storage_date()$sample_process_long)
    
    output$longproc_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_datetime_process_long.csv", sep="")
        },
        content = function(file) {
            write.csv(sample_storage_date()$sample_process_long, file,row.names = FALSE,na= ".")
        }
    )
    
    #output sample not stored
    output$text_notstored <- renderText({"Samples That Have Not Been Stored\n (If they have been stored, Check Whether they have Date of Freezing)"})
    output$notstored = DT::renderDataTable( sample_storage_date()$samples_not_stored)
    
    output$notstored_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_samples_not_stored.csv", sep="")
        },
        content = function(file) {
            write.csv(sample_storage_date()$samples_not_stored, file,row.names = FALSE,na= ".")
        }
    )
    #output the samples wrongly stored
    output$text_wronglystored <- renderText({"Samples That Should not be Stored\n
        (We Should not Store Mother samples of aliquots Or rectal swab for culture for sites doing culture)"})
    output$wronglystored = DT::renderDataTable( sample_storage_date()$samples_wrongly_stored)
    #download file
    output$wronglystored_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_samples_wrongly_stored.csv", sep="")
        },
        content = function(file) {
            write.csv(sample_storage_date()$samples_wrongly_stored, file,row.names = FALSE,na= ".")
        }
    ) 
    
    #we code the isolate queries
    r2_isolate_section = reactive({
        
        isolates_results1 <- isolates_results[date_collect <= input$date_range[2]&date_collect >=input$date_range[1]]
        
        isolates_results <- isolates_results1[site ==input$site]
        
        r2_pending_culture <- isolates_results[query_type == "r2_pending_culture",
                                               .(Serial_study_id,type_name,
                                                 date_collect,time_collect,date_brought,
                                                 time_brought,time_point,pk_specimen_id,site)]
        
        r2_missing_results <- isolates_results[query_type == "r2_missing_results",
                                               .(Serial_study_id,date_collect,
                                                 time_collect,date_brought,time_brought,
                                                 time_point,pk_specimen_id,site)]
        
        r2_missing_esbl <- isolates_results[query_type == "r2_missing_esbl",
                                            .(record_id,pk_isolate,date_collect,
                                              species_full_name,time_point,
                                              pk_specimen_id,site)]
        
        r2_zone_size_not_tested <- isolates_results[query_type=="r2_zone_size_not_tested",
                                                    .(record_id,pk_isolate,date_collect,
                                                      species_full_name,time_point,
                                                      pk_specimen_id,site,drug_not_tested)]
        
        
        r2_s_test_missing <- isolates_results[query_type == "r2_s_test_missing",
                                              .(record_id,pk_isolate,date_collect,
                                                species_full_name,time_point,pk_specimen_id,
                                                site,drug_missing_sensitivity)]
        
        
        zone_size_char1 <- isolates_results[query_type == "zone_size_char1",
                                            .(record_id,pk_isolate,
                                              date_collect,species_full_name,
                                              time_point,pk_specimen_id,site,drug)]
        #culture output list
        r2_isolates= list(r2_pending_culture=r2_pending_culture,r2_missing_results=r2_missing_results,
                          r2_missing_esbl=r2_missing_esbl,r2_zone_size_not_tested=r2_zone_size_not_tested,
                          r2_s_test_missing=r2_s_test_missing,zone_size_char1=zone_size_char1)
        r2_isolates
    })
    ###Output the R2 and isolate queries
    ##R2 not cultured
    output$text_R2_not_cultured <- renderText({"R2 sample not yet cultured"})
    output$R2_not_cultured = DT::renderDataTable(r2_isolate_section()$r2_pending_culture)
    #download file
    output$R2_not_cultured_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_r2_not_cultured.csv", sep="")
        },
        content = function(file) {
            write.csv(r2_isolate_section()$r2_pending_culture, file,row.names = FALSE,na= ".")
        }
    ) 
    ##R2 missing neg results
    output$text_R2_miss_culture <- renderText({"R2 sample Missing Culture results"})
    output$R2_miss_culture = DT::renderDataTable( r2_isolate_section()$r2_missing_results)
    #download file
    output$R2_miss_culture_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_r2_miss_culture_result.csv", sep="")
        },
        content = function(file) {
            write.csv(r2_isolate_section()$r2_missing_results, file,row.names = FALSE,na= ".")
        }
    ) 
    ##R2 missing esbl results
    output$text_R2_miss_esbl <- renderText({"R2 Culture Missing ESBL results"})
    output$R2_miss_esbl = DT::renderDataTable( r2_isolate_section()$r2_missing_esbl)
    #download file
    output$R2_miss_esbl_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_r2_miss_esbl_result.csv", sep="")
        },
        content = function(file) {
            write.csv(r2_isolate_section()$r2_missing_esbl, file,row.names = FALSE,na= ".")
        }
    )
    ##R2 missing drug test results
    output$text_R2_drug <- renderText({"Missing Zone Size Results"})
    output$R2_drug = DT::renderDataTable( r2_isolate_section()$r2_zone_size_not_tested)
    #download file
    output$R2_drug_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_r2_miss_zone_size_result.csv", sep="")
        },
        content = function(file) {
            write.csv(r2_isolate_section()$r2_zone_size_not_tested, file,row.names = FALSE,na= ".")
        }
    )
    
    ##R2 missing drug sensitivity test results
    output$text_R2_sense <- renderText({"Missing Sensitivity Results"})
    output$R2_sense = DT::renderDataTable( r2_isolate_section()$r2_s_test_missing)
    #download file
    output$R2_sense_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_r2_miss_sensitivity_result.csv", sep="")
        },
        content = function(file) {
            write.csv(r2_isolate_section()$r2_s_test_missing, file,row.names = FALSE,na= ".")
        }
        
    )
    ##R2 zone size entered as characters
    output$text_R2_txt <- renderText({"Zone sizes Appearing as Text"})
    output$text_R2_tbl = DT::renderDataTable( r2_isolate_section()$zone_size_char1)
    #download file
    output$text_R2_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_r2_zone_size_as_character.csv", sep="")
        },
        content = function(file) {
            write.csv(r2_isolate_section()$zone_size_char1, file,row.names = FALSE,na= ".")
        }
        
    )
    ##We shall query all missing cbc,chem and gas results
    
    haem_query= reactive({
        
        chem_haem <- chem_haem[site ==input$site]
        
        miss_results <- split(chem_haem, chem_haem$query)
        miss_results <- lapply(miss_results, function(x) x[, query := NULL])
        
        miss_results <- lapply(miss_results, function(x) as.data.table(x[, colSums(is.na(x)) < nrow(x), with = F]))
        
        
        
        miss_results
        
    })
    ####output the missing results
    output$text_mis_cbc_results <- renderText({"Missing CBC Results"})
    output$mis_cbc_results= DT::renderDataTable({haem_query()$miss_cbc_results})
    
    #download file
    output$mis_cbc_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_cbc_missing_results.csv", sep="")
        },
        content = function(file) {
            write.csv(haem_query()$miss_cbc_results, file,row.names = FALSE,na= ".")
        }
    )
    
    output$text_mis_chem_results <- renderText({"Missing Clinical Chemistry Results"})
    output$mis_chem_results= DT::renderDataTable({haem_query()$miss_chem_results})
    #download file
    output$mis_chem_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_chem_missing_results.csv", sep="")
        },
        content = function(file) {
            write.csv(haem_query()$miss_chem_results, file,row.names = FALSE,na= ".")
        }
    )
    output$text_miss_gas_results <- renderText({"Missing Blood Gas Results"})
    output$miss_gas_results= DT::renderDataTable({haem_query()$miss_gas_results})
    #download file
    output$mis_gas_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_gas_missing_results.csv", sep="")
        },
        content = function(file) {
            write.csv(haem_query()$miss_gas_results, file,row.names = FALSE,na= ".")
        }
    )
    ### We output the samples not indicated as received in lab
    output$text_cbc_status <- renderText({"CBC Sample Status at Lab not Indicated, eg whether Acceptable"})
    output$cbc_status<- DT::renderDataTable({haem_query()$edta_not_entered})
    #download file
    output$cbc_status_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_cbc_status_not_entered.csv", sep="")
        },
        content = function(file) {
            write.csv(haem_query()$edta_not_entered, file,row.names = FALSE,na= ".")
        }
    )
    ###serum samples
    output$text_serum_status <- renderText({"Serum Sample Status at Lab not Indicated, eg whether Acceptable"})
    output$serum_status<- DT::renderDataTable({haem_query()$serum_not_entered})
    #download file
    output$serum_status_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_serum_status_not_entered.csv", sep="")
        },
        content = function(file) {
            write.csv(haem_query()$serum_not_entered, file,row.names = FALSE,na= ".")
        }
    )
    ###gas samples
    output$text_mis_gas_results<- renderText({"Missing Gas results"})
    output$text_gas_status <- renderText({"Gas Sample Status at Lab not Indicated, eg whether Acceptable"})
    output$gas_status<- DT::renderDataTable({haem_query()$gas_not_entered})
    #download file
    output$gas_status_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_gas_status_not_entered.csv", sep="")
        },
        content = function(file) {
            write.csv(haem_query()$gas_not_entered, file,row.names = FALSE,na= ".")
        }
    )
    
    output$mis_chem_results= DT::renderDataTable({haem_query()$miss_chem_results})
    ### We output the haematology results that are outliers
    output$text_outlier_cbc_results <- renderText({"Outlier CBC results (NB:The values Maybe Correct,we just need to confirm from Machine Print-out"})
    output$outlier_cbc_results= DT::renderDataTable({haem_query()$cbc_beyond_range_check})
    
    #download file
    output$outlier_cbc_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_cbc_outliers.csv", sep="")
        },
        content = function(file) {
            write.csv(haem_query()$cbc_beyond_range_check, file,row.names = FALSE,na= ".")
        }
    )
    ### We output the haematology results that are outliers
    output$text_outlier_chem_results <- renderText({"Outlier Chemistry results (NB:The values Maybe Correct,we just need to confirm from Machine Print-out"})
    output$outlier_chem_results= DT::renderDataTable({haem_query()$chem_beyond_range})
    
    #download file
    output$outlier_chem_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_chem_outliers.csv", sep="")
        },
        content = function(file) {
            write.csv(haem_query()$chem_beyond_range, file,row.names = FALSE,na= ".")
        }
    )
    ### We output the haematology results that are outliers
    output$text_outlier_gas_results <- renderText({"Outlier GAS results (NB:The values Maybe Correct,we just need to confirm from Machine Print-out"})
    output$outlier_gas_results= DT::renderDataTable({haem_query()$gas_beyond_range})
    
    #download file
    output$outlier_gas_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_gas_outliers.csv", sep="")
        },
        content = function(file) {
            write.csv(haem_query()$gas_beyond_range, file,row.names = FALSE,na= ".")
        }
    )
    ###We output the results entered with characters hence making them converted to NA in as.numeric
    output$text_char_cbc_results <- renderText({"CBC Results Containing Characters hence cannot be Converted to Numbers"})
    output$char_cbc_results= DT::renderDataTable({haem_query()$cbc_character_results})
    #download file
    output$char_cbc_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_cbc_non_numeric.csv", sep="")
        },
        content = function(file) {
            write.csv(haem_query()$cbc_character_results, file,row.names = FALSE,na= ".")
        }
    )
    output$text_char_chem_results <- renderText({"Chemistry Results Containing Characters hence cannot be Converted to Numbers"})
    output$char_chem_results= DT::renderDataTable({haem_query()$chem_query_char})
    #download file
    output$char_chem_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_chem_non_numeric.csv", sep="")
        },
        content = function(file) {
            write.csv(haem_query()$chem_query_char, file,row.names = FALSE,na= ".")
        }
    )
    ####samples that needs participants to be replaced
    #Begin with community participants
    participant_replacement= reactive({
        
        
        
        participant_replacements <- participant_replacements[site ==input$site]
        
        sample_miss_present <- split(participant_replacements, participant_replacements$query)
        sample_miss_present <- lapply(sample_miss_present, function(x) x[, query := NULL])
        sample_miss_present <- lapply(sample_miss_present, function(x) x[, colSums(is.na(x)) < nrow(x), with = F])
        
        
        
        sample_miss_present
        
    })
    
    output$text_replace <- renderText({"Community Participants Missing Samples.\n
        Kindly Note that this is meant to help
        sites know which participants missed a lot of samples and can be replaced once set target has been hit.
        It is not a Query to be resolved, but can guide sites check whether the samples were truely missed."})
    output$replace= DT::renderDataTable({participant_replacement()$cp_miss_mother_sample})
    #download file
    output$replace_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_CP_missing_Samples.csv", sep="")
        },
        content = function(file) {
            write.csv(participant_replacement()$cp_miss_mother_sample, file,row.names = FALSE,na= ".")
        }
    )
    output$text_replace_aliq2 <- renderText({"Community Participants Having Aliquot1 But Missing Aliquot2"})
    output$replace_aliq2= DT::renderDataTable({participant_replacement()$cp_miss_alq2})
    #download file
    output$replace_aliq2dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_CP_missing_aliquot2.csv", sep="")
        },
        content = function(file) {
            write.csv(participant_replacement()$cp_miss_alq2, file,row.names = FALSE,na= ".")
        }
    )
    
    output$text_replace_aliq3 <- renderText({"Community Participants Having Aliquot 1 & 2 But Missing Aliquot3"})
    output$replace_aliq3= DT::renderDataTable({participant_replacement()$cp_miss_alq3})
    #download file
    output$replace_aliq3_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_CP_missing_aliquot3.csv", sep="")
        },
        content = function(file) {
            write.csv(participant_replacement()$cp_miss_alq3, file,row.names = FALSE,na= ".")
        }
    )
    #write the missing mother samples
    output$text_replace_part <- renderText({"Participants Missing Samples.\n
        Kindly Note that this is meant to help
        sites know which participants missed a lot of samples and can be replaced once set target has been hit.
        It is not a Query to be resolved, but can guide sites check whether the samples were truely missed."})
    output$replace_part= DT::renderDataTable({participant_replacement()$miss_mother_samples})
    output$replace_part_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_miss_samples.csv", sep="")
        },
        content = function(file) {
            write.csv(participant_replacement()$miss_mother_samples, file,row.names = FALSE,na= ".")
        }
    )
    #isolate storage QC
    chain_isolates_not_stored= reactive({
        
        isolate_storage = isolate_storage[site==input$site]
        isolate_storage <- split(isolate_storage, isolate_storage$query)
        isolate_storage <- lapply(isolate_storage, function(x) x[, query := NULL])
        isolate_storage <- lapply(isolate_storage, function(x) x[, colSums(is.na(x)) < nrow(x), with = F])
        
        
        
    })
    output$text_isolatestorage <- renderText({"Blood Culture Isolates missing Date frozen"})
    output$text_isolatestorage1 <- renderText({"Blood Culture Isolates missing Storage Position in LIMS"})
    output$isolatestorage= DT::renderDataTable({chain_isolates_not_stored()$blood_isolate_notstored})
    output$isolatestorage1= DT::renderDataTable({chain_isolates_not_stored()$blood_isolate_notstored1})
    output$isolatestorage_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_Blood_isolate_miss_datefrozen.csv", sep="")
        },
        content = function(file) {
            write.csv(chain_isolates_not_stored()$blood_isolate_notstored, file,row.names = FALSE,na= ".")
        }
    )
    
    output$isolatestorage_dwn1 <- downloadHandler(
        filename = function() {
            paste(input$site,"_Blood_isolate_miss_storage.csv", sep="")
        },
        content = function(file) {
            write.csv(chain_isolates_not_stored()$blood_isolate_notstored1, file,row.names = FALSE,na= ".")
        }
    )
    output$text_isolatestorage_rc <- renderText({"Rectal Culture Isolates missing Date frozen"})
    output$text_isolatestorage_rc1 <- renderText({"Rectal Culture Isolates missing Storage Position in LIMS"})
    
    output$isolatestorage_rc= DT::renderDataTable({chain_isolates_not_stored()$rectal_swab_isolate_notstored})
    output$isolatestorage_rc1= DT::renderDataTable({chain_isolates_not_stored()$rectal_swab_isolate_notstored1})
    
    output$isolatestorage_rc_dwn <- downloadHandler(
        filename = function() {
            paste(input$site,"_Rectal_isolate_miss_datefrozen.csv", sep="")
        },
        content = function(file) {
            write.csv(chain_isolates_not_stored()$rectal_swab_isolate_notstored, file,row.names = FALSE,na= ".")
        }
    )
    
    output$isolatestorage_rc_dwn1 <- downloadHandler(
        filename = function() {
            paste(input$site,"_Rectal_isolate_miss_storage.csv", sep="")
        },
        content = function(file) {
            write.csv(chain_isolates_not_stored()$rectal_swab_isolate_notstored1, file,row.names = FALSE,na= ".")
        }
    ) 
    
}


