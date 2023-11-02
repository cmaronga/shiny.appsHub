# Pre-liminary set up -----------------------------------------------------
# Read in country values for country specific metadata
country_vals <- read_csv("datasets/country.name.csv")[[1,1]]


# read index table
index_table <- read_csv("datasets/study_pop.csv") %>% 
  rename("Simulated sentinel fragility fractures" = 1)


# Patient Outcomes --------------------------------------------------------
# Subsequent refractures
health_out <- read_csv("datasets/summary.second.fx.t60.csv") %>% 
  select(-c(difference, study.pop.n),
         "Hip" = n.hip.fx,
         "Spine" = n.spine.fx,
         "Other" = n.other.fx,
         "Total re-fractures avoided" = n.fx) %>% df_transform(names_col = "metric") 
  

# QALYs
qalys <- read_csv("datasets/summary.qol.t60.index_fx.csv") %>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = qol) %>%
  janitor::adorn_totals("row", name = "Total QALYs gained")


# Discounted Qalys 
disc_qalys <- read_csv("datasets/summary.discounted.qol.t60.index_fx.csv") %>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = `discounted.qol`) %>%
  janitor::adorn_totals("row", name = "Discounted QALYs gained")



# Healthcare resource use -------------------------------------------------
# procedures
hcr_Use <- read_csv("datasets/summary.procedures.t60.index_fx.csv") %>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = `study.pop.procedures`) 

# hospital length of stay
hsp_los <- read_csv("datasets/summary.hosp.los.t60.index_fx.csv")%>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = `hosp.los`) 
# rehab days
rehab_los <- read_csv("datasets/summary.temp.rehab.los.t60.index_fx.csv")%>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = temp.rehab.los)

# community consultation
com_vis <- read_csv("datasets/summary.comm.visits.t60.index_fx.csv")%>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = study.pop.comm.visits) 

# clinic visits
clin_vis <- read_csv("datasets/summary.clinic.visits.t60.index_fx.csv")%>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = study.pop.clinic.visits) 
# social care
# soc_care <- read_csv("datasets/summary.location.t60.index_fx.csv")%>% 
#   select(-c(`study.pop.n`, difference)) %>%
#   pivot_wider(names_from = intervention, values_from = temp.rehab.los) %>%
#   janitor::adorn_totals("row") 

# Discharge clinic visits
disch_clin <- read_csv("datasets/summary.discharge.clinic.visits.t60.index_fx.csv")%>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = study.pop.discharge.clinic.visits) 

# long term care
long_care <- read_csv("datasets/summary.ever.ltc.index_fx.csv")%>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = ever.ltc) 
# Lab tests
lab <- read_csv("datasets/summary.lab.test.t60.index_fx.csv")%>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = study.pop.lab.test) 
# DXA
dxa <- read_csv("datasets/summary.dxa.t60.index_fx.csv")%>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = study.pop.dxa)

# Doctor mins
doctor <- read_csv("datasets/summary.doctor.mins.t60.index_fx.csv")%>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = doctor.mins) 

# admin mins
admin <- read_csv("datasets/summary.administrator.mins.t60.index_fx.csv")%>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = administrator.mins) 

# nurse mins
nurse <- read_csv("datasets/summary.nurse.mins.t60.index_fx.csv")%>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = nurse.mins) 

# radiographer
radiogr <- read_csv("datasets/summary.radiographer.mins.t60.index_fx.csv")%>% 
select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = radiographer.mins) 

# allied_health
alliedH <- read_csv("datasets/summary.allied_health.mins.t60.index_fx.csv")%>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = allied_health.mins) 

# fls cordinator
fls_cord <- read_csv("datasets/summary.fls_coordinator.mins.t60.index_fx.csv")%>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = fls_coordinator.mins) 

# other-mins
# othermins <- read_csv("datasets/summary.other.mins.t60.index_fx.csv") %>% 
#   select(-c(`study.pop.n`, difference)) %>%
#   pivot_wider(names_from = intervention, values_from = temp.rehab.los) %>%
#   janitor::adorn_totals("row") 
 
 
# Costs over 5 years ------------------------------------------------------
# total cost
tot.cost <- read_csv("datasets/summary.total.cost.t60.index_fx.csv") %>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = total.cost) 

# total discounted cost
tot.disc_cost <- read_csv("datasets/summary.discounted.total.cost.t60.index_fx.csv") %>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = discounted.total.cost) 

# location cost
location_cost <- read_csv("datasets/summary.total.cost.excl.location.t60.index_fx.csv") %>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = total.cost.excl.location) 

# Hospital cost
hosp_cost <- read_csv("datasets/summary.hosp.cost.t60.index_fx.csv") %>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = hosp.cost) 

# procedures cost
procedure_cost <- read_csv("datasets/summary.procedure.cost.t60.index_fx.csv") %>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = procedure.cost) 

# Home support costs
home_supp <- read_csv("datasets/summary.location_home_support.cost.t60.index_fx.csv") %>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = location_home_support.cost) 

# Longterm care costs
longterm_cost <- read_csv("datasets/summary.location_long_term_care.cost.t60.index_fx.csv") %>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = location_long_term_care.cost) 

# Community costs
comm_cost <- read_csv("datasets/summary.comm.cost.t60.index_fx.csv") %>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = comm.cost) 

# Clinic costs
clinic_cost <- read_csv("datasets/summary.clinic.cost.t60.index_fx.csv") %>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = clinic.cost) 

# Temporary rehab costs
rehab_cost <- read_csv("datasets/summary.temp.rehab.cost.t60.index_fx.csv") %>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = temp.rehab.cost) 

# discharge clinic cost
disch_clincost <- read_csv("datasets/summary.discharge.clinic.cost.t60.index_fx.csv") %>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = discharge.clinic.cost) 

# medication cost
med_cost <- read_csv("datasets/summary.medication.cost.t60.index_fx.csv") %>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = medication.cost) 

# staff costs
staff_cost <- read_csv("datasets/summary.fx_prev.staff.cost.t60.index_fx.csv") %>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = fx_prev.staff.cost) 

# lab costs
lab_costs <- read_csv("datasets/summary.lab.test.cost.t60.index_fx.csv") %>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = lab.test.cost) 

# dxa costs
dxa_cost <- read_csv("datasets/summary.dxa.cost.t60.index_fx.csv") %>% 
  select(-c(`study.pop.n`, difference)) %>%
  pivot_wider(names_from = intervention, values_from = dxa.cost) 

# sum of Hip fractures (from index table)
tot_hip <- index_table %>% 
  filter(`Simulated sentinel fragility fractures` == "Sentinel Hip fractures") %>% 
  mutate(
    total = Male + Female
  ) %>% select(total) %>% pull(total)


server <- function(input, output, session){

  # capture hip fractures from user input
  hip_nums <- reactive({
    input$hip_nums
  })

  # most important metric, used to scale each cell of tables of interest
  scaling_factor <- reactive({
    hip_nums()/tot_hip
  })
  
  # Input message
  output$input_message <- renderText(
    "NOTE: The Number you input ABOVE automatically generates the tables by CLICKING BELOW button"
  )
  
  

# Index table
  output$indexTable <- renderDataTable({
    
    input$generate_table
    
    isolate(
    
    # 1. Re-scale the index table
    index_table %>%
      mutate(across(c(Male, Female), ~ round(. * scaling_factor()))) %>%
      rowwise() %>%
      mutate(
        Total = sum(c_across(c(Male, Female)))
      ) %>% janitor::adorn_totals("row") %>% DT::datatable(
        rownames = F,
        style = 'bootstrap4',
        options = list(dom = 't')
      ) %>%  
      formatStyle(
        columns = "Total",
        fontWeight = "bold"
      )
    )
  })
  

  # Patient outcomes
  PatientOutcomes <- reactive({
    input$generate_table
    
    isolate(
      
      bind_rows(
      
    # Refractures
    health_out %>% 
      mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
      mutate(
        Difference = `Current practice` - FLS
      ) %>% rename("PFC"="FLS") %>% slice(4) ,
    
    # QALYs
    qalys %>% 
      mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
      mutate(
        Difference = FLS - `Current practice`
      ) %>% rename("PFC"="FLS", metric = 1) %>% slice(4),
    
    # discounted QALYs
    disc_qalys %>% 
      mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
      mutate(
        Difference = FLS - `Current practice`
      ) %>% rename("PFC"="FLS", metric = 1) %>% slice(4)
    
      ) %>% rename(" " = 1)
    
    )
  })
  

  # Resource use
  ResourceUse <- reactive({
    input$generate_table
    
    isolate(
      
      bind_rows(
        # suggeris
        hcr_Use %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Surgeris avoided") %>% slice(4), 
        
        # hospital los
        hsp_los%>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Hospital LOS") %>% slice(4), 
        
        # temporary rehab
        rehab_los%>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Rehab LOS") %>% slice(4), 
        
        # community consultation
        com_vis%>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Community consultation") %>% slice(4), 
        
        # clinic visits
        clin_vis%>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Clinic visits") %>% slice(4),
        
        # discharge clinic visits
        disch_clin %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Dish clinic visits") %>% slice(4),
        
        # longterm care
        long_care %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Longterm care") %>% slice(4),
        
        # laboratory tests
        lab %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Lab tests") %>% slice(4),
        
        # dxa
        dxa %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "DXA minutes") %>% slice(4),
        
        # doctor minutes
        doctor %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Doctor minutes") %>% slice(4),
        
        # admin minutes
        admin %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          )%>%
          janitor::adorn_totals("row", name = "Admin minutes") %>% slice(4),
        
        # nurse minutes
        nurse %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Nurse minutes") %>% slice(4), 
        
        # radiographer mins
        radiogr %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Radiographer minutes") %>% slice(4), 
        
        # allied health
        alliedH %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Allied health minutes") %>% slice(4),
        
        # FLS cordinator
        fls_cord %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "FLS cordinator minutes") %>% slice(4)
        
      ) %>% rename("PFC"="FLS", " " = 1)
      
    )
  })
  
  
  # Costs
  AllCosts <- reactive({
    input$generate_table
    
    isolate(
      
      bind_rows(
        # location cost
        location_cost  %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Location cost") %>% slice(4),
        
        # hospital cost
        hosp_cost  %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Hospital cost") %>% slice(4), 
        
        # Procedure cost
        procedure_cost  %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Procedurse cost") %>% slice(4), 
        
        # Home support
        home_supp  %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Home support cost") %>% slice(4), 
        
        # longterm cost
        longterm_cost  %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Longterm care cost") %>% slice(4), 
        
        # community cost
        comm_cost  %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Comunity care cost") %>% slice(4), 
        
        # Clinic cost
        clinic_cost  %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Clinic costs") %>% slice(4), 
        
        # rehab cost
        rehab_cost  %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Temporary rehabilitation costs") %>% slice(4), 
        
        # Disch clinical cost
        disch_clincost  %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Discharge clinical costs") %>% slice(4), 
        
        # medication cost
        med_cost  %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Medication costs") %>% slice(4), 
        
        # Staff cost
        staff_cost  %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "PFC staff costs") %>% slice(4), 
        
        # Lab cost
        lab_costs  %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Lab costs") %>% slice(4), 
        
        # DXA cost
        dxa_cost  %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "DXA costs") %>% slice(4),
        
        # total costs
        tot.cost %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Total cost") %>% slice(4), 
        
        # total discounted cost
        tot.disc_cost  %>% 
          mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor()))) %>% 
          mutate(
            Difference = `Current practice` - FLS
          ) %>%
          janitor::adorn_totals("row", name = "Discounted total cost") %>% slice(4)
        
      ) %>% rename("PFC"="FLS", " " = 1)
      
    )
  })
  
  


# Reactive output ---------------------------------------------------------
  
  output$all_outputs <- renderDataTable(
    
    # make dynamic based on user selection
    if (input$output_type == "Patient outcomes") {
      PatientOutcomes() %>% DT::datatable(
        rownames = F,
        options = list(dom = 't') 
      ) %>% formatStyle(
        columns = "Difference",
        backgroundColor = "grey",
        fontWeight = "bold"
      )
    } else if (input$output_type == "Resource use") {
      # output resource use
      ResourceUse() %>%  
             DT::datatable(
               options = list(searching = FALSE)
             ) %>% formatStyle(
               columns = "Difference",
               backgroundColor = "grey",
               fontWeight = "bold"
             )
    } else {
      # output costs
      AllCosts() %>% 
            nice_numOut() %>% DT::datatable(
              options = list(searching = FALSE)
            ) %>% formatStyle(
              columns = "Difference",
              backgroundColor = "grey",
              fontWeight = "bold"
            )
      
  
    }
    
  )
  
  

  # subtitle
  
  output$subtitle <- renderText(
    paste(country_vals)
  )
  
  
  
  # Patient outcome message
  output$output_messages <- renderText(
    if (input$output_type == "Patient outcomes") {
      paste("1. By implementing PFCs in your hospital/region you can expect a reduction of", 
            PatientOutcomes()[1,4] ,"subsequent
    fractures in people aged 50 or older with a fragility fracture during the first 5 years alone", 
    "and an expected gain of",PatientOutcomes()[3,4], "quality-adjusted life years",
    sep = " ")
    } else {NULL}
    
  )
  
}