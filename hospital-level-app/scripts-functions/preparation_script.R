# 

# read index table
index_table <- read_csv(here("HospitalLevelApp", "study_pop.csv")) %>% 
  rename("Simulated sentinel fragility fractures" = 1)


health_out <- read_csv(here("HospitalLevelApp", "summary.second.fx.t60.csv")) 



# sum of Hip fractures (from index table)
tot_hip <- index_table %>% 
  filter(`Simulated sentinel fragility fractures` == "Sentinel Hip fractures") %>% 
  mutate(
    total = Male + Female
  ) %>% select(total) %>% pull(total)

# these should be user input
hip_nums = 250

# most important metric, used to scale each cell of tables of interest
scaling_factor = hip_nums/(tot_hip)


# 1. Re-scale the index table
index_table %>% 
  mutate(across(c(Male, Female), ~ round(. * scaling_factor))) %>% 
  rowwise() %>% 
  mutate(
    Total = sum(c_across(c(Male, Female)))
  )



# 2. Refracture rates over 5 years
# by site and sex

health_out <- health_out %>% select(-c(difference, study.pop.n),
                                    "Re-fractures: hips" = n.hip.fx,
                                    "Re-fractures: spines" = n.spine.fx,
                                    "Re-fractures: other" = n.other.fx,
                                    "Total re-fractures" = n.fx) %>% 
  df_transform(names_col = "Re-fractures at 5 years") %>% 
  mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor))) %>% 
  mutate(
    `Difference (avoided)` = `Current practice` - FLS
  ) 


# 3.QALYs over 5 years
# by site and sex

qalys <- readxl::read_excel(here("HospitalLevelApp", "data_sheet.xlsx"), 
                            sheet = 2) %>% 
  select(-c(`N target population`, `QALYs gained`)) %>%
  pivot_wider(names_from = Intervention, values_from = QALYs) %>% 
  mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor))) %>% 
  mutate(
    `QALYs gained` = FLS - `Current practice`
  ) 




# 4. resource utiliazation (surgeris)
# by site and sex

hrcu <- readxl::read_excel(here("HospitalLevelApp", "data_sheet.xlsx"), sheet = 3) %>% 
  select(-c(`N target population`, `Surgeries avoided`)) %>% 
  pivot_wider(names_from = Intervention, values_from = Surgeries) %>%
  mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor))) %>% 
  mutate(
    `Surgeries avoided` = `Current practice` - FLS
  ) 
  
  

# Costs
costs <- readxl::read_excel(here("HospitalLevelApp", "data_sheet.xlsx"), 
                            sheet = 1) %>% 
  select(-c(`N target population`, `Total costs avoided`)) %>% 
  pivot_wider(names_from = Intervention, values_from = `Total costs`) %>%
  mutate(across(c(`Current practice`, FLS), ~ round(. * scaling_factor))) %>% 
  mutate(
    `Costs avoided` = `Current practice` - FLS
  ) 














