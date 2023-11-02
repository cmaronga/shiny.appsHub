# Copy report to temporary directory. This is mostly important when
# deploying the app, since often the working directory won't be writable
report_path <- tempfile(fileext = ".Rmd")
file.copy("PFC_model_inputs.Rmd", report_path, overwrite = TRUE)

render_report <- function(input, output, params) {
  rmarkdown::render(input,
                    output_file = output,
                    params = params,
                    envir = new.env(parent = globalenv())
  )
}


# start of server function
server <- function(input, output, session){
  thematic::thematic_shiny()
  # Get the data/table
  input_table <- reactive({
    CollectGsheetData(connection = str_replace(paste0(input$country), 
                                               pattern = " ", 
                                               replacement = ""),
                      table_name = paste0(input$scenario))
  })
  
  # subtitle for country
  output$subtitle <- renderText(
    paste0(input$country," inputs"," (", input$scenario, ")")
  )
  
  # display table based on input category
  display_table <- reactive({
    if (input$category == "All"){
      input_table() %>% 
        collect() %>% 
        select(-c(name, category))
    } else {
      input_table() %>% collect()  %>% 
        filter(category == input$category) %>% 
        select(-c(name, category))
    }
  })
  
  # remove some unwanted rows (This code needs to be scraped off)
  input_table.df <- reactive({
      display_table() %>% filter(!is.na(Description)) %>%
        mutate(across(everything(), ~str_squish(.))) %>%
        filter(!Description %in% c("fx.multiplier.low.risk",
                                   "fx.multiplier.intermediate.risk",
                                   "fx.multiplier.high.risk"))

  })
  
  # output the table
  output$inputs_table <- renderDataTable(
    input_table.df(),
    rownames = F,
    options = list(scrollX = TRUE,
                   pageLength = 20,
                   searching = FALSE),
    width = "100%"
  )
  
  # download the input_table
  output$download <-   downloadHandler(
    # function for file name (name of the dataset on file)
    filename = function(){
      paste0(input$country,"_", input$scenario,".csv")
    },
    
    # Handling how to export the dataset
    content = function(file){
      write.csv(input_table.df(), 
                row.names = F, 
                na = "", file)
    }
  )
  
  
  # comapring countries
  # A list of dataframes to compare
  
  big_df <- reactive({
    foreach(country = input$comp_countries)%do%{
      CollectGsheetData(
        connection = str_replace(
          paste0(country),
          pattern = " ",
          replacement = ""
        ),
        table_name = paste0(input$scenario2)
      ) %>% 
        select(name, Input, Description, Value, category) %>% 
        filter(!is.na(Description)) %>% 
        rename_at(vars(Value), ~ paste0(country)) %>% 
        collect() 
    }
  })
  
  # dataset for country comparison
  country_compare.df <- reactive({
    if (input$category2 == "All"){
      if (length(c(input$comp_countries)) %in% c(3, 5, 7, 9, 11)){
        big_df() %>%
          reduce(full_join,
                 by = "name") %>%
          rename(Name = 2, description = 3) %>% 
          select(-c(starts_with("Input."), 
                    starts_with("Description."),
                    "Description",
                    "Input")) %>% 
          select(-name, -starts_with("category"))
      } else {
        big_df() %>%
          reduce(full_join,
                 by = "name") %>%
          rename(Name = 2, description = 3) %>% 
          select(-c(starts_with("Input."), 
                    starts_with("Description."))) %>% 
          select(-name, -starts_with("category"))
      }
    } else {
      if (length(c(input$comp_countries)) %in% c(3, 5, 7, 9, 11)){
        big_df() %>%
          reduce(full_join,
                 by = "name") %>%
          rename(Name = 2, description = 3) %>% 
          select(-c(starts_with("Input."), 
                    starts_with("Description."),
                    "Description",
                    "Input")) %>% filter(category.x %in% input$category2) %>% 
          select(-name, -starts_with("category"))
      } else {
        big_df() %>%
          reduce(full_join,
                 by = "name") %>%
          rename(Name = 2, description = 3) %>% 
          select(-c(starts_with("Input."), 
                    starts_with("Description."))) %>% 
          filter(category.x %in% input$category2) %>% 
          select(-name, -starts_with("category"))
      }
    }
  })
  
  
  # Incoporate comparison by category
  output$c_countries <- renderDataTable(
    country_compare.df(),
    rownames = FALSE,
    options = list(scrollX = TRUE,
                   pageLength = 20,
                   searching = FALSE)
  )
  
  # download data comparing countries
  output$download_comparisons <- downloadHandler(
    filename = function(){
      paste0(input$scenario2, "_", input$category2, ".csv")
    },
    
    content = function(file){
      write.csv(country_compare.df(), file, row.names = F)
    }
  )
  
  
# Comparing scenarios -----------------------------------------------------
# create connection
  scenario_conn <- dbConnect(SQLite(),
                             paste0("inputs_databases/", "scenario_compare.db"))
  
scenario_dfs <- reactive({
  scenario_df.1 <- tbl(scenario_conn, input$comp2_countries) %>% 
     select(Description, category,all_of(input$scenario3)) %>% 
    collect()
  
  if(input$category3 == "All"){
    scenario_df.1 %>% 
      select(-category) %>% 
      setnames(old = c("FLS_Basecase",                      
                       "FLS_Identification_100",           
                       "FLS_Treatment_initiation_one_month",
                       "FLS_Monitoring_100",                
                       "FLS_Alendronate_only",             
                       "FLS_Injectables_only",             
                       "FLS_Injectables_only.max_reduction",
                       "FLS_Adherence_100",             
                       "FLS_Perfect_FLS",               
                       "FLS_Hips_and_spines_only",         
                       "FLS_Hips_only"),
               new = c("base",
                       "ident.100",
                       "trt.init.1mo",
                       "monitoring.100",          
                       "alendr.only",       
                       "inject.only",        
                       "max.reduct",
                       "adh.100",        
                       "perfect.fls",               
                       "hip.spine",          
                       "hips.only"), skip_absent = TRUE)
  } else {
    scenario_df.1 %>% filter(category == input$category3) %>% 
      select(-category) %>% 
      setnames(old = c("FLS_Basecase",                      
                       "FLS_Identification_100",           
                       "FLS_Treatment_initiation_one_month",
                       "FLS_Monitoring_100",                
                       "FLS_Alendronate_only",             
                       "FLS_Injectables_only",             
                       "FLS_Injectables_only.max_reduction",
                       "FLS_Adherence_100",             
                       "FLS_Perfect_FLS",               
                       "FLS_Hips_and_spines_only",         
                       "FLS_Hips_only"),
               new = c("base",
                       "ident.100",
                       "trt.init.1mo",
                       "monitoring.100",          
                       "alendr.only",       
                       "inject.only",        
                       "max.reduct",
                       "adh.100",        
                       "perfect.fls",               
                       "hip.spine",          
                       "hips.only"), skip_absent = TRUE)
  }
  
})
  
  
  output$c_scenarios <- renderDataTable({
    scenario_dfs()
  },
  rownames = FALSE,
  options = list(scrollX = TRUE,
                 pageLength = 20,
                 searching = FALSE))

  # Download the scenarios comparison dataset
  
  output$down_c_scenarios <- downloadHandler(
    
    filename = function(){
      paste0(input$comp2_countries, "_", input$category3, ".csv")
    },
    
    content = function(file){
      write.csv(scenario_dfs(), file, row.names = F)
    }
  )
  

# PDF report for the inputs -----------------------------------------------

output$generate_report <- downloadHandler(
  # fine name of the resultant report
  filename = function() {
    paste0(input$country,"_", input$scenario,".pdf")
  },
  
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    
    id <- showNotification(
      "Rendering the PDF report...",
      duration = NULL,
      closeButton = FALSE,
      type = "error"
    )
    
    on.exit(removeNotification(id), add = TRUE)
    
    # tempReport <- file.path(tempdir(), "PFC_model_inputs.Rmd")
    # file.copy("PFC_model_inputs.Rmd", tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    params <- list(country_name = input$country,
                   scenario_type = input$scenario,
                   dbase_path = active_working.dir)
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    # rmarkdown::render(tempReport, 
    #                   output_file = file,
    #                   params = params,
    #                   envir = new.env(parent = globalenv())
    # )
    
    callr::r(
      render_report,
      list(input = report_path, 
           output = file, 
           params = params)
    )
    
    
    
  }
)
  
}





