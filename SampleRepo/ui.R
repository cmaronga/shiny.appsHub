
## value box for shipped Samples all CHAIN
## Shipped Samples versuses available samples 
## Select a tray and box and download samples in there
##  deaths , samples,
## First page all samples by site
## download participants with this samples
## main cohort characteristics
##Sample Charasteritics
##actual shipped samples
# -------------------------------------------------------------------------
thematic::thematic_shiny()

# dashboard header items
header_contents <- dashboardHeader(
  title = "CHAIN SAMPLES",
  dropdownMenu(type = "messages"),
  titleWidth = 400)


# -------------------------------------------------------------------------


# dashboard side bar items
sidebar_contents <- dashboardSidebar(
  sidebarMenu(
    # menu
    menuItem("Home",
             tabName = "home"), ##Study and each type of samples, shipped samples

    #Main  cohort and 
    menuItem("MAIN Cohort",
            
             tabName = "main_cohort",
             
      
             menuSubItem("Main Cohort Samples",
                         tabName = "all_samples_mainc"),
             
             menuSubItem("NCC",
                         tabName = "ncc_samples")
             ),
    # menu
    menuItem("Shipment",
             tabName = "shipped_samples"), ##Study and each type of samples, shipped samples
    

    # Sub Studies 
    menuItem("Sub Studies",
             tabName = "sub_study",
             # laboratory reports
             menuSubItem("Young Infants",
                         tabName = "young_infants"),

             # 
             menuSubItem("BMC",
                         tabName = "bmc"),
             
             
             menuSubItem("Nutrition",
                         tabName = "nutrition_substudy")

             ),
    
    # Sub Studies 
    menuItem("Biobank Status",
             tabName = "biobank_status",
             # laboratory reports
             menuSubItem("Freezers & Boxes Staus",
                         tabName = "chain_freezers"),

             # laboratory queries (including KIDMS backlog)
 
               selectInput(inputId =  "freezer_select", 
                           label = "Select Freezer Name",
                           choices = freezer_nms,
                           selected = 55)
               
               
             
             
    )
  )
)


# -------------------------------------------------------------------------

# dashboard body items
body_contentds <- dashboardBody(
  
  tabItems(
    tabItem(tabName = "home",
            fluidRow(theme = mytheme,
              tabsetPanel(
                tabPanel("CHAIN BIOBANK",
                         box(width = 10, title = "CHAIN Biobank", solidHeader = TRUE,
                             collapsible = T, status = "primary",
                             dataTableOutput(outputId = "all_chain_samples_dt")),
                         box(width = 3,
                             downloadButton("all_samples_down", label = "CHAIN Samples"))),
                tabPanel("CHAIN SAMPLES By Timepoint",
                         box(width = 10, title = "CHAIN Samples",
                             dataTableOutput(outputId = "all_chain_bytime")),
                         box(width = 2, 
                             selectInput("select_site", label = "Select site",
                                         choices = all_sites, selected = "Kilifi"),
                             selectInput("select_study", label = "Select study",
                                         choices = all_studies, selected = "main_cohort"),
                             selectInput("select_time_point_all", label = "Select time point",
                                         choices = all_timepoints, selected = "A0"),
                             selectInput("select_sample_type", label = "Select sample type ",
                                         choices = all_typename, selected = type_name_s,
                                         multiple = T)),
                         box(width = 10, title = "CHAIN Samples",
                             dataTableOutput(outputId = "all_chain_bytime_all")),)
              )
            )),
    
    
    tabItem(tabName ="all_samples_mainc",
            fluidRow(
              tabsetPanel(
                
                tabPanel("Main Cohort Samples",
                         box(width = 10, title = "Main Cohort Samples",
                              status = "primary",solidHeader = TRUE,
                              dataTableOutput(outputId = "mc_samples")),
                    
                         
                  
                  box(width = 3,
                      downloadButton(outputId = "mc_down", label = "Main Cohort"))
                  
                  ),
                tabPanel("Main Cohort  Time point",
                         box(width = 10, title = "Main Cohort Samples",
                             status = "primary",solidHeader = TRUE,
                             dataTableOutput(outputId = "mc_samples_time")),
                         box(width = 2, 
                             selectInput("select_time_point", label = "Select Time Point",
                                         choices = time_points_vec, selected = "A0"),
                             selectInput("select_alive", label = "Select based deaths",
                                         choices = deaths_select, selected = "Died")),
                         box(width = 10, title = "Main Cohort Samples",
                             status = "primary",solidHeader = TRUE,
                             dataTableOutput(outputId = "mc_samples_time_alive"))
                         
                         ),
                tabPanel("Participants with samples",
                         
                         box(width = 10,  status = "primary",solidHeader = TRUE,
                             dataTableOutput(outputId = "part_with_samples")),
                         box(width = 2, 
                             selectInput("select_sample_type_mc", label = "Select sample type ",
                                         choices = all_typename, selected = type_name_s,
                                         multiple = T),
                             selectInput("select_aliquot_mc", label = "Select Aliquot", choices = c(1, 2),
                                         selected = 1),
                             selectInput("select_time_point_mc_part", label = "Select time point", choices = time_points_vec,
                                         selected = "A0"))
                  
                )
                )
             )),
    
    
    
    
    tabItem(tabName = "ncc_samples",
            fluidRow(
              tabsetPanel(
                tabPanel("NCC",
                         box(width = 10, title = "NCC Participants Summary", status = "primary",
                             dataTableOutput("ncc_dt")),
                         box(width = 3 ,downloadButton(outputId = "ncc_down", label = "NCC Samples")),
                         box(width = 10, title = "Available NCC Samples", status = "primary",
                             dataTableOutput("ncc_dt_available"))
                         
                         
                         
                )
                
              )
              
              
            )
    ),
    
    
    
    
    tabItem(tabName = "shipped_samples",
            fluidRow(
              tabsetPanel(
                tabPanel("Shipped Samples Tally",
                         box(width = 12, title = "Shipped Samples",solidHeader = T,
                             status = "primary",
                             dataTableOutput(outputId = "dt_shipped_samples")),
                         box(width = 2,
                             downloadButton(outputId = "down_shipped_samples", label = "Shipped Samples"))
                         )
              ))),
    
    
    
    tabItem(tabName = "young_infants",
            fluidRow(
              tabsetPanel(
                tabPanel("Young Infants Samples",
                         box(width = 12, title = "Young Infants Samples", status = "primary",
                             dataTableOutput("young_infants_dt")),
                         box(width = 2 ,downloadButton(outputId = "young_infants_all_down", label = "Young Infants Samples"))
                        ),
                
                tabPanel("Young Infants per Time",
                         box(width = 10, title =  "Young Infants Samples",
                             status = "primary", solidHeader = T,
                             dataTableOutput("young_infants_time")),
                         box(width = 2, 
                             selectInput("select_time_point_yi", label = "Select Time Point",
                                         choices = time_points_vec, selected = "A0"),
                             selectInput("select_alive_yi", label = "Select based deaths",
                                         choices = deaths_select, selected = "Died")),
                         box(width = 3, 
                             downloadButton(outputId = "y_i_time_down", label = "Download")),
                         box(width = 10, title =  "Young Infants Samples",
                             status = "primary", solidHeader = T,
                             dataTableOutput("yi_samples_time_alive")),
                         
                         )
                
              )
              
              
            )
            ),
    
    ## 
    tabItem(tabName = "bmc",
            fluidRow(
              tabsetPanel(
                tabPanel("BMC",
                         box(width = 12, title = "BMC Samples", status = "primary",
                             dataTableOutput("bmc_dt")),
                         box(width = 2 ,downloadButton(outputId = "bmc_down", label = "BMC Samples"))
                         
                         
                         
                )
                
              )
              
              
            )
    ),
    
    
    
    
    
    
    tabItem(tabName= "chain_freezers",
            #fluidPage(theme = mytheme,
            fluidRow(
              
              tabsetPanel(
                tabPanel("Biobank Freezers",
                         column(width = 12,
                                
                                
                           box(width = 10, title = "Freezer % Full", status = "primary",
                               plotlyOutput(outputId = "plot_freezer_space")),
                           #box(width = 2, title = "Select Input",
                               # selectInput(inputId =  "freezer_select", 
                               #             label = "Select Freezer Name",
                               #             choices = freezer_nms,
                               #             selected = 55)),
                           box(width = 2,
                               
                               conditionalPanel(
                                 condition = "input.freezer_select=='42'",
                                 selectInput("select_tray_42", label = "Select Tray",
                                             choices = freezer_trays[["42"]], 
                                             selected = freezer_trays[["42"]][1])
                                 
                               ),
                               
                               conditionalPanel(
                                 condition = "input.freezer_select=='43'",
                                 selectInput("select_tray_43", label = "Select Tray",
                                             choices = freezer_trays[["43"]],
                                             selected = freezer_trays[["43"]][1] )
                                 
                                 
                               ),
                               conditionalPanel(
                                 condition = "input.freezer_select=='49'",
                                 selectInput("select_tray_49", label = "Select Tray",
                                             choices = freezer_trays[["49"]],
                                             selected = freezer_trays[["49"]][1])
                               ),
                               conditionalPanel(
                                 condition = "input.freezer_select=='55'",
                                 selectInput("select_tray_55", label = "Select Tray",
                                             choices = freezer_trays[["55"]])
                               ),
                               conditionalPanel(
                                 condition = "input.freezer_select=='59'",
                                 selectInput("select_tray_59", label = "Select Tray",
                                             choices = freezer_trays[["59"]],
                                             selected = freezer_trays[["59"]][1])
                               )),
                           
                           
                           box(width = 10, title = "Trays Average Samples",
                               plotlyOutput(outputId = "plot_freezer_tray_box"))
                                )
                         
                        ## Conditional panel
                        
                         
                         
                         ),
                tabPanel("Find half full boxes",
                         
                         column(width = 12,
                                
                                box(width = 9, solidHeader = T, primary = T,
                                    dataTableOutput("choose_trays_boxes_full")
                                    ),
                                box(width = 3, title = "Select boxes within a range of % full",
                                    
                                    numericRangeInput("box_select_perc", value = c(0, 40), label = "Select values"),
                                    downloadButton(outputId = "down_boxes_half_full", label = "Samples is in boxes"),
                                    downloadButton(outputId = "down_boxes_half_full_boxes", label = "Selected boxes"))
                                
                                )
                         
                         )
              )
            )
            
            )

  )

)

# combine to user interface using dashboard page
ui <- dashboardPage(header_contents,
              sidebar_contents,
              body_contentds,
              controlbar = dashboardControlbar(collapsed = T, skinSelector())
              )

# -------------------------------------------------------------------------


