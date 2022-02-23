
# -------------------------------------------------------------------------


# dashboard header items
header_contents <- dashboardHeader(
  title = "CHAIN SAMPLES",
  dropdownMenu(type = "messages"),
  titleWidth = 400
)


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
             
      
             menuSubItem("All Samples",
                         tabName = "all_samples_mainc")
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
                         tabName = "bmc_substudy"),
             
             
             menuSubItem("Nutrition",
                         tabName = "nutrition_substudy")

             ),
    
    # Sub Studies 
    menuItem("Biobank Status",
             tabName = "biobank_status",
             # laboratory reports
             menuSubItem("CHAIN Trays",
                         tabName = "chain_trays"),
             
             # laboratory queries (including KIDMS backlog)
             menuSubItem("CHAIN Freezers",
                         tabName = "chain_freezers")
             
    )
  )
)


# -------------------------------------------------------------------------

# dashboard body items
body_contentds <- dashboardBody(
  
  tabItems(
    tabItem(tabName = "home",
            fluidPage(theme =shinytheme("flatly"),
              tabsetPanel(
                tabPanel("All CHAIN SAMPLES",
                         box(width = 12, title = "CHAIN Biobank", solidHeader = T,
                             dataTableOutput(outputId = "all_chain_samples_dt")),
                         box(width = 2,
                             downloadButton("all_samples_down", label = "CHAIN Samples")))
              )
            )),
    
    
    tabItem(tabName ="all_samples_mainc",
            fluidPage(
              box(width = 12, title = "Main Cohort Samples",
                  dataTableOutput(outputId = "mc_samples")),
              box(width = 2,
                  downloadButton(outputId = "mc_down"))
              
            )),
    
    
    
    tabItem(tabName = "shipped_samples",
            fluidPage(
              tabsetPanel(
                tabPanel("Shipped Samples Tally",
                         box(width = 12, title = "Shipped Samples",solidHeader = T,
                             dataTableOutput(outputId = "dt_shipped_samples")),
                         box(width = 2,
                             downloadButton(outputId = "down_shipped_samples", label = "Shipped Samples"))
                         )
              )))

  )

)

# combine to user interface using dashboard page
ui <- dashboardPage(header_contents,
              sidebar_contents,
              body_contentds,
              controlbar = dashboardControlbar(collapsed = T, skinSelector())
              # footer = dashboardFooter(left = "Created by: NeOBAC Data Team",
              #                          right = " Data sources : NeOBAC-CIN databases")
              )

# -------------------------------------------------------------------------


