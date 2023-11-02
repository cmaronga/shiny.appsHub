# List of countries
countries <- c(
  "Netherlands",
  "Spain",
  "Japan",
  "Brazil",
  "Colombia",
  "Generic country",
  "England",
  "France",
  "Mexico",
  "United Kingdom",
  "Russia")
 
# List of scenarios
scenarios <- c(
  "FLS_Basecase",                      
  "FLS_Identification_100",           
  "FLS_Treatment_initiation_one_month",
  "FLS_Monitoring_100",               
  "FLS_Alendronate_only",              
  "FLS_Injectables_only",              
  "FLS_Injectables_only.max_reduction",
  "FLS_Adherence_100",                 
  "FLS_Perfect_FLS",                   
  "FLS_Hips_and_spines_only",          
  "FLS_Hips_only")

# list of categories
categories <- c("Population characteristics",
                "Re-fracture rates",         
                "Pharmacologic",             
                "Case identification",       
                "Treatment start",           
                "Treatment effect",         
                "Adherence",                 
                "Monitoring rates",          
                "Mortality",                 
                "Cohort sizes",
                "Time lag effects",
                "Cost values")


# -------------------------------------------------------------------------

# define title for logo
my_title <- tags$a(
  href = 'https://www.google.co.uk/',
  tags$img(src = "IOF.png",
           height = 60)
  )

# dashboard header items
header_contents <- dashboardHeader(
   title = my_title,
  titleWidth = 300)

# -------------------------------------------------------------------------


# dashboard side bar items
sidebar_contents <- dashboardSidebar(minified = FALSE,
                                     width = 300,
                                     disable = TRUE)

# -------------------------------------------------------------------------

# dashboard body items
body_contents <- dashboardBody(
  tags$head(
    # 
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: #002147;
      }
      h2, h3, h5, h6 {
        font-family: 'Grandstander', sans-serif;
      }
      
      h2 {
      color: #002147;
      text-align: center;
      }
      
       h3 {
      font-weight: bold;
      color: white;
      text-align: center;
       }
      
      h5 {
        font-size: 16px;
        color: maroon;
       text-align: center;
      }
      
   
       /* logo */
      .skin-blue .main-header .logo {
      background-color: white;
      }
      
      h4{
      color: #002147;
      font-weight: bold;
      text-align: center;
      font-size: 20px;
      font-family: 'Grandstander', sans-serif;
      }
      
       #comp_countries{
      color: black;
      font-weight: bold;
      font-size: 16px;
      font-family: 'Grandstander', sans-serif;
      }
      
      #subtitle{
      text-align: center;
      font-size: 20px;
      }
      
      
      #download, #download_comparisons, #down_c_scenarios, #generate_report{
      background-color: #002147;
      color: white;
      font-size: 16px;
      }
      
     /*
      .skin-blue .main-sidebar {
      background-color: #002147;
      }
     */
      
      /* active selected tab in the sidebarmenu 
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
      background-color: #FF6600;
      } 
     */
      
       .skin-blue .main-sidebar .sidebar .sidebar-menu {
      background-color: #002147;
      } 
      
      .skin-blue .main-header .navbar {
      background-color: #002147;
      }
      
      /* body - main background colour */
      .content-wrapper, .right-side {
      background-color: steelblue;
      } 
      
      */
     .shiny-input-container {
        background-color: #002147;
      }
     */

     "))
  ),
  
  
  tabsetPanel(
    tabPanel(h5("Country model inputs"),
             fluidRow(theme = mytheme,
                      box(
                        width = 12,
                        title = h4("PFC Benefit and Budget Impact Calculator"),
                        textOutput("subtitle"),
                        solidHeader = F
                      )),
             fluidRow(
               box(width = 4,
                   selectInput("country", "1. Select Country",
                               choices = countries,
                               selected = "Generic country")),
               box(width = 3,
                   # select scenario
                   selectInput("scenario", "2. Select Scenario",
                               choices = scenarios,
                               selected = "FLS_Basecase")),
               box(width = 2,
                   selectInput("category", "3. Select input category",
                               choices = c("All", categories),
                               selected = "All")),
               box(width = 3,
                   title = h4("Model Inputs Report"),
                   solidHeader = T,
                   downloadButton("generate_report", 
                                  "Download PDF report"))
             ),
             
             fluidRow(
               box(
                 width = 12,
                 downloadButton("download", "Download dataset"),
                 dataTableOutput("inputs_table"),
                 solidHeader = T
               )
             ),
             tags$hr()
             
             ),
    
    tabPanel(h5("Model input comparison between countries"),
             
             fluidRow(
               box(
                 width = 8,
                 checkboxGroupInput("comp_countries", 
                                    label = "1. Select countries to compare",
                                    choices = countries,
                                    selected = c("Netherlands", "Spain"),
                                    inline = TRUE),
                 solidHeader = T
               ),
               
               box(width = 2,
                   # select scenario
                   selectInput("scenario2", "2. Select Scenario",
                               choices = scenarios,
                               selected = "FLS_Basecase")),
               box(width = 2,
                   selectInput("category2", "3. Select input category",
                               choices = c("All", categories),
                               selected = "All"))
             ),
             fluidRow(
               box(
                 width = 12,
                 downloadButton("download_comparisons", "Download dataset"),
                 dataTableOutput("c_countries"),
                 solidHeader = T
               )
             )),
    
    tabPanel(h5("Scenario comparison within countries"),
             
             fluidRow(
               box(
                 width = 2,
                 selectInput("comp2_countries", label = "1. Select country",
                                    choices = countries,
                                    selected = "Generic country"),
                 solidHeader = T
               ),
               
               box(
                 width = 2,
                 selectInput("category3", label = "2. Select category",
                             choices = c("All", categories),
                             selected = "All"),
                 solidHeader = T
               ),
               
               box(width = 8,
                   # select scenario
                   selectInput("scenario3", "3. Select Scenarios to compare",
                               choices = scenarios,
                               multiple = TRUE,
                               selected = scenarios[1:4]))
             ),
             fluidRow(
               box(
                 width = 12,
                 downloadButton("down_c_scenarios", "Download dataset"),
                 dataTableOutput("c_scenarios"),
                 solidHeader = T
               )
             )
             
             )
  )

)

# combine to user interface using dashboard page
ui <- dashboardPage(header_contents,
                    sidebar_contents,
                    body_contents
)

# -------------------------------------------------------------------------


