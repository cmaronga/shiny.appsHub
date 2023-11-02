# -------------------------------------------------------------------------
thematic::thematic_shiny()


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
      h2, h3, h4, h5, h6 {
        font-family: 'Grandstander', sans-serif;
      }
      
      h2, h3{
      color: #002147;
      text-align: center;
      }
      
       h3 {
      font-weight: bold;
      color: white;
      text-align: center;
      }
      h5 {
        font-size: 15px;
      }
      
   
       /* logo */
      .skin-blue .main-header .logo {
      background-color: white;
      }
      
      #input_message, #generate_table{
      color: red;
      font-weight: bold;
      }
      
       #generate_table{
      color: black;
      font-weight: bold;
      font-size: 18px;
      background-color: grey;
      }
      
       #output_type{
      color: black;
      font-weight: bold;
      font-size: 15px;
      }
      
      #subtitle{
      text-align: center;
      font-size: 20px;
      }
      
      
      #hip_nums{
      background-color: #002147;
      color: white;
      font-size: 18px;
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
  
  fluidRow(theme = mytheme,
           box(
             width = 12,
             title = h2("PFC Benefit and Budget Impact Calculator"),
             textOutput("subtitle"),
             solidHeader = F
           )),

  # inputs section
  fluidRow(theme = mytheme,
           box(width = 6,
               title = h4("Number of hip fractures in your hospital or region"),
               solidHeader = F, status = "primary",
               numericInput("hip_nums", 
                            label = "Enter the number Hip fractures below", 
                            min = 100, max = 2000, value = 0, width = "300px"),
               tags$hr(),
               # instert an action button
               actionBttn(inputId = "generate_table", 
                          label = h5("Click to Generate Tables"),
                          style = "bordered",
                          color = "success"),
               tags$hr()
               ),
           
           box(width = 6,
               solidHeader = F,
               title = h5("Simulation cohorts"),
               dataTableOutput("indexTable", height = "185px"))
           ),
  
  # Section: Outputs
  fluidRow(
    box(width = 12,
        solidHeader = F,
        title = h3("Outputs"), background = "navy")
  ),
  
  # Now, begining of actual outputs
  
           box(width = 3,
               solidHeader = F,
               title = "Controls",
               radioButtons("output_type", "", 
                            choices = c("Patient outcomes", 
                                        "Resource use", 
                                        "Costs"),
                            selected = "Patient outcomes"),
               tags$hr(),
               background = "aqua"),
           box(width = 5,
               solidHeader = F,
               title = "Outputs",
               background = "aqua",
               dataTableOutput("all_outputs", height = "480px")),
           box(width = 4,
               solidHeader = F,
               title = "Notes",
               background = "aqua",
               textOutput("output_messages"))
  
  
  
  
)

# combine to user interface using dashboard page
ui <- dashboardPage(header_contents,
                    sidebar_contents,
                    body_contents,
                    footer = dashboardFooter(left = h6("© Copyright,", paste(lubridate::year(Sys.Date()))))
)

# -------------------------------------------------------------------------


