
# Define header
header_contents <- dashboardHeader(
  title = "Exploring CRAN packages",
  titleWidth = 400
)

# define sidebar
sideBar_contents <- dashboardSidebar(
  sidebarMenu(
    menuItem(h5("CRAN packages list"),
             tabName = "cran_pkgs")
  )
)

# define body
body_contents <- dashboardBody(
  tabItems(
    tabItem(tabName = "cran_pkgs",
            fluidRow(
              box(width = 10, solidHeader = T, status = "primary",
                  dataTableOutput("all_pkgs"),
                  title = paste0("List of total number of packages available in CRAN as of ", format(Sys.time(), '%d %B, %Y'))
                  ),
              
              box(width = 2,
                  title = "Control Displays", solidHeader = T, status = "primary",
                  
                  radioButtons("display", "Choose display type:",
                               choices = c("Table", "Line Graph", "Bar plot"),
                               selected = "Table"),
                  tags$hr(),
                  
                  selectInput("year", "Select year (Table ONLY):",
                              choices = c("All time", unique(CRAN_pkgs$year_published)),
                              selected = "All time"),
                  
                  textOutput("pkg_statement"),
                  
                  tags$hr(),
                  radioButtons("compilation", "Need compilation(Table ONLY)",
                               choices = c("Yes", "No"))
                  )
            
            )
    )
  )
)


# combine into app

ui <- dashboardPage(header = header_contents,
                    sidebar = sideBar_contents,
                    body = body_contents)
