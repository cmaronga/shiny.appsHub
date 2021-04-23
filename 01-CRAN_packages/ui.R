
# Define header
header_contents <- dashboardHeader(
  title = "Exploring CRAN packages",
  titleWidth = 400
)

# define sidebar
sideBar_contents <- dashboardSidebar(
  sidebarMenu(
    menuItem("CRAN packages",
             tabName = "cran_pkgs")
  )
)

# define body
body_contents <- dashboardBody(
  tabItems(
    tabItem(tabName = "cran_pkgs",
            fluidRow(
              box(width = 8, solidHeader = T, status = "primary",
                  dataTableOutput("all_pkgs"),
                  title = paste0("List of total number of packages available in CRAN as of ", format(Sys.time(), '%d %B, %Y'))
                  )
            
            )
    )
  )
)


# combine into app

ui <- dashboardPage(header = header_contents,
                    sidebar = sideBar_contents,
                    body = body_contents)
