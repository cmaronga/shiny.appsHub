
# create function to download tables
# call the function like 'download_DT()' at end of the table

download_DT <- function(x) {
  DT::datatable(
    x, rownames = F,
    extensions = 'Buttons',
    options = list(
      dom = 'Blfrtip',
      buttons = c('csv', 'excel', 'pdf'),
      lengthMenu = list(c(15, 25, 50, -1),
                        c(15, 25, 50, "All"))
    )
  )
}



# define server
server <- function(input, output, session){

  # reactive dataset for the tables
  cran_pks <- reactive({
    if (input$year == "All time"){
      CRAN_pkgs %>% 
        select(pkg_name, Version, date_published, Title, Repository, NeedsCompilation) %>% 
        arrange(desc(date_published))
    } else {
      CRAN_pkgs %>% 
        filter(year_published == as.numeric(input$year)) %>% 
        select(pkg_name, Version, date_published, Title, Repository, NeedsCompilation) %>% 
        arrange(desc(date_published))
        
      
    }
  })

  output$all_pkgs <- renderDataTable(
    cran_pks() %>% download_DT(),
    rownames = F
  )
  
  # package statement
  output$pkg_statement <- renderText(
    
    if (input$year != "All time"){
      paste0("A total of ", nrow(cran_pks()), " packages were published in the year ", input$year) 
    } else {
      paste0("All time published packages ", nrow(cran_pks()))
    }
  )
  
  
  
}