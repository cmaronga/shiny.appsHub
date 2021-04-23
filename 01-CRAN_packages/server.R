# define server
server <- function(input, output, session){
  
  pkgs_df <- reactive({
    available.packages()[, c("Version",
                             "Depends",
                             "Repository", 
                             "NeedsCompilation",
                             "License")]
  })
  
  output$all_pkgs <- renderDataTable({
    pkgs_df()
  })
  
}