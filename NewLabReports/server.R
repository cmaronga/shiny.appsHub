

server <- function(input, output, session){
    
    
    output$all_chain_samples_dt <- DT::renderDataTable(
        
        all_chain_samples, 
        style = 'bootstrap4',
        options = list(scrollX = TRUE,
                       pageLength = 25))
    
    
    output$all_samples_down <- down_function(df = all_chain_samples,
                                             file_name = "all_chain_samples")
    
    
    output$dt_shipped_samples <- DT::renderDataTable(
        
        shipped, 
        style = 'bootstrap4',
        options = list(scrollX = TRUE,
                       pageLength =15))
    
    
    output$down_shipped_samples <- down_function(df = shipped,
                                                 file_name = "shipped_samples_tally")
    


}
