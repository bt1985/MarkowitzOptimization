searchresult<-function(search_table, searchterm){
  res<- search_table %>%
        filter(grepl(searchterm, ticker)|grepl(searchterm, description)) %>% 
        head(10) %>% 
        select(!(collection))%>%
        mutate(Add = shinyInput_col(actionButton, nrow(.), 'button_', class="btn_addtoportfolio", label = img (src="images/add.svg", width="20", height="20"), onclick = 'Shiny.setInputValue(\"select_button\",  this.id,{priority: \"event\"})'))
  return(res)
}