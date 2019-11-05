library(tidyverse)

shinyServer(function(input, output) {
  selected <- reactiveValues()
  observe({
    selected$topic1 <- input$topic
    selected$topic2 <- input$topics2
    selected$geo <- input$geo
    selected$estimate_type <- input$estimate_type
    selected$race <- input$group
    selected$pop_type <- input$pop_type
  })
  
observeEvent(input$goButton,{
  
dta_load <- reactive({
      if(selected$geo == "State"){
        dta <- read_rds(url("https://www.dropbox.com/s/9ydntngj2q7o0tf/dta_county.rds?raw=1"))
        return(dta)
      }else if(selected$geo == "County"){
        dta <- read_rds(url("https://www.dropbox.com/s/bbjryt6xsdt8dw1/dta_county.rds?raw=1"))
        return(dta)
      }else if(selected$geo == "Congressional District") {
        dta <- read_rds(url("https://www.dropbox.com/s/95dx57nyrlygo2d/dta_district.rds?raw=1"))
        return(dta)
      }else{
        print("WHAT")
      }
    }) 
    
output$sample_view <- DT::renderDataTable({
      DT::datatable(head(dta_load),
                    options = list(paging = FALSE,dom="t"),
                    rownames= FALSE)
    })
  })
  
  
})