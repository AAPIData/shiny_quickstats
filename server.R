#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#library(tidycensus)
library(tidyverse)
library(feather)
library(plotly)
library(tidyr)

#read in all options
topic_choices <- read_csv("quickstats_table_ref.csv")
#subset the data based on topics
#topic_choices <- topic_choices %>% filter(topic %in% c("edu","poverty", "Nativity"))

#read in all detailed ethnicity options
detailed_choices <- read_csv("detailed_race_reference.csv")
drace <- unique(detailed_choices$detailed_ethnicity)
daa <- unique(detailed_choices$detailed_ethnicity[detailed_choices$group=="AA"])
dnhpi <- unique(detailed_choices$detailed_ethnicity[detailed_choices$group=="NHPI"])

shinyServer(function(input, output,session) {
  
  #consequent drop down menu
  
  #select topic first
  observe({
    updateSelectizeInput(session,
                      "topic",
                      choices = unique(topic_choices$Topic))
  })
  
  #select geo second
  observe({
    updateSelectizeInput(
      session,
      "geo",
      choices = topic_choices %>%
        filter(Topic == input$topic) %>%
        select(Geography) %>%
        unique() %>% .[[1]]
    )
  })
  
  #select group third
  observe({
    updateSelectizeInput(
      session,
      "group",
      choices = topic_choices %>%
        filter(Topic == input$topic) %>%
        filter(Geography == input$geo) %>%
        select(Group) %>%
        unique() %>% .[[1]]
    )
  })
  
  #select detailed group fourth
  observeEvent(input$group,{
    req(input$group)
    if (str_detect(input$group, "detailed", negate = T)){
      return()
    }
    updateSelectInput(session, "detailed_filter", choices = as.list(drace))
  }
  )
  
observeEvent(input$do,{
  if (input$group == "") {
    return()
  }
  dta_load <- reactive({
    #match aa or nhpi based on selected detialed groups
    if(length(input$detailed_filter)==0){
      tags.aapi <- c()
    }else if(sum(input$detailed_filter %in% daa)/length(input$detailed_filter) == 0){
      tags.aapi <- c("NHPI", input$detailed_filter)
    }else if(sum(input$detailed_filter %in% daa)/length(input$detailed_filter) > 0 & sum(input$detailed_filter %in% daa)/length(input$detailed_filter) < 1){
      tags.aapi <- c("Asian American", "NHPI", input$detailed_filter)
    }else if(sum(input$detailed_filter %in% daa)/length(input$detailed_filter) == 1){
      tags.aapi <- c("Asian American", input$detailed_filter)
    }
    #match aa in combo or nhpi based on selected detialed groups
    if(length(input$detailed_filter)==0){
      tags.aapic <- c()
    }else if(sum(input$detailed_filter %in% daa)/length(input$detailed_filter) == 0){
      tags.aapic <- c("NHPI", input$detailed_filter)
    }else if(sum(input$detailed_filter %in% daa)/length(input$detailed_filter) > 0 & sum(input$detailed_filter %in% daa)/length(input$detailed_filter) < 1){
      tags.aapic <- c("Asian American alone or in combo", "NHPI", input$detailed_filter)
    }else if(sum(input$detailed_filter %in% daa)/length(input$detailed_filter) == 1){
      tags.aapic <- c("Asian American alone or in combo", input$detailed_filter)
    }
    #get data for each selection
    if(input$geo == "National" & input$topic == "poverty"){
      dta <- read_rds("dta/poverty_national.RDS")
      dta <- dta %>% 
        filter(group ==input$group) %>% 
        select(NAME,estimate_type,topic_type,estimate) %>% 
        rename(Geography = NAME) %>% 
        spread(estimate_type, estimate) %>% 
        rename(`Total` = count,
               `Share` = prop,
               `Estimate` = topic_type)
      return(dta)
    } else if(input$geo == "State" & input$topic == "poverty"){
      dta <- read_rds("dta/poverty_state.RDS")
      dta <- dta %>% 
        filter(group ==input$group) %>% 
        select(NAME,estimate_type,topic_type,estimate) %>% 
        rename(Geography = NAME) %>% 
        spread(estimate_type, estimate) %>% 
        rename(`Total` = count,
               `Share` = prop,
               `Estimate` = topic_type)
      return(dta)
    }else if(input$geo == "County" & input$topic == "poverty"){
      dta <- read_rds("dta/poverty_county.RDS")
      dta <- dta %>% 
        filter(group ==input$group) %>% 
        select(NAME,estimate_type,topic_type,estimate) %>% 
        rename(Geography = NAME) %>% 
        spread(estimate_type, estimate) %>% 
        rename(`Total` = count,
               `Share` = prop,
               `Estimate` = topic_type)
      return(dta)
    }else if(input$geo == "Congressional District" & input$topic == "poverty"){
      dta <- read_rds("dta/poverty_district.RDS")
      dta <- dta %>% 
        filter(group ==input$group) %>% 
        select(NAME,estimate_type,topic_type,estimate) %>% 
        rename(Geography = NAME) %>% 
        spread(estimate_type, estimate) %>% 
        rename(`Total` = count,
               `Share` = prop,
               `Estimate` = topic_type)
      return(dta)
    }else if(input$geo == "National" & input$topic == "edu"){
      dta <- read_rds("dta/edu_national.RDS")
      dta <- dta %>% 
        filter(group ==input$group) %>% 
        filter(topic_type == input$topic_type) %>%
        select(NAME,topic_type,estimate_type,estimate) %>% 
        rename(Geography = NAME) %>% 
        spread(estimate_type, estimate) %>% 
        rename(`Total` = count,
               `Share` = prop,
               `Estimate` = topic_type)
      return(dta)
    } else if(input$geo == "State" & input$topic == "edu"){
      dta <- read_rds("dta/edu_state.RDS")
      dta <- dta %>% 
        filter(group ==input$group) %>% 
        filter(topic_type == input$topic_type) %>%
        select(NAME,topic_type,estimate_type,estimate) %>% 
        rename(Geography = NAME) %>% 
        spread(estimate_type, estimate) %>% 
        rename(`Total` = count,
               `Share` = prop,
               `Estimate` = topic_type)
      return(dta)
    }else if(input$geo == "County" & input$topic == "edu"){
      dta <- read_rds("dta/edu_county.RDS")
      dta <- dta %>% 
        filter(group ==input$group) %>% 
        filter(topic_type == input$topic_type) %>%
        select(NAME,topic_type,estimate_type,estimate) %>% 
        rename(Geography = NAME) %>% 
        spread(estimate_type, estimate) %>% 
        rename(`Total` = count,
               `Share` = prop,
               `Estimate` = topic_type)
      return(dta)
    }else if(input$geo == "Congressional District" & input$topic == "edu"){
      dta <- read_rds("dta/edu_district.RDS")
      dta <- dta %>% 
        filter(group ==input$group) %>% 
        filter(topic_type == input$topic_type) %>%
        select(NAME,topic_type,estimate_type,estimate) %>% 
        rename(Geography = NAME) %>% 
        spread(estimate_type, estimate) %>% 
        rename(`Total` = count,
               `Share` = prop,
               `Estimate` = topic_type)
      return(dta)
    }else if(input$geo == "National" & input$topic == "Nativity"){
      dta <- read_rds("dta/nativity_national.RDS")
      dta <- dta %>% 
        filter(group ==input$group) %>% 
        filter(topic_type == input$topic_type) %>%
        select(NAME,topic_type,estimate_type,estimate) %>% 
        rename(Geography = NAME) %>% 
        spread(estimate_type, estimate) %>% 
        rename(`Total` = count,
               `Share` = prop,
               `Estimate` = topic_type)
      return(dta)
    } else if(input$geo == "State" & input$topic == "Nativity"){
      dta <- read_rds("dta/nativity_state.RDS")
      dta <- dta %>% 
        filter(group ==input$group) %>% 
        filter(topic_type == input$topic_type) %>%
        select(NAME,topic_type,estimate_type,estimate) %>% 
        rename(Geography = NAME) %>% 
        spread(estimate_type, estimate) %>% 
        rename(`Total` = count,
               `Share` = prop,
               `Estimate` = topic_type)
      return(dta)
    }else if(input$geo == "County" & input$topic == "Nativity"){
      dta <- read_rds("dta/nativity_county.RDS")
      dta <- dta %>% 
        filter(group ==input$group) %>% 
        filter(topic_type == input$topic_type) %>%
        select(NAME,topic_type,estimate_type,estimate) %>% 
        rename(Geography = NAME) %>% 
        spread(estimate_type, estimate) %>% 
        rename(`Total` = count,
               `Share` = prop,
               `Estimate` = topic_type)
      return(dta)
    }else if(input$geo == "Congressional District" & input$topic == "Nativity"){
      dta <- read_rds("dta/nativity_district.RDS")
      dta <- dta %>% 
        filter(group ==input$group) %>% 
        filter(topic_type == input$topic_type) %>%
        select(NAME,topic_type,estimate_type,estimate) %>% 
        rename(Geography = NAME) %>% 
        spread(estimate_type, estimate) %>% 
        rename(`Total` = count,
               `Share` = prop,
               `Estimate` = topic_type)
      return(dta)
      
    }else if(input$geo == "National" & input$topic == "Population"){
      dta <- read_rds("populationdata_full.RDS")
      dta <- dta %>% 
        filter(geography=="us") 
      if(grepl("detailed", input$group, fixed=T)){
        if(grepl("combo", input$group, fixed=T)){
          dta <- dta %>%
            filter(group == "detailed ethnicity (AAPI alone or in combo)") %>%
            filter(label %in% tags.aapic) %>%
            arrange(factor(label, levels = tags.aapic))
        }else{
          dta <- dta %>%
            filter(group == "detailed ethnicity (AAPI alone)") %>%
            filter(label %in% tags.aapi) %>%
            arrange(factor(label, levels = tags.aapi))
        }
      }else{
        if(grepl("combo", input$group, fixed=T)){
          dta <- dta %>%
            filter(group == "AAPI Alone or in combo")
        }else{
          dta <- dta %>%
            filter(group == "AAPI Alone")
        }
      }
      if(input$reliable == FALSE){
        dta.est <- dta %>%
          select(NAME,label,estimate) %>% 
          rename('Geography' = NAME) %>% 
          rename('Population size' = estimate)
        dta.per <- dta %>%
          select(NAME,label,pct_pop) %>% 
          rename('Geography' = NAME) %>% 
          rename('Population percentage' = pct_pop)
      }else if(input$reliable == TRUE){
        dta.est <- dta %>%
          select(NAME,label,est_reliable) %>% 
          rename('Geography' = NAME) %>% 
          rename('Population size' = est_reliable)
        dta.per <- dta %>%
          select(NAME,label,per_reliable) %>% 
          rename('Geography' = NAME) %>% 
          rename('Population percentage' = per_reliable)
      }
      return(list(dta.est, dta.per))
      
    }else if(input$geo == "State" & input$topic == "Population"){
      dta <- read_rds("populationdata_full.RDS")
      dta <- dta %>% 
        filter(geography=="state")
      if(grepl("detailed", input$group, fixed=T)){
        if(grepl("combo", input$group, fixed=T)){
          dta <- dta %>%
            filter(group == "detailed ethnicity (AAPI alone or in combo)") %>%
            filter(label %in% tags.aapic) %>%
            arrange(factor(label, levels = tags.aapic))
        }else{
          dta <- dta %>%
            filter(group == "detailed ethnicity (AAPI alone)") %>%
            filter(label %in% tags.aapi) %>%
            arrange(factor(label, levels = tags.aapi))
        }
      }else{
        if(grepl("combo", input$group, fixed=T)){
          dta <- dta %>%
            filter(group == "AAPI Alone or in combo")
        }else{
          dta <- dta %>%
            filter(group == "AAPI Alone")
        }
      }
      if(input$reliable == FALSE){
        dta.est <- dta %>%
          select(NAME,label,estimate) %>% 
          rename('Geography' = NAME) %>% 
          pivot_wider(names_from = label, values_from = estimate)
        dta.per <- dta %>%
          select(NAME,label,pct_pop) %>% 
          rename('Geography' = NAME) %>% 
          pivot_wider(names_from = label, values_from = pct_pop)
      }else if(input$reliable == TRUE){
        dta.est <- dta %>%
          select(NAME,label,est_reliable) %>% 
          rename('Geography' = NAME) %>% 
          pivot_wider(names_from = label, values_from = est_reliable)
        dta.per <- dta %>%
          select(NAME,label,per_reliable) %>% 
          rename('Geography' = NAME) %>% 
          pivot_wider(names_from = label, values_from = per_reliable)
      }
      return(list(dta.est, dta.per))
      
    }else if(input$geo == "Congressional District" & input$topic == "Population"){
      dta <- read_rds("populationdata_full.RDS")
      dta <- dta %>% 
        filter(geography=="district") %>% 
        separate(NAME, c("district", "state"), ", ")
      dta$district <- gsub("\\s*\\([0-9][^\\)]+\\)","",dta$district)
      if(grepl("detailed", input$group, fixed=T)){
        if(grepl("combo", input$group, fixed=T)){
          dta <- dta %>%
            filter(group == "detailed ethnicity (AAPI alone or in combo)") %>%
            filter(label %in% tags.aapic) %>%
            arrange(factor(label, levels = tags.aapic))
        }else{
          dta <- dta %>%
            filter(group == "detailed ethnicity (AAPI alone)") %>%
            filter(label %in% tags.aapi) %>%
            arrange(factor(label, levels = tags.aapi))
        }
      }else{
        if(grepl("combo", input$group, fixed=T)){
          dta <- dta %>%
            filter(group == "AAPI Alone or in combo")
        }else{
          dta <- dta %>%
            filter(group == "AAPI Alone")
        }
      }
      if(input$reliable == FALSE){
        dta.est <- dta %>%
          select(district,state,label,estimate) %>% 
          rename('District' = district) %>% 
          rename('State' = state) %>%
          pivot_wider(names_from = label, values_from = estimate)
        dta.per <- dta %>%
          select(district,state,label,pct_pop) %>% 
          rename('District' = district) %>% 
          rename('State' = state) %>%
          pivot_wider(names_from = label, values_from = pct_pop)
      }else if(input$reliable == TRUE){
        dta.est <- dta %>%
          select(district,state,label,est_reliable) %>% 
          rename('District' = district) %>% 
          rename('State' = state) %>%
          pivot_wider(names_from = label, values_from = est_reliable)
        dta.per <- dta %>%
          select(district,state,label,per_reliable) %>% 
          rename('District' = district) %>% 
          rename('State' = state) %>%
          pivot_wider(names_from = label, values_from = per_reliable)
      }
      return(list(dta.est, dta.per))
      
    }else if(input$geo == "County" & input$topic == "Population"){
      dta <- read_rds("populationdata_full.RDS")
      dta <- dta %>% 
        filter(geography=="county") %>%
        separate(NAME, c("county", "state"), ", ") 
      if(grepl("detailed", input$group, fixed=T)){
        if(grepl("combo", input$group, fixed=T)){
          dta <- dta %>%
            filter(group == "detailed ethnicity (AAPI alone or in combo)") %>%
            filter(label %in% tags.aapic) %>%
            arrange(factor(label, levels = tags.aapic))
        }else{
          dta <- dta %>%
            filter(group == "detailed ethnicity (AAPI alone)") %>%
            filter(label %in% tags.aapi) %>%
            arrange(factor(label, levels = tags.aapi))
        }
      }else{
        if(grepl("combo", input$group, fixed=T)){
          dta <- dta %>%
            filter(group == "AAPI Alone or in combo")
        }else{
          dta <- dta %>%
            filter(group == "AAPI Alone")
        }
      }
      if(input$reliable == FALSE){
        dta.est <- dta %>%
          select(county,state,label,estimate) %>% 
          rename('County' = county) %>% 
          rename('State' = state) %>%
          pivot_wider(names_from = label, values_from = estimate)
        dta.per <- dta %>%
          select(county,state,label,pct_pop) %>% 
          rename('County' = county) %>% 
          rename('State' = state) %>%
          pivot_wider(names_from = label, values_from = pct_pop)
      }else if(input$reliable == TRUE){
        dta.est <- dta %>%
          select(county,state,label,est_reliable) %>% 
          rename('County' = county) %>% 
          rename('State' = state) %>%
          pivot_wider(names_from = label, values_from = est_reliable)
        dta.per <- dta %>%
          select(county,state,label,per_reliable) %>% 
          rename('County' = county) %>% 
          rename('State' = state) %>%
          pivot_wider(names_from = label, values_from = per_reliable)
      }
      return(list(dta.est, dta.per))
      
    }else if(input$geo == "Metro Area" & input$topic == "Population"){
      dta <- read_rds("populationdata_full.RDS")
      dta <- dta %>% 
        filter(geography=="Metro Area") 
      if(grepl("detailed", input$group, fixed=T)){
          if(grepl("combo", input$group, fixed=T)){
            dta <- dta %>%
              filter(group == "detailed ethnicity (AAPI alone or in combo)") %>%
              filter(label %in% tags.aapic) %>%
              arrange(factor(label, levels = tags.aapic))
          }else{
            dta <- dta %>%
              filter(group == "detailed ethnicity (AAPI alone)") %>%
              filter(label %in% tags.aapi) %>%
              arrange(factor(label, levels = tags.aapi))
          }
        }else{
          if(grepl("combo", input$group, fixed=T)){
            dta <- dta %>%
              filter(group == "AAPI Alone or in combo")
          }else{
            dta <- dta %>%
              filter(group == "AAPI Alone")
          }
        }
      if(input$reliable == FALSE){
        dta.est <- dta %>%
          select(NAME,label,estimate) %>% 
          rename('Metro Area' = NAME) %>% 
          pivot_wider(names_from = label, values_from = estimate) 
        dta.per <- dta %>%
          select(NAME,label,pct_pop) %>% 
          rename('Metro Area' = NAME) %>% 
          pivot_wider(names_from = label, values_from = pct_pop)
      }else if(input$reliable == TRUE){
        dta.est <- dta %>%
          select(NAME,label,est_reliable) %>% 
          rename('Metro Area' = NAME) %>%
          pivot_wider(names_from = label, values_from = est_reliable)
        dta.per <- dta %>%
          select(NAME,label,per_reliable) %>% 
          rename('Metro Area' = NAME) %>% 
          pivot_wider(names_from = label, values_from = per_reliable)
      }
      return(list(dta.est, dta.per))
    }
    })

  output$selected_topic <- renderText({ 
    # glue::glue("Greetings Human! It looks like you want to look at {input$topic}, for {input$group} at the {input$geo}")
    paste("DEBUGGING: Your geo is: ", input$geo," your group is : ",input$race, " Your TOPIC IS: ", input$topic, "topic type: ", input$topic_type)
  })
  
  js <- c(
    "function(settings){",
    "  var datatable = settings.oInstance.api();",
    "  var table = datatable.table().node();",
    "  var caption = '*data source: 2018 ACS 5-year'",
    "  $(table).append('<caption style=\"caption-side: bottom\">' + caption + '</caption>');",
    "}"
  )
  
  output$preview <- DT::renderDataTable({
    DT::datatable(topic_choices,
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top; text-align: center; color:#EFA875; font-size:200%;',
                    input$topic
                  ),
                  extensions = 'Buttons',
                  options = list(drawCallback = JS(js), dom = 'Btp', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) 
      #formatCurrency(3,currency = "", interval = 3, mark = ",", digits = 0) %>% 
      #formatPercentage(4) 
      #formatStyle("Estimate", "reliable", color = styleEqual(c("TRUE", "FALSE"), c("black", "red")))
    
  })
  
  output$percentage <- DT::renderDataTable({
    DT::datatable(dta_load()[[2]],
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top; text-align: center; color:#EFA875; font-size:200%;',
                    input$topic
                  ),
                  extensions = 'Buttons',
                  options = list(drawCallback = JS(js), dom = 'Btp', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) 
    #formatCurrency(3,currency = "", interval = 3, mark = ",", digits = 0) %>% 
    #formatPercentage(4) 
    #formatStyle("Estimate", "reliable", color = styleEqual(c("TRUE", "FALSE"), c("black", "red")))
    
  })
  
  })

})
