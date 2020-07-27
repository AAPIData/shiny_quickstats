#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
<<<<<<< HEAD
# library(tidycensus)
=======
#library(tidycensus)
>>>>>>> 4900d5dff4e18c87a0ee11605b2fcd3eda6ab253
library(tidyverse)
library(feather)
library(plotly)
library(tidyr)

#read in all options
topic_choices <- read_csv("quickstats_table_ref.csv")

#read in all detailed ethnicity options
detailed_choices <- read_csv("detailed_race_reference.csv")
drace <- unique(detailed_choices$detailed_ethnicity)
daa <- unique(detailed_choices$detailed_ethnicity[detailed_choices$group=="AA"])
dnhpi <- unique(detailed_choices$detailed_ethnicity[detailed_choices$group=="NHPI"])
draceund<- c("Undocumented Indian", "Undocumented Chinese", "Undocumented Korean", "Undocumented Vietnamese", "Undocumented Pakistani")

#read in all data
dta.cvap <- read_csv("https://raw.githubusercontent.com/AAPIData/quickstats_datapull/master/acs_database/cvap_dta.csv")
dta.edu <- read_csv("https://raw.githubusercontent.com/AAPIData/quickstats_datapull/master/acs_database/education_dta.csv")
dta.ins <- read_csv("https://raw.githubusercontent.com/AAPIData/quickstats_datapull/master/acs_database/insurance_dta.csv")
dta.lep <- read_csv("https://raw.githubusercontent.com/AAPIData/quickstats_datapull/master/acs_database/lep_dta.csv")
dta.nat <- read_csv("https://raw.githubusercontent.com/AAPIData/quickstats_datapull/master/acs_database/nativity_dta.csv")
dta.pop <- read_csv("https://raw.githubusercontent.com/AAPIData/quickstats_datapull/master/acs_database/population_dta.csv")
dta.pov <- read_csv("https://raw.githubusercontent.com/AAPIData/quickstats_datapull/master/acs_database/poverty_dta.csv")
dta.und <- read_csv("https://raw.githubusercontent.com/AAPIData/quickstats_datapull/master/acs_database/undocumented.csv")

#shiny server
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
    if (str_detect(input$group, "Detailed", negate = T)){
      return()
<<<<<<< HEAD
    }else{
      if(str_detect(input$group, "American")){
        updateSelectInput(session, "detailed_filter", choices = as.list(draceund))
      }else{
        updateSelectInput(session, "detailed_filter", choices = as.list(drace))
      }
    } 
  })
  
  observeEvent(input$do,{
    #if (input$detailed_filter[1] =="") {
      #return(NULL)
    #}
    dta_load <- reactive({
      if(str_detect(input$group, "Detailed")){
        if (input$group=="" | input$topic=="" | input$geo=="" | input$detailed_filter[1]==""){
          return(NULL)
        }else{
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
            tags.aapic <- c("Asian American", "NHPI", input$detailed_filter)
          }else if(sum(input$detailed_filter %in% daa)/length(input$detailed_filter) == 1){
            tags.aapic <- c("Asian American", input$detailed_filter)
          }
          
          #get data for each selection
          if(input$geo == "National" & input$topic == "Citizen Voting Age Population"){
            ################################  National  CVAP ##########################################
            dta <- dta.cvap %>% 
              filter(geography=="national") %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            dta$NAME <- as.factor(dta$NAME)
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(NAME,label,estimate) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate)
              dta.per <- dta %>%
                select(NAME,label,pct) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(NAME,label,estimate_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(NAME,label,pct_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            nformat <- 0
            return(list(dta.est, dta.per, nformat))
            
          } else if(input$geo == "State" & input$topic == "Citizen Voting Age Population"){
            ################################  National  CVAP ##########################################
            dta <- dta.cvap %>% 
              filter(geography=="state") %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            dta$NAME <- as.factor(dta$NAME)
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(NAME,label,estimate) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate)
              dta.per <- dta %>%
                select(NAME,label,pct) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(NAME,label,estimate_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(NAME,label,pct_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            nformat <- 0
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "County" & input$topic == "Citizen Voting Age Population"){
            ################################  County  CVAP ##########################################
            dta <- dta.cvap %>% 
              filter(geography=="county") %>%
              separate(NAME, c("county", "state"), ", ") 
            dta$county <- as.factor(dta$county)
            dta$state <- as.factor(dta$state)
=======
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
      dta <- read_csv("populationdata_full.csv")
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
      dta <- read_csv("populationdata_full.csv")
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
      dta <- read_csv("populationdata_full.csv")
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
      dta <- read_csv("populationdata_full.csv")
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
      dta <- read_csv("populationdata_full.csv")
      dta <- dta %>% 
        filter(geography=="Metro Area") 
      if(grepl("detailed", input$group, fixed=T)){
          if(grepl("combo", input$group, fixed=T)){
>>>>>>> 4900d5dff4e18c87a0ee11605b2fcd3eda6ab253
            dta <- dta %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(county,state,label,estimate) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = estimate)
              dta.per <- dta %>%
                select(county,state,label,pct) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(county,state,label,estimate_reliable) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(county,state,label,pct_reliable) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            nformat <- 0
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "Congressional District" & input$topic == "Citizen Voting Age Population"){
            ################################  National  CVAP ##########################################
            dta <- dta.cvap %>% 
              filter(geography=="district") %>% 
              separate(NAME, c("district", "state"), ", ") 
            dta$district <- gsub("\\s*\\([0-9][^\\)]+\\)","",dta$district)
            dta$district <- as.factor(dta$district)
            dta$state <- as.factor(dta$state)
            dta <- dta %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(district,state,label,estimate) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = estimate)
              dta.per <- dta %>%
                select(district,state,label,pct) %>%
                rename('District' = district) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(district,state,label,estimate_reliable) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(district,state,label,pct_reliable) %>% 
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            nformat <- 0
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "National" & input$topic == "Education"){
            ################################  National  EDUCATION ##########################################
            dta <- dta.edu %>% 
              filter(geography=="national") %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            dta$NAME <- as.factor(dta$NAME)
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(NAME,label,estimate) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate)
              dta.per <- dta %>%
                select(NAME,label,pct) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(NAME,label,estimate_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(NAME,label,pct_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            nformat <- 3
            return(list(dta.est, dta.per, nformat))
            
          } else if(input$geo == "State" & input$topic == "Education"){
            ################################  State  EDUCATION ##########################################
            dta <- dta.edu %>% 
              filter(geography=="state") %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            dta$NAME <- as.factor(dta$NAME)
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(NAME,label,estimate) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate)
              dta.per <- dta %>%
                select(NAME,label,pct) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(NAME,label,estimate_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(NAME,label,pct_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            nformat <- 3
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "County" & input$topic == "Education"){
            ################################  County  EDUCATION ##########################################
            dta <- dta.edu %>% 
              filter(geography=="county") %>%
              separate(NAME, c("county", "state"), ", ") 
            dta$county <- as.factor(dta$county)
            dta$state <- as.factor(dta$state)
            dta <- dta %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(county,state,label,estimate) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = estimate)
              dta.per <- dta %>%
                select(county,state,label,pct) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(county,state,label,estimate_reliable) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(county,state,label,pct_reliable) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            nformat <- 3
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "Congressional District" & input$topic == "Education"){
            ################################  Congressional District  EDUCATION ##########################################
            dta <- dta.edu %>% 
              filter(geography=="district") %>% 
              separate(NAME, c("district", "state"), ", ")
            dta$district <- gsub("\\s*\\([0-9][^\\)]+\\)","",dta$district)
            dta$district <- as.factor(dta$district)
            dta$state <- as.factor(dta$state)
            dta <- dta %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(district,state,label,estimate) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = estimate)
              dta.per <- dta %>%
                select(district,state,label,pct) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(district,state,label,estimate_reliable) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(district,state,label,pct_reliable) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            nformat <- 3
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "National" & input$topic == "Health Insurance"){
            ################################  National  HEALTH INSURANCE ##########################################
            dta <- dta.ins %>% 
              filter(geography=="national") %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            dta$NAME <- as.factor(dta$NAME)
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(NAME,estimate) %>% 
                rename('Geography' = NAME) %>% 
                rename('Population with Insurance' = estimate) 
              dta.per <- dta %>%
                select(NAME,pct) %>% 
                rename('Geography' = NAME) %>% 
                rename('Population with Insurance' = pct) 
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(NAME,estimate_reliable) %>% 
                rename('Geography' = NAME) %>% 
                rename('Population with Insurance' = estimate_reliable)   
              dta.per <- dta %>%
                select(NAME,pct_reliable) %>% 
                rename('Geography' = NAME) %>% 
                rename('Population with Insurance' = pct_reliable)   
            }
            nformat <- 0
            return(list(dta.est, dta.per, nformat))
            
          } else if(input$geo == "State" & input$topic == "Health Insurance"){
            ################################  State  HEALTH INSURANCE ##########################################
            dta <- dta.ins %>% 
              filter(geography=="state") %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            dta$NAME <- as.factor(dta$NAME)
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(NAME,estimate) %>% 
                rename('Geography' = NAME) %>% 
                rename('Population with Insurance' = estimate) 
              dta.per <- dta %>%
                select(NAME,pct) %>% 
                rename('Geography' = NAME) %>% 
                rename('Population with Insurance' = pct) 
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(NAME,estimate_reliable) %>% 
                rename('Geography' = NAME) %>% 
                rename('Population with Insurance' = estimate_reliable)   
              dta.per <- dta %>%
                select(NAME,pct_reliable) %>% 
                rename('Geography' = NAME) %>% 
                rename('Population with Insurance' = pct_reliable)   
            }
            nformat <- 0
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "County" & input$topic == "Health Insurance"){
            ################################  County  HEALTH INSURANCE ##########################################
            dta <- dta.ins %>% 
              filter(geography=="county") %>%
              separate(NAME, c("county", "state"), ", ") 
            dta$county <- as.factor(dta$county)
            dta$state <- as.factor(dta$state)
            dta <- dta %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(county,state,estimate) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                rename('Population with Insurance' = estimate) 
              dta.per <- dta %>%
                select(county,state,pct) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                rename('Population with Insurance' = pct) 
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(county,state,estimate_reliable) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                rename('Population with Insurance' = estimate_reliable)   
              dta.per <- dta %>%
                select(county,state,pct_reliable) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                rename('Population with Insurance' = pct_reliable)   
            }
            nformat <- 0
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "Congressional District" & input$topic == "Health Insurance"){
            ################################  Congressional District  HEALTH INSURANCE ##########################################
            dta <- dta.ins %>% 
              filter(geography=="district") %>% 
              separate(NAME, c("district", "state"), ", ")
            dta$district <- gsub("\\s*\\([0-9][^\\)]+\\)","",dta$district)
            dta$district <- as.factor(dta$district)
            dta$state <- as.factor(dta$state)
            dta <- dta %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(district,state,estimate) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                rename('Population with Insurance' = estimate) 
              dta.per <- dta %>%
                select(district,state,pct) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                rename('Population with Insurance' = pct) 
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(district,state,estimate_reliable) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                rename('Population with Insurance' = estimate_reliable)  
              dta.per <- dta %>%
                select(district,state,pct_reliable) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                rename('Population with Insurance' = pct_reliable)  
            }
            nformat <- 0
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "National" & input$topic == "Limited English Proficiency"){
            ################################  National  LEP ##########################################
            dta <- dta.lep %>% 
              filter(geography=="national") %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            dta$NAME <- as.factor(dta$NAME)
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(NAME,label,estimate) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate)
              dta.per <- dta %>%
                select(NAME,label,pct) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(NAME,label,estimate_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(NAME,label,pct_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            nformat <- 1
            return(list(dta.est, dta.per, nformat))
            
          } else if(input$geo == "State" & input$topic == "Limited English Proficiency"){
            ################################  State  LEP ##########################################
            dta <- dta.lep %>% 
              filter(geography=="state") %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            dta$NAME <- as.factor(dta$NAME)
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(NAME,label,estimate) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate)
              dta.per <- dta %>%
                select(NAME,label,pct) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(NAME,label,estimate_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(NAME,label,pct_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            nformat <- 1
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "County" & input$topic == "Limited English Proficiency"){
            ################################  County  LEP ##########################################
            dta <- dta.lep %>% 
              filter(geography=="county") %>%
              separate(NAME, c("county", "state"), ", ") 
            dta$county <- as.factor(dta$county)
            dta$state <- as.factor(dta$state)
            dta <- dta %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))           
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(county,state,label,estimate) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = estimate)
              dta.per <- dta %>%
                select(county,state,label,pct) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(county,state,label,estimate_reliable) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(county,state,label,pct_reliable) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            nformat <- 1
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "Congressional District" & input$topic == "Limited English Proficiency"){
            ################################  Congressional District  LEP ##########################################
            dta <- dta.lep %>% 
              filter(geography=="district") %>% 
              separate(NAME, c("district", "state"), ", ")
            dta$district <- gsub("\\s*\\([0-9][^\\)]+\\)","",dta$district)
            dta$district <- as.factor(dta$district)
            dta$state <- as.factor(dta$state)
            dta <- dta %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(district,state,label,estimate) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = estimate)
              dta.per <- dta %>%
                select(district,state,label,pct) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(district,state,label,estimate_reliable) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(district,state,label,pct_reliable) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            nformat <- 1
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "National" & input$topic == "Nativity"){
            ################################  National  NATIVITY ##########################################
            dta <- dta.nat %>% 
              filter(geography=="national") %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            dta$NAME <- as.factor(dta$NAME)
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(NAME,estimate) %>% 
                rename('Geography' = NAME) %>% 
                rename('Foreign-born' = estimate)
              dta.per <- dta %>%
                select(NAME,pct_pop) %>% 
                rename('Geography' = NAME) %>% 
                rename('Foreign-born' = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(NAME,estimate_reliable) %>% 
                rename('Geography' = NAME) %>% 
                rename('Foreign-born' = estimate_reliable)  
              dta.per <- dta %>%
                select(NAME,pct_reliable) %>% 
                rename('Geography' = NAME) %>% 
                rename('Foreign-born' = pct_reliable)  
            }
            nformat <- 0
            return(list(dta.est, dta.per, nformat))
            
          } else if(input$geo == "State" & input$topic == "Nativity"){
            ################################  State  NATIVITY ##########################################
            dta <- dta.nat %>% 
              filter(geography=="state") %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            dta$NAME <- as.factor(dta$NAME)
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(NAME,estimate) %>% 
                rename('Geography' = NAME) %>% 
                rename('Foreign-born' = estimate)
              dta.per <- dta %>%
                select(NAME,pct) %>% 
                rename('Geography' = NAME) %>% 
                rename('Foreign-born' = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(NAME,estimate_reliable) %>% 
                rename('Geography' = NAME) %>% 
                rename('Foreign-born' = estimate_reliable)  
              dta.per <- dta %>%
                select(NAME,pct_reliable) %>% 
                rename('Geography' = NAME) %>% 
                rename('Foreign-born' = pct_reliable)  
            }
            nformat <- 0
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "County" & input$topic == "Nativity"){
            ################################  County  NATIVITY ##########################################
            dta <- dta.nat %>% 
              filter(geography=="county") %>%
              separate(NAME, c("county", "state"), ", ")
            dta$county <- as.factor(dta$county)
            dta$state <- as.factor(dta$state)
            dta <- dta %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(county,state,estimate) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                rename('Foreign-born' = estimate)
              dta.per <- dta %>%
                select(county,state,pct) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                rename('Foreign-born' = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(county,state,estimate_reliable) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                rename('Foreign-born' = estimate_reliable)  
              dta.per <- dta %>%
                select(county,state,pct_reliable) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                rename('Foreign-born' = pct_reliable)  
            }
            nformat <- 0
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "Congressional District" & input$topic == "Nativity"){
            ################################  Congressional District  NATIVITY ##########################################
            dta <- dta.nat %>% 
              filter(geography=="district") %>% 
              separate(NAME, c("district", "state"), ", ")
            dta$district <- gsub("\\s*\\([0-9][^\\)]+\\)","",dta$district)
            dta$district <- as.factor(dta$district)
            dta$state <- as.factor(dta$state)
            dta <- dta %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(district,state,estimate) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                rename('Foreign-born' = estimate)
              dta.per <- dta %>%
                select(district,state,pct) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                rename('Foreign-born' = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(district,state,estimate_reliable) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                rename('Foreign-born' = estimate_reliable) 
              dta.per <- dta %>%
                select(district,state,pct_reliable) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                rename('Foreign-born' = pct_reliable) 
            }
            nformat <- 0
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "National" & input$topic == "Population"){
            ################################  National  Population ##########################################
            dta <- dta.pop %>% 
              filter(geography=="national") 
            dta$NAME <- as.factor(dta$NAME)
            if(grepl("Detailed", input$group, fixed=T)){
              if(grepl("Combo", input$group, fixed=T)){
                dta <- dta %>%
                  filter(group == "Detailed AAPI Combo") %>%
                  filter(label %in% tags.aapic) %>%
                  arrange(factor(label, levels = tags.aapic))
                nformat <- length(tags.aapic)-1
              }else{
                dta <- dta %>%
                  filter(group == "Detailed AAPI Alone") %>%
                  filter(label %in% tags.aapi) %>%
                  arrange(factor(label, levels = tags.aapi))
                nformat <- length(tags.aapi)-1
              }
            }else{
              if(grepl("Combo", input$group, fixed=T)){
                dta <- dta %>%
                  filter(group == "AAPI Alone or in Combo")
                nformat <- 2
              }else{
                dta <- dta %>%
                  filter(group == "AAPI Alone")
                nformat <- 2
              }
            }
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(NAME,label,estimate) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate)
              dta.per <- dta %>%
                select(NAME,label,pct) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(NAME,label,estimate_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(NAME,label,pct_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "State" & input$topic == "Population"){
            ################################  State  Population ##########################################
            dta <- dta.pop %>% 
              filter(geography=="state")
            dta$NAME <- as.factor(dta$NAME)
            if(grepl("Detailed", input$group, fixed=T)){
              if(grepl("Combo", input$group, fixed=T)){
                dta <- dta %>%
                  filter(group == "Detailed AAPI Combo") %>%
                  filter(label %in% tags.aapic) %>%
                  arrange(factor(label, levels = tags.aapic))
                nformat <- length(tags.aapic)-1
              }else{
                dta <- dta %>%
                  filter(group == "Detailed AAPI Alone") %>%
                  filter(label %in% tags.aapi) %>%
                  arrange(factor(label, levels = tags.aapi))
                nformat <- length(tags.aapi)-1
              }
            }else{
              if(grepl("Combo", input$group, fixed=T)){
                dta <- dta %>%
                  filter(group == "AAPI Alone or in Combo")
                nformat <- 2
              }else{
                dta <- dta %>%
                  filter(group == "AAPI Alone")
                nformat <- 2
              }
            }
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(NAME,label,estimate) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate)
              dta.per <- dta %>%
                select(NAME,label,pct) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(NAME,label,estimate_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(NAME,label,pct_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "Congressional District" & input$topic == "Population"){
            ################################  Congressional District  Population ##########################################
            dta <- dta.pop %>% 
              filter(geography=="district") %>% 
              separate(NAME, c("district", "state"), ", ")
            dta$district <- gsub("\\s*\\([0-9][^\\)]+\\)","",dta$district)
            dta$district <- as.factor(dta$district)
            dta$state <- as.factor(dta$state)
            if(grepl("detailed", input$group, fixed=T)){
              if(grepl("combo", input$group, fixed=T)){
                dta <- dta %>%
                  filter(group == "Detailed AAPI Combo") %>%
                  filter(label %in% tags.aapic) %>%
                  arrange(factor(label, levels = tags.aapic))
                nformat <- length(tags.aapic)-1
              }else{
                dta <- dta %>%
                  filter(group == "Detailed AAPI Alone") %>%
                  filter(label %in% tags.aapi) %>%
                  arrange(factor(label, levels = tags.aapi))
                nformat <- length(tags.aapi)-1
              }
            }else{
              if(grepl("combo", input$group, fixed=T)){
                dta <- dta %>%
                  filter(group == "AAPI Alone or in Combo")
                nformat <- 2
              }else{
                dta <- dta %>%
                  filter(group == "AAPI Alone")
                nformat <- 2
              }
            }
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(district,state,label,estimate) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = estimate)
              dta.per <- dta %>%
                select(district,state,label,pct) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(district,state,label,estimate_reliable) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(district,state,label,pct_reliable) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "County" & input$topic == "Population"){
            ################################  County  Population ##########################################
            dta <- dta.pop %>% 
              filter(geography=="county") %>%
              separate(NAME, c("county", "state"), ", ") 
            dta$county <- as.factor(dta$county)
            dta$state <- as.factor(dta$state)
            if(grepl("detailed", input$group, fixed=T)){
              if(grepl("combo", input$group, fixed=T)){
                dta <- dta %>%
                  filter(group == "Detailed AAPI Combo") %>%
                  filter(label %in% tags.aapic) %>%
                  arrange(factor(label, levels = tags.aapic))
                nformat <- length(tags.aapic)-1
              }else{
                dta <- dta %>%
                  filter(group == "Detailed AAPI Alone") %>%
                  filter(label %in% tags.aapi) %>%
                  arrange(factor(label, levels = tags.aapi))
                nformat <- length(tags.aapi)-1
              }
            }else{
              if(grepl("combo", input$group, fixed=T)){
                dta <- dta %>%
                  filter(group == "AAPI Alone or in Combo")
                nformat <- 2
              }else{
                dta <- dta %>%
                  filter(group == "AAPI Alone")
                nformat <- 2
              }
            }
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(county,state,label,estimate) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = estimate)
              dta.per <- dta %>%
                select(county,state,label,pct) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(county,state,label,estimate_reliable) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(county,state,label,pct_reliable) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "Metro Area" & input$topic == "Population"){
            ################################  Metro Area  Population ##########################################
            dta <- dta.pop %>% 
              filter(geography=="metro") 
            dta$NAME <- as.factor(dta$NAME)
            if(grepl("Detailed", input$group, fixed=T)){
              if(grepl("combo", input$group, fixed=T)){
                dta <- dta %>%
                  filter(group == "Detailed AAPI Combo") %>%
                  filter(label %in% tags.aapic) %>%
                  arrange(factor(label, levels = tags.aapic))
                nformat <- length(tags.aapic)-1
              }else{
                dta <- dta %>%
                  filter(group == "Detailed AAPI Alone") %>%
                  filter(label %in% tags.aapi) %>%
                  arrange(factor(label, levels = tags.aapi))
                nformat <- length(tags.aapi)-1
              }
            }else{
              if(grepl("combo", input$group, fixed=T)){
                dta <- dta %>%
                  filter(group == "AAPI Alone or in Combo")
                nformat <- 2
              }else{
                dta <- dta %>%
                  filter(group == "AAPI Alone")
                nformat <- 2
              }
            }
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(NAME,label,estimate) %>% 
                rename('Metro Area' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate) 
              dta.per <- dta %>%
                select(NAME,label,pct) %>% 
                rename('Metro Area' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(NAME,label,estimate_reliable) %>% 
                rename('Metro Area' = NAME) %>%
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(NAME,label,pct_reliable) %>% 
                rename('Metro Area' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "National" & input$topic == "Poverty"){
            ################################  National  POVERTY ##########################################
            dta <- dta.pov %>% 
              filter(geography=="national") %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            dta$NAME <- as.factor(dta$NAME)
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(NAME,estimate) %>% 
                rename('Geography' = NAME) %>% 
                rename('Below Poverty' = estimate)
              dta.per <- dta %>%
                select(NAME,pct) %>% 
                rename('Geography' = NAME) %>% 
                rename('Below Poverty' = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(NAME,estimate_reliable) %>% 
                rename('Geography' = NAME) %>% 
                rename('Below Poverty' = estimate_reliable)  
              dta.per <- dta %>%
                select(NAME,pct_reliable) %>% 
                rename('Geography' = NAME) %>% 
                rename('Below Poverty' = pct_reliable)  
            }
            nformat <- 0
            return(list(dta.est, dta.per, nformat))
            
          } else if(input$geo == "State" & input$topic == "Poverty"){
            ################################  State  POVERTY ##########################################
            dta <- dta.pov %>% 
              filter(geography=="state") %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            dta$NAME <- as.factor(dta$NAME)
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(NAME,estimate) %>% 
                rename('Geography' = NAME) %>% 
                rename('Below Poverty' = estimate)
              dta.per <- dta %>%
                select(NAME,pct) %>% 
                rename('Geography' = NAME) %>% 
                rename('Below Poverty' = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(NAME,estimate_reliable) %>% 
                rename('Geography' = NAME) %>% 
                rename('Below Poverty' = estimate_reliable)  
              dta.per <- dta %>%
                select(NAME,pct_reliable) %>% 
                rename('Geography' = NAME) %>% 
                rename('Below Poverty' = pct_reliable)  
            }
            nformat <- 0
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "County" & input$topic == "Poverty"){
            ################################  County  POVERTY ##########################################
            dta <- dta.pov %>% 
              filter(geography=="county") %>%
              separate(NAME, c("county", "state"), ", ") 
            dta$county <- as.factor(dta$county)
            dta$state <- as.factor(dta$state)
            dta <- dta %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(county,state,estimate) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                rename('Below Poverty' = estimate)
              dta.per <- dta %>%
                select(county,state,pct) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                rename('Below Poverty' = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(county,state,estimate_reliable) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                rename('Below Poverty' = estimate_reliable)  
              dta.per <- dta %>%
                select(county,state,pct_reliable) %>% 
                rename('County' = county) %>% 
                rename('State' = state) %>%
                rename('Below Poverty' = pct_reliable)  
            }
            nformat <- 0
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "Congressional District" & input$topic == "Poverty"){
            ################################  Congressional District  POVERTY ##########################################
            dta <- dta.pov %>% 
              filter(geography=="district") %>% 
              separate(NAME, c("district", "state"), ", ")
            dta$district <- gsub("\\s*\\([0-9][^\\)]+\\)","",dta$district)
            dta$district <- as.factor(dta$district)
            dta$state <- as.factor(dta$state)
            dta <- dta %>%
              filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                               TRUE ~ group == "NHPI Alone"))
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(district,state,estimate) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                rename('Below Poverty' = estimate)
              dta.per <- dta %>%
                select(district,state,pct) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                rename('Below Poverty' = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(district,state,estimate_reliable) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                rename('Below Poverty' = estimate_reliable) 
              dta.per <- dta %>%
                select(district,state,pct_reliable) %>% 
                rename('District' = district) %>% 
                rename('State' = state) %>%
                rename('Below Poverty' = pct_reliable) 
            }
            nformat <- 0
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "National" & input$topic == "Undocumented"){
            ################################  National  Undocumented ##########################################
            dta <- dta.und %>% 
              filter(geography=="national") 
            dta$NAME <- as.factor(dta$NAME)
            if(grepl("Detailed", input$group, fixed=T)){
              dta <- dta %>%
                filter(group == "Detailed Asian Am Alone") %>%
                filter(label %in% input$detailed_filter) %>%
                arrange(factor(label, levels = input$detailed_filter))
              nformat <- length(input$detailed_filter)-1
            }else{
              dta <- dta %>%
                filter(group == "Asian American Alone")
              nformat <- 1
            }
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(NAME,label,estimate) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate)
              dta.per <- dta %>%
                select(NAME,label,pct) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(NAME,label,estimate_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(NAME,label,pct_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            return(list(dta.est, dta.per, nformat))
            
          }else if(input$geo == "State" & input$topic == "Undocumented"){
            ################################  State  Undocumented ##########################################
            dta <- dta.und %>% 
              filter(geography=="state")
            dta$NAME <- as.factor(dta$NAME)
            if(grepl("Detailed", input$group, fixed=T)){
              dta <- dta %>%
                filter(group == "Detailed Asian Am Alone") %>%
                filter(label %in% input$detailed_filter) %>%
                arrange(factor(label, levels = input$detailed_filter))
              nformat <- length(input$detailed_filter)-1
            }else{
              dta <- dta %>%
                filter(group == "Asian American Alone")
              nformat <- 0
            }
            if(input$reliable == FALSE){
              dta.est <- dta %>%
                select(NAME,label,estimate) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate)
              dta.per <- dta %>%
                select(NAME,label,pct) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct)
            }else if(input$reliable == TRUE){
              dta.est <- dta %>%
                select(NAME,label,estimate_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = estimate_reliable)
              dta.per <- dta %>%
                select(NAME,label,pct_reliable) %>% 
                rename('Geography' = NAME) %>% 
                pivot_wider(names_from = label, values_from = pct_reliable)
            }
            return(list(dta.est, dta.per, nformat))
          }
          # END of select tables
        }
      }else{
        
        
        
        ######################################  BLOCK 2  ###########################################################
        ############################################################################################################
        if (input$group=="" | input$topic=="" | input$geo==""){
          return(NULL)
          }else{
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
              tags.aapic <- c("Asian American", "NHPI", input$detailed_filter)
            }else if(sum(input$detailed_filter %in% daa)/length(input$detailed_filter) == 1){
              tags.aapic <- c("Asian American", input$detailed_filter)
            }
            
            #get data for each selection
            if(input$geo == "National" & input$topic == "Citizen Voting Age Population"){
              ################################  National  CVAP ##########################################
              dta <- dta.cvap %>% 
                filter(geography=="national") %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              dta$NAME <- as.factor(dta$NAME)
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(NAME,label,estimate) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate)
                dta.per <- dta %>%
                  select(NAME,label,pct) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(NAME,label,estimate_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(NAME,label,pct_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              nformat <- 0
              return(list(dta.est, dta.per, nformat))
              
            } else if(input$geo == "State" & input$topic == "Citizen Voting Age Population"){
              ################################  National  CVAP ##########################################
              dta <- dta.cvap %>% 
                filter(geography=="state") %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              dta$NAME <- as.factor(dta$NAME)
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(NAME,label,estimate) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate)
                dta.per <- dta %>%
                  select(NAME,label,pct) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(NAME,label,estimate_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(NAME,label,pct_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              nformat <- 0
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "County" & input$topic == "Citizen Voting Age Population"){
              ################################  County  CVAP ##########################################
              dta <- dta.cvap %>% 
                filter(geography=="county") %>%
                separate(NAME, c("county", "state"), ", ") 
              dta$county <- as.factor(dta$county)
              dta$state <- as.factor(dta$state)
              dta <- dta %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(county,state,label,estimate) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = estimate)
                dta.per <- dta %>%
                  select(county,state,label,pct) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(county,state,label,estimate_reliable) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(county,state,label,pct_reliable) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              nformat <- 0
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "Congressional District" & input$topic == "Citizen Voting Age Population"){
              ################################  National  CVAP ##########################################
              dta <- dta.cvap %>% 
                filter(geography=="district") %>% 
                separate(NAME, c("district", "state"), ", ") 
              dta$district <- gsub("\\s*\\([0-9][^\\)]+\\)","",dta$district)
              dta$district <- as.factor(dta$district)
              dta$state <- as.factor(dta$state)
              dta <- dta %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(district,state,label,estimate) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = estimate)
                dta.per <- dta %>%
                  select(district,state,label,pct) %>%
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(district,state,label,estimate_reliable) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(district,state,label,pct_reliable) %>% 
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              nformat <- 0
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "National" & input$topic == "Education"){
              ################################  National  EDUCATION ##########################################
              dta <- dta.edu %>% 
                filter(geography=="national") %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              dta$NAME <- as.factor(dta$NAME)
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(NAME,label,estimate) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate)
                dta.per <- dta %>%
                  select(NAME,label,pct) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(NAME,label,estimate_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(NAME,label,pct_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              nformat <- 3
              return(list(dta.est, dta.per, nformat))
              
            } else if(input$geo == "State" & input$topic == "Education"){
              ################################  State  EDUCATION ##########################################
              dta <- dta.edu %>% 
                filter(geography=="state") %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              dta$NAME <- as.factor(dta$NAME)
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(NAME,label,estimate) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate)
                dta.per <- dta %>%
                  select(NAME,label,pct) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(NAME,label,estimate_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(NAME,label,pct_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              nformat <- 3
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "County" & input$topic == "Education"){
              ################################  County  EDUCATION ##########################################
              dta <- dta.edu %>% 
                filter(geography=="county") %>%
                separate(NAME, c("county", "state"), ", ") 
              dta$county <- as.factor(dta$county)
              dta$state <- as.factor(dta$state)
              dta <- dta %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(county,state,label,estimate) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = estimate)
                dta.per <- dta %>%
                  select(county,state,label,pct) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(county,state,label,estimate_reliable) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(county,state,label,pct_reliable) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              nformat <- 3
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "Congressional District" & input$topic == "Education"){
              ################################  Congressional District  EDUCATION ##########################################
              dta <- dta.edu %>% 
                filter(geography=="district") %>% 
                separate(NAME, c("district", "state"), ", ")
              dta$district <- gsub("\\s*\\([0-9][^\\)]+\\)","",dta$district)
              dta$district <- as.factor(dta$district)
              dta$state <- as.factor(dta$state)
              dta <- dta %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(district,state,label,estimate) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = estimate)
                dta.per <- dta %>%
                  select(district,state,label,pct) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(district,state,label,estimate_reliable) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(district,state,label,pct_reliable) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              nformat <- 3
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "National" & input$topic == "Health Insurance"){
              ################################  National  HEALTH INSURANCE ##########################################
              dta <- dta.ins %>% 
                filter(geography=="national") %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              dta$NAME <- as.factor(dta$NAME)
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(NAME,estimate) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Population with Insurance' = estimate) 
                dta.per <- dta %>%
                  select(NAME,pct) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Population with Insurance' = pct) 
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(NAME,estimate_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Population with Insurance' = estimate_reliable)   
                dta.per <- dta %>%
                  select(NAME,pct_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Population with Insurance' = pct_reliable)   
              }
              nformat <- 0
              return(list(dta.est, dta.per, nformat))
              
            } else if(input$geo == "State" & input$topic == "Health Insurance"){
              ################################  State  HEALTH INSURANCE ##########################################
              dta <- dta.ins %>% 
                filter(geography=="state") %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              dta$NAME <- as.factor(dta$NAME)
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(NAME,estimate) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Population with Insurance' = estimate) 
                dta.per <- dta %>%
                  select(NAME,pct) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Population with Insurance' = pct) 
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(NAME,estimate_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Population with Insurance' = estimate_reliable)   
                dta.per <- dta %>%
                  select(NAME,pct_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Population with Insurance' = pct_reliable)   
              }
              nformat <- 0
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "County" & input$topic == "Health Insurance"){
              ################################  County  HEALTH INSURANCE ##########################################
              dta <- dta.ins %>% 
                filter(geography=="county") %>%
                separate(NAME, c("county", "state"), ", ") 
              dta$county <- as.factor(dta$county)
              dta$state <- as.factor(dta$state)
              dta <- dta %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(county,state,estimate) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  rename('Population with Insurance' = estimate) 
                dta.per <- dta %>%
                  select(county,state,pct) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  rename('Population with Insurance' = pct) 
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(county,state,estimate_reliable) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  rename('Population with Insurance' = estimate_reliable)   
                dta.per <- dta %>%
                  select(county,state,pct_reliable) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  rename('Population with Insurance' = pct_reliable)   
              }
              nformat <- 0
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "Congressional District" & input$topic == "Health Insurance"){
              ################################  Congressional District  HEALTH INSURANCE ##########################################
              dta <- dta.ins %>% 
                filter(geography=="district") %>% 
                separate(NAME, c("district", "state"), ", ")
              dta$district <- gsub("\\s*\\([0-9][^\\)]+\\)","",dta$district)
              dta$district <- as.factor(dta$district)
              dta$state <- as.factor(dta$state)
              dta <- dta %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(district,state,estimate) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  rename('Population with Insurance' = estimate) 
                dta.per <- dta %>%
                  select(district,state,pct) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  rename('Population with Insurance' = pct) 
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(district,state,estimate_reliable) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  rename('Population with Insurance' = estimate_reliable)  
                dta.per <- dta %>%
                  select(district,state,pct_reliable) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  rename('Population with Insurance' = pct_reliable)  
              }
              nformat <- 0
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "National" & input$topic == "Limited English Proficiency"){
              ################################  National  LEP ##########################################
              dta <- dta.lep %>% 
                filter(geography=="national") %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              dta$NAME <- as.factor(dta$NAME)
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(NAME,label,estimate) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate)
                dta.per <- dta %>%
                  select(NAME,label,pct) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(NAME,label,estimate_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(NAME,label,pct_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              nformat <- 1
              return(list(dta.est, dta.per, nformat))
              
            } else if(input$geo == "State" & input$topic == "Limited English Proficiency"){
              ################################  State  LEP ##########################################
              dta <- dta.lep %>% 
                filter(geography=="state") %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              dta$NAME <- as.factor(dta$NAME)
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(NAME,label,estimate) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate)
                dta.per <- dta %>%
                  select(NAME,label,pct) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(NAME,label,estimate_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(NAME,label,pct_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              nformat <- 1
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "County" & input$topic == "Limited English Proficiency"){
              ################################  County  LEP ##########################################
              dta <- dta.lep %>% 
                filter(geography=="county") %>%
                separate(NAME, c("county", "state"), ", ") 
              dta$county <- as.factor(dta$county)
              dta$state <- as.factor(dta$state)
              dta <- dta %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))           
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(county,state,label,estimate) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = estimate)
                dta.per <- dta %>%
                  select(county,state,label,pct) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(county,state,label,estimate_reliable) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(county,state,label,pct_reliable) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              nformat <- 1
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "Congressional District" & input$topic == "Limited English Proficiency"){
              ################################  Congressional District  LEP ##########################################
              dta <- dta.lep %>% 
                filter(geography=="district") %>% 
                separate(NAME, c("district", "state"), ", ")
              dta$district <- gsub("\\s*\\([0-9][^\\)]+\\)","",dta$district)
              dta$district <- as.factor(dta$district)
              dta$state <- as.factor(dta$state)
              dta <- dta %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(district,state,label,estimate) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = estimate)
                dta.per <- dta %>%
                  select(district,state,label,pct) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(district,state,label,estimate_reliable) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(district,state,label,pct_reliable) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              nformat <- 1
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "National" & input$topic == "Nativity"){
              ################################  National  NATIVITY ##########################################
              dta <- dta.nat %>% 
                filter(geography=="national") %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              dta$NAME <- as.factor(dta$NAME)
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(NAME,estimate) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Foreign-born' = estimate)
                dta.per <- dta %>%
                  select(NAME,pct_pop) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Foreign-born' = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(NAME,estimate_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Foreign-born' = estimate_reliable)  
                dta.per <- dta %>%
                  select(NAME,pct_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Foreign-born' = pct_reliable)  
              }
              nformat <- 0
              return(list(dta.est, dta.per, nformat))
              
            } else if(input$geo == "State" & input$topic == "Nativity"){
              ################################  State  NATIVITY ##########################################
              dta <- dta.nat %>% 
                filter(geography=="state") %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              dta$NAME <- as.factor(dta$NAME)
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(NAME,estimate) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Foreign-born' = estimate)
                dta.per <- dta %>%
                  select(NAME,pct) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Foreign-born' = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(NAME,estimate_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Foreign-born' = estimate_reliable)  
                dta.per <- dta %>%
                  select(NAME,pct_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Foreign-born' = pct_reliable)  
              }
              nformat <- 0
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "County" & input$topic == "Nativity"){
              ################################  County  NATIVITY ##########################################
              dta <- dta.nat %>% 
                filter(geography=="county") %>%
                separate(NAME, c("county", "state"), ", ")
              dta$county <- as.factor(dta$county)
              dta$state <- as.factor(dta$state)
              dta <- dta %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(county,state,estimate) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  rename('Foreign-born' = estimate)
                dta.per <- dta %>%
                  select(county,state,pct) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  rename('Foreign-born' = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(county,state,estimate_reliable) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  rename('Foreign-born' = estimate_reliable)  
                dta.per <- dta %>%
                  select(county,state,pct_reliable) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  rename('Foreign-born' = pct_reliable)  
              }
              nformat <- 0
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "Congressional District" & input$topic == "Nativity"){
              ################################  Congressional District  NATIVITY ##########################################
              dta <- dta.nat %>% 
                filter(geography=="district") %>% 
                separate(NAME, c("district", "state"), ", ")
              dta$district <- gsub("\\s*\\([0-9][^\\)]+\\)","",dta$district)
              dta$district <- as.factor(dta$district)
              dta$state <- as.factor(dta$state)
              dta <- dta %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(district,state,estimate) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  rename('Foreign-born' = estimate)
                dta.per <- dta %>%
                  select(district,state,pct) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  rename('Foreign-born' = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(district,state,estimate_reliable) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  rename('Foreign-born' = estimate_reliable) 
                dta.per <- dta %>%
                  select(district,state,pct_reliable) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  rename('Foreign-born' = pct_reliable) 
              }
              nformat <- 0
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "National" & input$topic == "Population"){
              ################################  National  Population ##########################################
              dta <- dta.pop %>% 
                filter(geography=="national") 
              dta$NAME <- as.factor(dta$NAME)
              if(grepl("Detailed", input$group, fixed=T)){
                if(grepl("Combo", input$group, fixed=T)){
                  dta <- dta %>%
                    filter(group == "Detailed AAPI Combo") %>%
                    filter(label %in% tags.aapic) %>%
                    arrange(factor(label, levels = tags.aapic))
                  nformat <- length(tags.aapic)-1
                }else{
                  dta <- dta %>%
                    filter(group == "Detailed AAPI Alone") %>%
                    filter(label %in% tags.aapi) %>%
                    arrange(factor(label, levels = tags.aapi))
                  nformat <- length(tags.aapi)-1
                }
              }else{
                if(grepl("Combo", input$group, fixed=T)){
                  dta <- dta %>%
                    filter(group == "AAPI Alone or in Combo")
                  nformat <- 2
                }else{
                  dta <- dta %>%
                    filter(group == "AAPI Alone")
                  nformat <- 2
                }
              }
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(NAME,label,estimate) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate)
                dta.per <- dta %>%
                  select(NAME,label,pct) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(NAME,label,estimate_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(NAME,label,pct_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "State" & input$topic == "Population"){
              ################################  State  Population ##########################################
              dta <- dta.pop %>% 
                filter(geography=="state")
              dta$NAME <- as.factor(dta$NAME)
              if(grepl("Detailed", input$group, fixed=T)){
                if(grepl("Combo", input$group, fixed=T)){
                  dta <- dta %>%
                    filter(group == "Detailed AAPI Combo") %>%
                    filter(label %in% tags.aapic) %>%
                    arrange(factor(label, levels = tags.aapic))
                  nformat <- length(tags.aapic)-1
                }else{
                  dta <- dta %>%
                    filter(group == "Detailed AAPI Alone") %>%
                    filter(label %in% tags.aapi) %>%
                    arrange(factor(label, levels = tags.aapi))
                  nformat <- length(tags.aapi)-1
                }
              }else{
                if(grepl("Combo", input$group, fixed=T)){
                  dta <- dta %>%
                    filter(group == "AAPI Alone or in Combo")
                  nformat <- 2
                }else{
                  dta <- dta %>%
                    filter(group == "AAPI Alone")
                  nformat <- 2
                }
              }
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(NAME,label,estimate) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate)
                dta.per <- dta %>%
                  select(NAME,label,pct) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(NAME,label,estimate_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(NAME,label,pct_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "Congressional District" & input$topic == "Population"){
              ################################  Congressional District  Population ##########################################
              dta <- dta.pop %>% 
                filter(geography=="district") %>% 
                separate(NAME, c("district", "state"), ", ")
              dta$district <- gsub("\\s*\\([0-9][^\\)]+\\)","",dta$district)
              dta$district <- as.factor(dta$district)
              dta$state <- as.factor(dta$state)
              if(grepl("Detailed", input$group, fixed=T)){
                if(grepl("combo", input$group, fixed=T)){
                  dta <- dta %>%
                    filter(group == "Detailed AAPI Combo") %>%
                    filter(label %in% tags.aapic) %>%
                    arrange(factor(label, levels = tags.aapic))
                  nformat <- length(tags.aapic)-1
                }else{
                  dta <- dta %>%
                    filter(group == "Detailed AAPI Alone") %>%
                    filter(label %in% tags.aapi) %>%
                    arrange(factor(label, levels = tags.aapi))
                  nformat <- length(tags.aapi)-1
                }
              }else{
                if(grepl("combo", input$group, fixed=T)){
                  dta <- dta %>%
                    filter(group == "AAPI Alone or in Combo")
                  nformat <- 2
                }else{
                  dta <- dta %>%
                    filter(group == "AAPI Alone")
                  nformat <- 2
                }
              }
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(district,state,label,estimate) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = estimate)
                dta.per <- dta %>%
                  select(district,state,label,pct) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(district,state,label,estimate_reliable) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(district,state,label,pct_reliable) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "County" & input$topic == "Population"){
              ################################  County  Population ##########################################
              dta <- dta.pop %>% 
                filter(geography=="county") %>%
                separate(NAME, c("county", "state"), ", ") 
              dta$county <- as.factor(dta$county)
              dta$state <- as.factor(dta$state)
              if(grepl("Detailed", input$group, fixed=T)){
                if(grepl("combo", input$group, fixed=T)){
                  dta <- dta %>%
                    filter(group == "Detailed AAPI Combo") %>%
                    filter(label %in% tags.aapic) %>%
                    arrange(factor(label, levels = tags.aapic))
                  nformat <- length(tags.aapic)-1
                }else{
                  dta <- dta %>%
                    filter(group == "Detailed AAPI Alone") %>%
                    filter(label %in% tags.aapi) %>%
                    arrange(factor(label, levels = tags.aapi))
                  nformat <- length(tags.aapi)-1
                }
              }else{
                if(grepl("combo", input$group, fixed=T)){
                  dta <- dta %>%
                    filter(group == "AAPI Alone or in Combo")
                  nformat <- 2
                }else{
                  dta <- dta %>%
                    filter(group == "AAPI Alone")
                  nformat <- 2
                }
              }
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(county,state,label,estimate) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = estimate)
                dta.per <- dta %>%
                  select(county,state,label,pct) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(county,state,label,estimate_reliable) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(county,state,label,pct_reliable) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "Metro Area" & input$topic == "Population"){
              ################################  Metro Area  Population ##########################################
              dta <- dta.pop %>% 
                filter(geography=="metro") 
              dta$NAME <- as.factor(dta$NAME)
              if(grepl("Detailed", input$group, fixed=T)){
                if(grepl("combo", input$group, fixed=T)){
                  dta <- dta %>%
                    filter(group == "Detailed AAPI Combo") %>%
                    filter(label %in% tags.aapic) %>%
                    arrange(factor(label, levels = tags.aapic))
                  nformat <- length(tags.aapic)-1
                }else{
                  dta <- dta %>%
                    filter(group == "Detailed AAPI Alone") %>%
                    filter(label %in% tags.aapi) %>%
                    arrange(factor(label, levels = tags.aapi))
                  nformat <- length(tags.aapi)-1
                }
              }else{
                if(grepl("combo", input$group, fixed=T)){
                  dta <- dta %>%
                    filter(group == "AAPI Alone or in Combo")
                  nformat <- 2
                }else{
                  dta <- dta %>%
                    filter(group == "AAPI Alone")
                  nformat <- 2
                }
              }
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(NAME,label,estimate) %>% 
                  rename('Metro Area' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate) 
                dta.per <- dta %>%
                  select(NAME,label,pct) %>% 
                  rename('Metro Area' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(NAME,label,estimate_reliable) %>% 
                  rename('Metro Area' = NAME) %>%
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(NAME,label,pct_reliable) %>% 
                  rename('Metro Area' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "National" & input$topic == "Poverty"){
              ################################  National  POVERTY ##########################################
              dta <- dta.pov %>% 
                filter(geography=="national") %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              dta$NAME <- as.factor(dta$NAME)
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(NAME,estimate) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Below Poverty' = estimate)
                dta.per <- dta %>%
                  select(NAME,pct) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Below Poverty' = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(NAME,estimate_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Below Poverty' = estimate_reliable)  
                dta.per <- dta %>%
                  select(NAME,pct_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Below Poverty' = pct_reliable)  
              }
              nformat <- 0
              return(list(dta.est, dta.per, nformat))
              
            } else if(input$geo == "State" & input$topic == "Poverty"){
              ################################  State  POVERTY ##########################################
              dta <- dta.pov %>% 
                filter(geography=="state") %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              dta$NAME <- as.factor(dta$NAME)
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(NAME,estimate) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Below Poverty' = estimate)
                dta.per <- dta %>%
                  select(NAME,pct) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Below Poverty' = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(NAME,estimate_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Below Poverty' = estimate_reliable)  
                dta.per <- dta %>%
                  select(NAME,pct_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  rename('Below Poverty' = pct_reliable)  
              }
              nformat <- 0
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "County" & input$topic == "Poverty"){
              ################################  County  POVERTY ##########################################
              dta <- dta.pov %>% 
                filter(geography=="county") %>%
                separate(NAME, c("county", "state"), ", ") 
              dta$county <- as.factor(dta$county)
              dta$state <- as.factor(dta$state)
              dta <- dta %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(county,state,estimate) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  rename('Below Poverty' = estimate)
                dta.per <- dta %>%
                  select(county,state,pct) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  rename('Below Poverty' = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(county,state,estimate_reliable) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  rename('Below Poverty' = estimate_reliable)  
                dta.per <- dta %>%
                  select(county,state,pct_reliable) %>% 
                  rename('County' = county) %>% 
                  rename('State' = state) %>%
                  rename('Below Poverty' = pct_reliable)  
              }
              nformat <- 0
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "Congressional District" & input$topic == "Poverty"){
              ################################  Congressional District  POVERTY ##########################################
              dta <- dta.pov %>% 
                filter(geography=="district") %>% 
                separate(NAME, c("district", "state"), ", ")
              dta$district <- gsub("\\s*\\([0-9][^\\)]+\\)","",dta$district)
              dta$district <- as.factor(dta$district)
              dta$state <- as.factor(dta$state)
              dta <- dta %>%
                filter(case_when(input$group == "Asian American Alone" ~ group == "Asian American Alone",
                                 TRUE ~ group == "NHPI Alone"))
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(district,state,estimate) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  rename('Below Poverty' = estimate)
                dta.per <- dta %>%
                  select(district,state,pct) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  rename('Below Poverty' = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(district,state,estimate_reliable) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  rename('Below Poverty' = estimate_reliable) 
                dta.per <- dta %>%
                  select(district,state,pct_reliable) %>% 
                  rename('District' = district) %>% 
                  rename('State' = state) %>%
                  rename('Below Poverty' = pct_reliable) 
              }
              nformat <- 0
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "National" & input$topic == "Undocumented"){
              ################################  National  Undocumented ##########################################
              dta <- dta.und %>% 
                filter(geography=="national") 
              dta$NAME <- as.factor(dta$NAME)
              if(grepl("Detailed", input$group, fixed=T)){
                dta <- dta %>%
                  filter(group == "Detailed Asian Am Alone") %>%
                  filter(label %in% input$detailed_filter) %>%
                  arrange(factor(label, levels = input$detailed_filter))
                nformat <- length(input$detailed_filter)-1
              }else{
                dta <- dta %>%
                  filter(group == "Asian American Alone")
                nformat <- 1
              }
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(NAME,label,estimate) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate)
                dta.per <- dta %>%
                  select(NAME,label,pct) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(NAME,label,estimate_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(NAME,label,pct_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              return(list(dta.est, dta.per, nformat))
              
            }else if(input$geo == "State" & input$topic == "Undocumented"){
              ################################  State  Undocumented ##########################################
              dta <- dta.und %>% 
                filter(geography=="state")
              dta$NAME <- as.factor(dta$NAME)
              if(grepl("Detailed", input$group, fixed=T)){
                dta <- dta %>%
                  filter(group == "Detailed Asian Am Alone") %>%
                  filter(label %in% input$detailed_filter) %>%
                  arrange(factor(label, levels = input$detailed_filter))
                nformat <- length(input$detailed_filter)-1
              }else{
                dta <- dta %>%
                  filter(group == "Asian American Alone")
                nformat <- 0
              }
              if(input$reliable == FALSE){
                dta.est <- dta %>%
                  select(NAME,label,estimate) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate)
                dta.per <- dta %>%
                  select(NAME,label,pct) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct)
              }else if(input$reliable == TRUE){
                dta.est <- dta %>%
                  select(NAME,label,estimate_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = estimate_reliable)
                dta.per <- dta %>%
                  select(NAME,label,pct_reliable) %>% 
                  rename('Geography' = NAME) %>% 
                  pivot_wider(names_from = label, values_from = pct_reliable)
              }
              return(list(dta.est, dta.per, nformat))
            }
            # END of select tables
          }
      }
    }) # END of observeevnet 
    
    # output$selected_topic <- renderText({ 
    #   # glue::glue("Greetings Human! It looks like you want to look at {input$topic}, for {input$group} at the {input$geo}")
    #   paste("DEBUGGING: Your geo is: ", input$geo," your group is : ",input$race, " Your TOPIC IS: ", input$topic, "topic type: ", input$topic_type)
    # })
    
    js <- c(
      "function(settings){",
      "  var datatable = settings.oInstance.api();",
      "  var table = datatable.table().node();",
      "  var caption = '*data source: 2018 ACS 5-year'",
      "  $(table).append('<caption style=\"caption-side: bottom\">' + caption + '</caption>');",
      "}"
    )
    
    output$preview <- DT::renderDataTable({
      DT::datatable(dta_load()[[1]],
                    rownames = FALSE,
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top; text-align: center; color:#EFA875; font-size:200%;',
                      input$topic),
                    extensions = 'Buttons',
                    options = list(
                      drawCallback = JS(js), dom = 'Btp', pageLength = 25, 
                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
                    filter = 'top') %>%
        formatCurrency((ncol(dta_load()[[1]])-dta_load()[[3]]):ncol(dta_load()[[1]]),currency = "", interval = 3, mark = ",", digits = 0) 
    })
    
    output$percentage <- DT::renderDataTable({
      DT::datatable(dta_load()[[2]],
                    rownames = FALSE,
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top; text-align: center; color:#EFA875; font-size:200%;',
                      input$topic
                    ),
                    extensions = 'Buttons',
                    options = list(
                      drawCallback = JS(js), dom = 'Btp', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                      ),
                    filter = 'top') %>%
        formatPercentage((ncol(dta_load()[[1]])-dta_load()[[3]]):ncol(dta_load()[[1]]), 1) 
    })

  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      # tempReport <- file.path(tempdir(), "report.Rmd")
      # file.copy("report.Rmd", tempReport, overwrite = TRUE)
      src <- normalizePath('report.Rmd')
      src2 <- normalizePath('aapidata.png') #NEW 
      owd <- setwd(tempdir())
      on.exit(setwd(owd)) 
      file.copy(src, 'report.Rmd')
      file.copy(src2, 'aapidata.png')
      
      my.dt <- dta_show()
      my.source<- source_show()
      my.states <-top_states_show()
      runstate<- runstate()
      params <- list(Group = input$group,
                     Topics = input$topic,
                     dataset= my.dt,
                     source = my.source,
                     state_pop= my.states,
                     runst = runstate)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render('report.Rmd', output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
      # rmarkdown::render(tempReport, output_file = file,
      #                                     params = params,
      #                                     envir = new.env(parent = globalenv())                  
      # )
    }
  )
  
  })
<<<<<<< HEAD
})
=======
  
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
>>>>>>> 4900d5dff4e18c87a0ee11605b2fcd3eda6ab253
