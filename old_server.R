#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidycensus)
library(tidyverse)
census_api_key("4e9d7fd959555208210856aaa5061b593c3722af")
v16 <- load_variables(2016, "acs5", cache = TRUE)

source("universal_formatR.r")

# Defining some functions, maybe later we can just have these as outside files and just call them in the server.r script
# get_estimates <- function(tablename,geo){
#   # Pull raw estimate
#   tbl <- get_acs(table = tablename, geography = geo)
# }



shinyServer(function(input, output) {
selected <- reactiveValues()
observe({
  selected$topic <- input$topics
  selected$topic2 <- input$topics2
  selected$geo <- input$geo
  selected$estimate_type <- input$estimate_type
  selected$race <- input$race
  selected$pop_type <- input$pop_type
})


observeEvent(input$goButton,{

final_query <- reactive({
  if(selected$estimate_type == "Outcomes"){
    final_query <- paste(selected$topic2,selected$race,sep="")
    # final_query <- "THIS IS not updating"
  }else {
    final_query <- paste("You selected topic: ",selected$topic)
    if (selected$topic == "pop_by_race") {
      if (selected$pop_type == "alone") {
        final_query <- "alone"
      } else {
        final_query <- "combo"
      }
    } else {
      final_query <- selected$topic
    }
  }
}) 

acs <- reactive({
    acs <- universal_formatter(tablename = final_query(),
                               geo = as.character(input$geo),
                               year= 2016)
})

output$info <- renderText({paste("Pulling data from Census Table: ",final_query())})

output$acs <- DT::renderDataTable({
  DT::datatable(acs(),
                options = list(paging = FALSE,dom="t"),
                rownames= FALSE) # %>%
    # formatStyle('Population', 'moe check 1',backgroundColor = styleEqual(c( "Uncertain Estimate"), 'yellow')) %>%
    # formatCurrency("Population",interval = 3,currency = "",digits = 0) %>%
    # formatPercentage("percent Population")
  })
})


})