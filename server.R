library(shiny)
library(tidycensus)
library(tidyverse)
library(feather)
library(plotly)
library(sf)
topic_choices <- read_csv("ui_options.csv", col_types = "cccccc")
cty <- readRDS("countymaps.RDS")
states <- readRDS("statemaps.RDS")
districts <- readRDS("districtmaps.RDS")
pull_data <- function(rgroup, type, topic, geography){
  
  geoformat <- str_split(tolower(geography), " ")[[1]]
  geoformat <- geoformat[length(geoformat)] 
  #formats input$geo from a user-friendly form into the form displayed in filenames
  
  filename <- topic_choices %>% 
    filter(topic == topic, topic_type == type, 
          geo == geography, `racial group` == rgroup) %>% 
    pull(filename)
  filename <- paste0("dta/", filename, "_", geoformat, ".RDS")

  dta <- readRDS(filename) %>%
    filter(group == rgroup, topic_type == type) %>%
    select(NAME,estimate_type,topic_type,estimate) %>%
    rename(Geography = NAME) %>% 
    spread(estimate_type, estimate) %>%
    rename(`Total` = count,
           `Share` = prop,
           `Variable Name` = topic_type)
  
  return(dta)
}

make_meta <- function(type, dta){
  high <- arrange(dta, -Total) %>% slice(1)
  low <- arrange(dta, Total) %>% slice(1)
  
  highshare <- arrange(dta, -Share) %>% slice(1)
  lowshare <- arrange(dta, Share) %>% slice(1)
  
  tribble(~var, ~mean, ~median, ~sd, ~high, ~low, ~avgshare, ~highshare, ~lowshare, ~pctna,
          type, round(mean(dta$Total, na.rm=T), 2),
          round(median(dta$Total, na.rm=T), 2),
          round(sd(dta$Total, na.rm=T), 2),
          paste(high$Geography[1], "with", high$Total[1], "people.", sep = " "), 
          paste(low$Geography[1], "with", low$Total[1], "people.", sep = " "),
          round(mean(dta$Share, na.rm=T),2), 
          paste0(highshare$Geography[1], " with ", round(lowshare$Share[1] * 100,2), "%."), 
          paste0(lowshare$Geography[1], " with ", round(lowshare$Share[1] * 100,2), "%."),
          100* round(sum(is.na(dta$Total))/nrow(dta), 2)
          )
}








shinyServer(function(input, output,session) {

  
  observe({
    updateSelectizeInput(session,
                      "topic",
                      choices = unique(topic_choices$topic))
  })

  observe({
    topic_type_choices <- topic_choices %>%
      filter(topic == input$topic) %>%
      select(topic_type) %>%
      unique() %>% .[[1]]
    updateSelectizeInput(
      session,
      "topic_type",
      choices = topic_type_choices,
      selected = topic_type_choices[1]
    )

  })
  

  
  output$ntlmessage <- renderUI({ HTML("<h1> Graph not available for national-level data!")})
  
  
  
  
#when "Load Data!" button is pressed
observeEvent(input$load,{
  graphtitle <- paste0(
    "Number of ", str_remove(input$group, "Alone"), " Americans ", 
    topic_choices %>% filter(topic_type == input$topic_type) %>% pull(conj) %>% unique, " ",
    input$topic_type, 
    ", by ", input$geo
  ) 
  if (input$topic_type == "") {
    return()
  }
  
  dta_load <-pull_data(input$group, input$topic_type, input$topic, input$geo)
  
  dta <- reactive({ 
     if(input$flgraph == "Show highest"){
      dta_load$Geography <- reorder(dta_load$Geography, dta_load$Total)
      arrange(dta_load, Total) %>% droplevels %>% top_n(input$flgraph2, Total)
     } else { dta_load$Geography <- reorder(dta_load$Geography, -dta_load$Total)
       dta_load %>% droplevels %>% top_n(input$flgraph2,-Total)
     } 
    })
  print(dta())
  
  output$plot1 <- renderPlotly({
    plot1 <- dta() %>% 
        ggplot(aes(x = Geography, y = Total)) + 
        geom_col() +
        theme_modern_rc(grid="Y") + 
        coord_flip() +
        scale_y_continuous(labels = scales::comma)+
        labs(title = graphtitle, x = paste(input$geo, "Name"), y = str_remove(graphtitle, ", by State")) #+ theme_bw()
      ggplotly(plot1, width =1000, height = 1000,tooltip=c("text")) %>% 
        config(
          #displayModeBar = T, # Always show the toolbar if set to TRUE
          #displaylogo = FALSE, # Display plotly logo
          modeBarButtonsToRemove = list(
            'pan2d',
            'resetScale2d',
            'autoScale2d',
            'zoomIn2d',
            'zoomOut2d',
            'select2d',
            'zoom2d',
            'hoverClosestCartesian',
            'lasso2d',
            'toggleSpikelines',
            'sendDataToCloud'
          )) 
      
    })
           

      
  
  output$selected_topic <- renderText({ 
    # glue::glue("Greetings Human! It looks like you want to look at {input$topic}, for {input$group} at the {input$geo}")
    paste("DEBUGGING: Your geo is: ", input$geo," your group is : ",input$race, " Your TOPIC IS: ", input$topic, "topic type: ", input$topic_type)
  })
  
  
  output$meta <- renderUI({
    HTML(
    ifelse(input$geo != "National", {
    meta <- make_meta(input$topic_type, dta_load)
    paste("<h1> Summary of Data: </h1>", "<p>",
      "<b> Variable: </b>", meta$var, "<br>",
          "<b> Average: </b>", meta$mean, "<br>",
          "<b> Median: </b>", meta$median, "<br>",
          "<b> Standard Deviation: </b>", meta$sd, "<br>",
          "<b>", input$geo, "with highest: </b>", meta$high, "<br>", 
          "<b>", input$geo, "with lowest: </b>", meta$low, "<br>", 
          "<b> Average Share: </b>", meta$avgshare, "<br>", 
          "<b> Percent Missing: </b>", meta$pctna, "%. </p>"
          )
    },
    {
      ""
    })
    )
  })
    
    
  output$preview <- DT::renderDataTable({
    dta_load  %>% 
      drop_na %>% 
    DT::datatable(rownames = FALSE,
                  extensions = 'Buttons',
                  options = list(dom = 'Btp', pageLength = 25, 
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>% 
      formatCurrency(3,currency = "", interval = 3, mark = ",", digits = 0) %>% 
      formatPercentage(4)
    
  })
  
  maps <- reactive({
    if(input$geo == "State"){
      states
    }else if(input$geo == "County"){
      cty
    } else if(input$geo == "Congressional District"){
      districts
    }
  })
  

  observeEvent(input$mapload, { 
    print("dta_load:")
    print(dta_load)
    dta_map <- dta_load %>% left_join(maps(), by = c("Geography" = "NAME")) 
    
    print(dta_map) 
    
    dta_map <- dta_map %>% st_as_sf %>% 
      filter(word(Geography, -1) == input$subgeo)
  print(dta_map)
    
  output$map <- renderPlot({
    dta_map %>% 
      ggplot() + geom_sf(aes(fill = Total))
  })

  })
  
  
  })



})