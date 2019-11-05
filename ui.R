library(shiny)
library(readxl)
library(shinythemes)
library(tidyverse)
library(DT)
library(plotly)
library(shinyjs) #for hiding and showing an input area for the graph
library(hrbrthemes)
# Menu Options:Geography---------------------------------------------
geography <- c("National",
               "State",
               "County",
               "Congressional District")
geographyvals <- geography
names(geographyvals) <- geography


# Menu Options: Racial Groups ---------------------------------------------
# race <- c("Detailed Asian Alone", "Detailed NHPI Alone", "Asian Alone", "NHPI Alone", "Black Alone", "Non-Hispanic White Alone", "Latino Alone", "AIAN Alone")
race <- c("Asian Alone", "NHPI Alone", "Black Alone", "Non-Hispanic White Alone", "Latino Alone", "AIAN Alone")

# Menu Options: Topics -------------------------------------------------
# est_edu <- c("Less than HS", "HS or GED", "Some College or AA", "BA or higher")
# est_ins <- c("No Insurance")
# est_lep <- c("lep", "speak another language")
# est_nativity <- c("foreign-born")
# est_pov <- c("Overall Poverty")
# est_cvap <- c("Citizen Voting-age Population")

# Menu Options: TYPE ------------------------------------------------------
est_type <- c("count", "prop")
# Define UI for application that draws a histogram

shinyUI(fluidPage(
  theme = "test_style.css",
  tags$div(
    class = "header", checked = NA,
    tags$img(src = "aapidata.png", style = "align:center; display: block; width:250px; min-width: 200px max; margin-left: auto; margin-right: auto; margin-top:30px;")
  ),
  column(3, offset = 1,
         titlePanel("Choose your options"),
         # actionButton("goButton", "Go!"),
         radioButtons("geo", "Geography",
                      choices = geographyvals),

         selectizeInput("group", "Select A Racial Group",
                      choices = race,
                      options = list(
                        placeholder = 'Please selection option below',
                        onInitialize = I('function() { this.setValue(""); }')
                      )),
         selectizeInput("topic",
                     "Selected Topic:",
                     choices = "",
                     options = list(
                       placeholder = 'Please selection option below',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
         hr(),
         # uiOutput("selected_country_UI"),
         selectizeInput("topic_type",
                     "Selected Topic Type:",
                     choices = "",
                     options = list(
                       placeholder = 'Please selection option below',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
         hr(),
         actionButton("load", "Load Data!"),
         hr()),
  mainPanel(
    h1(""),
    hr(),
    # textOutput("selected_topic"),
    # textOutput("selected_topic2"),
    htmlOutput("meta"),
    tabsetPanel(
      
      tabPanel("Table", dataTableOutput("preview")),
      
      tabPanel("Graph", 
        conditionalPanel("input.geo != 'National'", 
                        fluidRow(selectInput("flgraph", NULL, choices = c("Show highest", "Show lowest"), 
                                             selected = "Show highest", width = 150), 
                        numericInput("flgraph2", NULL, value = 10, min = 0, max = 20, step = 1, width = 50))),
        conditionalPanel("input.geo != 'National'", plotlyOutput("plot1")),
        conditionalPanel("input.geo == 'National'", htmlOutput("ntlmessage"))),
      
      
      tabPanel("Map", selectizeInput("subgeo", "Select a State:",
                                  choices = state.name, options = list(
                                    placeholder = "Please select a sub-geography.", 
                                    onInitialize = I('function() {this.setValue(""); }')
                                  )),
                                  actionButton("mapload", "Load Map!"), plotOutput("map"))
    )
    
  )))
