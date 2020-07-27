library(shiny)
library(readxl)
# library(shinythemes)
library(tidyverse)
library(DT)
library(plotly)
library(hrbrthemes)
library(shinyWidgets)

shinyUI(fluidPage(
<<<<<<< HEAD
  # theme = "test_style.css",
=======
>>>>>>> 4900d5dff4e18c87a0ee11605b2fcd3eda6ab253
  includeCSS("test_style.css"),
  tags$div(
    class = "header", checked = NA,
    tags$img(src = "aapidata.png", style = "align:center; display: block; width:250px; min-width: 200px max; margin-left: auto; margin-right: auto; margin-top:30px;")
  ),
  tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
            color: #EE8B46 !important;
        }")),
  column(3, offset = 1,
         titlePanel("Choose your options"),
         selectizeInput("topic", "Selected Topic:",
                        choices = "",
                        options = list(
                          placeholder = 'Please selection option below',
                          onInitialize = I('function() { this.setValue(""); }')
                        )),
         selectizeInput("geo", "Geography",
                        choices = "",
                        options = list(
                          placeholder = 'Please selection option below',
                          onInitialize = I('function() { this.setValue(""); }')
                        )),
         selectizeInput("group", "Select A Racial Group",
                        choices = "",
                        options = list(
                          placeholder = 'Please selection option below',
                          onInitialize = I('function() {this.setValue(""); }')
                        )),
         conditionalPanel(condition="input.group=='Detailed ethnicity (AAPI alone)' | input.group=='Detailed ethnicity (AAPI alone or in combo)' | input.group=='Detailed Asian American Alone' ",
                          selectizeInput("detailed_filter", 
                                         "Which groups? (up to 3 groups)",
                                         choices = "",
                                         multiple = TRUE, 
                                         options = list(
                                           placeholder = 'Please selection option below',
                                           onInitialize = I('function() {this.setValue(""); }'),
                                           maxItems = 3))),
         
         
         checkboxInput("reliable", "Reliable Data ONLY", TRUE),
         p("Data is considered as unreliable if the marginal error is larger than a half of the estimate. For example, a populaiton estimate of 10,000 with 
           marginal error greater or equal to 5,000 will be considered as unreliable."),
         downloadButton("report", "Download Report (PDF)"),
         hr(),
         actionButton("do", "Load Data")),
  mainPanel(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    h1(""),
    hr(),
    textOutput("selected_topic"),
    tabsetPanel(
      tabPanel("Population Size",dataTableOutput("preview")),
      tabPanel("Population Share",dataTableOutput("percentage"))
    )
    
  )))