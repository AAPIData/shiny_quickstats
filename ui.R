library(shiny)
library(readxl)
# library(shinythemes)
library(tidyverse)
library(DT)
library(plotly)
library(hrbrthemes)
library(shinyWidgets)
# library(rintrojs)

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


# Menu Options: Topics (NON AA OR PI) ----------------------------------------------------
topic <- c("Educational Attainment", "Health Insurance Coverage", "Limited English Proficiency",
            "Nativity", "Poverty", "Citizen Voting-Age Population")
topicval <- c( "edu", "insurance", "LEP", "nativity", "poverty", "CVAP")

topics_dta <- tibble::tribble(
                                         ~topic,                     ~topic_type,
                       "Educational Attainment",                  "Less than HS",
                       "Educational Attainment",                     "HS or GED",
                       "Educational Attainment",            "Some College or AA",
                       "Educational Attainment",                  "BA or Higher",
                    "Health Insurance Coverage",                  "No Insurance",
                  "Limited English Proficiency",                           "LEP",
                  "Limited English Proficiency",        "Speak another language",
                                     "Nativity",                  "foreign-born",
                                      "Poverty",               "Overall Poverty",
                "Citizen Voting Age Population", "Citizen Voting-age Population"
                )


names(topicval) <- topic


# Menu Options: Detailed Origin -------------------------------------------
est_detailed_aa <- c("Bangladeshi", "Bhutanese", "Burmese", "Cambodian", "Chinese except Taiwanese", "Filipino", "Hmong", "Indonesian", "Japanese", "Korean", "Laotian", "Malaysian", "Mongolian", "Nepalese", "Okinawan", "Pakistani", "Sri Lankan", "Taiwanese", "Thai", "Vietnamese", "Other Asian specified", "Other Asian not specified", "Two or more Asian")
est_detailed_nhpi <- c("Samoan", "Tongan", "Other Polynesian", "Guamanian or Chamorro", "Marshallese", "Other Micronesian", "Fijian", "Other Melanesian", "Other Pacific Islander not specified (check box only)", "Two or More NHPI")


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
  includeCSS("test_style.css"),
  tags$div(
    class = "header", checked = NA,
    tags$img(src = "aapidata.png", style = "align:center; display: block; width:250px; min-width: 200px max; margin-left: auto; margin-right: auto; margin-top:30px;")
  ),
  column(3, offset = 1,
         titlePanel("Choose your options"),
         # actionButton("goButton", "Go!"),
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
         #uiOutput("detailed"),
         conditionalPanel(condition="input.group=='detailed ethnicity (AAPI alone)' | input.group=='detailed ethnicity (AAPI alone or in combo)'",
                          selectizeInput("detailed_filter", 
                                         "Which groups? (up to 3 groups)",
                                         choices = "",
                                         multiple = TRUE, 
                                         options = list(
                                           placeholder = 'Please selection option below',
                                           onInitialize = I('function() {this.setValue(""); }'),
                                           maxItems = 3))),
         actionButton("do", "Click to see"),
         hr(),
         checkboxInput("reliable", "Reliable Data ONLY", TRUE)),
  mainPanel(
    h1(""),
    hr(),
    textOutput("selected_topic"),
    tabsetPanel(
      tabPanel("Table 1",dataTableOutput("preview")),
      tabPanel("Table 2",dataTableOutput("percentage"))
    )
    
  )))
