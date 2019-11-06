
library(shiny)
library(readxl)
library(shinythemes)
library(tidyverse)
library(DT)

## geographic options, I guess theoretically, we could offer whatever the census API had, but that would mean we would need to have corresponding PUMS code.
geography <- c("National",
               "State",
               "County","Congressional District")
geographyvals <- c("us","state","county","congressional district")
names(geographyvals) <- geography


# I couldn't figure out a more elegant way of doing this. Bascially, each sheet is just a character vector of the topics that are avaiable for each geography
# We can probably have this read from our raw_dta repository eventually? 
ref_pop <- readxl::read_excel("ref_table.xlsx",sheet="pop")
pop_choices <- as.list(ref_pop$code)
names(pop_choices) <- ref_pop$topics

ref_outcomes <- readxl::read_excel("ref_table.xlsx",sheet="outcomes")
outcome_choices <- as.list(ref_outcomes$code)
names(outcome_choices) <- ref_outcomes$topics

race_prettyV1 <- c("Asian Alone","NHPI Alone","Black Alone","Hispanic (any race)", "White, Non-Hispanic Alone")
race_valuesV1 <- c("D","E","B","I", "H")
names(race_valuesV1) <- race_prettyV1

# race_prettyV2 <- c("Asian Alone","Asian Alone or in Combination","NHPI Alone","NHPI Alone or in Combination","Black Alone","Hispanic (any race)", "White, Non-Hispanic Alone")
# race_valuesV2 <- c("B02001_005E","B02011_001E","B02001_006E","B02012_001E", "B02001_003E","B03002_012E","B03002_003E")
# names(race_valuesV2) <- race_prettyV2


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinytheme("spacelab"),
  column(3, offset= 1,
         titlePanel("QuickStats"),
         radioButtons("estimate_type", "What type of estimate are you looking for?",
                      choices= c("Population", "Outcomes")),
         conditionalPanel(condition ="input.estimate_type =='Population'",
                          radioButtons("topics", "Topics",
                                       choices= pop_choices),
                          conditionalPanel(condition="input.topics =='pop_by_race'",
                          radioButtons("pop_type", "Choose Type",
                                       choices= list("Alone" = "alone", "Alone or in Combination" = "combo")))),
         conditionalPanel(condition ="input.estimate_type =='Outcomes'",
                          radioButtons("topics2", "Topics",
                                       choices= outcome_choices),
                          radioButtons("race", "Race",
                                       choices= race_valuesV1)),
         radioButtons("geo", "Geography",
                            choices= geographyvals),
         actionButton("goButton", "Go!")),
  
  
  
  
  
    mainPanel(
      h1("AAPI Data QuickStats"),
      hr(),
      h3(em(textOutput("info"))),
      br(),
      h5(dataTableOutput("acs"))
    )
  )
)
