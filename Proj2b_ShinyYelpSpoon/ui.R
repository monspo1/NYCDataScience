###############################################################################
# [YelpSpoon ShinyApp]
# by Sung Pil Moon
###############################################################################

# cat("\014")
#setwd("~/Desktop/Dataset for NYC DSA ")
setwd("~/Desktop/ShinyYelpSpoon/")

load('proj2_YelpSpoon.RData')


library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(DT)
library(leaflet)
library(rsconnect)

ui = dashboardPage( 
  
  header <- dashboardHeader(
    title = "YelpSpoon Shiny App"
  ),
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("YelpSpoon Map", tabName = "ylpSpoonMap", icon = icon("fa fa-table")),
      menuItem("YelpSpoon Grid", tabName = "ylpSpoonGrid", icon = icon("bar-chart-o"))
    )
  ),
  
  body <- dashboardBody(
    
    tabItems(
      
      ##########################################################
      # First contents (YlpSpoon Map & DataTable)
      ###########################################################
      tabItem("ylpSpoonMap",
              div(class="outer",
                  
                  fluidRow(
                    valueBoxOutput("boxForNumRowsInMap"),
                    valueBoxOutput("boxForReviewCountInMap"),
                    valueBoxOutput("boxForAvgRatingsInMap")
                  ), 
                  
                  fluidRow(
                    column(4,
                           selectizeInput('multiSelectForCategories', 'Categories:', 
                                          c("Choose multiple"= '', 
                                            "Food", "Restaurant",
                                            "Health", "Education",
                                            "Coffee", "Cafe",
                                            "Pubs", "Bars", "Brewery", 
                                            "Shopping", "Fashion",
                                            "Italian", "Mexican","Korean", "Taiwanese", "Local", 
                                            "Japanese","French", "Indian", "Brazilian", 
                                            "Pizza", "Fast Food", "Burger", "Steak", 
                                            "Desserts", "Seafood","Vegetarian","Sushi", "Salad",
                                            "Asian", "American", "African", "European", "Caribbean",
                                            "Dance","Salsa", "Aerobic","Yoga", 
                                            "Pilates"
                                          ), 
                                          multiple = TRUE
                           )
                    ),
                    column(4,
                           selectizeInput('multiSelectForStates', 'States:', 
                                          c("Choose multiple"= '', "California" = "CA", "Georgia" = "GA", "Illinois" = "IL", "Indiana" = "IN", 
                                            "Massachusetts"="MA", "Maryland"="MD", "Michigan"="MI", "North Carolina"="NC", "New Jersey"="NJ", 
                                            "New York"="NY", "Ontario"="ON", "Pennsylvania"="PA", "Rhode Island"="RI", "Texas"="TX", 
                                            "Virginia"="VA", "Washington"="WA"),
                                          multiple = TRUE
                           )
                    ),
                    column(4,
                           selectizeInput("multiSelectForSchools", "Schools:", 
                                          c("Choose One"= '', "Brown University", "California Institute of Technology",
                                            "California Polytechnic State University", "Carnegie Mellon University", 
                                            "Columbia University", "Cornell University", "Georgia Institute of Technology",
                                            "Harvard University","Harvey Mudd College", "Massachusetts Institute of Technology", 
                                            "Princeton University","Purdue University","Rensselaer Polytechnic Institute",
                                            "Rice University", "Stanford University", "University of California at Berkeley", 
                                            "University of California - Los Angeles", "University of California - San Diego", 
                                            "University of Illinois - Urbana-Champaign", "University of Maryland - College Park", 
                                            "University of Massachusetts - Amherst", "University of Michigan - Ann Arbor",
                                            "University of North Carolina - Chapel Hill",  "University of Pennsylvania",
                                            "University of Southern California", "University of Texas - Austin",
                                            "University of Washington", "University of Waterloo", "Virginia Tech"
                                          ),
                                          multiple = F
                           )
                    )
                  ),
                  fluidRow(
                    column(4, textInput("searchInputForCategory","Category Search:","")),
                    column(4, textInput("searchInputForCity","City Search:","") ),
                    column(4, textInput("searchInputForBizName","Business Name Search:",""))
                  ),
                  fluidRow(
                    column(4, sliderInput("sliderForRating", "Avg.Rating Range:", 
                                          min = 1.0, max = 5.0, value = c(3.0, 4.5), step = 0.5)
                    ),
                    column(4, sliderInput("sliderForReviewCount", "Review Count Range:", 
                                          min = 1, max = 2874, value = c(10, 2500), step = 1)
                    ),
                    column(4)
                  ),
                  
                  leafletOutput("myYelpMap", width=900, height=550),
                  
                  #br(),
                  DT::dataTableOutput("myYelpDataTable")
              )
              
      ),
      
      ##########################################################
      # Second content (YlpSpoon Grid & Table)
      ###########################################################
      tabItem("ylpSpoonGrid",
              
              fluidRow(
                valueBoxOutput("boxForNumRows"),
                valueBoxOutput("boxForReviewCount"),
                valueBoxOutput("boxForAvgRatings")
              ), 
              
              
              fluidRow(
                column(4,
                       selectizeInput('multiSelectForCategoriesForGrid', 'Categories:', 
                                      c("Choose multiple"= '', 
                                        "Food", "Restaurant",
                                        "Health", "Education",
                                        "Coffee", "Cafe",
                                        "Pubs", "Bars", "Brewery", 
                                        "Shopping", "Fashion",
                                        "Italian", "Mexican","Korean", "Taiwanese", "Local", 
                                        "Japanese","French", "Indian", "Brazilian", 
                                        "Pizza", "Fast Food", "Burger", "Steak", 
                                        "Desserts", "Seafood","Vegetarian","Sushi", "Salad",
                                        "Asian", "American", "African", "European", "Caribbean",
                                        "Dance","Salsa", "Aerobic","Yoga", 
                                        "Pilates"
                                      ), 
                                      multiple = TRUE
                       )
                ),
                
                column(4,
                       selectizeInput('multiSelectForStatesForGrid', 'States:', 
                                      c("Choose multiple"= '', "California" = "CA", "Georgia" = "GA", "Illinois" = "IL", "Indiana" = "IN", 
                                        "Massachusetts"="MA", "Maryland"="MD", "Michigan"="MI", "North Carolina"="NC", "New Jersey"="NJ", 
                                        "New York"="NY", "Ontario"="ON", "Pennsylvania"="PA", "Rhode Island"="RI", "Texas"="TX", 
                                        "Virginia"="VA", "Washington"="WA"),
                                      multiple = TRUE
                       )
                ),
                column(4,
                       selectizeInput("multiSelectForSchoolsForGrid", "Schools:", 
                                      c("Choose One"= '', "Brown University", "California Institute of Technology",
                                        "California Polytechnic State University", "Carnegie Mellon University", 
                                        "Columbia University", "Cornell University", "Georgia Institute of Technology",
                                        "Harvard University","Harvey Mudd College", "Massachusetts Institute of Technology", 
                                        "Princeton University","Purdue University","Rensselaer Polytechnic Institute",
                                        "Rice University", "Stanford University", "University of California at Berkeley", 
                                        "University of California - Los Angeles", "University of California - San Diego", 
                                        "University of Illinois - Urbana-Champaign", "University of Maryland - College Park", 
                                        "University of Massachusetts - Amherst", "University of Michigan - Ann Arbor",
                                        "University of North Carolina - Chapel Hill",  "University of Pennsylvania",
                                        "University of Southern California", "University of Texas - Austin",
                                        "University of Washington", "University of Waterloo", "Virginia Tech"
                                      ),
                                      multiple = F
                       )
                )
                
              ),
              
              fluidRow(
                column(4, textInput("searchInputForCategoryForGrid","Category Search:","")), 
                column(4, textInput("searchInputForCityForGrid","City Search:","") ),
                column(4 #, #textInput("searchInputForBizNameForGrid","Business Name Search:","")
                )
              ),
              fluidRow(
                column(4,
                       sliderInput("sliderForRatingForGrid", "Avg.Rating Range:", 
                                   min = 1.0, max = 5.0, value = c(2.5, 5), step = 0.5)
                )
              ),
              
              plotOutput("yelpSpoonGridPlot", height="700"),
              
              DT::dataTableOutput("myYelpDataTableForGrid")
              
      )
    )
  ), skin = "yellow"
)
