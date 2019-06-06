library("shiny")
library("ggplot2")
library("shinythemes")

source("finalScript.R")

# Define a UI for the application
my_ui <- navbarPage( theme = shinytheme("superhero"),
  "Washington Home Price Analysis",
  
  #the first tab: summary inforamtion
  tabPanel(
    "Summary",
    includeMarkdown("README.md")
  ),
  
  #second tab: general trend
  tabPanel(
   "Trend", 
   titlePanel("What is the overall trend in a 3-bedroom house value over the past years?"), 
   
   sidebarLayout(
     sidebarPanel(
       selectInput(inputId = "county_input",
                   label = "County: ",
                   choices = c(melted$RegionName[1:30])),
       radioButtons(inputId = "year_input",
                    label = "Year",
                    choices = c("2009", "2010", "2011", "2012", "2013", "2014",
                                "2015", "2016", "2017", "2018", "2019"))
     ),
     
     mainPanel(
       h5("Thinking long-term when buying a house is very important. One is likely 
          to resell their property eventually, so it is useful to know what a home 
          purchased today would be valued at in the future by using past trends. "),
       plotOutput("overall_value"),
       textOutput("overall_value_caption"),
       plotOutput("year_value_plot"),
       textOutput("year_value_plot_caption")
     )
   )
 ),
 
  #third tab: comparision of rent 
  tabPanel(
    "Rent or Buy",
    titlePanel("Side by Side Comparison of Rent and Sales in Washington"),
    
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "chosen_county",
          label = h4("Select Your County of Interest"),
          choices = c("King County", "Pierce County", "Snohomish County", "Spokane County", 
                      "Clark County", "Thurston County", "Kitsap County", "Yakima County",
                      "Whatcom County", "Benton County", "Skagit County", "Grant County",
                      "Island County", "Franklin County"),
          selected = "King County"
        ),
        h4("These two graphs shows the general trend of Washington Housing values both in rent
           and sales during the past 7 years. Available options above include top 14 Washington 
           counties. Based on the graph, the price of both rent and buy in Washington state increased
           steadily over the past 7 years. Possibile explanations include the increasing 
           immigrations and the increasing availability of jobs in tech companies. ")
        ),
      mainPanel(
        #display the graph
        plotOutput("rent_plot"),
        plotOutput("buy_plot")
      )
    )
  ),
  
  #fourth tab： Determine the ideal county
  tabPanel(
    "Ideal County",
    titlePanel("Ideal Counties for Houses in Washington"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        sliderInput("priceRange",
                    "Select your price range for a house",
                    min = 100000,
                    max = 800000,
                    value = c(200000, 500000)),
        
        h6("For the typical family looking for a 3-Bedroom House in the state of Washington,
           price can be one of the most important factors in determining what house to choose."),
        
        radioButtons("mapType", "Select how you want to color the map", 
                     c("Criminal Incidents by County", "High School Dropout Rates by County")),
        h6("We can examine how these counties vary by certain charasteristics important to families 
           when searching for the ideal home. We can look at the high school dropout rates of 
           counties or the number of criminal incidents in each of the counties "),
        
        uiOutput("countyNumber")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("mapOfWashington"),
        textOutput("descriptionOfMap"),
        plotOutput("informationForCounties"),
        textOutput("descriptionOfGraph")
      )
    )
  )
) 
  
  
  

 

  
 