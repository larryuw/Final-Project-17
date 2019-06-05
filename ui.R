

library("shiny")
library("ggplot2")
library("shinythemes")


# Define a UI for the application
my_ui <- navbarPage( theme = shinytheme("superhero"),
  "Washington Home Price Analysis",
  
  #the first tab
  tabPanel(
    "Rent or Buy",
    
    titlePanel("Side by Side Comparison of Rent and Sales in Washington"),
    h5("Provide some explanations here"),
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
        )
      ),
      mainPanel(
        #display the graph
        plotOutput("rent_plot"),
        plotOutput("buy_plot")
      )
    )
  )
  
  
  
)
  
 