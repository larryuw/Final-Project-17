#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
source("finalScript.R")

# Define UI for application that draws a histogram
ui <- navbarPage(
   
  theme = shinytheme("superhero"),
  
   # Application title
   titlePanel("What will a home bought today be worth in the future?"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "county_input",
                     label = "County:",
                     choices = c(melted$RegionName[1:30])),
         textInput(inputId = "year_input",
                   label = "Year",
                   value = "2019"),
         submitButton(text = "Apply Changes")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h5("Thinking long-term when buying a house is very important. One is likely 
          to resell their property eventually, so it is useful to know what a home 
           purchased today would be valued at in the future. "),
        plotOutput("Plot1"),
        textOutput("text1"),
        plotOutput("Plot2"),
        textOutput("text2")
      )
      
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$Plot1 <- renderPlot({
     melted <- filter(melted, RegionName == input$county_input)
     ggplot(melted, aes(variable, value, group=factor(RegionName))) +
       geom_line() +
       ggtitle(paste("Washington Home Values in",input$county_input)) + 
       xlab("Year 1996-2019") +
       ylab("Value $") + 
       geom_smooth(method = "lm") +
       theme(axis.text.x = element_blank()) +
       scale_y_continuous(labels = comma)
    
   })
   
   output$text1 <- renderText({
     paste("The graph shows the Zillow data of home value prices of a 3 bedroom 
           house from 1996 to 2019. Using linear regression, we can use the trend
           line to predict the home's value years from now.")
   })
   
   output$Plot2 <- renderPlot({
     melted <- filter(melted,RegionName==input$county_input,str_detect(variable,input$year_input))
     ggplot(melted, aes(variable,value,group=factor(RegionName))) +
       geom_line() +
       ggtitle(paste(input$county_input,"Home Values in",input$year_input)) +
       xlab("Year") +
       ylab("Value $")
   })
   
   output$text2 <- renderText({
     paste("This 2nd graph shows the trend for the year input. This information is 
           useful for one to examine 2019's current trend which will most likely
           continue and be the most accurate representation of the predicted values
           in the years to come.")
   })
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

