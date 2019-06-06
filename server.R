# load the necessary library
library("shiny")
library("ggplot2")
library("dplyr")
library("reshape2")
library("stringr")
library("tidyr")
library("markdown")
library("maps")
library("mapproj")
# set the source file
source("finalScript.R")

#Define a server function
my_server <- function(input, output) {

  output$rent_plot <- renderPlot({
    
    rentdata <- filter(filtered_rent, RegionName == input$chosen_county)
    rentdata <- mutate(rentdata, 
                       "2011" = round(sum(rentdata[2:13])/12),
                       "2012" = round(sum(rentdata[14:25])/12),
                       "2013" = round(sum(rentdata[26:37])/12),
                       "2014" = round(sum(rentdata[38:49])/12),
                       "2015" = round(sum(rentdata[50:61])/12),
                       "2016" = round(sum(rentdata[62:73])/12),
                       "2017" = round(sum(rentdata[74:85])/12),
                       "2018" = round(sum(rentdata[86:97])/12),
                       "2019" = round(sum(rentdata[98:101])/4))
    rentdata <- select(rentdata, "RegionName", "2012", "2013", "2014", 
                       "2015", "2016", "2017", "2018", "2019")
    
    plot(colnames(rentdata), rentdata, type = "b", col = "red", xlab = "Year",
         ylab = "Value $", main = "Monthly Rent Average")+
      text(colnames(rentdata),rentdata[1:8], 
           paste0("$", rentdata), pos = 3) +
      text(colnames(rentdata[9]), rentdata[9],
           paste0("$", rentdata$'2019'), pos = 1)
    
  })
  
  output$buy_plot <- renderPlot({
    buydata <- filter(filtered_buy, RegionName == input$chosen_county)
    buydata <-  mutate(buydata, 
                       "2011" = round(sum(buydata[2:13])/12),
                       "2012" = round(sum(buydata[14:25])/12),
                       "2013" = round(sum(buydata[26:37])/12),
                       "2014" = round(sum(buydata[38:49])/12),
                       "2015" = round(sum(buydata[50:61])/12),
                       "2016" = round(sum(buydata[62:73])/12),
                       "2017" = round(sum(buydata[74:85])/12),
                       "2018" = round(sum(buydata[86:97])/12),
                       "2019" = round(sum(buydata[98:101])/4))
    
    buydata <- select(buydata, "RegionName", "2012", "2013", "2014", 
                      "2015", "2016", "2017", "2018", "2019")
    
    plot(colnames(buydata), buydata, type = "b", col = "green", xlab = "year", 
         ylab = "Value $", main = "Monthly Sales Average") +
      text(colnames(buydata), buydata[1:8],
           paste0("$", buydata), pos = 4) +
      text(colnames(buydata[9]), buydata[9],
           paste0("$", buydata$'2019'), pos = 1)
  })
  
  output_newData <- reactive({
    new_data <- filter(merged_filtered_data,
                       input$priceRange[1] <= merged_filtered_data$PriceAvg,
                       merged_filtered_data$PriceAvg <= input$priceRange[2])
    return(new_data)
    
  })
  
  
  output$countyNumber <- renderUI({
    checkboxGroupInput("County", "Pick a county", output_newData()$county)
  })
  
  output$informationForCounties <- renderPlot({
    filtered_county_set <- filter(output_newData(), county %in% input$County)
    ggplot(filtered_county_set, aes(county, fill = county )) + geom_col(aes
    (y=POPDEN_2018)) + scale_fill_hue(c=45, l=80) + theme_minimal() + labs(title
  = "Graph showing population densities for each county \n in the given price 
  range in Washington", x = "County", y = "Population Density in 2018") + 
    theme(plot.title = element_text(color = "black", size = 20, face = "bold",
                                    hjust = 0.5))
  })
  
  
  
  output$mapOfWashington <- renderPlot({
    county <- map_data("county")
    county_data <- filter(county, region == "washington")
    
    colnames(county_data)[colnames(county_data)=="subregion"] <- "county"
    
    new_data_set <- full_join(county_data,output_newData(), by = "county")
    if(input$mapType == "Criminal Incidents by County") {
      ggplot()+geom_polygon(data = new_data_set, aes(x = long, y = lat, 
      group=group, col=county, fill = 2 * Rank.by.Percent)) + theme_minimal() + 
        theme(legend.position = "none") 
    }
    else {
      ggplot()+geom_polygon(data = new_data_set, aes(x = long, y = lat, 
      group=group, col=county, fill = 2 * rank)) + scale_colour_hue(na.value = 
                "white") + theme_minimal() + theme(legend.position = "none")
    }
    
    
  })

  
  output$descriptionOfMap <- renderText({
    paste("This displays a map of washington and is colored based on high school
          drop out rates or criminal rates in counties in Washington. The darker the county 
          color, the higher the dropout or criminal incident prevalence rate, and the
grayed counties are ones that do not have any data on them or do not fit the price range criteria.
This data becomes especially interesting as we can
          see where in Washington each of these characteristics are more prevalent and 
with every change in the price range or selection of what data to display
          through the buttons, the data gets updated.                                                        

Below we have another charasteristic of counties which is their population density plotted in a bar
          graph allowing you to compare densities across all counties within the price range.
          
Usually counties that have higher population densities were showed to also have higher dropout rates 
and higher criminal incidents. There is additionally much more housing in a price range lower 
          than $500000 then there is in the upper range.")
  })
 
  output$overall_value <- renderPlot({

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
  
  output$overall_value_caption <- renderText({
    paste("The graph shows the Zillow data of home value prices of a 3 bedroom
          house from 1996 to 2019. Using linear regression, we can use the trend
          line to predict the home's value years from now.")
  })
  
  output$year_value_plot <- renderPlot({
    melted <- filter(melted, RegionName == input$county_input)
    colnames(melted) <- gsub("X", "", colnames(melted))
    melted <- filter(melted, str_detect(variable, input$year_input))
    ggplot(melted, aes(variable,value,group=factor(RegionName))) +
      geom_line() +
      ggtitle(paste(input$county_input,"Home Values in",input$year_input)) +
      xlab("Year") +
      ylab("Value $")
  })
  
  output$year_value_plot_caption <- renderText({
    paste("This 2nd graph shows the trend for the year input. This information is
          useful for one to examine 2019's current trend which will most likely
          continue and be the most accurate representation of the predicted values
          in the years to come.")
  })
  

}

