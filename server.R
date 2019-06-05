# load the necessary library
library("shiny")
library("ggplot2")
library("dplyr")
library("reshape2")

source("rent_buy_dataset.R")

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
   rentdata <- select(rentdata, "RegionName", "2011", "2012", "2013", "2014", 
                      "2015", "2016", "2017", "2018", "2019")
   
   plot(colnames(rentdata), rentdata, type = "b", col = "red", xlab = "Year",
        ylab = "Value $", main = "Monthly Rent Average")+
     text(colnames(rentdata),rentdata[1:9], 
          paste0("$", rentdata), pos = 3) +
     text(colnames(rentdata[10]), rentdata[10],
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
     
      buydata <- select(buydata, "RegionName", "2011", "2012", "2013", "2014", 
             "2015", "2016", "2017", "2018", "2019")
    
    plot(colnames(buydata), buydata, type = "b", col = "green", xlab = "year", 
         ylab = "Value $", main = "Monthly Sales Average") +
      text(colnames(buydata), buydata[1:9],
           paste0("$", buydata), pos = 4) +
      text(colnames(buydata[10]), buydata[10],
                    paste0("$", buydata$'2019'), pos = 1)
  })
}

