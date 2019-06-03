Pop_Dens_by_County <- read.csv("WAOFM_-_April_1_-_Population_Density_by_County__2000_to_Present.csv")
HS_Dropout_by_County <- read.csv("High_School_Dropout_Statistics_by_County_2012-2013_School_Year_5-Year_Cohort_Dropout_Rates.csv")

#home values
#home listings and sales
#rental values
#rental listings
#forecasts
#more metrics

County_Zhvi_3b <- read.csv("County_Zhvi_3bedroom.csv")
County_MedianRentalPrice_3B <- read.csv("County_MedianRentalPrice_3Bedroom.csv")

library(dplyr)
County_Zhvi_3b <- filter(County_Zhvi_3b, State == "WA")
County_MedianRentalPrice_3B <- filter(County_MedianRentalPrice_3B, State == "WA")
County_Zhvi_3b_values <- County_Zhvi_3b[,c(2,8:284)]

library(reshape2)
melted <- melt(County_Zhvi_3b_values)
melted$rowid <- 1:30
library(ggplot2)
ggplot(melted, aes(variable, value, group=factor(RegionName))) +
  geom_line(aes(color=RegionName)) +
  ggtitle("Washington Home Values by County") + 
  xlab("Year (1996-2019)") +
  ylab("Value $")
# can use this data to predict trend line in 10 years


ggplot(data=HS_Dropout_by_County, aes(x=County, y=Rank.by.Percent, fill=County)) +
  geom_bar(stat="identity") +
  ggtitle("HS Dropout Rates by County") +
  ylab("Dropout %")


county <- map_data("county")
county <- filter(county, region == "washington")
ggplot(county)+geom_polygon(aes(long,lat,group=group,col=subregion,fill=subregion))

