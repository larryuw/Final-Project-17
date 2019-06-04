library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)

Pop_Dens_by_County <- read.csv("data/WAOFM_-_April_1_-_Population_Density_by_County__2000_to_Present.csv")
HS_Dropout_by_County <- read.csv("data/High_School_Dropout_Statistics_by_County_2012-2013_School_Year_5-Year_Cohort_Dropout_Rates.csv")
Criminal_Data_by_County <- read.csv("data/Criminal_Justice_Data_Book.csv")
County_Zhvi_3b <- read.csv("data/County_Zhvi_3bedroom.csv")
County_MedianRentalPrice_3B <- read.csv("data/County_MedianRentalPrice_3Bedroom.csv")
County_MedianPrice_3B <- read.csv("data/County_MedianListingPrice_AllHomes.csv")

County_MedianPrice_filtered <- filter(County_MedianPrice_3B, State == "WA") %>% mutate(county 
                                                                                       = str_remove_all(RegionName, " County")) %>% select(-RegionName) %>% select(county,starts_with("X2019"))
County_MedianPrice_filtered$county <- sapply(as.list(County_MedianPrice_filtered$county), tolower)
Criminal_Data_by_County_filtered <- select(Criminal_Data_by_County, year, county, 
                                           SRS_TOTAL) %>% group_by(county)%>% mutate(TotalAvg = 
                                                                                       sum(SRS_TOTAL)/16) %>% filter(year == "1990") %>% select(county, TotalAvg)
Criminal_Data_by_County_filtered$county <- sapply(as.list(Criminal_Data_by_County_filtered$county), tolower)


Pop_Dens_by_County_filtered <- select(Pop_Dens_by_County, COUNTY, POPDEN_2018, Location.1)
colnames(Pop_Dens_by_County_filtered)[colnames(Pop_Dens_by_County_filtered)=="COUNTY"] <- "county"
Pop_Dens_by_County_filtered$county <- sapply(as.list(Pop_Dens_by_County_filtered$county), tolower)


HS_Dropout_by_County_filtered <- select(HS_Dropout_by_County, County, Rank.by.Percent)
colnames(HS_Dropout_by_County_filtered)[colnames(HS_Dropout_by_County_filtered)=="County"] <- "county"
HS_Dropout_by_County_filtered$county <- sapply(as.list(HS_Dropout_by_County_filtered$county), tolower)


merged_filtered_half_data <- full_join(Criminal_Data_by_County_filtered, 
                                       Pop_Dens_by_County_filtered, by = "county")
merged_filtered_data <- full_join(merged_filtered_half_data, 
                                  HS_Dropout_by_County_filtered, by = "county")


County_Zhvi_3b <- filter(County_Zhvi_3b, State == "WA")
County_MedianRentalPrice_3B <- filter(County_MedianRentalPrice_3B, State == "WA")

County_Zhvi_3b_values <- County_Zhvi_3b[,c(2,8:284)]




melted <- melt(County_Zhvi_3b_values, c("RegionName"))
melted$rowid <- 1:30

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
