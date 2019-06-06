library("shiny")
library("dplyr")
library("stringr")
library("reshape2")
library("ggplot2")
library("tidyr")


Pop_Dens_by_County <- read.csv("data/WAOFM_-_April_1_-_Population_Density_by_County__2000_to_Present.csv",
                               stringsAsFactors = FALSE)
HS_Dropout_by_County <- read.csv("data/High_School_Dropout_Statistics_by_County_2012-2013_School_Year_5-Year_Cohort_Dropout_Rates.csv",
                                 stringsAsFactors = FALSE)
Criminal_Data_by_County <- read.csv("data/Criminal_Justice_Data_Book.csv", stringsAsFactors = FALSE)
County_Zhvi_3b <- read.csv("data/County_Zhvi_3bedroom.csv", stringsAsFactors = FALSE)
County_MedianRentalPrice_3B <- read.csv("data/County_MedianRentalPrice_3Bedroom.csv", stringsAsFactors = FALSE)
County_MedianPrice_3B <- read.csv("data/County_MedianListingPrice_AllHomes.csv", stringsAsFactors = FALSE)

County_MedianPrice_filtered <- filter(County_MedianPrice_3B, State == "WA") %>% mutate(county 
                                                                                       = str_remove_all(RegionName, " County")) %>% select(-RegionName) %>% select(county,starts_with("X2019"))
County_MedianPrice_filtered$county <- sapply(as.list(County_MedianPrice_filtered$county), tolower)
County_MedianPrice_filtered <- County_MedianPrice_filtered %>% mutate(PriceAvg = (X2019.01 + X2019.02 + X2019.03 + X2019.04)/4) %>% select(county, PriceAvg)
#View(County_MedianPrice_filtered)

Criminal_Data_by_County_filtered <- select(Criminal_Data_by_County, year, county, 
                                           SRS_TOTAL) %>% group_by(county)%>% mutate(TotalAvg = 
                                                                                       sum(SRS_TOTAL)/16) %>% filter(year == "1990") %>% select(county, TotalAvg)
Criminal_Data_by_County_filtered$county <- sapply(as.list(Criminal_Data_by_County_filtered$county), tolower)
Criminal_Data_by_County_filtered$rank <- NA


Pop_Dens_by_County_filtered <- select(Pop_Dens_by_County, COUNTY, POPDEN_2018, Location.1)
colnames(Pop_Dens_by_County_filtered)[colnames(Pop_Dens_by_County_filtered)=="COUNTY"] <- "county"
Pop_Dens_by_County_filtered$county <- sapply(as.list(Pop_Dens_by_County_filtered$county), tolower)


HS_Dropout_by_County_filtered <- select(HS_Dropout_by_County, County, Rank.by.Percent)
colnames(HS_Dropout_by_County_filtered)[colnames(HS_Dropout_by_County_filtered)=="County"] <- "county"
HS_Dropout_by_County_filtered$county <- sapply(as.list(HS_Dropout_by_County_filtered$county), tolower)


merged_filtered_half_data <- left_join(Criminal_Data_by_County_filtered, 
                                       Pop_Dens_by_County_filtered, by = "county")
merged_filtered_half_data_more <- left_join(merged_filtered_half_data, 
                                            HS_Dropout_by_County_filtered, by = "county")
merged_filtered_data <- left_join(County_MedianPrice_filtered, merged_filtered_half_data_more, by = "county") %>% select(-Location.1)

merged_filtered_data$rank <- NA
merged_filtered_data$rank[order(-merged_filtered_data$TotalAvg)] <- 1:nrow(merged_filtered_data)
#View(merged_filtered_data)
