
#load necessary libraries
library("dplyr")
library("stringr")
library("reshape2")
library("ggplot2")
library("scales")
library("tidyr")

#Read dataset
Pop_Dens_by_County <- read.csv(paste0("data/WAOFM_-_April_1_-_Population_Density_",
                                      "by_County__2000_to_Present.csv"))

HS_Dropout_by_County <- read.csv(paste0("data/High_School_Dropout_Statistics_by_County_",
                                "2012-2013_School_Year_5-Year_Cohort_Dropout_Rates.csv"))
                                 
Criminal_Data_by_County <- read.csv("data/Criminal_Justice_Data_Book.csv", stringsAsFactors = FALSE)

County_Zhvi_3b <- read.csv("data/County_Zhvi_3bedroom.csv", stringsAsFactors = FALSE)

County_MedianRentalPrice_3B <- read.csv("data/County_MedianRentalPrice_3Bedroom.csv", 
                                        stringsAsFactors = FALSE)

County_MedianPrice_3B <- read.csv("data/County_MedianListingPrice_AllHomes.csv", 
                                  stringsAsFactors = FALSE)


#filter datasets

#MedianPrice Filtered
County_MedianPrice_filtered <- filter(County_MedianPrice_3B, State == "WA") %>%
  mutate(county = str_remove_all(RegionName, " County")) %>%
  select(-RegionName) %>% select(county,starts_with("X2019"))
County_MedianPrice_filtered$county <- sapply(as.list(County_MedianPrice_filtered$county), tolower)


#filter rental price/buying price data
County_Zhvi_3b <- filter(County_Zhvi_3b, State == "WA")
County_MedianRentalPrice_3B <- filter(County_MedianRentalPrice_3B, State == "WA")

County_Zhvi_3b_values <- County_Zhvi_3b[,c(2,8:284)]


#home values sorted datasets
melted <- melt(County_Zhvi_3b_values, c("RegionName"))
melted$variable <- str_remove(melted$variable, "X")
melted$rowid <- 1:30


#medianprice dataset
County_MedianPrice_filtered <- County_MedianPrice_filtered %>% mutate(PriceAvg = (X2019.01 + X2019.02 + X2019.03 + X2019.04)/4) %>% select(county, PriceAvg)

#criminalincidents datasets
Criminal_Data_by_County_filtered <- select(Criminal_Data_by_County, year, county, 
                                           SRS_TOTAL) %>% group_by(county)%>% mutate(TotalAvg = 
                                                                                       sum(SRS_TOTAL)/16) %>% filter(year == "1990") %>% select(county, TotalAvg)
Criminal_Data_by_County_filtered$county <- sapply(as.list(Criminal_Data_by_County_filtered$county), tolower)
Criminal_Data_by_County_filtered$rank <- NA

#populationdensity datasets
Pop_Dens_by_County_filtered <- select(Pop_Dens_by_County, COUNTY, POPDEN_2018, Location.1)
colnames(Pop_Dens_by_County_filtered)[colnames(Pop_Dens_by_County_filtered)=="COUNTY"] <- "county"
Pop_Dens_by_County_filtered$county <- sapply(as.list(Pop_Dens_by_County_filtered$county), tolower)

#HSDropout datasets
HS_Dropout_by_County_filtered <- select(HS_Dropout_by_County, County, Rank.by.Percent)
colnames(HS_Dropout_by_County_filtered)[colnames(HS_Dropout_by_County_filtered)=="County"] <- "county"
HS_Dropout_by_County_filtered$county <- sapply(as.list(HS_Dropout_by_County_filtered$county), tolower)


#combine all data into one joint file
merged_filtered_half_data <- left_join(Criminal_Data_by_County_filtered, 
                                       Pop_Dens_by_County_filtered, by = "county")
merged_filtered_half_data_more <- left_join(merged_filtered_half_data, 
                                            HS_Dropout_by_County_filtered, by = "county")
merged_filtered_data <- left_join(County_MedianPrice_filtered, merged_filtered_half_data_more, by = "county") %>% select(-Location.1)

merged_filtered_data$rank <- NA
merged_filtered_data$rank[order(-merged_filtered_data$TotalAvg)] <- 1:nrow(merged_filtered_data)



################
#select useful data and reformat the dataset
rent_wa <- County_MedianRentalPrice_3B %>% filter(State == 'WA')
buy_wa <- County_Zhvi_3b %>% filter(State == 'WA') 

filtered_rent <- rent_wa[, c(1, 19:118)]
filtered_buy <- buy_wa[, c(2,185:284)]

colnames(filtered_rent) <- gsub("X", "", colnames(filtered_rent))
colnames(filtered_buy) <- gsub("X", "", colnames(filtered_buy))
