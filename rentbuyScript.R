#read the dataset

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



#select useful data and reformat the dataset
rent_wa <- buy %>% filter(State == 'WA')
buy_wa <- rent %>% filter(State == 'WA') 

filtered_rent <- rent_wa[, c(1, 19:118)]
filtered_buy <- buy_wa[, c(2,185:284)]

colnames(filtered_rent) <- gsub("X", "", colnames(filtered_rent))
colnames(filtered_buy) <- gsub("X", "", colnames(filtered_buy))


